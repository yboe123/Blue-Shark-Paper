library(dplyr)
library(moveHMM)
library(readr)
library(ggplot2)

# Load dataset
data <- read_csv("all_bsh_pred-collocated.csv")

# Select a specific shark ID
shark_id <- "160424_2014_134176.01" 

# Filter data for the selected shark
single_shark <- data %>%
  filter(id == shark_id) %>%
  dplyr::select(id, date, lon, lat) %>%
  mutate(date = as.POSIXct(date, format = "%d/%m/%Y")) %>%
  arrange(date)

# Prepare data
single_shark_df <- as.data.frame(single_shark)
prep <- prepData(single_shark_df, type = "LL", coordNames = c("lon", "lat"))

## Initial parameters
mu0 <- c(5, 2.5)  # step mean
sigma0 <- c(2.5, 1.2) # step SD
stepPar0 <- c(mu0,sigma0)
angleMean0 <- c(0, 0) # angle mean
kappa0 <- c(8, 1) # angle concentration
anglePar0 <- c(angleMean0, kappa0)

# Fit HMM
m <- fitHMM(data=prep,
            nbStates=2,
            stepPar0=stepPar0,
            anglePar0=anglePar0)

# Decode behavior 
prep$state <- viterbi(m)

# Plot behavior states
ggplot(prep, aes(x = x, y = y, color = factor(state))) +
  geom_point(size = 2) +
  geom_path(aes(group = 1), linewidth = 0.6) +
  geom_point(size = 2) +
  coord_fixed() +
  scale_color_manual(values = c("1" = "blue", "2" = "red"),labels = c("1" = "Travelling", "2" = "Resident")) +
  labs(color = "Behavior State", title = paste("Shark Behavior Classification - ID:", shark_id)) +
  theme_minimal()

# Summarize step length and angle parameters to determine if initial parameter need to be adjusted
summary(prep$angle)
quantile(
  prep$angle, probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
  na.rm = TRUE
)

summary(prep$step)
quantile(
  prep$step, probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
  na.rm = TRUE
)

print(m)

