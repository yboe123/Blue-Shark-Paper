library(dplyr)
library(moveHMM)
library(readr)
library(ggplot2)

# Load the data
data <- read_csv("all_bsh_pred-collocated.csv")

# Get shark IDs
shark_ids <- unique(data$id)

for (shark_id in shark_ids) {
  message(paste("Processing shark ID:", shark_id))
  
  # Filter for the shark
  single_shark <- data %>%
    filter(id == shark_id) %>%
    dplyr::select(id, date, lon, lat) %>%
    mutate(date = as.POSIXct(date, format = "%d/%m/%Y")) %>%
    arrange(date)
  
  # Skip if less than 30 standardised location points
  if (nrow(single_shark) < 30) {
    message(paste("Skipping shark ID:", shark_id, "- less than 30 points"))
    next
  }
  
  # Prepare data for HMM
  single_shark_df <- as.data.frame(single_shark)
  prep <- prepData(single_shark_df, type = "LL", coordNames = c("lon", "lat"))
  
  # Set initial parameters
  mu0 <- c(15, 3)  # step mean
  sigma0 <- c(3, 2) # step SD
  stepPar0 <- c(mu0,sigma0)
  angleMean0 <- c(0, 0) # angle mean
  kappa0 <- c(15, 1) # angle concentration
  anglePar0 <- c(angleMean0, kappa0)
  
  # Fit HMM and decode state
  tryCatch({
    m <- fitHMM(data = prep,
                nbStates = 2,
                stepPar0 = stepPar0,
                anglePar0 = anglePar0)
    
    prep$state <- viterbi(m)

    # Create plot
    ggplot(prep, aes(x = x, y = y, color = factor(state))) +
      geom_point(size = 2) +
      geom_path(aes(group = 1), linewidth = 0.6) +
      geom_point(size = 2) +
      coord_fixed() +
      scale_color_manual(values = c("1" = "blue", "2" = "red"),labels = c("1" = "Travelling", "2" = "Resident")) +
      labs(color = "Behavior State", title = paste("Shark Behavior Classification - ID:", shark_id)) +
      theme_minimal()
    
    # Display plots
    print(p)
    
  })
}