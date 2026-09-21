#devtools::load_all('../mvtweedie') # Thorson et al. 2022 Ecology
#devtools::install_github('James-Thorson-NOAA/mvtweedie')
library(mvtweedie)
library(plyr)
library(tidyverse)
library(reshape)
library(mgcv)

# function for calculating area per eddy zone for normalization
area <- function(tbin) {
  bounds_tbin <- c(tbin,tbin+0.25)
  area_tbin <- pi*(bounds_tbin[2]^2 - bounds_tbin[1]^2)
  return(area_tbin)
}

#### _BEGIN_

#### REAL ####

bsh_pred <- data.table::fread("all_bsh_pred-collocated.csv",header = TRUE)

sel.crawl <- bsh_pred %>% drop_na(etype)

# Grab appropriate data and re-format
dhist <- data.frame(sel.crawl %>% group_by(tbin, etype, instrument_name) %>%
                      dplyr::summarise(n = n(), .groups = "drop")
)

dhist <- dhist %>% pivot_wider(names_from = tbin,values_from = n,values_fill = list(n = 0)) %>%
  arrange(instrument_name, etype) %>% relocate(instrument_name, etype, '0','0.25','0.5','0.75','1','1.25','1.5','1.75','2','2.25') %>%
  as.data.frame()

# Calculate total number of observations
dhist$n <- rowSums(dhist[, 2:11])

# Filter combinations with >= 40 observations
dhist <- subset(dhist, n >= 40)

# Convert to long format
dhist <- melt(
  dhist,
  id = c('instrument_name', 'etype', 'n'),
  variable_name = 'tbin'
)

# Normalize counts per unit area
for (i in 1:nrow(dhist)) {
  dhist$area[i] <- area(
    as.numeric(as.character(dhist$tbin[i]))
  )
}

dhist$resp_norm <- dhist$value / dhist$area

real <- dhist
real$data <- "real"

#### SIMULATED ####

bsh_sim <- data.table::fread("all_bsh_sim-collocated.csv",header = TRUE)

sel.crw <- bsh_sim %>% drop_na(etype)

# Grab appropriate data and re-format
dhist <- data.frame(sel.crw %>% group_by(tbin, etype, instrument_name) %>%
                      dplyr::summarise(n = n(), .groups = "drop")
)

dhist <- dhist %>% pivot_wider(names_from = tbin,values_from = n,values_fill = list(n = 0)) %>%
  arrange(instrument_name, etype) %>% relocate(instrument_name,etype,'0','0.25','0.5','0.75','1','1.25','1.5','1.75','2','2.25') %>%
  as.data.frame()

# Calculate total number of observations
dhist$n <- rowSums(dhist[, 2:11])

# Filter combinations with >= 40 observations
dhist <- subset(dhist, n >= 40)

# Convert to long format
dhist <- melt(dhist,id = c('instrument_name', 'etype', 'n'),variable_name = 'tbin')

# Normalize counts per unit area
for (i in 1:nrow(dhist)) {
  dhist$area[i] <- area(
    as.numeric(as.character(dhist$tbin[i]))
  )
}

dhist$resp_norm <- dhist$value / dhist$area

sim <- dhist
sim$data <- "sim"

### JOINT ###
# Combine data
dhist <- rbind(real,sim)

# establish factors and ordering of 'tbin' factor levels
dhist$tbinF <- ordered(dhist$tbin)
dhist$etype <- factor(dhist$etype)
dhist$data <- factor(dhist$data)

## modelling
gam_dhist = gam(formula = resp_norm ~ 0 + etype*tbinF*data, data = dhist, family = tw)
class(gam_dhist) = c("mvtweedie", class(gam_dhist))

# Model statistics
model_summary <- summary(gam_dhist) #coefficients
estimates <- model_summary$p.coeff # Coefficient estimates
se <- model_summary$se # standard error
z_value <- estimates / se # z-statistics
p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE) # calculate p_values

# Create results table
p_values <- data.frame(term = names(estimates), estimate = estimates, std_error = se, z_value = z_value, p_value = p_value)

print(p_values)
