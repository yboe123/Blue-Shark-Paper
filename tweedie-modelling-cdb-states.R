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

#### Resident ####

bsh_pred <- data.table::fread("bsh_pred_collocated_with_states.csv",header = TRUE)

sel.crawl <- bsh_pred %>% drop_na(etype) %>% filter(bathy <= -800) %>% filter(state == 2)

# Grab appropriate data and re-format
dhist <- data.frame(sel.crawl %>% group_by(tbin, etype, instrument_name, platform) %>%
                      dplyr::summarise(n = n(), .groups = "drop")
)

dhist <- dhist %>% pivot_wider(names_from = tbin,values_from = n,values_fill = list(n = 0)) %>%
  arrange(instrument_name, etype) %>% relocate(instrument_name, etype, '0','0.25','0.5','0.75','1','1.25','1.5','1.75','2','2.25') %>%
  as.data.frame()

# Calculate total number of observations
dhist$n <- rowSums(dhist[, c(
  "0", "0.25", "0.5", "0.75", "1",
  "1.25", "1.5", "1.75", "2", "2.25")
])

# Convert to long format
dhist <- melt(
  dhist,
  id = c('instrument_name', 'etype', 'n', 'platform'),
  variable_name = 'tbin'
)

# Normalize counts per unit area
for (i in 1:nrow(dhist)) {
  dhist$area[i] <- area(
    as.numeric(as.character(dhist$tbin[i]))
  )
}

dhist$resp_norm <- dhist$value / dhist$area

res <- dhist
res$data <- "resident"


#### Traveling ####

sel.crw <- bsh_pred %>% drop_na(etype) %>% filter(bathy <= -800) %>% filter(state == 1)

# Grab appropriate data and re-format
dhist <- data.frame(sel.crw %>% group_by(tbin, etype, instrument_name, platform) %>%
                      dplyr::summarise(n = n(), .groups = "drop")
)

dhist <- dhist %>% pivot_wider(names_from = tbin,values_from = n,values_fill = list(n = 0)) %>%
  arrange(instrument_name, etype) %>% relocate(instrument_name,etype,'0','0.25','0.5','0.75','1','1.25','1.5','1.75','2','2.25') %>%
  as.data.frame()

# Calculate total number of observations
dhist$n <- rowSums(dhist[, c(
  "0", "0.25", "0.5", "0.75", "1",
  "1.25", "1.5", "1.75", "2", "2.25")
])

# Convert to long format
dhist <- melt(dhist,id = c('instrument_name', 'etype', 'n', 'platform'),variable_name = 'tbin')

# Normalize counts per unit area
for (i in 1:nrow(dhist)) {
  dhist$area[i] <- area(
    as.numeric(as.character(dhist$tbin[i]))
  )
}

dhist$resp_norm <- dhist$value / dhist$area

trv <- dhist
trv$data <- "traveling"

#### JOINT ####
# bring real and simulated data together
dhist <- rbind(res,trv)

# establish factors and ordering of 'tbin' factor levels
dhist$tbinF <- ordered(dhist$tbin)
dhist$etype <- factor(dhist$etype)
dhist$data <- factor(dhist$data)

## modelling
gam_dhist = gam(formula = resp_norm ~ etype*tbinF*data,
                data = dhist,
                family = tw)
class(gam_dhist) = c("mvtweedie", class(gam_dhist))


## ggplot formatting
newdata <- expand.grid(
  etype = levels(dhist$etype),
  tbinF = levels(dhist$tbinF),
  data = levels(dhist$data)
)

newdata$tbinF <- ordered(newdata$tbinF)

pred_dhist <- mvtweedie:::predict.mvtweedie(
  gam_dhist,
  se.fit = TRUE,
  category_name = "tbinF",
  origdata = dhist,
  newdata = newdata
)

newdata <- cbind(
  newdata,
  fit = pred_dhist$fit,
  se.fit = pred_dhist$se.fit
)

newdata$lower <- newdata$fit - newdata$se.fit
newdata$upper <- newdata$fit + newdata$se.fit


#### RESIDENT DATA PLOT ####

p.res <- newdata %>%
  filter(data == "resident") %>%
  ggplot(aes(tbinF, fit, color = etype)) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.3)
  ) +
  labs(
    x = "Eddy-centric distance",
    y = "Predicted proportion"
  ) +
  scale_color_manual(
    values = c(
      "-1" = "blue",
      "1" = "red"
    ),
    labels = c(
      "-1" = "Cyclone - resident",
      "1" = "Anticyclone- resident"
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(
    color = guide_legend(nrow = 1)
  )

p.res


#### TRAVELING DATA PLOT ####

p.trv <- newdata %>%
  filter(data == "traveling") %>%
  ggplot(aes(tbinF, fit, color = etype)) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper)
  ) +
  labs(
    x = "Eddy-centric distance",
    y = "Predicted proportion"
  ) +
  scale_color_manual(
    values = c(
      "-1" = "blue",
      "1" = "red"
    ),
    labels = c(
      "-1" = "Cyclone - traveling",
      "1" = "Anticyclone- traveling"
    )
  ) +
  theme_bw()

p.trv


#### ACE PLOT ####

p.ace <- newdata %>%
  filter(etype == "1") %>%
  ggplot(aes(tbinF, fit, color = data)) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper)
  ) +
  labs(
    y = "Predicted proportion"
  ) +
  scale_color_manual(
    values = c(
      "resident" = "red4",
      "traveling" = "red1"
    )
  ) +
  theme_bw()

p.ace


#### CE PLOT ####

p.ce <- newdata %>%
  filter(etype == "-1") %>%
  ggplot(aes(tbinF, fit, color = data)) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper)
  ) +
  labs(
    y = "Predicted proportion"
  ) +
  scale_color_manual(
    values = c(
      "resident" = "dodgerblue4",
      "traveling" = "dodgerblue"
    )
  ) +
  theme_bw()

p.ce


#### COMBINED PLOT ####

p2 <- ggplot(
  newdata,
  aes(tbinF, fit, color = interaction(etype, data))
) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.3)
  ) +
  labs(
    x = "Eddy-centric distance",
    y = "Predicted proportion"
  ) +
  scale_color_manual(
    values = c(
      "-1.resident" = "dodgerblue4",
      "1.resident" = "red4",
      "-1.traveling" = "dodgerblue",
      "1.traveling" = "red1"
    ),
    labels = c(
      "-1.resident" = "Cyclone - resident",
      "1.resident" = "Anticyclone - resident",
      "-1.traveling" = "Cyclone - traveling",
      "1.traveling" = "Anticyclone - traveling"
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(
    color = guide_legend(nrow = 1)
  )

p2

#ggsave('hist_mvtweedie_REALSIMcombined_thresh30.png', width=8, height=14, p2)
#ggsave('hist_mvtweedie_REALSIMcombined_thresh30.pdf', width=8, height=14, p2)

