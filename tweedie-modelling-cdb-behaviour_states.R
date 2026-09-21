#devtools::load_all('../mvtweedie') # Thorson et al. 2022 Ecology
#devtools::install_github('James-Thorson-NOAA/mvtweedie')
library(mvtweedie)
library(plyr)
library(tidyverse)
library(reshape)
library(mgcv)
library(dplyr)

# Function for calculating area per eddy zone for normalization
area <- function(tbin) {
  bounds_tbin <- c(tbin,tbin+0.25)
  area_tbin <- pi*(bounds_tbin[2]^2 - bounds_tbin[1]^2)
  return(area_tbin)
}

#### _BEGIN_

#### REAL ####
bsh_pred <- data.table::fread("all_bsh_pred-collocated.csv",header=T)
sel.crawl <- bsh_pred %>% drop_na(etype)

# Grab appropriate data and re-format
dhist <- data.frame(sel.crawl %>% group_by(tbin, etype, instrument_name, platform) %>% dplyr::summarise(n=n()))

dhist <- dhist %>% pivot_wider(names_from = tbin, values_from = n, values_fill = list(n = 0)) %>% 
  arrange(instrument_name, etype) %>% 
  relocate(instrument_name, etype, '0','0.25','0.5','0.75','1','1.25','1.5','1.75','2','2.25') %>%
  as.data.frame()

# Calculate (and filter by) number of observations per individual*distance*polarity combination
dhist$n <- rowSums(dhist[,2:11])

dhist <- subset(dhist, n >= 40) # total number of data points across bins used to filter

dhist <- melt(dhist, id = c('instrument_name','etype','n','platform'),variable_name='tbin') 

# Normalize counts per unit area
for(i in 1:nrow(dhist)) {
  dhist$area[i] <- area(as.numeric(as.character(dhist$tbin[i])))
}

dhist$resp_norm <- dhist$value/dhist$area # normalized

real <- dhist
real$data <- "real"

#### SIMULATED ####
bsh_sim <- data.table::fread("all_bsh_sim-collocated.csv",header=T)
sel.crw <- bsh_sim %>% drop_na(etype)

# Grab appropriate data and re-format
dhist <- data.frame(sel.crw %>% group_by(tbin, etype, instrument_name, platform) %>% dplyr::summarise(n=n()))

dhist <- dhist %>% pivot_wider(names_from = tbin, values_from = n, values_fill = list(n = 0)) %>% 
  arrange(instrument_name, etype) %>% 
  relocate(instrument_name, etype, '0','0.25','0.5','0.75','1','1.25','1.5','1.75','2','2.25') %>%
  as.data.frame()

# Calculate (and filter by) number of observations per individual*distance*polarity combination
dhist$n <- rowSums(dhist[,2:11])

dhist <- subset(dhist, n >= 40) # total number of data points across bins used to filter

dhist <- melt(dhist, id = c('instrument_name','etype','n','platform'),variable_name='tbin')

# Normalize counts per unit area
for(i in 1:nrow(dhist)) {
  dhist$area[i] <- area(as.numeric(as.character(dhist$tbin[i])))
}

dhist$resp_norm <- dhist$value/dhist$area # normalized

sim <- dhist
sim$data <- "sim"

#### JOINT ####
# Bring real and simulated data together
dhist <- rbind(real,sim)

# Establish factors and ordering of 'tbin' factor levels
dhist$tbinF <- ordered(dhist$tbin)
dhist$etype <- factor(dhist$etype)
dhist$data <- factor(dhist$data)
dhist$platform <- factor(dhist$platform)

# Modelling
gam_dhist = gam(formula = resp_norm ~ 0 + etype*tbinF*data,
                data = dhist,
                family = tw)
class(gam_dhist) = c("mvtweedie", class(gam_dhist))

# GGPlot formatting
newdata = expand.grid("etype"=levels(dhist$etype), "tbinF"=levels(dhist$tbinF), "data"=levels(dhist$data), "platform"=levels(dhist$platform))
newdata$tbinF <- ordered(newdata$tbinF) # ensure ordered factor

pred_dhist = mvtweedie:::predict.mvtweedie(gam_dhist,
                                           se.fit=TRUE,
                                           category_name = "tbinF", # variable across which proportions sum to 1
                                           origdata = dhist,
                                           newdata = newdata)

newdata = cbind(newdata, fit=pred_dhist$fit, se.fit=pred_dhist$se.fit )
newdata$lower = newdata$fit - newdata$se.fit
newdata$upper = newdata$fit + newdata$se.fit

p.real <- newdata %>% filter(data == 'real') %>%
  ggplot(aes(tbinF, fit, color = etype)) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.3)
  ) +
  #facet_wrap(vars(platform,region,data), ncol=2) +
  facet_wrap(vars(platform,data), ncol=2) +
  #ylim(0,0.35) +
  labs(
    x = "Eddy-centric distance",
    y = "Predicted proportion",
  ) +
  scale_color_manual(values = c("-1" = "blue",
                                "1" = "red"),
                     labels = c(
                       "-1" = "Cyclone",
                       "1"  = "Anticyclone")) +
  theme_bw()+
  theme(
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(nrow = 1)
  ) +
  theme(
    legend.title = element_blank()
  )
p.real
#ggsave('hist_mvtweedie_REAL_bsh.png', width=8, height=14, p.real)
#ggsave('hist_mvtweedie_REAL_bsh.pdf', width=8, height=14, p.real)

p.sim <- newdata %>% filter(data == 'sim') %>%
  ggplot(aes(tbinF, fit, color = etype)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  facet_wrap(vars(platform,data), ncol=2) +
  labs(y="Predicted proportion") +
  scale_color_manual(values = c("-1" = "blue",
                                "1" = "red")) +
  theme_bw()
p.sim
#ggsave('hist_mvtweedie_SIM_bsh.png', width=8, height=14, p.sim)
#ggsave('hist_mvtweedie_SIM_bsh.pdf', width=8, height=14, p.sim)

p.ace <- newdata %>% filter(etype == '1') %>%
  ggplot(aes(tbinF, fit, color = data)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  facet_wrap(vars(platform), ncol=2) +
  #ylim(0,0.35) +
  labs(y="Predicted proportion") +
  scale_color_manual(values = c("real" = "red4",
                                "sim" = "red1")) +
  theme_bw()
p.ace
#ggsave('hist_mvtweedie_ACE_bsh.png', width=8, height=14, p.ace)
#ggsave('hist_mvtweedie_ACE_bsh.pdf', width=8, height=14, p.ace)

p.ce <- newdata %>% filter(etype == '-1') %>%
  ggplot(aes(tbinF, fit, color = data)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  facet_wrap(vars(platform), ncol=2) +
  #ylim(0,0.35) +
  labs(y="Predicted proportion") +
  scale_color_manual(values = c("real" = "dodgerblue4",
                                "sim" = "dodgerblue")) +
  theme_bw()
p.ce
#ggsave('hist_mvtweedie_CE_bsh.png', width=8, height=14, p.ce)
#ggsave('hist_mvtweedie_CE_bsh.pdf', width=8, height=14, p.ce)

p2 <- ggplot(newdata,
             aes(tbinF, fit, color = interaction(etype, data))) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.3)
  ) +
  facet_wrap(vars(platform), ncol = 2) +
  labs(
    x = "Eddy-centric distance",
    y = "Predicted proportion",
  ) +
  scale_color_manual(
    values = c(
      "-1.real" = "dodgerblue4",
      "1.real"  = "red4",
      "-1.sim"  = "dodgerblue",
      "1.sim"   = "red1"
    ),
    labels = c(
      "-1.real" = "Cyclone - real",
      "1.real"  = "Anticyclone - real",
      "-1.sim"  = "Cyclone - simulated",
      "1.sim"   = "Anticyclone - simulated"
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(nrow = 1)
  ) +
  theme(
    legend.title = element_blank()
  )
p2
#ggsave('hist_mvtweedie_REALSIMcombined_bsh.png', width=8, height=14, p2)
#ggsave('hist_mvtweedie_REALSIMcombined_bsh.pdf', width=8, height=14, p2)


