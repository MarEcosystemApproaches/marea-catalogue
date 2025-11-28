# make figures for amo chapter

library(marea)
library(dplyr)

data("amo")
amo@data %>% glimpse()
plot(amo, style = "plain")
?marea::plot
