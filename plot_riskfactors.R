
setwd("C:/Users/antmat/OneDrive/ki/projects/covid19-prognosticfactors-sweden")

library(readr)
library(dplyr)
#devtools::install_github('reinholdsson/swemaps')
library(swemaps)
library(ggplot2)
library(gridExtra)
library(purrr)
library(cowplot)

dat <- read_delim("data/numriskfactors_3y", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  rename(county=LKF) %>% 
  transform(county = as.numeric(county)) %>% 
  select(-oneevent_str, -twoevents_str, -threeevents_str, -prop_oneevent, -prop_twoevents, -prop_threeevents) %>% 
  
  mutate(total_n=sum(n)) %>% 
  
  mutate(total_oneevent=sum(oneevent)) %>% 
  mutate(oneevent_prop=oneevent/n) %>% 
  mutate(total_oneevent_prop=total_oneevent/total_n) %>%
  mutate(One=round(oneevent_prop/total_oneevent_prop, 2)) %>% 
  select(-oneevent, -total_oneevent, -oneevent_prop, -total_oneevent_prop) %>% 
  
  mutate(total_twoevents=sum(twoevents)) %>% 
  mutate(twoevents_prop=twoevents/n) %>% 
  mutate(total_twoevents_prop=total_twoevents/total_n) %>%
  mutate(Two=round(twoevents_prop/total_twoevents_prop, 2)) %>% 
  select(-twoevents, -total_twoevents, -twoevents_prop, -total_twoevents_prop) %>% 
  
  mutate(total_threeevents=sum(threeevents)) %>% 
  mutate(threeevents_prop=threeevents/n) %>% 
  mutate(total_threeevents_prop=total_threeevents/total_n) %>%
  mutate(Three=round(threeevents_prop/total_threeevents_prop, 2)) %>% 
  select(-threeevents, -total_threeevents, -threeevents_prop, -total_threeevents_prop) %>%
  
  select(-n, -total_n)


mapdata <- map_ln %>% 
  transform(lnkod = as.numeric(lnkod)) %>% 
  rename(county=lnkod)

y <- merge(mapdata, dat, by="county")

y <- prepare_map_data(y)

outcomes = names(y)[11:13]
outcomes = set_names(outcomes)  

map_fun = function(x) {
  
  ggplot(y, aes_string('ggplot_long', 'ggplot_lat', group = 'county', fill = x)) +
    scale_fill_continuous(low="#e7110c", high="#5b0705") + #dd120c
    geom_polygon() +
    coord_equal() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_blank(), 
          panel.background = element_blank(), axis.title.x=element_blank(),
          axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(), axis.text.y=element_blank(), 
          axis.ticks.y=element_blank(), 
          legend.position="none",
          plot.title=element_text(hjust=0.7)) +
    ggtitle(as.character(x))

}

all_plots = map(outcomes, ~map_fun(.x))  

legend_plot <- ggplot(y, aes_string('ggplot_long', 'ggplot_lat', group = 'county', fill = 'Two')) +
                  scale_fill_continuous(low="#f76662", high="#5b0705") +
                  geom_polygon() +
                  coord_equal() +
                  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_blank(), 
                        panel.background = element_blank(), axis.title.x=element_blank(),
                        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                        axis.title.y=element_blank(), axis.text.y=element_blank(), 
                        axis.ticks.y=element_blank()) +
                  labs(fill="Ratio")

legend <- get_legend(legend_plot)

final <-  plot_grid(plotlist = all_plots, legend, nrow = 1)

ggsave2(final, file="plot_riskfactors.png", width = 9.89, height = 5.61)



