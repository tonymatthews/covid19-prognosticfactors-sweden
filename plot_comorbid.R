
setwd("C:/Users/antmat/OneDrive/ki/projects/covid19-burden-sweden")

library(readr)
library(dplyr)
devtools::install_github('reinholdsson/swemaps')
library(swemaps)
library(ggplot2)
library(gridExtra)
library(purrr)
library(cowplot)

over70 <- read_delim("data/over70_county", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
          select(-age_events_str, -age_prop)

dat <- read_delim("data/comorbidities_county_3y", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  merge(over70, by=c("county", "n")) %>% 
  transform(county = as.numeric(county)) %>% 
  select(-cvd_events_str, -cancer_events_str, -copd_events_str, -asthma_events_str, -diabetes_events_str, 
  -cvd_prop, -cancer_prop, -copd_prop, -asthma_prop, -diabetes_prop) %>% 
 
  mutate(total_n=sum(n)) %>% 
  
  mutate(total_cvd_events=sum(cvd_events)) %>% 
  mutate(cvd_prop=cvd_events/n) %>% 
  mutate(total_cvd_prop=total_cvd_events/total_n) %>%
  mutate(Cardiovascular=round(cvd_prop/total_cvd_prop, 2)) %>% 
  select(-cvd_events, -total_cvd_events, -cvd_prop, -total_cvd_prop) %>% 
  
  mutate(total_cancer_events=sum(cancer_events)) %>% 
  mutate(cancer_prop=cancer_events/n) %>% 
  mutate(total_cancer_prop=total_cancer_events/total_n) %>%
  mutate(Cancer=round(cancer_prop/total_cancer_prop, 2)) %>% 
  select(-cancer_events, -total_cancer_events, -cancer_prop, -total_cancer_prop) %>%
  
  mutate(total_copd_events=sum(copd_events)) %>% 
  mutate(copd_prop=copd_events/n) %>% 
  mutate(total_copd_prop=total_copd_events/total_n) %>%
  mutate(COPD=round(copd_prop/total_copd_prop, 2)) %>% 
  select(-copd_events, -total_copd_events, -copd_prop, -total_copd_prop) %>%
  
  mutate(total_asthma_events=sum(asthma_events)) %>% 
  mutate(asthma_prop=asthma_events/n) %>% 
  mutate(total_asthma_prop=total_asthma_events/total_n) %>%
  mutate(Asthma=round(asthma_prop/total_asthma_prop, 2)) %>% 
  select(-asthma_events, -total_asthma_events, -asthma_prop, -total_asthma_prop) %>%
  
  mutate(total_diabetes_events=sum(diabetes_events)) %>% 
  mutate(diabetes_prop=diabetes_events/n) %>% 
  mutate(total_diabetes_prop=total_diabetes_events/total_n) %>%
  mutate(Diabetes=round(diabetes_prop/total_diabetes_prop, 2)) %>% 
  select(-diabetes_events, -total_diabetes_events, -diabetes_prop, -total_diabetes_prop) %>%
  
  mutate(total_age_events=sum(age_events)) %>% 
  mutate(age_prop=age_events/n) %>% 
  mutate(total_age_prop=total_age_events/total_n) %>%
  mutate(age=round(age_prop/total_age_prop, 2)) %>% 
  select(-age_events, -total_age_events, -age_prop, -total_age_prop) %>%
  
  select(-n, -total_n)

mapdata <- map_ln %>% 
  transform(lnkod = as.numeric(lnkod)) %>% 
  rename(county=lnkod)

y <- merge(mapdata, dat, by="county")

outcomes = names(y)[11:15]
outcomes = set_names(outcomes)

map_fun = function(x) {

ggplot(y, aes_string('ggplot_long', 'ggplot_lat', group = 'county', fill = x)) +
          scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
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

age <- ggplot(y, aes_string('ggplot_long', 'ggplot_lat', group = 'county', fill = 'age')) +
          scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
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
          ggtitle("70+ years")

legend_plot <-ggplot(y, aes_string('ggplot_long', 'ggplot_lat', group = 'county', fill = 'Diabetes')) +
        scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
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

final <- plot_grid(plotlist = all_plots, legend,age, nrow = 1)

ggsave2(final, file="plot_comorbid.png", width = 9.89, height = 5.61)


