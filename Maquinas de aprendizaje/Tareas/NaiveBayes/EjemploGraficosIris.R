library(tidyverse)
data = iris
# Density plot for each species
data %>%
  gather(Attributes, value, 1:4) %>%
  ggplot(aes(x=value, fill=Species)) + 
  geom_density(colour="black", alpha=0.5) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Iris data set",
       subtitle="Density plot for each attribute") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank())
