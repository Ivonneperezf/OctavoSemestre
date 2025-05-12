library(tidyverse)
library(tsibble)
library(readxl)

# Carga de datos
df <- read_excel("gasoline.xlsx")
str(df)

#Building the dataset
df_tidy <- df %>%
  mutate(gasoline = case_when(gasoline - lag(gasoline) < 0 ~ "down",
                              gasoline - lag(gasoline) > 0 ~ "up", TRUE ~ "steady") %>% as.factor(),
         xe_lag = lag(xe),
         brent_lag = lag(brent),
         date=as.Date(date)
         ) %>%
  as_tsibble() %>%
  fill_gaps() %>% #makes regular time series by filling the time gaps
  #fills in NAs with previous values
  fill(-date,.direction = "down") %>%
  na.omit() %>%
  as.data.frame()

#Treemap of factor
library(treemap)
df_tidy %>% 
  count(gasoline) %>%
  mutate(label=paste(gasoline,scales::percent(n/sum(n)),sep = "\n")) %>%
  treemap(
    index="label",
    vSize="n",
    title="",
    palette="Accent",
    border.col=c("black"),
    border.lwds=1,
    fontcolor.labels="white",
    fontface.labels=1,
    inflate.labels=F 
    )

library(adabag) #Boosting
library(ipred) #Bagging
library(caret) #Bagging control object
library(vcd) #Kappa
library(plotly) #interactive plot
# Bagging
ctrl <- trainControl(method = "cv", number = 10)
kappa_bagg <-
  lapply(
    1:20,
    function(x){
      set.seed(x)
      train(gasoline ~ ., 
            data = df_tidy,
            method = "treebag",
            trControl = ctrl)$results[["Kappa"]]}
    ) %>% 
  unlist()

# Boosting
kappa_boost <-
  lapply(
    1:20,
    function(x){
      set.seed(x)
      boosting.cv(gasoline ~ ., data = df_tidy) %>%
        .$confusion %>%
        Kappa() %>% 
        .$Unweighted %>%
        .[[1]]}
    ) %>%
  unlist()

# Kappa simulation on a plotly chart 
kappa_plot <-
  data.frame( 
    seed=rep(1:20, 2), 
    kappa= c(kappa_bagg, kappa_boost), 
    ensembles=c(rep("Bagging", 20), rep("Boosting", 20))
    ) %>% 
  ggplot(aes(x=seed, y=kappa, color=ensembles))+
  geom_point(size=3)+ 
  geom_line(size=1)+ 
  theme_minimal()+ 
  theme(panel.background = element_rect(fill = "midnightblue", color = NA), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank())

ggplotly(kappa_plot) %>% 
  layout(legend=list(orientation="v", y=0.5))