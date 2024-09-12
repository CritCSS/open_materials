library(tidyverse)
library(tufte)
library(ggthemes)

#gifs made using https://ezgif.com/maker

for (df in 3:15) {
 
  ggplot(data = data.frame(x = c(0,30)), aes(x)) +
    stat_function(fun = dchisq, n = 1000, args = list(df = df)) + 
    labs(x='',y="", title = "Chi-Squared distribution",
         subtitle = paste0("Degrees of freedom: ",df)) +
    scale_y_continuous(breaks = NULL)+
    geom_vline(xintercept = df,linetype = "dashed")+
    theme_tufte()
  
  ggsave(paste0("materials/4_descriptive_stats/img/gifs/chi_",df,".png"),bg="white",width = 5,height = 3)
   
}


for (m in c(-5,0,5)) {
  
  for (sd in 1:4) {

ggplot(data = data.frame(x = c(-10,10)), aes(x)) +
  stat_function(fun = dnorm, n = 1000, args = list(mean = m, sd = sd)) + 
  labs(x='',y="", title = "Normal distribution",
       subtitle = paste0("Mean: ",m, "; standard deviation: ",sd)) +
  scale_y_continuous(breaks = NULL)+
  geom_vline(xintercept = m,linetype = "dashed")+
  theme_tufte()
    
    aux <- m+5
    
    ggsave(paste0("materials/4_descriptive_stats/img/gifs/normal_",aux,"_",sd,".png"),bg="white",width = 5,height = 3)
    
}
}
