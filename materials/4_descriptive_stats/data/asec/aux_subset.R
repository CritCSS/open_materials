library(readr)
library(tidyverse)
pppub22 <- read_csv("materials/4_descriptive_stats/data/asec/pppub22.csv")


base <- pppub22 %>% 
  filter(A_AGE > 18) %>% 
  filter(PEMLR %in% c(1,2)) %>% #employed
  filter(ERN_SRCE == 1) %>% #wage workers
  filter(WSAL_VAL > 0) %>% #in universe 
  select(A_AGE,
         A_SEX,
         #PRDTRACE,
         WSAL_VAL) %>% 
  mutate(A_SEX = case_when(A_SEX == 1~"Male",
                           A_SEX == 2~"Female")
         # ,
         # PRDTRACE = case_when(PRDTRACE == 1~"White only",
         #                      PRDTRACE == 3~"American Indian, Alaskan Native only (AI)",
         #                      PRDTRACE == 7~"White-AI",
         #                      PRDTRACE == 2~"Black only",
         #                      PRDTRACE == 4~"Asian only",
         #                      PRDTRACE == 5~"Hawaiian/Pacific Islander only (HP) ",
         #                      PRDTRACE == 6~"White-Black",
         #                      PRDTRACE == 8~"White-Asian",
         #                      TRUE~"Other combinations")
         )

#head(base)

saveRDS(base, "materials/4_descriptive_stats/data/asec/sub_dataset.RDS")
