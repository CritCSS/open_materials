library(readr)
pppub22 <- read_csv("materials/1_intro_I/data/asec/pppub22.csv")

names(pppub22)

length(unique(pppub22$PH_SEQ))

unique(pppub22$A_AGE)
unique(pppub22$A_SEX)
#Values: 1 = Male;2 = Female

unique(pppub22$PEHSPNON)#Are you Spanish, Hispanic, or Latino?

unique(pppub22$PRDTRACE)#Race

unique(pppub22$PEMLR)#Major labor force recode

unique(pppub22$A_HRS1)#How many hrs did ... work last week at all jobs?

unique(pppub22$ERN_YN)#Earnings from employer or net earnings from business/ farm after expenses from longest job during 20.. ?
#1 = yes

unique(pppub22$ERN_VAL)#How much did ... earn from this employer before deductions in 20..? what was ... net earnings from this business/ farm after expenses during 20..?
  
unique(pppub22$ERN_OTR)#wage and salary money earned from other work, y/n
#1 = yes

unique(pppub22$ERN_SRCE)#source of earnings from longest job
#1 = wage and salary

unique(pppub22$WSAL_VAL)#total wage and salary earnings (combined amounts in ern-val, if ern-srce=1, and ws-val)


unique(pppub22$PERLIS)#POVERTY LEVEL OF PERSONS

unique(pppub22$A_GRSWK)#How much does ... usually earn per week at this job before deductions 

#too_small <- pppub22 %>% group_by(A_SEX,PRDTRACE) %>% summarise(n = n())%>%  filter(n < 100) %>% pull(PRDTRACE)

#recod <- unique(pppub22$PRDTRACE[!(pppub22$PRDTRACE %in% too_small)])


base <- pppub22 %>% 
  filter(A_AGE > 18) %>% 
  filter(PEMLR %in% c(1,2)) %>% #employed
  filter(ERN_SRCE == 1) %>% #wage workers
  filter(WSAL_VAL > 0) %>% #in universe 
  mutate(age_grouped = case_when(A_AGE < 35 ~ "Younger than 35",
                                 A_AGE >= 35 ~ "Older than 35")) %>% 
  mutate(PRDTRACE = case_when(PRDTRACE %in% too_small~"Other combinations",
                              PRDTRACE == 1~"White only",
                              PRDTRACE == 3~"American Indian, Alaskan Native only (AI)",
                              PRDTRACE == 7~"White-AI",
                              PRDTRACE == 2~"Black only",
                              PRDTRACE == 4~"Asian only",
                              PRDTRACE == 5~"Hawaiian/Pacific Islander only (HP) ",
                              PRDTRACE == 6~"White-Black",
                              PRDTRACE == 8~"White-Asian")) %>% 
  mutate(A_SEX = case_when(A_SEX == 1~"Male",
                           A_SEX == 2~"Female")) %>% 
  group_by(A_SEX,PRDTRACE,age_grouped) %>% 
  summarise("Average income" = weighted.mean(WSAL_VAL,MARSUPWT),
            Obs = n()) %>% 
  ungroup()

saveRDS(base, "materials/1_intro_I/data/asec/summary_dataset.RDS")

