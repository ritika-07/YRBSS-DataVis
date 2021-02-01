library(data.table)
library(tidyverse)
library(plotly)
library(ggthemes)
library(dplyr)
library(shiny)
demo <- read.csv("data/demographic.csv")
answers <- read.csv("data/weighted_2017.csv")
cdc_data2 = read.csv("data/MICE_Impute.csv")
code = read.csv("data/YRBSS_codebook.csv")


total <- nrow(demo)
gender <- demo %>%  group_by(sex) %>%  summarise(total = n())
girls <- gender %>% filter(sex == "Female") %>% select(total)

boys <- gender %>% filter(sex == "Male") %>% select(total)

median_grade <- median(na.omit(substr(demo$grade,1, str_locate(demo$grade, "th grade")[,1]-1)))

median_age <- median(na.omit(demo$age))

raceeth <- demo %>% select(raceeth,sex) %>% group_by(raceeth,sex) %>% summarise(total = n())

raceeth_gender <- dcast(raceeth, raceeth +  total ~ sex)

ages <- demo %>% select(age,sex) %>%  group_by(age,sex) %>% summarise(total = n())

age_gender <- dcast(ages, age +  total ~ sex)

height_kde <- demo %>% filter(sex == "Male" | sex == "Female") 

kde_height <- ggplot(height_kde, aes(x = height)) + 
    geom_density(aes(fill = sex), alpha = 0.5) + ggtitle("KDE Of Heights by Gender")

kde_height <- ggplotly(kde_height)

kde_weight <- ggplot(height_kde, aes(x = weight)) + geom_density(aes(fill = sex, color= sex), 
    alpha = 0.5) + ggtitle("KDE Of Weight by Gender")

kde_weight <- ggplotly(kde_weight)

## PLOTS 
gender_race_plot <- raceeth_gender %>% 
  plot_ly() %>%
  add_trace(x = ~raceeth, y = ~Male, type = 'bar', 
            text = ~Male, textposition = 'auto',
            name = "Male",
            marker = list(color = 'rgb(65,105,225)',
                          line = list(color = 'rgb(65,105,225)', width = 1.5))) %>%
  add_trace(x = ~raceeth, y = ~Female, type = 'bar', 
            text = ~Female, textposition = 'auto',
            name = "Female",
            marker = list(color = 'rgb(220,20,60)',
                          line = list(color = 'rgb(220,20,60)', width = 1.5))) %>%
  layout(title = "Race and Ethnicity By Gender",
         barmode = 'group',
         xaxis = list(title = ""),
         yaxis = list(title = ""))


age_gender_plot <- age_gender %>% 
  plot_ly() %>%
  add_trace(x = ~age, y = ~Male, type = 'bar', 
            text = ~Male, textposition = 'auto',
            name = "Male",
            marker = list(color = 'rgb(65,105,225)',
                          line = list(color = 'rgb(65,105,225)', width = 1.5))) %>%
  add_trace(x = ~age, y = ~Female, type = 'bar', 
            text = ~Female, textposition = 'auto',
            name = "Female",
            marker = list(color = 'rgb(220,20,60)',
                          line = list(color = 'rgb(220,20,60)', width = 1.5))) %>%
  layout(title = "Age By Gender",
         barmode = 'group',
         xaxis = list(title = ""),
         yaxis = list(title = ""))
#-----------------------------DRUG USE---------------------------------#

druguse <- read.csv("data/XXHq_2017.csv")

### ADD QUESTION 49

usage <- druguse %>%
  select(q50, q51,q52,q53,q56) %>% 
  mutate(drug_use = case_when(
    q50==1 & q51==1 & q52==1 & q53==1 & q56==1 ~ 0,
    q50!=1 | q51!=1 | q52!=1 | q53!=1 | q56!=1 ~ 1)) %>% 
  select(drug_use)

questions <- subset(druguse, select = c(q50,q51,q52,q53,q56))
names(questions) <- c("Inhalants", "Heroin", "Methamphetamines","MDMA","Opioids")

use_demo <- cbind(usage, demo,questions)

missing_pred <- is.na(usage)
use_demo <- use_demo[!missing_pred,]
use_demo$drug_use <- as.factor(use_demo$drug_use)
use_demo$age <- as.factor(use_demo$age)

total_use <- nrow(use_demo)

drug_eth <- use_demo %>%
  ggplot(aes(x = raceeth, weight = total_use)) +
  geom_bar(aes(fill = drug_use), position = "fill") +
  coord_flip() +
  labs(y="Proportion", x = "") +
  scale_fill_manual(values = c("#d0a3ff", "#a360fb"),
                    labels=c("Never Used", "Used at Least Once"),
                    name="",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"), 
        legend.position="top")  


drug_grade <- use_demo %>%
  ggplot(aes(x = grade, weight = total_use)) +
  geom_bar(aes(fill = drug_use), position = "fill") +
  coord_flip() +
  labs(y="Proportion", x = "") +
  scale_fill_manual(values = c("#d0a3ff", "#a360fb"),
                    labels=c("Never Used", "Used at Least Once"),
                    name="",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"),
        legend.position="top")  

drug_age <- use_demo %>%
  ggplot(aes(x = age, weight = total_use)) +
  geom_bar(aes(fill = drug_use), position = "fill") +
  coord_flip() +
  labs(y="Proportion", x = "") +
  scale_fill_manual(values = c("#d0a3ff", "#a360fb"),
                    labels=c("Never Used", "Used at Least Once"),
                    name="",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"),
        legend.position="top") 


drug_gen <- use_demo %>%
  ggplot(aes(x = sex, weight = total_use)) +
  geom_bar(aes(fill = drug_use), position = "fill") +
  coord_flip() +
  labs(y="Proportion", x = "") +
  scale_fill_manual(values = c("#d0a3ff", "#a360fb"),
                    labels=c("Never Used", "Used at Least Once"),
                    name="",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"),
        legend.position="top")  


drug_wt <- use_demo %>%
  ggplot(aes(x = weight_status, weight = total_use)) +
  geom_bar(aes(fill = drug_use), position = "fill") +
  coord_flip() +
  labs(y="Proportion", x = "") +
  scale_fill_manual(values = c("#d0a3ff", "#a360fb"),
                    labels=c("Never Used", "Used at Least Once"),
                    name="",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"),
        legend.position="top")  


only_users <- use_demo %>% 
  filter(drug_use == 1) 

full_demo_qs <- only_users %>% 
  mutate(Inhalants = ifelse(Inhalants == 1, "Never",
        ifelse(Inhalants == 2, "1-2 times",
          ifelse(Inhalants == 3, "3-9 times",
            ifelse(Inhalants == 4, "10-19 times",
              ifelse(Inhalants == 5, "20-39 times","40+ times")))))) %>% 
  mutate(Heroin = ifelse(Heroin == 1, "Never",
                            ifelse(Heroin == 2, "1-2 times",
                                   ifelse(Heroin == 3, "3-9 times",
                                          ifelse(Heroin == 4, "10-19 times",
                                                 ifelse(Heroin == 5, "20-39 times","40+ times")))))) %>% 
  mutate(Methamphetamines = ifelse(Methamphetamines == 1, "Never",
                            ifelse(Methamphetamines == 2, "1-2 times",
                                   ifelse(Methamphetamines == 3, "3-9 times",
                                          ifelse(Methamphetamines == 4, "10-19 times",
                                                 ifelse(Methamphetamines == 5, "20-39 times","40+ times")))))) %>% 
  mutate(Opioids = ifelse(Opioids== 1, "Never",
                            ifelse(Opioids == 2, "1-2 times",
                                   ifelse(Opioids == 3, "3-9 times",
                                          ifelse(Opioids == 4, "10-19 times",
                                                 ifelse(Opioids == 5, "20-39 times","40+ times")))))) %>% 
  mutate(MDMA = ifelse(MDMA == 1, "Never",
                            ifelse(MDMA == 2, "1-2 times",
                                   ifelse(MDMA == 3, "3-9 times",
                                          ifelse(MDMA == 4, "10-19 times",
                                                 ifelse(MDMA == 5, "20-39 times","40+ times"))))))


####

hero <- as.data.frame(table(full_demo_qs$Heroin,full_demo_qs$sex))
names(hero) <- c("Times","Sex", "Heroin")

inha <- as.data.frame(table(full_demo_qs$Inhalants,full_demo_qs$sex))
names(inha) <- c("Times2","Sex2", "Inhalants")

meth <- as.data.frame(table(full_demo_qs$Methamphetamines,full_demo_qs$sex))
names(meth) <- c("Times3","Sex3", "Methamphetamines")

opi <- as.data.frame(table(full_demo_qs$Opioids,full_demo_qs$sex))
names(opi) <- c("Times4","Sex4", "Opioids (Illicit)")

mdma <- as.data.frame(table(full_demo_qs$MDMA,full_demo_qs$sex))
names(mdma) <- c("Times5","Sex5", "MDMA")

lf_usage <- cbind(hero,inha,meth,opi,mdma)

lf_usage <- lf_usage %>% 
  select(Sex,Times,Heroin,Inhalants,Methamphetamines,"Opioids (Illicit)",MDMA) %>% 
  filter(Times != "Never")


lf_usage2 <- melt(lf_usage, id = c("Times","Sex"))
names(lf_usage2) <- c("Times", "Sex", "Drug","n_times")
lf_usage2$Drug <- as.factor(lf_usage2$Drug)


lf_usage2$Times <- factor(lf_usage2$Times , levels = c("1-2 times", 
                                                       "3-9 times", 
                                                       "10-19 times",
                                                       "20-39 times",
                                                       "40+ times"))



lf_use_plot <- 
  lf_usage2 %>%
  ggplot(aes(x = Drug, weight = n_times) ) +
  geom_bar(aes(fill = Times), position = "fill") +
  coord_flip() +
  labs(y="", x = "") +
  scale_fill_brewer(palette="Purples",direction = 1) 
    
total_per_drug <- lf_usage2 %>%  group_by(Drug) %>%  summarise(total = sum(n_times))

total_per_drug$percent <- (total_per_drug$total/total)

hero_risk <- total_per_drug %>% filter(Drug == "Heroin") %>% select(percent)
inha_risk <- total_per_drug %>% filter(Drug == "Inhalants") %>% select(percent)
meth_risk <- total_per_drug %>% filter(Drug == "Methamphetamines") %>% select(percent)
opi_risk <- total_per_drug %>% filter(Drug == "Opioids (Illicit)") %>% select(percent)
mdma_risk <- total_per_drug %>% filter(Drug == "MDMA") %>% select(percent)

hero_risk <- paste(round((hero_risk)*100,digits=2),"%",sep="")
inha_risk <- paste(round((inha_risk)*100,digits=2),"%",sep="")
meth_risk <- paste(round((meth_risk)*100,digits=2),"%",sep="")
opi_risk <- paste(round((opi_risk)*100,digits=2),"%",sep="")



gen_drug_u <- lf_usage2 %>% 
  select(Sex, Drug, n_times) %>% 
  group_by(Sex, Drug) %>% 
  summarise(t_times = sum(n_times)) %>% 
  ungroup()
  

f_gen_drug_u <- gen_drug_u %>%filter(Sex=="Female") %>%  select(Drug,t_times) 
names(f_gen_drug_u) <- c("Drug","Female")
  
m_gen_drug_u <- gen_drug_u %>%filter(Sex=="Male") %>%  select(t_times) 
names(m_gen_drug_u) <- c("Male")

use_gen <- cbind(f_gen_drug_u,m_gen_drug_u)


gen_drug_use <- use_gen %>% 
  plot_ly() %>%
  add_trace(x = ~Drug, y = ~Male, type = 'bar', 
            text = ~Male, textposition = 'auto',
            name = "Male",
            marker = list(color = 'rgb(65,105,225)',
                          line = list(color = 'rgb(65,105,225)', width = 1.5))) %>%
  add_trace(x = ~Drug, y = ~Female, type = 'bar', 
            text = ~Female, textposition = 'auto',
            name = "Female",
            marker = list(color = 'rgb(220,20,60)',
                          line = list(color = 'rgb(220,20,60)', width = 1.5))) %>%
  layout(title = "",
         barmode = 'group',
         xaxis = list(title = ""),
         yaxis = list(title = ""))  


###### PLOTS violence
cdc_data2 = dplyr::select(cdc_data2, -1, -7, - 8,  -100, -101, -103, -104)

fill_options <-colnames(cdc_data2)

######### Weapons

wep_all = (1711/11738)
wep_all = paste(round((wep_all)*100,digits=2),"%",sep="")

wep_tosch = (604/14507)
wep_tosch = paste(round((wep_tosch)*100,digits=2),"%",sep="")

wep_gun = (728/14195)
wep_gun = paste(round((wep_gun)*100,digits=2),"%",sep="")

wep_inj = (934/14702)
wep_inj = paste(round((wep_inj)*100,digits=2),"%",sep="")

##### Bully 


sch_risk=  (2665/14606)
sch_risk =  paste(round((sch_risk)*100,digits=2),"%",sep="")

sch_resp = (14606/14765)
sch_resp = paste(round((sch_resp)*100,digits=2),"%",sep="")

elec_risk = (2113/14595)
elec_risk = paste(round((elec_risk)*100,digits=2),"%",sep="")

elec_resp = (14595/14765)
elec_resp = paste(round((elec_resp)*100,digits=2),"%",sep="")


#- 142 of the 192 schools participated = 75% 
per_school = paste(round((.75)*100,digits=2),"%",sep="")
  
#- 14,956 of the 18,324 sampled students submitted questionnaires = 81% student response
per_submit = paste(round((.81)*100,digits=2),"%",sep="")

#- Overall response = 75% * 80% = 60% overall response
overall_per = paste(round((.60)*100,digits=2),"%",sep="")



