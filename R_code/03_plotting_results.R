###
### Plotting code for results section
###
### Authors: Claire Kelling
### Created: 06/08/20
### Last Modified: 11/30/21
###

#Libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(xtable)

#start clean
rm(list = ls())

#working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./00_plot_functions.R")

country_names <- c("Pakistan", "Somalia", "Turkey", "Colombia", "Afghanistan", "Iraq", "Nepal")

# Read data
final_data <- read_data_warea(country_names = country_names)


#First round of data processing
final_data2 <- final_data %>% filter(variable == "population") %>% # & Experiment.Number %in% c(1,2)) %>%
  pivot_longer(-c(variable, coefficient, Model, Experiment.Number, summary, country), names_to = "Significance", values_to = "Percentage.of.Runs") %>%
  mutate(Effect = case_when(str_detect(summary, "No Effect") ~ "No Effect",
                            TRUE ~ "Effect"),
         Unit = case_when(str_detect(Significance, "ADM1") ~ "ADM1",
                          str_detect(Significance, "ADM2") ~ "ADM2",
                          str_detect(Significance, "PG05") ~ "PG0.5",
                          str_detect(Significance, "PG025") ~ "PG0.25") %>% as.factor(),
         Significance = str_replace(Significance, "ADM1_", "") %>%
           str_replace("ADM2_", "") %>% str_replace("PG05_", "") %>% str_replace("PG025_", "") %>% as.factor(),
         Truth = case_when(Effect == "Effect" & Significance == "Pos.and.sig." ~ "Truth",
                           Effect == "No Effect" & Significance == "No.effect" ~ "Truth",
                           TRUE ~ "Not Truth"),
         Percentage.of.Runs = 100*Percentage.of.Runs) %>% filter(!is.na(Percentage.of.Runs))
final_data2$Significance <- factor(final_data2$Significance,levels(final_data2$Significance)[c(3,1,2)]) #reorder
final_data2$Unit <- factor(final_data2$Unit,levels(final_data2$Unit)[c(1,2,4,3)]) #reorder



final_data3 <- final_data2 %>%
  mutate(Rate = case_when((Effect == "Effect" & Significance == "No.effect") ~ Percentage.of.Runs,
                          (Effect == "No Effect" & (Significance == "Pos.and.sig." | Significance == "Neg.and.sig.")) ~ Percentage.of.Runs,
                          TRUE ~ NA_real_),
         error_type = case_when((Effect == "Effect" & Significance == "No.effect") ~ "FN",
                                (Effect == "No Effect" & (Significance == "Pos.and.sig." | Significance == "Neg.and.sig.")) ~ "FP",
                                TRUE ~ NA_character_)) %>%
  filter(!is.na(Rate)) %>% 
  group_by_at(c(names(final_data2)[-c(7,8)], "error_type")) %>% summarise(Rate=sum(Rate))%>%
  mutate(acceptable = case_when(error_type == "FP" & Rate <= 5 ~ "Permissable Rate",
                                error_type == "FN" & Rate <= 20 ~ "Permissable Rate",
                                TRUE ~ "Not Permissable Rate"))


final_data3_w_area <- final_data3 %>% filter(variable == "population") %>%
  as.data.frame()

######################################
######################################
###### Without area controls
######################################
######################################
final_data2 <- read_data_woarea(country_names = country_names)

final_data2 <- final_data2 %>% filter(variable == "population") %>% #& Experiment.Number %in% c(1,2)) %>%
  pivot_longer(-c(variable, coefficient, Model, Experiment.Number, summary, country), names_to = "Significance", values_to = "Percentage.of.Runs") %>%
  mutate(Effect = case_when(str_detect(summary, "No Effect") ~ "No Effect",
                            TRUE ~ "Effect"),
         Unit = case_when(str_detect(Significance, "ADM1") ~ "ADM1",
                          str_detect(Significance, "ADM2") ~ "ADM2",
                          str_detect(Significance, "PG05") ~ "PG0.5",
                          str_detect(Significance, "PG025") ~ "PG0.25") %>% as.factor(),
         Significance = str_replace(Significance, "ADM1_", "") %>%
           str_replace("ADM2_", "") %>% str_replace("PG05_", "") %>% str_replace("PG025_", "") %>% as.factor(),
         Truth = case_when(Effect == "Effect" & Significance == "Pos.and.sig." ~ "Truth",
                           Effect == "No Effect" & Significance == "No.effect" ~ "Truth",
                           TRUE ~ "Not Truth"),
         Percentage.of.Runs = 100*Percentage.of.Runs) %>% filter(!is.na(Percentage.of.Runs))
final_data2$Significance <- factor(final_data2$Significance,levels(final_data2$Significance)[c(3,1,2)]) #reorder
final_data2$Unit <- factor(final_data2$Unit,levels(final_data2$Unit)[c(1,2,4,3)]) #reorder


final_data3 <- final_data2 %>%
  mutate(Rate = case_when((Effect == "Effect" & Significance == "No.effect") ~ Percentage.of.Runs,
                          (Effect == "No Effect" & (Significance == "Pos.and.sig." | Significance == "Neg.and.sig.")) ~ Percentage.of.Runs,
                          TRUE ~ NA_real_),
         error_type = case_when((Effect == "Effect" & Significance == "No.effect") ~ "FN",
                                (Effect == "No Effect" & (Significance == "Pos.and.sig." | Significance == "Neg.and.sig.")) ~ "FP",
                                TRUE ~ NA_character_)) %>%
  filter(!is.na(Rate)) %>% 
  group_by_at(c(names(final_data2)[-c(7,8)], "error_type")) %>% summarise(Rate=sum(Rate))%>%
  mutate(acceptable = case_when(error_type == "FP" & Rate <= 5 ~ "Permissable Rate",
                                error_type == "FN" & Rate <= 20 ~ "Permissable Rate",
                                TRUE ~ "Not Permissable Rate"))


#editing to just include false positives
final_data3_wo_area <- final_data3 %>% as.data.frame()#%>% filter(error_type == "FP")

###
### Figure 1:
### Communicate results here, for linear model WITHOUT AREA CONTROLS and simple DGP from experiments 
### 1 and 2. False positives are the key metric
###
fig1_dat <- final_data3_wo_area %>% filter(Model == "lm" & Experiment.Number %in% c(1,2))%>%
  mutate(Model = recode(Model, spatial_lag = "Spatial Lag",
                        spatial_error = "Spatial Error",
                        lm = "Linear Model"))

pdf(file = "./figure1a_final_4.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 6)
ggplot(fig1_dat, aes(error_type, Model)) + geom_tile(aes(fill = Rate), color = "grey",
                                                     width = 0.97, height =0.97) +
  scale_fill_gradient(low = "white", high = "black") + facet_grid(country ~ Unit, switch = "y")+
  geom_text(data = fig1_dat[which(fig1_dat$Rate > 30),], aes(label= round(Rate,1)), col = "white")+
  geom_text(data = fig1_dat[which(fig1_dat$Rate <= 30),], aes(label= round(Rate,1)), col = "black")+
  theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x= "Error Type", fill = "Rate, %")
dev.off()


###
### Figure 2:
### Communicate results here, for linear model WITH AREA CONTROLS and simple DGP from 
### experiments 1 and 2. False positives are the key metric
###
fig2_dat <- final_data3_w_area %>% filter(Model == "lm" & Experiment.Number %in% c(1,2))%>%
  mutate(Model = recode(Model, spatial_lag = "Spatial Lag",
                        spatial_error = "Spatial Error",
                        lm = "Linear Model"))

pdf(file = "./figure2a_final_4.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 6)
ggplot(fig2_dat, aes(error_type, Model)) + geom_tile(aes(fill = Rate), color = "grey",
                                                     width = 0.97, height =0.97) +
  scale_fill_gradient(low = "white", high = "black") + facet_grid(country ~ Unit, switch = "y")+
  geom_text(data = fig2_dat[which(fig2_dat$Rate > 30),], aes(label= round(Rate,1)), col = "white")+
  geom_text(data = fig2_dat[which(fig2_dat$Rate <= 30),], aes(label= round(Rate,1)), col = "black")+
  theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x= "Error Type", fill = "Rate, %")
dev.off()


###
### Figure 3:
### Show false positive results for just spatial diffusion scenario for OLS, OLS with lag, and SAR. 
### All models should feature area controls.
### (experiments 3 and 4)
###
fig3_dat <- final_data3_w_area %>% filter(Experiment.Number %in% c(3,4))%>%
  mutate(Model = recode(Model, spatial_lag = "Spatial Lag",
                        spatial_error = "Spatial Error",
                        lm = "Linear Model")) %>% filter(Model %in% c("Spatial Lag", "Linear Model"))

pdf(file = "./figure3a_final_4.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 6)
ggplot(fig3_dat, aes(error_type, Model)) + geom_tile(aes(fill = Rate), color = "grey",
                                                     width = 0.97, height =0.97) +
  scale_fill_gradient(low = "white", high = "black") + facet_grid(country ~ Unit, switch = "y")+
  geom_text(data = fig3_dat[which(fig3_dat$Rate > 30),], aes(label= round(Rate,1)), col = "white")+
  geom_text(data = fig3_dat[which(fig3_dat$Rate <= 30),], aes(label= round(Rate,1)), col = "black")+
  theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x= "Error Type", fill = "Rate, %")
dev.off()


###
### Figure 4:
### Show false positive results for just spatial error scenario for OLS and spatial error models. 
### All models should feature area controls.
### (experiments 5 and 6)
###
fig4_dat <- final_data3_w_area %>% filter(Model %in% c("lm", "spatial_error") & 
                                            Experiment.Number %in% c(5,6)) %>%
  mutate(Model = recode(Model, spatial_lag = "Spatial Lag",
                        spatial_error = "Spatial Error",
                        lm = "Linear Model"))

pdf(file = "./figure4a_final_4.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 6)
ggplot(fig4_dat, aes(error_type, Model)) + geom_tile(aes(fill = Rate), color = "grey",
                                                     width = 0.97, height =0.97) +
  scale_fill_gradient(low = "white", high = "black") + facet_grid(country ~ Unit, switch = "y")+
  geom_text(data = fig4_dat[which(fig4_dat$Rate > 30),], aes(label= round(Rate,1)), col = "white")+
  geom_text(data = fig4_dat[which(fig4_dat$Rate < 30),], aes(label= round(Rate,1)), col = "black")+
  theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x= "Error Type", fill = "Rate, %")
dev.off()


#summary statistics on amount of false positives
summ_dat1 <- final_data3_w_area %>% group_by(country, Unit) %>% 
  summarise(mean_rate = mean(Rate), n = n())
summ_dat2 <- final_data3_w_area %>% group_by(Unit) %>% 
  summarise(mean_rate = mean(Rate), n = n()) %>% select(-n)


xtable(t(as.matrix(summ_dat2)))




####
#### Take full output files for units and compare
####
#working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./00_functions_repcode.R")

#read data
robust_dat <- read_robustness_dat(country_names) 


robust_dat2 <- robust_dat %>%
  pivot_longer(-c(coefficient, model_type, exp_num, unit, country), 
               names_to = "Significance", values_to = "Percentage.of.Runs") %>%
  mutate(error_type = case_when(exp_num %in% c(2,4,6,8) & Significance %in% c("perc_both_pos_sig", "perc_both_neg_sig") ~ "FP",
                                exp_num %in% c(1,3,5,7) & Significance == "perc_not_sig" ~ "FN",
                                TRUE ~ NA_character_),
         Percentage.of.Runs = 100*as.numeric(Percentage.of.Runs)) %>%
  filter(!is.na(error_type)) %>% filter(exp_num %in% c(7,8)) %>% 
  group_by(coefficient, model_type, unit, country, error_type) %>% #need to group all experiments
  summarise(Percentage.of.Runs = sum(Percentage.of.Runs)) %>% as.data.frame() %>%
  mutate(model_type = recode(model_type, spatial_lag = "Spatial Lag",
                             spatial_error = "Spatial Error",
                             lm = "Linear Model"))

pdf(file = "./figure5a_final_4.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 6)
ggplot(robust_dat2, aes(error_type, model_type)) + geom_tile(aes(fill = Percentage.of.Runs), color = "grey",
                                                             width = 0.97, height =0.97) +
  geom_text(data = robust_dat2[which(robust_dat2$Percentage.of.Runs > 30),], aes(label= round(Percentage.of.Runs,1)), col = "white")+
  geom_text(data = robust_dat2[which(robust_dat2$Percentage.of.Runs <= 30),], aes(label= round(Percentage.of.Runs,1)), col = "black")+
  scale_fill_gradient(low = "white", high = "black", limits = c(0,100)) + facet_grid(country ~ unit, switch = "y")+
  theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x= "Error Type", fill = "Rate, %", y = "Model")
dev.off()



### Moran and Shapiro Plot
#working directory
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./00_plot_functions.R")


moran_dat <- read.csv("./moran_out.csv")
shapiro_dat <- read.csv("./shapiro_out.csv")
aic_lm_dat <- read.csv("./lm_aic_out.csv")

moran_dat <- moran_dat[-c(1,2),]
shapiro_dat <- shapiro_dat[-c(1,2),]
aic_lm_dat <- aic_lm_dat[-c(1,2),]

#final_ 1
country_names <- c("Afghanistan", "Pakistan", "Nepal", "Iraq", "Colombia", "Somalia", "Turkey")

#combine cases
cases <- all_cases_combine(country_names)

moran_dat <- cbind(moran_dat, cases)
shapiro_dat <- cbind(shapiro_dat, cases)
aic_lm_dat <- cbind(aic_lm_dat, cases)

colnames(moran_dat)[10:12] <- c("country", "unit", "model_form")
colnames(shapiro_dat)[10:12] <- c("country", "unit", "model_form")
colnames(aic_lm_dat)[10:12] <- c("country", "unit", "model_form")


moran_witharea <- moran_dat %>% select(-X) %>%
  pivot_longer(-c(country, unit, model_form), 
               names_to = "Experiment", values_to = "Percentage.of.Runs")
moran_witharea$Experiment <- rep(1:8, nrow(moran_dat))

moran_witharea <- moran_witharea %>% filter(Experiment %in% c(1,3), model_form == "with area") %>%
  mutate(test_type = "Moran")

shapiro_witharea <- shapiro_dat %>% select(-X) %>%
  pivot_longer(-c(country, unit, model_form), 
               names_to = "Experiment", values_to = "Percentage.of.Runs")
shapiro_witharea$Experiment <- rep(1:8, nrow(shapiro_dat))

#AIC tests
aic_with_area <- aic_lm_dat %>% select(-X) %>%
  pivot_longer(-c(country, unit, model_form), 
               names_to = "Experiment", values_to = "Percentage.of.Runs")
aic_with_area$Experiment <- rep(1:8, nrow(aic_lm_dat))

aic_with_area <- aic_with_area %>% filter(Experiment %in% c(1,5), model_form == "with area") %>%
  mutate(test_type = "AIC")

moran_witharea$Experiment <- recode(moran_witharea$Experiment, "1" = "1a", "3" = "2a")
moran_witharea$Experiment <- as.factor(moran_witharea$Experiment)
moran_witharea$Experiment  <- factor(moran_witharea$Experiment , levels = c("2a", "1a"))

shapiro_witharea$Experiment <- recode(shapiro_witharea$Experiment, "1" = "1a", "5" = "3a")
shapiro_witharea$Experiment <- as.factor(shapiro_witharea$Experiment)
shapiro_witharea$Experiment  <- factor(shapiro_witharea$Experiment , levels = c("3a", "1a"))

aic_with_area$Experiment <- recode(aic_with_area$Experiment, "1" = "1a", "5" = "3a")
aic_with_area$Experiment <- as.factor(aic_with_area$Experiment)
aic_with_area$Experiment  <- factor(aic_with_area$Experiment , levels = c("3a", "1a"))


#fix levels
shapiro_witharea$unit  <- factor(shapiro_witharea$unit , levels = c("ADM1",  "ADM2",  "PG05",  "PG025"))
moran_witharea$unit  <- factor(shapiro_witharea$unit , levels = c("ADM1",  "ADM2",  "PG05",  "PG025"))
aic_with_area$unit  <- factor(aic_with_area$unit , levels = c("ADM1",  "ADM2",  "PG05",  "PG025"))


pdf(file = "./figure6a_final_4.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 6)
ggplot(moran_witharea, aes(test_type, as.factor(Experiment))) + 
  geom_tile(aes(fill = 100*Percentage.of.Runs), color = "grey",width = 0.97, height =0.97) +
  geom_text(data = moran_witharea[which(moran_witharea$Percentage.of.Runs > 0.30),], aes(label= round(100*Percentage.of.Runs,1)), col = "white")+
  geom_text(data = moran_witharea[which(moran_witharea$Percentage.of.Runs <= 0.30),], aes(label= round(100*Percentage.of.Runs,1)), col = "black")+
  scale_fill_gradient(low = "white", high = "black", limits = c(0,100)) + facet_grid(country ~ unit, switch = "y")+
  theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x= "Test Type", fill = "Rate, %", y = "Experiment")
dev.off()


pdf(file = "./figure6b_final_4.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 6)
ggplot(shapiro_witharea, aes(test_type, as.factor(Experiment))) + 
  geom_tile(aes(fill = 100*Percentage.of.Runs), color = "grey",width = 0.97, height =0.97) +
  geom_text(data = shapiro_witharea[which(shapiro_witharea$Percentage.of.Runs > 0.30),], aes(label= round(100*Percentage.of.Runs,1)), col = "white")+
  geom_text(data = shapiro_witharea[which(shapiro_witharea$Percentage.of.Runs <= 0.30),], aes(label= round(100*Percentage.of.Runs,1)), col = "black")+
  scale_fill_gradient(low = "white", high = "black", limits = c(0,100)) + facet_grid(country ~ unit, switch = "y")+
  theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x= "Test Type", fill = "Rate, %", y = "Experiment")
dev.off()


#AIC plot
aic_with_area$test_type <- "AIC_lm < AIC_pois"
pdf(file = "./figure6c.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 6)
ggplot(aic_with_area, aes(test_type, as.factor(Experiment))) + 
  geom_tile(aes(fill = 100*Percentage.of.Runs), color = "grey",width = 0.97, height =0.97) +
  geom_text(data = aic_with_area[which(aic_with_area$Percentage.of.Runs > 0.30),], aes(label= round(100*Percentage.of.Runs,1)), col = "white")+
  geom_text(data = aic_with_area[which(aic_with_area$Percentage.of.Runs <= 0.30),], aes(label= round(100*Percentage.of.Runs,1)), col = "black")+
  scale_fill_gradient(low = "white", high = "black", limits = c(0,100)) + facet_grid(country ~ unit, switch = "y")+
  theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x= "Test Type", fill = "Rate, %", y = "Experiment")
dev.off()
