###
### Replication Code - All Countries
###
### Manuscript Title: "A Monte Carlo Analysis of False Inference in Spatial Conflict Event Studies"
### Manuscript Authors: Sebastian Schutte and Claire Kelling
### 
### Code Authors: Claire Kelling
### Last Modified: 11/30/21
###

#Clear the deck
rm(list=ls())


# Call libraries
library(maptools)
library(dplyr)
library(ggplot2)
library(sf)
library(spdep)
library(rgeos)
library(rgdal)
library(spatialreg)
library(R.utils)

# Working directory for project
# Sets the working directory to the location of this file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load functions
source("00_functions_repcode.R")

# All cases
cases <- rbind(c("Afghanistan", "events_afghanistan.csv", "Afghanistan_admin1", "freq ~ population"),
               c("Afghanistan", "events_afghanistan.csv", "Afghanistan_admin2", "freq ~ population"),
               c("Afghanistan", "events_afghanistan.csv", "Afghanistan_pg05", "freq ~ population"),
               c("Afghanistan", "events_afghanistan.csv", "Afghanistan_pg025", "freq ~ population"),
               c("Afghanistan", "events_afghanistan.csv", "Afghanistan_admin1", "freq ~ population + area"),
               c("Afghanistan", "events_afghanistan.csv", "Afghanistan_admin2", "freq ~ population + area"),
               c("Afghanistan", "events_afghanistan.csv", "Afghanistan_pg05", "freq ~ population + area"),
               c("Afghanistan", "events_afghanistan.csv", "Afghanistan_pg025", "freq ~ population + area"),

               c("Pakistan", "events_pakistan.csv", "Pakistan_admin1", "freq ~ population"),
               c("Pakistan", "events_pakistan.csv", "Pakistan_admin2", "freq ~ population"),
               c("Pakistan", "events_pakistan.csv", "Pakistan_pg05", "freq ~ population"),
               c("Pakistan", "events_pakistan.csv", "Pakistan_pg025", "freq ~ population"),
               c("Pakistan", "events_pakistan.csv", "Pakistan_admin1", "freq ~ population + area"),
               c("Pakistan", "events_pakistan.csv", "Pakistan_admin2", "freq ~ population + area"),
               c("Pakistan", "events_pakistan.csv", "Pakistan_pg05", "freq ~ population + area"),
               c("Pakistan", "events_pakistan.csv", "Pakistan_pg025", "freq ~ population + area"),
               
               c("Nepal", "events_nepal.csv", "Nepal_admin1", "freq ~ population"),
               c("Nepal", "events_nepal.csv", "Nepal_admin2", "freq ~ population"),
               c("Nepal", "events_nepal.csv", "Nepal_pg05", "freq ~ population"),
               c("Nepal", "events_nepal.csv", "Nepal_pg025", "freq ~ population"),
               c("Nepal", "events_nepal.csv", "Nepal_admin1", "freq ~ population + area"),
               c("Nepal", "events_nepal.csv", "Nepal_admin2", "freq ~ population + area"),
               c("Nepal", "events_nepal.csv", "Nepal_pg05", "freq ~ population + area"),
               c("Nepal", "events_nepal.csv", "Nepal_pg025", "freq ~ population + area"),

               c("Iraq", "events_iraq.csv", "Iraq_admin1", "freq ~ population"),
               c("Iraq", "events_iraq.csv", "Iraq_admin2", "freq ~ population"),
               c("Iraq", "events_iraq.csv", "Iraq_pg05", "freq ~ population"),
               c("Iraq", "events_iraq.csv", "Iraq_pg025", "freq ~ population"),
               c("Iraq", "events_iraq.csv", "Iraq_admin1", "freq ~ population + area"),
               c("Iraq", "events_iraq.csv", "Iraq_admin2", "freq ~ population + area"),
               c("Iraq", "events_iraq.csv", "Iraq_pg05", "freq ~ population + area"),
               c("Iraq", "events_iraq.csv", "Iraq_pg025", "freq ~ population + area"),
               
               c("Colombia", "events_colombia.csv", "Colombia_admin1", "freq ~ population"),
               c("Colombia", "events_colombia.csv", "Colombia_admin2", "freq ~ population"),
               c("Colombia", "events_colombia.csv", "Colombia_pg05", "freq ~ population"),
               c("Colombia", "events_colombia.csv", "Colombia_pg025", "freq ~ population"),
               c("Colombia", "events_colombia.csv", "Colombia_admin1", "freq ~ population + area"),
               c("Colombia", "events_colombia.csv", "Colombia_admin2", "freq ~ population + area"),
               c("Colombia", "events_colombia.csv", "Colombia_pg05", "freq ~ population + area"),
               c("Colombia", "events_colombia.csv", "Colombia_pg025", "freq ~ population + area"),
               
               c("Somalia", "events_somalia.csv", "Somalia_admin1", "freq ~ population"),
               c("Somalia", "events_somalia.csv", "Somalia_admin2", "freq ~ population"),
               c("Somalia", "events_somalia.csv", "Somalia_pg05", "freq ~ population"),
               c("Somalia", "events_somalia.csv", "Somalia_pg025", "freq ~ population"),
               c("Somalia", "events_somalia.csv", "Somalia_admin1", "freq ~ population + area"),
               c("Somalia", "events_somalia.csv", "Somalia_admin2", "freq ~ population + area"),
               c("Somalia", "events_somalia.csv", "Somalia_pg05", "freq ~ population + area"),
               c("Somalia", "events_somalia.csv", "Somalia_pg025", "freq ~ population + area"),
               
               c("Turkey", "events_turkey.csv", "Turkey_admin1", "freq ~ population"),
               c("Turkey", "events_turkey.csv", "Turkey_admin2", "freq ~ population"),
               c("Turkey", "events_turkey.csv", "Turkey_pg05", "freq ~ population"),
               c("Turkey", "events_turkey.csv", "Turkey_pg025", "freq ~ population"),
               c("Turkey", "events_turkey.csv", "Turkey_admin1", "freq ~ population + area"),
               c("Turkey", "events_turkey.csv", "Turkey_admin2", "freq ~ population + area"),
               c("Turkey", "events_turkey.csv", "Turkey_pg05", "freq ~ population + area"),
               c("Turkey", "events_turkey.csv", "Turkey_pg025", "freq ~ population + area")
               )

#stores empty dataframes, will add to them with each case considered
all_moran <- rbind(rep(NA,8), rep(NA, 8))
write.csv(all_moran, "./moran_out.csv")

all_shapiro <- rbind(rep(NA,8), rep(NA, 8))
write.csv(all_shapiro, "./shapiro_out.csv")

all_lm_aic <- rbind(rep(NA,8), rep(NA, 8))
write.csv(all_lm_aic, "./lm_aic_out.csv")

for(i in 1:nrow(cases)){
  country_of_interest <- cases[i,1]
  name_of_event_data <- cases[i,2]
  name_of_units <- cases[i,3]
  formula <- as.formula(as.character(cases[i,4]))
  
  print(paste(i, "******************************************"))
  
  ###
  ### Code to return dataframe with count of events per PG grid cell as "freq" variable
  ### 
  
  # Takes several minutes due to size of event file
  output <- full_function(country_of_interest, name_of_event_data, 
                          name_of_units = name_of_units)
  
  shp_int <- output$shp_int
  output_dfs <- output$df_list
  
  #subset to just those grid cells where we have data
  shp_int <- shp_int[which(shp_int$gid %in% output_dfs[[1]][[1]]$gid),]
  
  #Create neighborhood matrix
  W.nb <- poly2nb(shp_int, row.names = rownames(shp_int@data))
  W.mat <- nb2mat(W.nb, style="B", zero.policy = T)
  rownames(W.mat) <- NULL #need this for test if matrix is symmetric
  W.list <- nb2listw(W.nb, zero.policy = T)
  W.list.moran <- nb2listw(W.nb, style="B", zero.policy = T)
  

  #extract agreeance in each experiment on sign and significance
  model_type = "lm" #lm or spatial_lag or spatial_error
  print(model_type)
  eval_out <- eval_exp(formula, output_dfs, model_type, W.list,W.list.moran, alpha = 0.05)
  full_out <- eval_out[[2]]
  moran_out <- eval_out[[3]]
  shapiro_out <- eval_out[[4]]
  lm_aic_out <- eval_out[[5]]
  eval_out <- eval_out[[1]]

  model_type = "spatial_lag" #lm or spatial_lag or spatial_error
  print(model_type)
  eval_out2 <- eval_exp(formula, output_dfs, model_type, W.list,W.list.moran, alpha =0.05)  full_out2 <- eval_out2[[2]]
  eval_out2 <- eval_out2[[1]]

  model_type = "spatial_error" #lm or spatial_lag or spatial_error
  print(model_type)
  eval_out3 <- eval_exp(formula, output_dfs, model_type, W.list,W.list.moran, alpha =0.05)
  full_out3 <- eval_out3[[2]]
  eval_out3 <- eval_out3[[1]]

  #needs to include all eval_out[[3]]'s when estimating area as well
  #only eval_out[[1]] and eval_out[[2]] when not estimating area
  if(formula == as.formula(freq ~ population)){
    complete_output <- rbind(eval_out[[1]], eval_out[[2]],
                             eval_out2[[1]], eval_out2[[2]],
                             eval_out3[[1]], eval_out3[[2]])
    model_form <- ""
  }else if(formula == as.formula(freq ~ population + area)){
    complete_output <- rbind(eval_out[[1]], eval_out[[2]], eval_out[[3]],
                             eval_out2[[1]], eval_out2[[2]], eval_out2[[3]],
                             eval_out3[[1]], eval_out3[[2]], eval_out3[[3]])
    model_form <- "with_area"
  }

  complete_output <- as.data.frame(complete_output)
  complete_output$summary <- "NA"
  complete_output$summary[complete_output$`Experiment Number` == 1] <- "Effect, No DF, No EC"
  complete_output$summary[complete_output$`Experiment Number` == 2] <- "No Effect, No D, No EC"
  complete_output$summary[complete_output$`Experiment Number` == 3] <- "Effect, No DF, EC"
  complete_output$summary[complete_output$`Experiment Number` == 4] <- "No Effect, No DF, EC"
  complete_output$summary[complete_output$`Experiment Number` == 5] <- "Effect, DF, No EC"
  complete_output$summary[complete_output$`Experiment Number` == 6] <- "No Effect, DF, No EC"
  complete_output$summary[complete_output$`Experiment Number` == 7] <- "Effect, DF, EC"
  complete_output$summary[complete_output$`Experiment Number` == 8] <- "No Effect, DF, EC"

  fullout_unit <- list(full_out, full_out2, full_out3)

  #save data structure with run_id
  save(fullout_unit, file= paste("./",name_of_units, model_form, ".Rdata", sep = ""))

  #Write data
  write.csv(complete_output,paste("./",name_of_units, model_form, ".csv", sep = ""))

  res <- subset(complete_output,coefficient != "beta_0")
  write.csv(res,paste("./",name_of_units, "_beta1", model_form, ".csv", sep = ""))
  
  #write the results into a csv
  all_moran <- read.csv("./moran_out.csv") %>% select(-1)
  all_moran <- rbind(all_moran, moran_out)
  write.csv(all_moran, "./moran_out.csv")
  
  all_shapiro <- read.csv("./shapiro_out.csv") %>% select(-1)
  all_shapiro <- rbind(all_shapiro, shapiro_out)
  write.csv(all_shapiro, "./shapiro_out.csv")
  
  all_lm_aic <- read.csv("./lm_aic_out.csv") %>% select(-1)
  all_lm_aic <- rbind(all_lm_aic, lm_aic_out)
  write.csv(all_shapiro, "./lm_aic_out.csv")

}

