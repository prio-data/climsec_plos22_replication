
read_data_warea <- function(country_names){
  
  all_final_dat <- NULL
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  for(i in 1:length(country_names)){
    country_name <- country_names[i]
    
    if(country_name == "Nepal2"){
      dat_2 <- read.csv(paste("./", country_name,
                              "_admin2_beta1with_area.csv", sep = ""))
      dat_05 <- read.csv(paste("./", country_name,
                               "_pg05_beta1with_area.csv", sep = ""))
      dat_025 <- read.csv(paste("./", country_name,
                                "_pg025_beta1with_area.csv", sep = ""))
      final_dat <- cbind(rep(rep(c("population","area"), each =8), times = 1), 
                         #nepal_1[,c(2:5)], 
                         dat_2[,c(2:5)], dat_05[,c(3:5)], dat_025[,c(3:8)])
      colnames(final_dat) <- c("variable", "coefficient", "ADM2_Pos.and.sig.", "ADM2_Neg.and.sig.", "ADM2_No.effect",
                               "PG05_Pos.and.sig.", "PG05_Neg.and.sig.", "PG05_No.effect", "PG025_Pos.and.sig.",
                               "PG025_Neg.and.sig.", "PG025_No.effect", "Model", "Experiment.Number", "summary")
      final_dat$country  <- country_name
    }else{
      dat_1 <- read.csv(paste("./", country_name, 
                              "_admin1_beta1with_area.csv", sep = ""))
      dat_2 <- read.csv(paste("./", country_name,
                              "_admin2_beta1with_area.csv", sep = ""))
      dat_05 <- read.csv(paste("./", country_name,
                               "_pg05_beta1with_area.csv", sep = ""))
      dat_025 <- read.csv(paste("./", country_name,
                                "_pg025_beta1with_area.csv", sep = ""))
      final_dat <- cbind(rep(rep(c("population","area"), each =8), times = 1), 
                         dat_1[,c(2:5)], dat_2[,c(3:5)], dat_05[,c(3:5)], 
                         dat_025[,c(3:8)])
      colnames(final_dat) <- c("variable", "coefficient", "ADM1_Pos.and.sig.", "ADM1_Neg.and.sig.", 
                               "ADM1_No.effect", "ADM2_Pos.and.sig.", "ADM2_Neg.and.sig.", "ADM2_No.effect",
                               "PG05_Pos.and.sig.", "PG05_Neg.and.sig.", "PG05_No.effect", "PG025_Pos.and.sig.",
                               "PG025_Neg.and.sig.", "PG025_No.effect", "Model", "Experiment.Number", "summary")
      final_dat$country  <- country_name
    }
    all_final_dat <- bind_rows(all_final_dat, final_dat)
  }
  
  return(all_final_dat)
}



read_data_woarea <- function(country_names){
  
  all_final_dat <- NULL
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  for(i in 1:length(country_names)){
    #i <- 1
    print(i)
    country_name <- country_names[i]
    
    if(country_name %in% c("Nepal2")){#, "Pakistan", "Somalia", "Turkey", "Colombia", "Iraq")){
      dat_2 <- read.csv(paste("./", country_name,
                              "_admin2_beta1.csv", sep = ""))
      dat_05 <- read.csv(paste("./", country_name,
                               "_pg05_beta1.csv", sep = ""))
      dat_025 <- read.csv(paste("./", country_name,
                                "_pg025_beta1.csv", sep = ""))
      final_dat <- cbind(rep(rep(c("population"), each =8), times = 1), 
                         dat_2[,c(2:5)], dat_05[,c(3:5)], dat_025[,c(3:8)])
      colnames(final_dat) <- c("variable", "coefficient", "ADM2_Pos.and.sig.", "ADM2_Neg.and.sig.", "ADM2_No.effect",
                               "PG05_Pos.and.sig.", "PG05_Neg.and.sig.", "PG05_No.effect", "PG025_Pos.and.sig.",
                               "PG025_Neg.and.sig.", "PG025_No.effect", "Model", "Experiment.Number", "summary")
      final_dat$country  <- country_name
    }else{
      dat_1 <- read.csv(paste("./", country_name, 
                              "_admin1_beta1.csv", sep = ""))
      dat_2 <- read.csv(paste("./", country_name,
                              "_admin2_beta1.csv", sep = ""))
      dat_05 <- read.csv(paste("./", country_name,
                               "_pg05_beta1.csv", sep = ""))
      dat_025 <- read.csv(paste("./", country_name,
                                "_pg025_beta1.csv", sep = ""))
      final_dat <- cbind(rep(rep(c("population"), each =8), times = 1), 
                         dat_1[,c(2:5)], dat_2[,c(3:5)], dat_05[,c(3:5)], 
                         dat_025[,c(3:8)])
      colnames(final_dat) <- c("variable", "coefficient", "ADM1_Pos.and.sig.", "ADM1_Neg.and.sig.", 
                               "ADM1_No.effect", "ADM2_Pos.and.sig.", "ADM2_Neg.and.sig.", "ADM2_No.effect",
                               "PG05_Pos.and.sig.", "PG05_Neg.and.sig.", "PG05_No.effect", "PG025_Pos.and.sig.",
                               "PG025_Neg.and.sig.", "PG025_No.effect", "Model", "Experiment.Number", "summary")
      final_dat$country  <- country_name
    }
    all_final_dat <- bind_rows(all_final_dat, final_dat)
  }
  
  return(all_final_dat)
}



read_robustness_dat <- function(country_names){
  
  all_robust_dat <- NULL
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  for(i in 1:length(country_names)){
    #i <- 1
    country_name <- country_names[i]
    
    if(country_name == "Nepal2"){
      load(paste("./", country_name,
                 "_pg05with_area.Rdata", sep = ""))
      dat_pg05 <- fullout_unit
      load(paste("./", country_name,
                 "_pg025with_area.Rdata", sep = ""))
      dat_pg025 <- fullout_unit
      
      dat_pg <- robustness_check(dat_pg05, dat_pg025, num_coeff=2) %>% as.data.frame()
      dat_pg$unit <- "PG"
      dat_pg$country <- country_name
      
    }else{
      load(paste("./", country_name, 
                              "_admin1with_area.Rdata", sep = ""))
      dat_adm1 <- fullout_unit
      load(paste("./", country_name,
                              "_admin2with_area.Rdata", sep = ""))
      dat_adm2 <- fullout_unit
      load(paste("./", country_name,
                               "_pg05with_area.Rdata", sep = ""))
      dat_pg05 <- fullout_unit
      load(paste("./", country_name,
                                "_pg025with_area.Rdata", sep = ""))
      dat_pg025 <- fullout_unit
      
      dat_adm <- robustness_check(dat_adm1, dat_adm2, num_coeff=2) %>% as.data.frame()
      dat_adm$unit <- "ADM"
      dat_adm$country  <- country_name
      
      dat_pg <- robustness_check(dat_pg05, dat_pg025, num_coeff=2) %>% as.data.frame()
      dat_pg$unit <- "PG"
      dat_pg$country <- country_name
      
    }
    all_robust_dat <- bind_rows(all_robust_dat, dat_adm, dat_pg)
  }
  return(all_robust_dat)
}



all_cases_combine <- function(country_names){
  
  all_cases_df <- NULL
  
  for(i in 1:length(country_names)){
    country_name <- country_names[i]
    if(country_name %in% c("Nepal2")){#, "Pakistan", "Somalia", "Turkey", "Colombia", "Iraq")){
      units <- c("ADM1", "ADM2", "PG05", "PG025",
                 "ADM1", "ADM2", "PG05", "PG025")
      mod_type <- c("without_area", "with_area")
      for(j in 2:8){
        new_row <- c(country_name)
        
        mod <- ifelse(j %in% c(1,2,3,4), "without area", "with area")
        new_row <- c(country_name, units[j], mod)
        
        all_cases_df <- rbind(all_cases_df, new_row)
      }
    }else{
      units <- c("ADM1", "ADM2", "PG05", "PG025",
                 "ADM1", "ADM2", "PG05", "PG025")
      mod_type <- c("without_area", "with_area")
      for(j in 1:8){
        #j <- 1
        mod <- ifelse(j %in% c(1,2,3,4), "without area", "with area")
        new_row <- c(country_name, units[j], mod)
        
        all_cases_df <- rbind(all_cases_df, new_row)
      }
    }
    
  }
  
  return(all_cases_df)
}
