#Full set of functions for replication_code.R
# Last modified: 06/16/20
# 06/16/20: changing pop2010 and area to c_pop2010 and c_area
#           changing working directory, everything read from inputs/
# 06/29/20: added robustness_check code

sub_shp_adm <- function(country_of_interest,name_of_adm){
  #load country shapefile
  cshapes <- readOGR(paste(getwd(),"/inputs", sep =""),layer=country_of_interest) 
  
  #load adm shape file from inputs
  adm <- readOGR(paste(getwd(),"/inputs", sep =""),layer=name_of_adm) 
  adm <- spTransform(adm,CRSobj = CRS(proj4string(cshapes)))
  
  #combines the files to get the population for each gid
  if (is.null(adm$GID_2)& is.null(adm$pgid)){
    admin_data <- as.data.frame(cbind(as.character(adm$GID_1),2010,as.character(cshapes$GWCODE[as.character(cshapes$CNTRY_NAME) == country_of_interest]),
                                      adm$c_pop2010, adm$c_area, #pg's have these column names
                                      adm$pop2010, adm$area)) #adms have these column names
    adm$gid <- adm$GID_1
  } else if ((is.null(adm$GID_1) & is.null(adm$pgid)) | name_of_adm == "India_admin2.shp") {
    admin_data <- as.data.frame(cbind(as.character(adm$GID_2),2010,as.character(cshapes$GWCODE[as.character(cshapes$CNTRY_NAME) == country_of_interest]),
                                      adm$c_pop2010, adm$c_area, #pg's have these column names
                                      adm$pop2010, adm$area)) #adms have these column names
    adm$gid <- adm$GID_2
  } else if (is.null(adm$GID_1) & is.null(adm$GID_2)){
    admin_data <- as.data.frame(cbind(as.character(adm$pgid),2010,as.character(cshapes$GWCODE[as.character(cshapes$CNTRY_NAME) == country_of_interest]),
                                      adm$c_pop2010, adm$c_area, #pg's have these column names
                                      adm$pop2010, adm$area)) #adms have these column names
    adm$gid <- adm$pgid
  }else{
    admin_data <- as.data.frame(cbind(as.character(adm$GID_1),2010,as.character(cshapes$GWCODE[as.character(cshapes$CNTRY_NAME) == country_of_interest]),
                                      adm$c_pop2010, adm$c_area, #pg's have these column names
                                      adm$pop2010, adm$area)) #adms have these column names
    adm$gid <- adm$GID_1
  }
  
  names(admin_data) <- c("gid", "year", "gwno", "population", "area")
  
  out <- list(admin_data,adm)
}




super_imp <- function(event_data, shp_int){
  # First, we need to convert both to same projection
  coordinates(event_data) <- ~x+y
  proj4string(event_data) <- CRS(proj4string(shp_int))
  
  #overlay the spatial points of event_data onto the spatial polygons of shp_int
  overlap_set <- over(event_data, shp_int)
  nrow(event_data@coords)
  nrow(overlap_set) #it has classified each of the points in the dataset into a group
  sum(is.na(overlap_set$gid)) #there are some points that actually occur outside of the shape file for the grid
  
  event_data_df <- as.data.frame(event_data)
  det_dat_over <- cbind(event_data_df, overlap_set)
  det_dat_ov <- det_dat_over[!is.na(det_dat_over$gid),]
  
  agg_dat <- plyr::count(det_dat_ov, c('gid'))
  agg_dat$gid <- as.character(agg_dat$gid)
  agg_dat$freq <- as.integer(as.character(agg_dat$freq))
  
  return(agg_dat)
  
}


full_function <- function(country_of_interest, name_of_event_data, 
                          name_of_units){
  ###
  ### Get relevant files
  ###
  #Return Priogrid dataframe and shape file based on country of interest
  ### Takes about 7 min because of large shapefile
  prio_merged <- sub_shp_adm(country_of_interest, name_of_units) 
  
  # Extract shape file of interest and data frame of interest
  shp_int <- prio_merged[[2]]
  df_int <- prio_merged[[1]]
  
  ###
  ### Load simulated event data
  ###
  print("Load simulated event data.")
  event_data <- read.csv(paste(getwd(),"/inputs/", name_of_event_data, sep =""))
  colnames(event_data)[which(colnames(event_data)== "long")] <- c("x")
  colnames(event_data)[which(colnames(event_data)== "lat")] <- c("y")
  
  # Take out year for priogrid dataset
  df_int <- subset(df_int, select = -c(year))
  df_int <- unique(df_int)
  
  #record how many dataframes to expect
  num_df <- nrow(unique(event_data[,c("experiment", "run_id")]))
  k <- 1
  df_list <- list()
  tick <- 0
  print("Superimposing.")
  for(i in unique(event_data$experiment)){
    
    #subset to only this experiment
    new_data <- event_data[which(event_data$experiment == i), ]
    run_id_set <- unique(new_data$run_id)
    
    
    exp_list <- list()
    l <- 1
    
    for(j in run_id_set){
      tick <- tick + 1
      print(paste("Superimposing for", tick, "out of", num_df, sep = " "))
      
      final_event <- event_data[which(event_data$experiment == i),]
      final_event <- final_event[which(final_event$run_id == j),]
      
      ###
      ### Now we would like to superimpose on grid cells
      ### 
      super_imp_dat <- super_imp(final_event, shp_int)#UTM_zone

      
      ###
      ### Return dataframe with PG cells as unit of observation and count of conflict
      ### 
      df_int$gid <- as.character(df_int$gid)
      super_imp_dat$gid <- as.character(super_imp_dat$gid)
      
      final_df <- left_join(df_int, super_imp_dat, by = "gid") 
      
      final_df$population <- as.numeric(as.character(final_df$population))
      
      #change NA to 0
      final_df$freq[which(is.na(final_df$freq))] <- 0
      final_df$experiment <- i
      final_df$run_id <- j
      
      exp_list[[l]] <- final_df
      l <- l+1
      
    }
    
    df_list[[k]] <- exp_list
    k <- k+1
  }
  
  output <- NULL
  output$df_list <- df_list
  output$shp_int <- shp_int
  
  rm(event_data)
  
  return(output)
  
}


eval_exp <- function(model_form, output_dfs, model_type, W.list,W.list.moran, alpha){
  ## Extract coefficients
  full_output <- list()
  
  #keep track
  l <- 0
  all_moran <- c()
  all_shapiro <- c()
  all_lm_aic <- c()
  #number of experiments
  for(k in 1:length(output_dfs)){
    coeff_sign <- NULL
    coeff_sig <- NULL
    exp_output <- output_dfs[[k]]
    
    sig_moran_all <- rep(NA, length(exp_output))
    sig_shapiro_all <- rep(NA, length(exp_output))
    lm_aic_bin_all <- rep(NA, length(exp_output))
    
    #number of runs
    for(i in 1:length(exp_output)){
      data <- exp_output[[i]]
      data$area <- as.numeric(as.character(data$area))
      data$population[is.na(data$population)] <- 0
      
      # Comparing two GLM models
      glm_lm <- glm(model_form, data= data, family = gaussian(link = "identity"))
      glm_poisson <- glm(model_form, data= data, family = poisson(link = "log"))
      
      
      lm_aic_bin_all[i] <- ifelse(glm_lm$aic < glm_poisson$aic, 1, 0) #1 - significant (not normal)
      
      
      if(model_type == "lm"){
        mod1 <- lm(model_form, data = data)
        
        coeff <- mod1$coefficients
        p_val <- summary(mod1)$coefficients[,4]
        
        #Moran's I Test:
        resid.model <- residuals(mod1)
        if(max(as.numeric(is.na(data$population))) == 1){
          resid.model <- insert(resid.model, (which(is.na(data$population))-1:length(which(is.na(data$population)))+1), NA)
        }
        num_neighbors <- length(W.list.moran$neighbours)
        nsim <- ifelse(gamma(num_neighbors + 1) > 5000, 5000, gamma(num_neighbors + 1))
        results_moran <- moran.mc(x=resid.model, listw=W.list.moran, nsim=nsim, zero.policy = T, na.action = na.omit)
        
        #extract p-value and test for significance
        sig_moran_all[i] <- ifelse(results_moran$p.value < alpha, 1, 0)
        
        #Test for normality
        results_shapiro <- shapiro.test(resid.model)
        sig_shapiro_all[i] <- ifelse(results_shapiro$p.value < alpha, 1, 0)
        
      }else if(model_type == "spatial_lag"){
        mod1 = lagsarlm(model_form, data=data, listw = W.list, tol.solve=1.0e-30, zero.policy = T) #zero.policy added 12/9/19
        
        coeff <- mod1$coefficients
        p_val <- summary(mod1)$Coef[,4]
        
        sig_moran_all <- NULL
        sig_shapiro_all <- NULL
      }else if(model_type == "spatial_error"){
        mod1 = errorsarlm(model_form, data=data, W.list, tol.solve=1.0e-30, zero.policy = T) #zero.policy added 12/9/19
        
        coeff <- mod1$coefficients
        p_val <- summary(mod1)$Coef[,4]
        sig_moran_all <- NULL
        sig_shapiro_all <- NULL
      }
      l <- l+1
      print(paste("Done with", l, "out of", length(output_dfs)*length(exp_output), "models", sep = " "))
      
      new_coeff_sign <- NULL
      new_coeff_sig <- NULL
      for(j in 1:length(coeff)){
        if(coeff[j] > 0){
          new_sign <- "+"
        }else{
          new_sign <- "-"
        }
        new_coeff_sign <- cbind(new_coeff_sign, new_sign)
        
        if(p_val[j] < alpha){
          new_sig <- "sig"
        }else{
          new_sig <- "not sig"
        }
        new_coeff_sig <- cbind(new_coeff_sig, new_sig)
      }
      
      coeff_sign <- rbind(coeff_sign, new_coeff_sign)
      coeff_sig <- rbind(coeff_sig, new_coeff_sig)
    }
    coeff_sign <- as.data.frame(coeff_sign)
    coeff_sig <- as.data.frame(coeff_sig)
    rownames(coeff_sign) <- c()
    rownames(coeff_sig) <- c()
    all_moran <- c(all_moran, sum(sig_moran_all)/length(exp_output))
    all_shapiro <- c(all_shapiro, sum(sig_shapiro_all)/length(exp_output))
    all_lm_aic <- c(all_lm_aic, sum(lm_aic_bin_all)/length(exp_output))
    full_output[[k]] <- list(coeff_sign, coeff_sig)
  }
  
  eval_list <- list()
  for(i in 1:length(coeff_sig)){ #iterates through number of covariates
    beta_output <- NULL
    for(j in 1:length(full_output)){ #iterates through number of experiments
      sign_results <- full_output[[j]][[1]]
      sig_results <- full_output[[j]][[2]]
      
      pos_sig <- length(which(sig_results[,i] == "sig" & sign_results[,i] == "+"))/nrow(sig_results)
      neg_sig <- length(which(sig_results[,i] == "sig" & sign_results[,i] == "-"))/nrow(sig_results)
      not_sig <- length(which(sig_results[,i] == "not sig"))/nrow(sig_results)
      
      #exp number
      exp_num <- unique(output_dfs[[j]][[1]]$experiment)
      
      new_beta_output <- c(paste("beta_", i-1, sep = ""), pos_sig, neg_sig, not_sig, model_type, exp_num)
      beta_output <- rbind(beta_output, new_beta_output)
    }
    rownames(beta_output) <- c()
    colnames(beta_output) <- c("coefficient", "Pos. and sig.", "Neg. and sig.", "No effect", "Model", "Experiment Number")
    eval_list[[i]] <- beta_output
  }
  return(list(eval_list, full_output, all_moran, all_shapiro, all_lm_aic))
  
  
}


robustness_check <- function(full_output1, full_output2, num_coeff){
  robustness_output <- NULL
  #first layer is the model
  for(i in 1:length(full_output1)){
    comp1 <- full_output1[[i]]
    comp2 <- full_output2[[i]]
    
    #second layer is experiment number
    for(j in 1:length(comp1)){
      #first is sign, second is significance
      df1 <- cbind(as.character(comp1[[j]][[1]][,num_coeff]), as.character(comp1[[j]][[2]][,num_coeff]))
      df2 <- cbind(as.character(comp2[[j]][[1]][,num_coeff]), as.character(comp2[[j]][[2]][,num_coeff]))
      
      #track how many are positive and significant for BOTH units
      perc_pos_sig <- length(which((df1[,2] == "sig" & df1[,1] == "+") & 
                                   (df2[,2] == "sig" & df2[,1] == "+")))/nrow(df1)
      
      #track how many are negative and significant for BOTH units
      perc_neg_sig <- length(which((df1[,2] == "sig" & df1[,1] == "-") & 
                                    (df2[,2] == "sig" & df2[,1] == "-")))/nrow(df1)
      
      model_type <- c("lm", "spatial_lag", "spatial_error")[i]
      exp_num <- j
      new_row <- c(paste("beta_", num_coeff-1, sep = ""), perc_pos_sig, perc_neg_sig, model_type, exp_num)
      robustness_output <- rbind(robustness_output, new_row)
    }
    
  }
  row.names(robustness_output) <- NULL
  colnames(robustness_output) <- c("coefficient", "perc_both_pos_sig", "perc_both_neg_sig", "model_type", "exp_num")
  
  return(robustness_output)
}
