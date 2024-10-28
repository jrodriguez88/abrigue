image <- rast("data/spatial/zepayzemp.jpg")
plot(image)


obs_data <- test_data$ideam[[4]]
target_data <- test_data$chirps[[4]]


remote_data_correction <-  function(obs_data, target_data, wth_var = c("tmax", "tmin")) {
    
    # varsW  = colnames(obs_data)
  
  if(wth_var == "rain"){
    
    obs <-  obs_data %>% mutate(month = month(date))  %>% 
      rename(rain = ideam) 
    
    sim <- target_data %>% rename(rain = value)
    
    monthly_diff <- left_join(obs, sim, by = join_by(date, month)) %>%  drop_na() %>%
      mutate(bias = case_when("rain" %in% wth_var ~ rain.x/rain.y,
                              TRUE ~ 1)) %>%
      dplyr::select(month, bias) %>% set_names(c("month", wth_var)) %>%
      pivot_longer(cols = -c(month), names_to = "var", values_to = "corr") %>% 
      group_by(month) %>%
      summarise(corr = mean(corr))
    
    
  } else if (wth_var == "tmin"){
    
    obs <-  obs_data %>% mutate(month = month(date))  %>% 
      rename(tmin = ideam) 
    
    sim <- target_data %>% rename(tmin = value)
    
    monthly_diff <- left_join(obs, sim, by = join_by(date, month)) %>%  drop_na() %>%
      mutate(bias = case_when("tmin" %in% wth_var  ~ tmin.x - tmin.y,
                              TRUE ~ 1)) %>%
      dplyr::select(month, bias) %>% set_names(c("month", wth_var)) %>%
      pivot_longer(cols = -c(month), names_to = "var", values_to = "corr") %>% 
      group_by(month) %>%
      summarise(corr = mean(corr))
    
    
    
  } else if (wth_var == "tmax"){
    
    obs <-  obs_data %>% mutate(month = month(date))  %>% rename(rain = ideam)
    
    sim <- target_data %>% rename(rain = value)
    
    monthly_diff <- left_join(obs, sim, by = "month") %>% 
      mutate(bias = case_when(wth_var == "rain" ~ rain.x/rain.y,
                              TRUE ~ 1,
                              wth_var == "tmax"  ~ tmax.x - tmax.y,
                              TRUE ~ 1,
                              wth_var =="tmin" ~ tmin.x - tmin.y,
                              TRUE ~ 1)) %>%
      dplyr::select(month, bias) %>% set_names(c("month", wth_var)) %>%
      pivot_longer(cols = -c(month), names_to = "var", values_to = "corr")
    
    
    
  }
  
    

    target_data %>% 
 #     pivot_longer(cols = -c(date, month), names_to = "var") %>%
      left_join(monthly_diff, by = c("month")) %>%
      mutate(value = case_when(wth_var == "tmax" | wth_var == "tmin" ~ value + corr,
                               wth_var == "rain" ~ value*corr, 
                               TRUE ~ value)) %>%
      dplyr::select(date, value) 
    
    
  }