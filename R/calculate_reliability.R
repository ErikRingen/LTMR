calculate_reliability <- function(data){
  alpha_rituals <- data$d_ritual %>% 
    pivot_wider(id_cols = pid, names_from = item, values_from = response) %>% 
    select(-pid) %>% 
    alpha()
  
  alpha_summary_rituals <- alpha_rituals$total %>% as.data.frame() %>% 
    mutate(instrument = "rituals")
  
  alpha_prosoc <- data$d_prosoc %>% 
    pivot_wider(id_cols = pid, names_from = item, values_from = response) %>% 
    select(-pid) %>% 
    alpha()
  
  alpha_summary_prosoc <- alpha_prosoc$total %>% as.data.frame() %>% 
    mutate(instrument = "prosoc")
  
  alpha_SWB <- data$d_SWB %>% 
    pivot_wider(id_cols = pid, names_from = item, values_from = response) %>% 
    select(-pid) %>% 
    alpha()
  
  alpha_summary_SWB <- alpha_SWB$total %>% as.data.frame() %>% 
    mutate(instrument = "SWB")
  
  d_alpha <- bind_rows(alpha_summary_rituals, alpha_summary_prosoc, alpha_summary_SWB)
  return(d_alpha)
}