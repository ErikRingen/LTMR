sum_pid <- function(data){
  # Rituals
  d_ritual <- data$d_rituals %>% 
    mutate(label = factor(response, labels = c("never", "closely witnessed", "participated"))) %>% 
    mutate(subscale = factor(subscale, levels = c("pre", "during", "post"), labels = c("pre burial", "during burial", "post burial")))
  
  # Compute summary statistics, for participants
  d_ritual_summary_pid <- d_ritual %>% 
    group_by(pid, subscale) %>% 
    summarise(sum_score = sum(response)) 

  # Prosociality
  d_prosoc <- data$d_prosoc %>% 
  mutate(
  label = factor(response, labels = c("Never/Almost Never", "Rarely", "Occasionally", "Often", "Always/Almost Always")),
  subscale = ifelse(item_number %in% c(5, 8, 12, 16), "prosocial feelings", "prosocial actions")
  )

  # Compute summary statistics, for participants
  d_prosoc_summary_pid <- d_prosoc %>% 
    group_by(pid, subscale) %>% 
    summarise(sum_score = sum(response)) 

    d_SWB <- read.csv("data/subjective_wellbeing.csv") %>% 
    mutate(
    label = factor(response, labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    subscale = case_when(
      item_number %in% 5:16 ~ "psychological well-being",
      item_number %in% c(1, 2, 3, 22, 23, 4) ~ "physical health and well-being",
      item_number %in% 17:21 ~ "relationships"
    )
    )

  # Subjective well-being
  d_SWB <- data$d_SWB %>% 
  mutate(
  label = factor(response, labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
  subscale = case_when(
    item_number %in% 5:16 ~ "psychological well-being",
    item_number %in% c(1, 2, 3, 22, 23, 4) ~ "physical health and well-being",
    item_number %in% 17:21 ~ "relationships"
  )
  )
  # Compute summary statistics, for participants
  d_SWB_summary_pid <- d_SWB %>% 
    group_by(pid, subscale) %>% 
    summarise(sum_score = sum(response)) 

  # Get overall summary scores for all scales
  d_ritual_summary_pid2 <- d_ritual_summary_pid %>% 
    group_by(pid) %>% 
    summarise(ritual_sum_score = sum(sum_score)) 

  d_prosoc_summary_pid2 <- d_prosoc_summary_pid %>% 
    group_by(pid) %>% 
    summarise(prosoc_sum_score = sum(sum_score)) 

  d_SWB_summary_pid2 <- d_SWB_summary_pid %>% 
    group_by(pid) %>% 
    summarise(SWB_sum_score = sum(sum_score))

  # Demographics
  d_demographics <- data$d_demographics

    
  # Join dataframes
  d_summary_pid <- left_join(d_ritual_summary_pid2, d_prosoc_summary_pid2) %>%
    left_join(d_SWB_summary_pid2) |> 
    mutate(
      prosoc_z = as.numeric(scale(prosoc_sum_score)),
      ritual_z = as.numeric(scale(ritual_sum_score)),
      SWB_z = as.numeric(scale(SWB_sum_score))
    ) |> 
      left_join(d_demographics)

  # lose just a couple of cases
  d_summary_pid <- d_summary_pid[complete.cases(d_summary_pid),]

  return(d_summary_pid)
}