hypothesis_model_compare <- function(model_list, hypothesis) {
  if (hypothesis == "H1a") variables = "ritual_z"
  if (hypothesis == "H2a") variables = c("ritual_z", "SWB_z")
  
  # init dfs
  ME <- data.frame(
    term = c(),
    group = c(),
    contrast = c(),
    estimate = c(),
    conf.low = c(),
    conf.high = c(),
    model = c()
  )

  pd <- data.frame(
    pd = c(),
    term = c(),
    group = c(),
    model = c()
  )
  
  # marginal effects and pd for each model
  for (i in 1:length(model_list)) {

    model <- model_list[[i]]
    model_name <- names(model_list)[i]
    
    if (model_name %in% c("IPTW", "IPTW_reg")) ME_temp <- marginaleffects::avg_comparisons(model, variables = variables, wts = model$data$weights)
    if (model_name %in% c("unadjusted", "reg")) ME_temp <- marginaleffects::avg_comparisons(model, variables = variables)
    
    ME_draws <- ME_temp %>% posterior_draws()
    ME_temp <- ME_temp %>% mutate(hypothesis = hypothesis)
  
    if (hypothesis == "H2a") {
      # indirect effect of ritual = effect of ritual on SWB * effect of SWB on prosoc
      mediation_temp <- ME_draws %>% 
        group_by(drawid) %>% 
        summarise(indirect_effect = draw[term == "ritual_z" & group == "SWBz"] * draw[term == "SWB_z" & group == "prosocz"]) %>% 
        summarise(estimate = median(indirect_effect), conf.low = quantile(indirect_effect, 0.025), conf.high = quantile(indirect_effect, 0.975), pd = mean(indirect_effect > 0)) %>% 
        mutate(term = "ritual_z", group = "prosocz", contrast = "mean(+1)", hypothesis = paste0(hypothesis, "_mediation"))
      
      # calculate probability of direction 
      pd_temp <- ME_draws %>% 
        group_by(group, term) %>% 
        summarise(pd = mean(draw > 0)) %>% 
        mutate(hypothesis = hypothesis)
      # bind indirect effect with other estimates
      ME_temp <- bind_rows(ME_temp, mediation_temp)
      
    } else if (hypothesis == "H1a") {
      pd_temp <- ME_draws %>%  
        group_by(term) %>% 
        summarise(pd = mean(draw > 0)) %>% 
        mutate(group = "prosocz", hypothesis = hypothesis)
    }
    
    ME <- bind_rows(ME, ME_temp |> mutate(model = model_name) |> 
                      select(-c(starts_with("predicted"), tmp_idx)))
    pd <- bind_rows(pd, pd_temp %>% mutate(model = model_name))
  }

  if (hypothesis == "H2a") {
    results <- ME |> 
      left_join(pd, by = c("term", "group", "model", "hypothesis")) %>% 
    mutate(pd = coalesce(pd.x, pd.y)) %>%
    select(-pd.x, -pd.y)
  } else if (hypothesis == "H1a") {
    results <- ME |> 
      left_join(pd, by = c("term", "model", "hypothesis"))
  }
  
  # export results
  write_csv(results %>% filter(pd != 0, !(group == "SWB" & term == "SWB")), paste0("results/", hypothesis, "_results.csv"))
  return(results %>% filter(pd != 0, !(group == "SWB" & term == "SWB")))
}