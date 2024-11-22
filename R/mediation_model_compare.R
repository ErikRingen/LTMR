mediation_model_compare <- function(model_list){

  d_indirect <- data.frame(
    estimate = c(),
    conf.low = c(),
    conf.high = c(),
    model = c(),
    pd = c()
  )
  
  for (i in 1:length(model_list)) {
    post <- as_draws_df(model_list[[i]])

    indirect_effect <- post$b_SWBz_ritual_z * post$b_prosocz_SWB_z

    d_temp <- data.frame(
      estimate = median(indirect_effect),
      conf.low = as.numeric(quantile(indirect_effect, 0.025)),
      conf.high = as.numeric(quantile(indirect_effect, 0.975)),
      pd = mean(indirect_effect > 0),
      model = names(model_list)[[i]]
    )

    d_indirect <- bind_rows(d_indirect, d_temp)
  }

  d_indirect <- d_indirect |> 
    mutate(term = "ritual_z", group = "prosocz", contrast = "mean(+1)")

  write_csv(d_indirect, "results/H2a_indirect_effect_results.csv")
  return(d_indirect)
  }