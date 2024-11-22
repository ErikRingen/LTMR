plot_model_results <- function(h1_results, h2_results){

  combined_results <- h1_results |> 
    bind_rows(h2_results) |> 
    # readable labels
    mutate(term = str_remove_all(
      term, "_z"),
      group = str_remove_all(
        group, "z")
    ) |> 
    mutate(
      path = paste(term, "→", group)
    ) |> 
    mutate(path = case_when(
      path == "ritual → prosoc" & hypothesis == "H1a" ~ "ritual → prosoc \n(total)",
      path == "ritual → prosoc" & hypothesis == "H2a" ~ "ritual → prosoc \n(direct)",
      path == "ritual → prosoc" & hypothesis == "H2a_mediation" ~ "ritual → SWB → prosoc \n(indirect)",
      .default = path
    )
  ) |> 
    mutate(
      model_label = fct_recode(factor(model),
       `regression adjustment` = "reg",
       `IPTW + regression adjustment` = "IPTW_reg"
      ), 
      model = fct_relevel(model_label, "unadjusted", "regression adjustment", "IPTW", "IPTW + regression adjustment"),
      evidence = fct_drop(case_when(
        pd > 0.5 & pd < 0.8 ~ "0.5 < pd < 0.8",
        pd > 0.8 & pd < 0.95 ~ "0.8 < pd < 0.95",
        pd > 0.95 ~ "pd > 0.95"
      ))
    ) |>
    mutate(path = fct_relevel(path, "SWB → prosoc", "ritual → SWB", "ritual → prosoc \n(total)", "ritual → prosoc \n(direct)", "ritual → SWB → prosoc \n(indirect)")) %>% 
    filter(path != "SWB → SWB")
  
  p <- ggplot(combined_results, aes(x = estimate, y = fct_rev(path), color = model, linetype = fct_rev(evidence))) + 
    geom_point(position = position_dodge(width = 0.5), size = 3) + 
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, position = position_dodge(width = 0.5), lwd = 1) + 
    scale_color_viridis_d() +
    geom_vline(xintercept = 0, lwd = 0.5) +
    theme_classic(base_size = 18) + 
    ylab("") + 
    theme(legend.title = element_blank()) +
    xlab("Average Marginal Effect (standardized)")

  ggsave("figures/avg_marginal.png", plot = p, dpi = 900, width = 11, height = 6)

  return(p)
}