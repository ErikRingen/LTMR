plot_model_checks <- function(m_h2_double, d_models){
  
  prosoc_z <- m_h2_double$data$prosoc_z
  SWB_z <- m_h2_double$data$SWB_z
  
  prosoc_rep <- posterior_predict(m_h2_double, resp = "prosocz")
  SWB_rep <- posterior_predict(m_h2_double, resp = "SWBz")
  
  d_rep <- data.frame(
    prosoc_z = prosoc_z,
    SWB_z = SWB_z,
    ritual_z = d_models$ritual_z,
    prosoc_rep_avg = apply(prosoc_rep, 2, mean),
    SWB_rep_avg = apply(SWB_rep, 2, mean)
  ) %>% 
    mutate(residual_prosoc = prosoc_z - prosoc_rep_avg, residual_SWB = SWB_z - SWB_rep_avg)
  
  bayesplot::color_scheme_set("red")
  
  p_dens_prosoc <- ppc_dens_overlay(y = prosoc_z, yrep = prosoc_rep[1:200,]) + theme_classic(base_size = 15) + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + 
    labs(subtitle = "Prosociality (z-score)")
  
  p_dens_SWB <-  ppc_dens_overlay(y = SWB_z, yrep = SWB_rep[1:200,]) + theme_classic(base_size = 15) + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + 
    labs(subtitle = "Subjective Wellbeing (z-score)")
    
    # Visualize prosociality residuals
    p_res_prosoc_pred <- ggplot(d_rep, aes(x = prosoc_rep_avg, y = residual_prosoc)) + 
    geom_point(alpha = 0.6) +
    geom_smooth(method = "gam", color = "indianred", fill = "indianred") +
    xlab("Avg. yrep (Prosociality)") +
    ylab("Avg. residual (Prosociality)") +
    theme_classic(base_size = 15)
  
    p_res_prosoc_ritual <- ggplot(d_rep, aes(x = ritual_z, y = residual_prosoc)) + 
      geom_point(alpha = 0.6) +
      geom_smooth(method = "gam", color = "indianred", fill = "indianred") +
      xlab("Ritual participation (z-score)") +
      ylab("Avg. Residual (Prosociality)") +
      theme_classic(base_size = 15)
    
    p_res_prosoc_SWB <- ggplot(d_rep, aes(x = SWB_z, y = residual_prosoc)) + 
      geom_point(alpha = 0.6) +
      geom_smooth(method = "gam", color = "indianred", fill = "indianred") +
      xlab("Subjective Wellbeing (z-score)") +
      ylab("Avg. Residual (Prosociality)") +
      theme_classic(base_size = 15)
    
    # Visualize SWB residuals
    p_res_SWB_pred <- ggplot(d_rep, aes(x = SWB_rep_avg, y = residual_SWB)) + 
      geom_point(alpha = 0.6) +
      geom_smooth(method = "gam", color = "indianred", fill = "indianred") +
      xlab("Avg. yrep (SWB)") +
      ylab("Avg. residual (SWB)") +
      theme_classic(base_size = 15)
    
    p_res_SWB_ritual <- ggplot(d_rep, aes(x = ritual_z, y = residual_SWB)) + 
      geom_point(alpha = 0.6) +
      geom_smooth(method = "gam", color = "indianred", fill = "indianred") +
      xlab("Ritual participation (z-score)") +
      ylab("Avg. Residual (SWB)") +
      theme_classic(base_size = 15)
    
    p_comb <- (p_dens_prosoc + p_dens_SWB) / (p_res_prosoc_pred + p_res_SWB_pred) / (p_res_prosoc_ritual + p_res_SWB_ritual) + (p_res_prosoc_SWB + plot_spacer()) +  plot_layout(guides = 'collect')
    
    ggsave("figures/model_checks.png", plot = p_comb, dpi = 900, width = 8.5, height = 11)
    return(p_comb)
}
