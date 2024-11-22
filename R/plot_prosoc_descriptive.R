plot_prosoc_descriptive <- function(data){
  d_prosoc <- data$d_prosoc %>% 
    mutate(
      label = factor(response, labels = c("Never/Almost Never", "Rarely", "Occasionally", "Often", "Always/Almost Always")),
      subscale = ifelse(item_number %in% c(5, 8, 12, 16), "prosocial feelings", "prosocial actions")
    )
  
  # Compute summary statistics
  d_prosoc_summary <- d_prosoc %>% 
    group_by(item) %>% 
    summarise(mean = mean(response, na.rm = T), subscale = unique(subscale)) 
  
  # Compute summary statistics, for participants
  d_prosoc_summary_pid <- d_prosoc %>% 
    group_by(pid, subscale) %>% 
    summarise(sum_score = sum(response)) 
  
  p_bars <- d_prosoc %>% 
    filter(!is.na(response)) %>% 
    ggplot(aes(x = fct_reorder(item, response, .fun = mean), fill = fct_rev(label))) + 
    geom_bar() +
    scale_fill_manual(values= rev(c("#440145FF", "#404788FF", "#238A8DFF", "#55C667FF", "#FDE725FF"))) + 
    facet_grid(subscale ~., scales = "free_y") +
    coord_flip() +
    theme_minimal() +
    theme( strip.background = element_blank(), legend.position = c(-1.1, 0.5), legend.title = element_blank()) +
    xlab("") +
    ylab("count")
  
  p_mean <- ggplot(d_prosoc_summary, aes(x = fct_reorder(item, mean), y = mean)) + 
    facet_grid(subscale ~., scales = "free_y") +
    geom_point() + 
    coord_flip() + 
    theme_minimal() +
    xlab("")
  
  p_pairs <- d_prosoc_summary_pid %>% 
    pivot_wider(names_from = subscale, values_from = sum_score) %>% ggpairs(columns = 2:3) + 
    theme_minimal() +
    xlab("sum score") +
    ylab("sum score") +
    theme(panel.spacing = unit(2, "lines"))
  
  ggsave("figures/prosoc_descriptive_bars.png", plot = p_bars, dpi = 900, width = 10, height = 4, bg = "white")
  ggsave("figures/prosoc_descriptive_mean.png", plot = p_mean, dpi = 900, width = 10, height = 4, bg = "white")
  ggsave("figures/prosoc_descriptive_pairs.png", plot = p_pairs, dpi = 900, width = 6, height = 6, bg = "white")
  
  return(list(p_bars = p_bars, p_mean = p_mean, p_pairs = p_pairs))
}