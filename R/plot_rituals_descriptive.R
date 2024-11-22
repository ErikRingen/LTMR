plot_rituals_descriptive <- function(data){
  d_ritual <- data$d_rituals %>% 
    mutate(label = factor(response, labels = c("never", "closely witnessed", "participated"))) %>% 
    mutate(subscale = factor(subscale, levels = c("pre", "during", "post"), labels = c("pre burial", "during burial", "post burial")))
  
  p_bars <- d_ritual %>% 
    ggplot(aes(x = fct_reorder(item, response, .fun = mean), fill = fct_rev(label))) + 
    geom_bar() +
    scale_fill_manual(values= rev(c("#440145FF", "#238A8DFF", "#55C667FF"))) + 
    facet_grid(subscale ~., scales = "free_y") +
    coord_flip() +
    theme_minimal() +
    theme( strip.background = element_blank(), legend.position = c(-2, 0.85), legend.title = element_blank()) +
    xlab("") +
    ylab("count")
  
  # Compute summary statistics
  d_ritual_summary <- d_ritual %>% 
    group_by(item) %>% 
    summarise(mean = mean(response), subscale = unique(subscale)) 
  
  # Compute summary statistics, for participants
  d_ritual_summary_pid <- d_ritual %>% 
    group_by(pid, subscale) %>% 
    summarise(sum_score = sum(response)) 
  
  p_mean <- ggplot(d_ritual_summary, aes(x = fct_reorder(item, mean), y = mean)) + 
    facet_grid(subscale ~., scales = "free_y") +
    geom_point() + 
    coord_flip() + 
    scale_y_continuous(limits = c(1, 3)) +
    theme_minimal() +
    xlab("")
  
  p_pairs <- d_ritual_summary_pid %>% 
    pivot_wider(names_from = subscale, values_from = sum_score) %>% ggpairs(columns = 2:4) + 
    theme_minimal() +
    xlab("sum score") +
    ylab("sum score") +
    theme(panel.spacing = unit(2, "lines"))
  
  ggsave("figures/rituals_descriptive_bars.png", plot = p_bars, dpi = 900, width = 6, height = 4, bg = "white")
  ggsave("figures/rituals_descriptive_mean.png", plot = p_mean, dpi = 900, width = 6, height = 4, bg = "white")
  ggsave("figures/rituals_descriptive_pairs.png", plot = p_pairs, dpi = 900, width = 6, height = 6, bg = "white")
  
  return(list(p_bars = p_bars, p_mean = p_mean, p_pairs = p_pairs))
}