plot_prosoc_descriptive <- function(data){
  d_SWB <- data$d_SWB %>% 
    mutate(
      label = factor(response, labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
      subscale = case_when(
        item_number %in% 5:16 ~ "psychological well-being",
        item_number %in% c(1, 2, 3, 22, 23, 4) ~ "physical health and well-being",
        item_number %in% 17:21 ~ "relationships"
      )
    )
  
  # Compute summary statistics
  d_SWB_summary <- d_SWB %>% 
    group_by(item) %>% 
    summarise(mean = mean(response, na.rm = T), subscale = unique(subscale)) 
  
  # Compute summary statistics, for participants
  d_SWB_summary_pid <- d_SWB %>% 
    group_by(pid, subscale) %>% 
    summarise(sum_score = sum(response)) 
  
  p_bars <- d_SWB %>% 
    filter(!is.na(response)) %>% 
    ggplot(aes(x = fct_reorder(item, response, .fun = mean), fill = fct_rev(label))) + 
    geom_bar() +
    scale_fill_manual(values= rev(c("#440145FF", "#404788FF", "#238A8DFF", "#55C667FF", "#FDE725FF"))) + 
    facet_grid(subscale ~., scales = "free_y") +
    coord_flip() +
    theme_minimal() +
    theme( strip.background = element_blank(), legend.position = c(-0.8, 0.55), legend.title = element_blank()) +
    xlab("") +
    ylab("count")
  
  p_mean <- ggplot(d_SWB_summary, aes(x = fct_reorder(item, mean), y = mean)) + 
    facet_grid(subscale ~., scales = "free_y") +
    geom_point() + 
    coord_flip() + 
    theme_minimal() +
    xlab("")
  
  p_pairs <- d_SWB_summary_pid %>% 
    pivot_wider(names_from = subscale, values_from = sum_score) %>% ggpairs(columns = 2:3) + 
    theme_minimal() +
    xlab("sum score") +
    ylab("sum score") +
    theme(panel.spacing = unit(2, "lines"))
  
  ggsave("figures/SWB_descriptive_bars.png", plot = p_bars, dpi = 900, width = 10, height = 6, bg = "white")
  ggsave("figures/SWB_descriptive_mean.png", plot = p_mean, dpi = 900, width = 10, height = 6, bg = "white")
  ggsave("figures/SWB_descriptive_pairs.png", plot = p_pairs, dpi = 900, width = 6, height = 6, bg = "white")
  
  return(list(p_bars = p_bars, p_mean = p_mean, p_pairs = p_pairs))
}