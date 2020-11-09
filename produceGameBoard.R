# A function to plot the game board
produce_board_plot <- function(plot_df, 
                               quarantine_labels, quarantine_levels, 
                               infection_labels, infection_levels,
                               test_labels, test_levels){
  
  plot_df$shown <- factor(plot_df$shown, levels = infection_levels)
  plot_df$hidden <- factor(plot_df$hidden, levels = infection_levels)
  plot_df$quarantined <- factor(plot_df$quarantined, levels = quarantine_levels)
  plot_df$tested <- factor(plot_df$tested, levels = test_levels)
  
  pnt_size = 3.5
  board_plot <- ggplot(plot_df) +
    geom_point(aes(x = X, y = Y, col = quarantined),
               shape = 15, size = pnt_size + 2, alpha = 0.8) +
    scale_color_manual("EXPOSURE STATUS",
                       labels = quarantine_labels,
                       values = c("No" = "lightgray", "Yes" = "coral"),
                       drop = FALSE) +
    geom_point(aes(X, Y, fill = shown),
               shape = 21, size = pnt_size) +
    scale_fill_manual("INFECTION STATUS",
                      labels = infection_labels,
                      values = c("S" = "gray", "I" = "red", "R" = "blue"),
                      drop = FALSE) +
    geom_point(aes(X, Y, shape = tested), size = pnt_size, stroke = 1) +
    scale_shape_manual("TEST STATUS",
                       labels = test_labels,
                       values = c("unknown" = 1, "tested" = 13),
                       drop = FALSE) +
    coord_fixed() +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          title = element_text(size = 16, hjust = 0.5)) +
    guides(fill = guide_legend(order=1),
           col = guide_legend(order=2),
           shape = guide_legend(order=3))
  
  return(board_plot)
  
}