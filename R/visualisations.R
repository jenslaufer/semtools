distribution.quantitative.plot <- function(data) {
  data %>%
    select_if(is.numeric) %>%
    gather(key = "feature", value = "value") %>%
    ggplot(aes(x = value)) +
    geom_histogram() +
    facet_grid( ~ feature, scales = "free") +
    labs(title = "Distribution")
}


distribution.qualitative.plot <- function(data) {
  data %>%
    select_if(negate(is.numeric)) %>%
    gather(key = "feature", value = "value") %>%
    ggplot(aes(x = value)) +
    geom_bar() +
    facet_grid( ~ feature, scales = "free")
}

keyword.plot <- function(data,
                         .alpha = 1,
                         .x.trans = "log10",
                         .y.trans = "log10",
                         .labels = T,
                         bid.feature.name = "bid") {
  bid.feature <- sym(bid.feature.name)
  plot <- data %>%
    mutate(price.chance = 1 / !!sym(bid.feature.name)) %>%
    ggplot(aes(
      x = avg.monthly.searches,
      y = competition,
      color = !!sym(bid.feature.name)
    )) +
    geom_point(aes(size = price.chance), alpha = .alpha) +
    scale_colour_gradientn(colours = rev(terrain.colors(10))) +
    scale_x_continuous(trans = .x.trans) +
    scale_y_continuous(trans = .y.trans) +
    labs(title = "Average Monttly Searches vs Competition vs Bid Price")
  
  if (.labels) {
    plot <- plot +
      geom_label_repel(aes(label = keyword))
  }
  plot
}
