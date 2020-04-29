distribution.quantitative.plot <- function(data) {
  data %>%
    select_if(is.numeric) %>%
    gather(key = "feature", value = "value") %>%
    ggplot(aes(x = value)) +
    geom_histogram() +
    facet_grid( ~ feature, scales = "free")
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
                         bid.feature.name = "bid") {
  bid.feature <- sym(bid.feature.name)
  data %>%
    mutate(price.chance = 1 / !!sym(bid.feature.name)) %>%
    ggplot(aes(
      x = avg.monthly.searches,
      y = competition,
      size = price.chance,
      color = !!sym(bid.feature.name)
    )) +
    geom_point(alpha = .alpha) +
    geom_label_repel(aes(label = keyword)) +
    scale_color_gradient(low = "#59a14f", high = "#bab0ac") +
    scale_x_log10() +
    scale_y_log10()
}
