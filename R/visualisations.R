distribution.quantitative.plots <- function(data) {
  data %>%
    select_if(is.numeric) %>%
    gather(key = "feature", value = "value") %>%
    ggplot(aes(x = value)) +
    geom_histogram() +
    facet_wrap( ~ feature, scales = "free") +
    labs(title = "Distribution")
}

distribution.quantitative.plot <- function(data, feature.name) {
  data %>%
    ggplot(aes(x = !!sym(feature.name))) +
    geom_histogram() +
    labs(title = "Distribution {feature.name}" %>% glue())
}


distribution.qualitative.plots <- function(data) {
  data %>%
    select_if(negate(is.numeric)) %>%
    gather(key = "feature", value = "value") %>%
    ggplot(aes(x = value)) +
    geom_bar() +
    facet_wrap( ~ feature, scales = "free") +
    labs(title = "Distribution")
}

distribution.qualitative.plot <- function(data, feature.name) {
  data %>%
    ggplot(aes(x = !!sym(feature.name))) +
    geom_bar() +
    labs(title = "Distribution {feature.name}" %>% glue())
}

keyword.plot <- function(data,
                         .alpha = 1,
                         .x.trans = "identity",
                         .y.trans = "identity",
                         .size.trans = "identity",
                         .color.trans = "identity",
                         .labels = T,
                         x.feature.name,
                         y.feature.name,
                         color.feature.name,
                         size.feature.name) {
  plot <- data %>%
    ggplot(aes(
      x = !!sym(x.feature.name),
      y = !!sym(y.feature.name)
    )) +
    geom_point(aes(
      size = !!sym(size.feature.name),
      color = !!sym(color.feature.name)
    ), alpha = .alpha) +
    scale_x_continuous(trans = .x.trans) +
    scale_y_continuous(trans = .y.trans) +
    scale_size_continuous(trans = .size.trans) +
    labs(title = "{x.feature.name} vs {y.feature.name} vs {size.feature.name} vs {color.feature.name}" %>% glue())
  
  
  if (data %>%
      select(color.feature.name) %>%
      map_chr(class) == "numeric") {
    plot <- plot +
      scale_colour_gradientn(colours = terrain.colors(10),
                             trans = .color.trans)
  }
  
  
  if (.labels) {
    plot <- plot +
      geom_label_repel(aes(label = keyword))
  }
  plot
}
