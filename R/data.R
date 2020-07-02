load.keywords <- function(.files, .seed = F) {
  tryCatch({
    logging::logdebug("semrush")
    load.keywords(.files)
  }, error = function(e) {
    logging::logdebug("google")
    tryCatch({
      load.google.keywords(.files, .seed = F)
    }, error = function(e) {
      logging::logdebug("microsoft")
      load.microsoft.keywords(.files, .seed = F)
    })
    
  })
}

load.semrush.keywords <- function(.files) {
  if (dir.exists(.files)) {
    .files <- list.files(path = .files,
                         full.names = T,
                         pattern = ".csv")
  }
  
  .files %>%
    map(~ read_csv(.)) %>%
    bind_rows() %>%
    rename(
      keyword = Keyword,
      volume = Volume,
      competition = `Keyword Difficulty`,
      cpc = `CPC (USD)`,
      serp_features = `SERP Features`,
      competitive_density = `Competitive Density`,
      number_results = `Number of Results`
    ) %>%
    mutate(
      competition_scale = scales::rescale(competition, c(1, 0)),
      competitive_density_scale = scales::rescale(competitive_density, c(1, 0)),
      cpc_scale = scales::rescale(cpc, c(1, 0)),
      volume_scale = scales::rescale(volume, c(0, 1)),
      number_results_scale = scales::rescale(number_results, c(1, 0)),
    ) %>%
    mutate(overall_rank = rowSums(select(., ends_with("_scale"))) /
             5) %>%
    select(-ends_with("_scale")) %>%
    mutate(
      profit_pontential = profit_for_specified_traffic(volume, cpc, 40, 0.0075, .015),
      product_cost = product_cost_for_profit(0.5, cpc, 0.0075)
    )
  
  
}


load.google.keywords <- function(.files, .seed = F) {
  cols <-
    c("Avg..monthly.searches",
      "Min.search.volume",
      "Max.search.volume")
  if (dir.exists(.files)) {
    .files <- list.files(path = .files,
                         full.names = T,
                         pattern = ".csv")
  }
  
  
  tryCatch({
    .files %>%
      map(
        ~ read.delim(
          .,
          fileEncoding = 'UTF-16LE',
          stringsAsFactors = F,
          skip = 2
        ) %>%
          mutate_if(is.numeric, as.character) %>%
          mutate_at(cols, funs(str_replace_all(., ",", "")))
      ) %>%
      bind_rows() %>%
      distinct(Keyword, .keep_all = T) %>%
      mutate_at(cols, funs(as.integer)) %>%
      rename(
        keyword = Keyword,
        currency = Currency,
        volume = Avg..monthly.searches,
        min_volume = Min.search.volume,
        max_volume = Max.search.volume,
        competition_quality = Competition,
        competition = Competition..indexed.value.
      ) %>%
      mutate(
        Top.of.page.bid..low.range. = as.numeric(Top.of.page.bid..low.range.),
        Top.of.page.bid..high.range. = as.numeric(Top.of.page.bid..high.range.),
        cpc = (
          Top.of.page.bid..low.range. + Top.of.page.bid..high.range.
        ) /
          2,
        volume = ifelse(is.na(volume),
                        (min_volume + max_volume) / 2 ,
                        volume),
        competition = as.numeric(competition)
      ) %>%
      mutate(provider = "google") %>%
      as_tibble()
  }, error = function(e) {
    .files %>%
      map( ~ read_csv(.)) %>%
      bind_rows()
  })
}

load.microsoft.keywords <- function(.files, .seed = F) {
  if (dir.exists(.files)) {
    .files <- list.files(path = .files,
                         full.names = T,
                         pattern = ".csv")
  }
  
  tryCatch({
    logdebug("loading microsoft planner format data...")
    data <- .files %>%
      map(
        ~ read.delim(., fileEncoding = 'UTF-16LE', stringsAsFactors = F) %>%
          mutate_if(is.numeric, as.character)
      ) %>%
      bind_rows() %>%
      rename(Seed = Ad.group) %>%
      mutate(Seed = ifelse(.seed == T &
                             Seed == "Seed Keywords", Keyword, NA)) %>%
      mutate(
        `Average.monthly.searches` = as.numeric(`Average.monthly.searches`),
        `Competition` = as.numeric(`Competition`),
        `Suggested.bid..EUR.` = as.numeric(`Suggested.bid..EUR.`)
      ) %>%
      select(-Ad.impr..share) %>%
      rename(
        keyword = Keyword,
        volume = Average.monthly.searches,
        competition = Competition,
        cpc = Suggested.bid..EUR.
      ) %>%
      mutate(provider = "microsoft",
             competition = competition * 100)
    
    logdebug("loaded microsoft planner format data.")
    data
  }, error = function(e) {
    logdebug("error {e} try to load standard csv"  %>% glue())
    data <-
      .files %>%
      map( ~ read_csv(.)) %>%
      bind_rows()
    
    data
  })
  
  
}

split.write <- function(data, folder, num.per.it = 998) {
  total.num <- data %>% count() %>% pull(n)
  num.it <- (total.num %/% num.per.it) + 1
  
  
  for (num in seq(0, num.it)) {
    start <- num * num.per.it + 1
    end <- (num + 1) * num.per.it
    google.keywords %>% slice(start:end) %>%
      select(keyword) %>%
      write.csv(
        "{folder}/keywords_list_{num}.csv" %>% glue,
        col.names = F,
        row.names = F,
        quote = F
      )
  }
  
}
