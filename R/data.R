load.google.keywords <- function(.path) {
  cols <-
    c("Avg..monthly.searches",
      "Min.search.volume",
      "Max.search.volume")
  google <- list.files(path = .path,
                       full.names = T,
                       pattern = ".csv") %>%
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
      avg.monthly.searches = Avg..monthly.searches,
      min.search.volume = Min.search.volume,
      max.search.volume = Max.search.volume,
      competition.quality = Competition,
      competition = Competition..indexed.value.
    ) %>%
    mutate(
      Top.of.page.bid..low.range. = as.numeric(Top.of.page.bid..low.range.),
      Top.of.page.bid..high.range. = as.numeric(Top.of.page.bid..high.range.),
      bid = (Top.of.page.bid..low.range. + Top.of.page.bid..high.range.) /
        2,
      avg.monthly.searches = ifelse(
        is.na(avg.monthly.searches),
        (min.search.volume + max.search.volume) / 2 ,
        avg.monthly.searches
      ),
      competition = as.numeric(competition)
    ) %>%
    mutate(provider = "google") %>%
    as_tibble()
}

load.microsoft.keywords <- function(.path, .seed = F) {
  list.files(path = .path %>% glue(),
             full.names = T,
             pattern = ".csv") %>%
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
    mutate(Seed = as.factor(na.locf(
      Seed, fromLast = F, na.rm = F
    ))) %>%
    select(-Ad.impr..share) %>%
    rename(
      keyword = Keyword,
      avg.monthly.searches = Average.monthly.searches,
      competition = Competition,
      bid = Suggested.bid..EUR.
    ) %>%
    mutate(provider = "microsoft", competition = competition * 100) %>%
    as_tibble()

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
