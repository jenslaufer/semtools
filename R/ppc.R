cost_per_conversion <- function(cpc, cr) {
  cpc / cr
}

num_of_conversions <- function(ad_budget, cpc, cr) {
  ad_budget / cost_per_conversion(cpc, cr)
}

revenue <- function(ad_budget, product_cost, cpc, cr) {
  num_of_conversions(ad_budget, cpc, cr) * product_cost
}

profit <- function(ad_budget, product_cost, cpc, cr) {
  revenue(ad_budget, product_cost, cpc, cr) - ad_budget
}

ad_budget_for_profit <- function(profit, product_cost, cpc, cr) {
  profit * cost_per_conversion(cpc, cr) / (product_cost - cost_per_conversion(cpc, cr))
}

traffic_for_profit <- function(profit, product_cost, cpc, cr, ctr) {
  ad_budget_for_profit(profit, product_cost, cpc, cr) / (ctr * cpc)
}

profit_for_specified_traffic  <-
  function(traffic, cpc, product_cost, cr, ctr) {
    traffic_to_site <- traffic * ctr
    ad_cost <- traffic_to_site * cpc
    profit(ad_cost, product_cost, cpc, cr)
  }

product_cost <- function(profit_ad_cost_ratio, cpc, cr) {
  ad_budget <- 1 / profit_ad_cost_ratio
  (1 + ad_budget) / num_of_conversions(ad_budget, cpc, cr)
}