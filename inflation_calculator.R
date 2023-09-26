# "What's your rate of inflation" interactive worksheet
library(tidyverse)
library(scales)
library(lubridate)
library(magrittr)

# Create DF with weights and item codes. For now only need this back a few years.
cpi_working <- cpi_data %>% 
  filter(year >= 2020,
         !is.na(date)) %>% 
  select(series_id, year, period, date, value) %>% 
  left_join(cpi_series %>% select(series_id, item_code),
            by = "series_id") %>% 
  left_join(cpi_wts, by = c("date", "item_code"))

# Function for reweighting an item
rewt <- function(item, adj_factor, startdate, all_items = "CUUR0000SA0") {
  
  total_chg <- cpi_working %>% 
    filter(series_id == all_items,
           period != "M13",
           date >= ymd(startdate)) %>% 
    arrange(date) %>% 
    mutate(chg = value/first(value)) %>% 
    filter(date == max(date)) %>% 
    .$chg %>% as.numeric
  
  item_chg <- cpi_working %>% 
    filter(series_id == item,
           period != "M13",
           date >= ymd(startdate)) %>% 
    arrange(date) %>% 
    mutate(chg = value/first(value)) %>% 
    filter(date == max(date)) %>% 
    .$chg %>% as.numeric
  
  item_start_wt <- cpi_working %>% 
    filter(series_id == item,
           date == ymd(startdate)) %>% 
    .$wt %>% as.numeric
  
  item_end_wt <- item_start_wt * item_chg
  
  ex_item_chg <- (100 * total_chg - item_end_wt)/
    (100 - item_start_wt)
  
  new_wt <- item_start_wt * adj_factor
  
  reweighted <- (100 - new_wt) * (ex_item_chg - 1) +
    new_wt * (item_chg - 1)
  
  # How much did we adjust up or down the weights of everything else in the basket?
  total_wt_adj <- (100 - new_wt)/(100 - item_start_wt)
  
  output <- tibble(series = c("ex_item_chg", 
                              "new_wt",
                              "total_wt_adj",
                              "reweighted"),
                   value = c(ex_item_chg - 1, 
                             new_wt, 
                             total_wt_adj,
                             reweighted))
  
  output
}

rewt("CUUR0000SA0E", 1.2, "2021-02-01")

ex_items("CUUR0000SA0E", "2021-02-01", all_items = "CUUR0000SA0")

# To adjust more items, we need to start with the previous calculations
# `pre_wt_adj` is adjustment factor for weights based on previous calculations.
# `pre_chg` is total change based on previous calculations.

re_rewt <- function(item, adj_factor, startdate,
                    pre_wt_adj, pre_chg) {
  
  total_chg <- pre_chg/100 + 1
  
  item_chg <- cpi_working %>% 
    filter(series_id == item,
           period != "M13",
           date >= ymd(startdate)) %>% 
    arrange(date) %>% 
    mutate(chg = value/first(value)) %>% 
    filter(date == max(date)) %>% 
    .$chg %>% as.numeric
  
  item_start_wt <- cpi_working %>% 
    filter(series_id == item,
           date == ymd(startdate)) %>% 
    .$wt %>% as.numeric
  item_start_wt <- item_start_wt * pre_wt_adj
  
  item_end_wt <- item_start_wt * item_chg
  
  ex_item_chg <- (100 * total_chg - item_end_wt)/
    (100 - item_start_wt)
  
  new_wt <- item_start_wt * adj_factor
  
  reweighted <- (100 - new_wt) * (ex_item_chg - 1) +
    new_wt * (item_chg - 1)
  
  # How much did we adjust up or down the weights of everything else in the basket?
  total_wt_adj <- (100 - new_wt)/(100 - item_start_wt)
  total_wt_adj <- total_wt_adj * pre_wt_adj
  
  output <- tibble(series = c("ex_item_chg", 
                              "new_wt",
                              "total_wt_adj",
                              "reweighted"),
                   value = c(ex_item_chg - 1, 
                             new_wt, 
                             total_wt_adj,
                             reweighted))
  
  output
  
}

rewt("CUUR0000SA0E", 0, "2021-02-01") # ex-energy
re_rewt("CUUR0000SAF1", 0, "2021-02-01", 1.07, 6.63) # ex-food and energy
re_rewt("CUUR0000SAH1", 0, "2021-02-01", 1.26, 6.41) # ex-food, shelter and energy

# Try doing these sequentially:

adj_list <- tibble(series = c("CUUR0000SA0E", "CUUR0000SAF1", "CUUR0000SAH1"),
       adj = c(0, 0, 0))

mult_cpi_adj <- function(inputs, total_chg) {
  inputs <- inputs %>% 
    mutate(calc_wt_adj = NA,
           calc_pre_chg = NA)
  
  pre_wt_adj <- 1
  pre_chg <- total_chg
  
  for (i in seq_along(1:nrow(inputs))) {
    inputs$calc_wt_adj[i] <- pre_wt_adj
    inputs$calc_pre_chg[i] <- pre_chg
    
    output <- re_rewt(item = inputs$series[i],
                      adj_factor = inputs$adj[i],
                      "2021-02-01",
                      pre_wt_adj = inputs$calc_wt_adj[i], 
                      pre_chg = inputs$calc_pre_chg[i])
    pre_wt_adj <- output$value[output$series == "total_wt_adj"]
    pre_chg <- output$value[output$series == "reweighted"]
    
  }
  
  inputs %>% 
    rename(chg_post_adj = calc_pre_chg) %>% 
    bind_rows(tibble(series = "final_output",
                     calc_wt_adj = pre_wt_adj,
                     chg_post_adj = pre_chg))
}

mult_cpi_adj(adj_list, 7.87)

# No car, 75% less gas, 25% more eating out
tibble(series = c("CUUR0000SETA02", "CUUR0000SETA01", "CUUR0000SETB01", "CUUR0000SEFV"),
       adj = c(0, 0, 0.25, 1.25)) %>% 
  mult_cpi_adj(., 7.87)

# New car
# CPI weight: 3.73
# Share conditional on buying new: 17.3, so 4.6x
# Zero out used
tibble(series = c("CUUR0000SETA01", "CUUR0000SETA02"),
       adj = c(4.6, 0)) %>% 
  mult_cpi_adj(., 8.54)

# Used car
# CPI weight: 2.71
# Share conditional on buying used: 9, so 3.3x
# Zero out new
tibble(series = c("CUUR0000SETA01", "CUUR0000SETA02"),
       adj = c(0, 3.3)) %>% 
  mult_cpi_adj(., 7.87)


tibble(series = c("CUUR0000SETA01", "CUUR0000SETA02"),
       adj = c(0, 0)) %>% 
  mult_cpi_adj(., 7.87)


tibble(series = c("CUUR0000SA0E", "CUUR0000SAF1", "CUUR0000SAH1", "CUUR0000SETA02"),
       adj = c(0, 0, 0, 0)) %>% 
  mult_cpi_adj(., 7.87)


# Vegetarians
# zero out meat, double vegetables
tibble(series = c("CUUR0000SAF1121", "CUUR0000SAF113"),
       adj = c(0, 2)) %>% 
  mult_cpi_adj(., 7.87)



rewt2 <- function(items, wts, startdate, all_items = "CUUR0000SA0") {
  
  item_start_wt_cum <- 0
  item_end_wt_cum <- 0
  item_chg_cum <- 0
  new_wt_cum <- sum(wts)
  
  for (i in seq_along(1:length(items))) {
    
    item_chg <- cpi_working %>% 
      filter(series_id == items[i],
             period != "M13",
             date >= ymd(startdate)) %>% 
      arrange(date) %>% 
      mutate(chg = value/first(value)) %>% 
      filter(date == max(date)) %>% 
      .$chg %>% as.numeric
    
    item_start_wt <- cpi_working %>% 
      filter(series_id == items[i],
             date == ymd(startdate)) %>% 
      .$wt %>% as.numeric
    
    item_end_wt <- item_start_wt * item_chg
    
    item_start_wt_cum <- item_start_wt_cum + item_start_wt
    item_end_wt_cum <- item_end_wt_cum + item_end_wt
    
    item_chg_cum <- (item_chg - 1) * (wts[i]/new_wt_cum) + item_chg_cum
    
  }
  
  total_chg <- cpi_working %>% 
    filter(series_id == all_items,
           period != "M13",
           date >= ymd(startdate)) %>% 
    arrange(date) %>% 
    mutate(chg = value/first(value)) %>% 
    filter(date == max(date)) %>% 
    .$chg %>% as.numeric
  
  ex_item_chg <- (100 * total_chg - item_end_wt_cum)/
    (100 - item_start_wt_cum)
  
  reweighted <- (100 - new_wt_cum) * (ex_item_chg - 1) +
    new_wt_cum * (item_chg_cum)
  
  # How much did we adjust up or down the weights of everything else in the basket?
  total_wt_adj <- (100 - new_wt_cum)/(100 - item_start_wt_cum)
  
  output <- tibble(series = c("ex_item_chg", 
                              "total_wt_adj",
                              "reweighted"),
                   value = c(ex_item_chg - 1, 
                             total_wt_adj,
                             reweighted))
  
  output
}

# New car:
rewt2(items = c("CUUR0000SETA01", "CUUR0000SETA02"), 
      wts = c(17.3, 0), "2021-02-01")
# Used car:
rewt2(items = c("CUUR0000SETA01", "CUUR0000SETA02"), 
      wts = c(0, 9), "2021-02-01")



# Rent vs own
# Rent: Share about 30%
rewt2(items = c("CUUR0000SEHA", "CUUR0000SEHC01"),
       wts = c(30, 0), "2021-02-01")

# Own: Share about 30%
rewt2(items = c("CUUR0000SEHA", "CUUR0000SEHC01"),
      wts = c(0, 30), "2021-02-01")


rewt2(items = c("CUUR0000SEHA", "CUUR0000SEHC01"),
      wts = c(7.37, 22.9), "2021-02-01")



# Scenario: Homeowner, bought used car, drives significantly more than usual
rewt2(items = c("CUUR0000SEHA", "CUUR0000SEHC01", 
                "CUUR0000SETA01", "CUUR0000SETA02",
                "CUUR0000SETB01"),
      wts = c(0, 30, 0, 9, 8),
      "2021-02-01")

# Scenario: Same, but didn't buy a car
rewt2(items = c("CUUR0000SEHA", "CUUR0000SEHC01", 
                "CUUR0000SETA01", "CUUR0000SETA02",
                "CUUR0000SETB01"),
      wts = c(0, 30, 0, 0, 9),
      "2021-02-01")

# Renter, didn't buy a car, drives a lot, has kid in private school, travels a lot (airfare = 6, hotels = 6)
rewt2(items = c("CUUR0000SEHA", "CUUR0000SEHC01", 
                "CUUR0000SETA01", "CUUR0000SETA02",
                "CUUR0000SETB01", "CUUR0000SEEB",
                "CUUR0000SETG01", "CUUR0000SEHB02"),
      wts = c(30, 0, 0, 0, 8, 14, 6, 6),it's going '
      "2021-02-01")

# Renter, didn't buy a car, doesn't drive, vegetarian, travels a lot
rewt2(items = c("CUUR0000SEHA", "CUUR0000SEHC01", 
                "CUUR0000SETA01", "CUUR0000SETA02",
                "CUUR0000SETB01",
                "CUUR0000SETG01", "CUUR0000SEHB02",
                "CUUR0000SAF1121", "CUUR0000SAF113"),
      wts = c(30, 0, 0, 0, 0, 6, 6, 0, 2.8),
      "2021-02-01")

# Same, but drives normal amount
rewt2(items = c("CUUR0000SEHA", "CUUR0000SEHC01", 
                "CUUR0000SETA01", "CUUR0000SETA02",
                "CUUR0000SETG01", "CUUR0000SEHB02",
                "CUUR0000SAF1121", "CUUR0000SAF113"),
      wts = c(30, 0, 0, 0, 6, 6, 0, 2.8),
      "2021-02-01")



scenario_setup <- read_csv("scenario_setup_v2.csv")

scenario_setup %>% 
  pivot_longer(-c(question, series_id, item_code, series_name, mar_2021_weight, cond_exp_share),
               names_to = "option",
               values_to = "new_weight") %>% 
  filter(!is.na(new_weight)) %>% 
  group_by(question) %>% 
  count(option) %>% 
  count(question) %>% 
  ungroup() %>% 
  summarize(prod = prod(n))

scenarios <- tibble(scenario_id = 1:14400,
                    Q2 = c(rep("opt_0", 3600), rep("opt_1", 3600), rep("opt_2", 3600), rep("opt_3", 3600)))

# q <- tibble(Q2 = rep(c("opt_0", "opt_1", "opt_2", "opt_3"), 2160)) %>% 
#   arrange(Q2)
# q <- q %>% 
#   bind_rows(q) %>% 
#   bind_rows(q)

# scenarios <- bind_cols(scenarios, q)

q <- tibble(Q3 = rep(c("opt_0", "opt_1", "opt_2", "opt_3", "opt_4"), 720)) %>% 
  arrange(Q3)
q <- q %>% 
  bind_rows(q) %>% 
  bind_rows(q) %>% 
  bind_rows(q)
# q <- bind_rows(q, q) %>% 
#   bind_rows(q)

scenarios <- bind_cols(scenarios, q)

# final <- tibble(Q4 = NA_character_, Q5 = NA_character_, Q6 = NA_character_, Q7 = NA_character_, Q8 = NA_character_, .rows = 0)

q4 <- c("opt_0", "opt_1", "opt_2", "opt_3")
q5 <- c("opt_0", "opt_1", "opt_2")
q6 <- c("opt_0", "opt_1", "opt_2")
q7 <- c("opt_0", "opt_1", "opt_2", "opt_3")
q8 <- c("opt_0", "opt_1", "opt_2", "opt_3", "opt_4")

w <- tibble(Q4 = NA_character_, Q5 = NA_character_, Q6 = NA_character_, Q7 = NA_character_, Q8 = NA_character_, .rows = 0)
row <- 0
for (j in seq_along(1:4)) {
  for (k in seq_along(1:3)) {
    for (l in seq_along(1:3)) {
      for (m in seq_along(1:4)) {
        for (n in seq_along(1:5)) {
          row <- row + 1
          w[row,] <- tibble(Q4 = q4[j],
                            Q5 = q5[k],
                            Q6 = q6[l],
                            Q7 = q7[m],
                            Q8 = q8[n])
        }
      }
    }
  }
}

w <- map_dfr(1:20, ~w)
scenarios <- bind_cols(scenarios, w)
View(scenarios)


scenarios


scenario_select <- function(scenario_id) {
  
  scen <- scenarios[scenario_id,] %>% 
    gather(question, option, -scenario_id) %>% 
    filter(option != "opt_0")
  
  w <- scenario_setup %>% 
    select(question, series_id, series_name) %>% 
    filter(question %in% scen$question)
  
  w <- w %>%
    mutate(new_wt = map2_dbl(series_id, question, function(x, y) {
      scenario_setup[scenario_setup$series_id == x,
                     which(names(scenario_setup) == scen$option[scen$question == y])] %>%
        as.numeric()
    #   print(x, y)

    }))
    
  w

}

library(magrittr)
scenario_select(1000) %$% 
  rewt2(items = series_id,
        wts = new_wt,
        "2021-03-01")

random_sample <- scenarios %>% 
  sample_n(500) %>% 
  mutate(inflation_rate = map_dbl(scenario_id, function(x) {
    scenario_select(x) %$% 
      rewt2(items = series_id,
            wts = new_wt,
            "2021-02-01") %>% 
      filter(series == "reweighted") %>% 
      select(value) %>% 
      as.numeric
  }))

random_sample %>% 
  write_csv("sample_for_ella.csv")


# full_calcs <- scenarios %>% 
#   mutate(inflation_rate = map_dbl(scenario_id, function(x) {
#     scenario_select(x) %$% 
#       rewt2(items = series_id,
#             wts = new_wt,
#             "2021-03-01") %>% 
#       filter(series == "reweighted") %>% 
#       select(value) %>% 
#       as.numeric
#   }))

full_calcs <- scenarios %>% 
  slice(1:1000) %>% 
  mutate(inflation_rate = map_dbl(scenario_id, function(x) {
    scenario_select(x) %$% 
      rewt2_v2(items = series_id,
            wts = new_wt,
            "2021-03-01") 
  }))
# 2:20

full_calcs <- scenarios %>% 
  slice(1001:5000) %>% 
  mutate(inflation_rate = map_dbl(scenario_id, function(x) {
    scenario_select(x) %$% 
      rewt2_v2(items = series_id,
               wts = new_wt,
               "2021-03-01") 
  })) %>% 
  bind_rows(full_calcs)
# 2:29

full_calcs <- scenarios %>% 
  slice(5001:10000) %>% 
  mutate(inflation_rate = map_dbl(scenario_id, function(x) {
    scenario_select(x) %$% 
      rewt2_v2(items = series_id,
               wts = new_wt,
               "2021-03-01") 
  })) %>% 
  bind_rows(full_calcs)
# 1:13

full_calcs <- scenarios %>%
  slice(10001:14400) %>%
  mutate(inflation_rate = map_dbl(scenario_id, function(x) {
    scenario_select(x) %$%
      rewt2_v2(items = series_id,
               wts = new_wt,
               "2021-03-01")
  })) %>%
  bind_rows(full_calcs)
# 
# full_calcs <- scenarios %>% 
#   slice(15001:20000) %>% 
#   mutate(inflation_rate = map_dbl(scenario_id, function(x) {
#     scenario_select(x) %$% 
#       rewt2_v2(items = series_id,
#                wts = new_wt,
#                "2021-03-01") 
#   })) %>% 
#   bind_rows(full_calcs)
# # 2:05

# full_calcs <- scenarios %>% 
#   slice(20001:25920) %>% 
#   mutate(inflation_rate = map_dbl(scenario_id, function(x) {
#     scenario_select(x) %$% 
#       rewt2_v2(items = series_id,
#                wts = new_wt,
#                "2021-03-01") 
#   })) %>% 
#   bind_rows(full_calcs)


full_calcs %>% 
  write_csv("full_calcs_for_ella_v5.csv")

full_calcs %>% 
  mutate(avg = mean(inflation_rate)) %>% 
  ggplot(aes(inflation_rate)) +
  geom_histogram(fill = "gray50") +
  geom_vline(xintercept = 8.54, colour = "blue", size = 1) +
  geom_vline(aes(xintercept = avg), colour = "red", size = 1)



rewt2_v2 <- function(items, wts, startdate, all_items = "CUUR0000SA0") {
  
  item_start_wt_cum <- 0
  item_end_wt_cum <- 0
  item_chg_cum <- 0
  new_wt_cum <- sum(wts)
  
  for (i in seq_along(1:length(items))) {
    
    item_chg <- cpi_working$value[cpi_working$series_id == items[i] & cpi_working$date == max(cpi_working$date)]/
      cpi_working$value[cpi_working$series_id == items[i] & cpi_working$date == ymd(startdate)]
    
    # item_chg <- cpi_working %>% 
    #   filter(series_id == items[i],
    #          period != "M13",
    #          date >= ymd(startdate)) %>% 
    #   arrange(date) %>% 
    #   mutate(chg = value/first(value)) %>% 
    #   filter(date == max(date)) %>% 
    #   .$chg %>% as.numeric
    
    item_start_wt <- cpi_working$wt[cpi_working$series_id == items[i] & cpi_working$date == ymd(startdate)] 
    
    item_end_wt <- item_start_wt * item_chg
    
    item_start_wt_cum <- item_start_wt_cum + item_start_wt
    item_end_wt_cum <- item_end_wt_cum + item_end_wt
    
    item_chg_cum <- (item_chg - 1) * (wts[i]/new_wt_cum) + item_chg_cum
    
  }
  
  total_chg <- cpi_working$value[cpi_working$series_id == all_items & cpi_working$date == max(cpi_working$date)]/
    cpi_working$value[cpi_working$series_id == all_items & cpi_working$date == ymd(startdate)]
  
  # total_chg <- cpi_working %>% 
  #   filter(series_id == all_items,
  #          period != "M13",
  #          date >= ymd(startdate)) %>% 
  #   arrange(date) %>% 
  #   mutate(chg = value/first(value)) %>% 
  #   filter(date == max(date)) %>% 
  #   .$chg %>% as.numeric
  
  ex_item_chg <- (100 * total_chg - item_end_wt_cum)/
    (100 - item_start_wt_cum)
  
  reweighted <- (100 - new_wt_cum) * (ex_item_chg - 1) +
    new_wt_cum * (item_chg_cum)
  
  if (is.na(reweighted)) reweighted <- (ex_item_chg - 1)*100
  
  reweighted
}

microbenchmark::microbenchmark("orig" = 
                                 walk(1:10, ~
  scenario_select(.x) %$% 
  rewt2(items = series_id,
        wts = new_wt,
        "2021-03-01")),
  "new" = walk(1:10, ~
                 scenario_select(.x) %$% 
                 rewt2_v2(items = series_id,
                          wts = new_wt,
                          "2021-03-01")),
  times = 10L
)

scenario_select(1) %$% 
  rewt2_v2(items = series_id,
           wts = new_wt,
           "2021-03-01")


full_calcs %>% 
  arrange((inflation_rate))


rent_scenario <- function(rent_rate, wt = .35, ex_shelter = 10.3) {
  (1-wt) * ex_shelter + wt * rent_rate
}

rent_scenario(25)


ex_items("CUSR0000SETB01", startdate = "2020-02-01") %>% 
  select(date, all_items, ex_gas = ex_item) %>% 
  gather(series, value, -date) %>% 
  ggplot(aes(date, value, colour = series)) +
  geom_line()


ex_items("CUSR0000SETB01", startdate = "2020-02-01") %>% 
  select(date, all_items, ex_gas = ex_item) %>% 
  write_csv("ex_gas_data.csv")

# Gasoline:
# 2019 mean spending: $2,100
# 2019 mean gas price: $2.64/gal
# 2100/2.64 = 795 gal = approx. 15 gal per week *25 mpg = 375 miles per week


full_calcs %>% 
  filter(Q2 == "opt_0",
         Q3 == "opt_0",
         Q4 == "opt_0",
         Q5 == "opt_0",
         Q6 == "opt_2",
         Q7 == "opt_0",
         Q8 == "opt_0")

rewt2(items = c("CUUR0000SAF1", "CUUR0000SAH1", "CUUR0000SA0E", "CUUR0000SETA02"), wts = c(0,0,0,0), "2021-03-01")

rewt2_v2(items = c("CUUR0000SETA01", "CUUR0000SETA02"), wts = c(17,0), "2021-03-01")

scenario_select(41) %$% 
  rewt2_v2(items = series_id,
           wts = new_wt,
           "2021-03-01")

ex_items_yy("CUUR0000SEHE01", startdate = "2021-03-01")


# New version!
scenarios <- tibble(scenario_id = 1:18000,
                    Q2 = c(rep("opt_0", 3600), rep("opt_1", 3600), rep("opt_2", 3600), rep("opt_3", 3600), rep("opt_4", 3600)))


# scenarios <- bind_cols(scenarios, q)

q <- tibble(Q3 = rep(c("opt_0", "opt_1", "opt_2", "opt_3", "opt_4"), 720)) %>% 
  arrange(Q3)
q <- q %>% 
  bind_rows(q) %>% 
  bind_rows(q) %>% 
  bind_rows(q) %>% 
  bind_rows(q)
# q <- bind_rows(q, q) %>% 
#   bind_rows(q)

scenarios <- bind_cols(scenarios, q)

# final <- tibble(Q4 = NA_character_, Q5 = NA_character_, Q6 = NA_character_, Q7 = NA_character_, Q8 = NA_character_, .rows = 0)

q4 <- c("opt_0", "opt_1", "opt_2", "opt_3")
q5 <- c("opt_0", "opt_1", "opt_2")
q6 <- c("opt_0", "opt_1", "opt_2")
q7 <- c("opt_0", "opt_1", "opt_2", "opt_3")
q8 <- c("opt_0", "opt_1", "opt_2", "opt_3", "opt_4")

w <- tibble(Q4 = NA_character_, Q5 = NA_character_, Q6 = NA_character_, Q7 = NA_character_, Q8 = NA_character_, .rows = 0)
row <- 0
for (j in seq_along(1:4)) {
  for (k in seq_along(1:3)) {
    for (l in seq_along(1:3)) {
      for (m in seq_along(1:4)) {
        for (n in seq_along(1:5)) {
          row <- row + 1
          w[row,] <- tibble(Q4 = q4[j],
                            Q5 = q5[k],
                            Q6 = q6[l],
                            Q7 = q7[m],
                            Q8 = q8[n])
        }
      }
    }
  }
}


w <- map_dfr(1:25, ~w)
scenarios <- bind_cols(scenarios, w)
View(scenarios)

full_calcs_2 <- scenarios %>% 
  slice(1) %>% 
  mutate(inflation_rate = 8.54)

full_calcs_2 <- scenarios %>% 
  slice(2:1000) %>% 
  mutate(inflation_rate = map_dbl(scenario_id, function(x) {
    scenario_select(x) %$% 
      rewt2_v2(items = series_id,
               wts = new_wt,
               "2021-03-01") 
  })) %>% 
  bind_rows(full_calcs_2)
# 2:20

full_calcs_2 <- scenarios %>% 
  slice(1001:5000) %>% 
  mutate(inflation_rate = map_dbl(scenario_id, function(x) {
    scenario_select(x) %$% 
      rewt2_v2(items = series_id,
               wts = new_wt,
               "2021-03-01") 
  })) %>% 
  bind_rows(full_calcs_2)
# 2:29

full_calcs_2 <- scenarios %>% 
  slice(5001:10000) %>% 
  mutate(inflation_rate = map_dbl(scenario_id, function(x) {
    scenario_select(x) %$% 
      rewt2_v2(items = series_id,
               wts = new_wt,
               "2021-03-01") 
  })) %>% 
  bind_rows(full_calcs_2)
# 7:22

full_calcs_2 <- scenarios %>%
  slice(10001:15000) %>%
  mutate(inflation_rate = map_dbl(scenario_id, function(x) {
    scenario_select(x) %$%
      rewt2_v2(items = series_id,
               wts = new_wt,
               "2021-03-01")
  })) %>%
  bind_rows(full_calcs_2)

full_calcs_2 <- scenarios %>%
  slice(15001:18000) %>%
  mutate(inflation_rate = map_dbl(scenario_id, function(x) {
    scenario_select(x) %$%
      rewt2_v2(items = series_id,
               wts = new_wt,
               "2021-03-01")
  })) %>%
  bind_rows(full_calcs_2)
# 2:05

# full_calcs_2 <- scenarios %>% 
#   slice(20001:25920) %>% 
#   mutate(inflation_rate = map_dbl(scenario_id, function(x) {
#     scenario_select(x) %$% 
#       rewt2_v2(items = series_id,
#                wts = new_wt,
#                "2021-03-01") 
#   })) %>% 
#   bind_rows(full_calcs_2)


full_calcs_2 %>% 
  arrange(scenario_id) %>% 
  write_csv("full_calcs_for_ella_v6.csv")


full_calcs_2 %>% 
  filter(Q2 == "opt_2",
         Q3 == "opt_2",
         Q4 == "opt_1",
         Q5 == "opt_2",
         Q6 == "opt_2",
         Q7 == "opt_3",
         Q8 == "opt_4")

scenario_select(9000) %$% 
  rewt2_v2(items = series_id,
           wts = new_wt,
           "2021-04-01")
