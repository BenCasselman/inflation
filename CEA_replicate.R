# Recreating WH pandemic-affected CPI

cea_covid <- read_tsv("series_id	name	category
CUSR0000SETG01	airline fares	pandemic-services
CUSR0000SEHB02	hotels	pandemic-services
CUSR0000SERF02	event admissions	pandemic-services
CUSR0000SETA01	new cars and trucks	vehicle-related
CUSR0000SETA02	used cars and trucks	vehicle-related
CUSR0000SETC	car parts	vehicle-related
CUSR0000SETA04	car rental	vehicle-related
")
  
cpi_working <- cpi_data %>% 
  filter(year >= 2018,
         !is.na(date)) %>% 
  select(series_id, year, period, date, value) %>% 
  left_join(cpi_series %>% select(series_id, item_code),
            by = "series_id") %>% 
  left_join(cpi_wts, by = c("date", "item_code"))

ex_items_mm("CUSR0000SETA02", end_date = "2022-12-01" )

cea_covid %>% 
  # filter(category == "pandemic-services") %>% 
  .$series_id %>% 
  ex_items_mm(., end_date = "2022-05-01")

cea_replicate <- tibble(date = seq.Date(from = ymd("2019-01-01"), to = ymd("2022-12-01"), by = "month"))
cea_replicate <- cea_replicate %>% 
  mutate(ex_all = map_dbl(date, ~ex_items_mm(cea_covid$series_id, end_date = .x) %>% 
                            .$ex_item_pp %>% 
                            as.numeric),
         pandemic_svc = map_dbl(date, ~ex_items_mm(cea_covid$series_id[cea_covid$category == "pandemic-services"], end_date = .x) %>% 
                                  .$contrib_pp %>% 
                                  as.numeric),
         vehicle = map_dbl(date, ~ex_items_mm(cea_covid$series_id[cea_covid$category == "vehicle-related"], end_date = .x) %>% 
                                  .$contrib_pp %>% 
                                  as.numeric))

cea_replicate <- cpi_working %>% 
  filter(series_id == "CUSR0000SA0") %>% 
  mutate(total = value/lag(value, 1, na.pad = T) - 1) %>% 
  select(date, total) %>% 
  right_join(cea_replicate, by = "date") %>% 
  mutate(ex_v2 = total - pandemic_svc - vehicle)

p <- cea_replicate %>% 
  select(date, ex_all, pandemic_svc, vehicle) %>% 
  gather(group, value, -date) %>% 
  # filter(date <= ymd("2021-07-01")) %>% 
  mutate(group = factor(group, levels = c("pandemic_svc", "vehicle", "ex_all"),
                        labels = c("Pandemic-affected services", "Vehicle-related", "Other"))) %>% 
  ggplot(aes(date, value, fill = group)) +
  geom_col() +
  scale_fill_manual(values = c(`Pandemic-affected services` = "#0571b0",
                               `Vehicle-related` = "#ca0020",
                               Other = "gray75")) +
  labs(x = NULL, y = NULL, title = "Contributions to Monthly Headline CPI Inflation",
       subtitle = "Percentage points, monthly rate",
       caption = "Note: Based on chart from the Council of Economic Advisers. | Source: Bureau of Labor Statistics") +
  cpi_theme() +
  theme(legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(-.8, 1.4, .2)) +
  geom_hline(yintercept = 0)

ggsave("cpi_charts/cea_headline.png", p, device = "png", width = 14.2, height = 8)



cea_replicate %>% 
  select(date, ex_all, pandemic_svc, vehicle) %>% 
  gather(group, value, -date) %>% 
  group_by(date) %>% 
  mutate(total = sum(value)) %>% 
  # filter(date <= ymd("2021-07-01")) %>% 
  mutate(group = factor(group, levels = c("pandemic_svc", "vehicle", "ex_all"),
                        labels = c("Pandemic-affected services", "Vehicle-related", "Other"))) %>% 
  ggplot(aes(date, value, fill = group)) +
  geom_col() +
  # geom_line(aes(y = total), size = 1) +
  geom_point(aes(y = total), size = 3) +
  scale_fill_manual(values = c(`Pandemic-affected services` = "#0571b0",
                               `Vehicle-related` = "#ca0020",
                               Other = "gray75")) +
  labs(x = NULL, y = NULL, title = "Contributions to Monthly Headline CPI Inflation",
       subtitle = "Percentage points, monthly rate. Dots denote headline CPI.") +
  cpi_theme() +
  theme(legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(-.8, 1.4, .2)) +
  geom_hline(yintercept = 0)


cea_replicate %>% 
  arrange(desc(date))


cea_replicate <- cea_replicate %>% 
  mutate(pandemic_svc = map_dbl(date, ~ex_items_mm(cea_covid$series_id[cea_covid$category == "pandemic_services"], end_date = .x) %>% 
                            .$ex_item_chg %>% 
                            as.numeric))

ex_items_mm("CUSR0000SETB01", "2022-03-01")


# CORE

cea_replicate_core <- tibble(date = seq.Date(from = ymd("2019-01-01"), to = ymd("2022-12-01"), by = "month"))
cea_replicate_core <- cea_replicate_core %>% 
  mutate(ex_all = map_dbl(date, ~ex_items_mm(cea_covid$series_id, end_date = .x, all_items = "CUSR0000SA0L1E") %>% 
                            .$ex_item_pp %>% 
                            as.numeric),
         pandemic_svc = map_dbl(date, ~ex_items_mm(cea_covid$series_id[cea_covid$category == "pandemic-services"], end_date = .x, all_items = "CUSR0000SA0L1E") %>% 
                                  .$contrib_pp %>% 
                                  as.numeric),
         vehicle = map_dbl(date, ~ex_items_mm(cea_covid$series_id[cea_covid$category == "vehicle-related"], end_date = .x, all_items = "CUSR0000SA0L1E") %>% 
                             .$contrib_pp %>% 
                             as.numeric))

cea_replicate_core <- cpi_working %>% 
  filter(series_id == "CUSR0000SA0L1E") %>% 
  mutate(total = value/lag(value, 1, na.pad = T) - 1) %>% 
  select(date, total) %>% 
  right_join(cea_replicate_core, by = "date") %>% 
  mutate(ex_v2 = total - pandemic_svc - vehicle)

p <- cea_replicate_core %>% 
  select(date, ex_all, pandemic_svc, vehicle) %>% 
  gather(group, value, -date) %>% 
  # filter(date <= ymd("2021-07-01")) %>% 
  mutate(group = factor(group, levels = c("pandemic_svc", "vehicle", "ex_all"),
                        labels = c("Pandemic-affected services", "Vehicle-related", "Other"))) %>% 
  ggplot(aes(date, value, fill = group)) +
  geom_col() +
  scale_fill_manual(values = c(`Pandemic-affected services` = "#0571b0",
                               `Vehicle-related` = "#ca0020",
                               Other = "gray75")) +
  labs(x = NULL, y = NULL, title = "Contributions to Monthly Core CPI Inflation",
       subtitle = "Percentage points, monthly rate",
       caption = "Note: Based on chart from the Council of Economic Advisers. | Source: Bureau of Labor Statistics") +
  cpi_theme() +
  theme(legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(-.8, 1.4, .2)) +
  geom_hline(yintercept = 0)

ggsave("cpi_charts/cea_core.png", p, device = "png", width = 14.2, height = 8)


ex_items_mm <- function(items, end_date, all_items = "CUSR0000SA0") {
  
  item_start_wt_cum <- cpi_working %>% 
    filter(series_id %in% items,
           date == ymd(end_date) - months(1)) %>% 
    summarize(total = sum(wt)) %>% 
    as.numeric()
  item_end_wt_cum <- 0
  item_chg_cum <- 0

  for (i in seq_along(1:length(items))) {
    
    item_chg <- cpi_working$value[cpi_working$series_id == items[i] & cpi_working$date == ymd(end_date)]/
      cpi_working$value[cpi_working$series_id == items[i] & cpi_working$date == ymd(end_date) - months(1)]
    
    item_start_wt <- cpi_working$wt[cpi_working$series_id == items[i] & cpi_working$date == ymd(end_date) - months(1)] 
    
    item_end_wt <- item_start_wt * item_chg
    
    item_start_wt_cum <- item_start_wt_cum + item_start_wt
    item_end_wt_cum <- item_end_wt_cum + item_end_wt
    
    item_chg_cum <- (item_chg - 1) * (item_start_wt) + item_chg_cum
    
  }
  
  total_chg <- cpi_working$value[cpi_working$series_id == all_items & cpi_working$date == ymd(end_date)]/
    cpi_working$value[cpi_working$series_id == all_items & cpi_working$date == ymd(end_date) - months(1)]
  
  ex_item_chg <- (100 * total_chg - item_end_wt_cum)/
    (100 - item_start_wt_cum)

  output <- tibble(ex_item_chg = 100*(ex_item_chg - 1),
                   contrib_pp = item_chg_cum,
                   contrib_share = item_chg_cum/(total_chg - 1),
                   ex_item_pp = 100*(total_chg - 1) - item_chg_cum,
                   total_chg = total_chg - 1)
  
  output
}

ex_items_mm <- function(series, startdate, 
                        all_items = "CUSR0000SA0", 
                        df = cpi_data,
                        wt_var = wt,
                        wt_df = cpi_wts) {
  df %>% 
    filter(series_id %in% c(series, all_items),
           date >= ymd(startdate),
           period != "M13") %>% 
    left_join(cpi_series, by = "series_id") %>% 
    left_join(wt_df, by = c("item_code", "date")) %>% 
    mutate(series_id = factor(series_id, levels = c(series, all_items),
                              labels = c("item", "all"))) %>% 
    select(series_id, date, value, wt) %>% 
    pivot_wider(names_from = series_id, values_from = c(value, wt)) %>% 
    mutate(item_chg = lag(wt_item, 1, na.pad = T) * (value_item/lag(value_item, 1, na.pad = T)) - lag(wt_item, 1, na.pad = T),
           all_item_chg = wt_all * (value_all/lag(value_all, 1, na.pad = T)) - wt_all,
           contrib_pct = item_chg/all_item_chg,
           contrib_pp = contrib_pct * all_item_chg,
           adj_wt = first(wt_item) * (value_item/first(value_item)),
           ex_item_index = 100 * (value_all/first(value_all)) - adj_wt, # calc new index value
           ex_item = 100*(ex_item_index/lag(ex_item_index, 1, na.pad = T) - 1), # calc new change
           series_id = series) %>% 
    select(date, series_id, all_items = all_item_chg, ex_item, wt_item, 
           contrib_pct, contrib_pp, item_chg) %>% 
    filter(!is.na(all_items))
  
}
