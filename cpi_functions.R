# Functions
load("cpi_wts.RData")

cpi_m <- function(series, label, comp_series = "CUSR0000SA0", comp_label = "All items", df = cpi_data) {
  df %>% 
    filter(series_id %in% c(series, comp_series),
           !is.na(date)) %>% 
    mutate(series_id = factor(series_id, levels = c(series, comp_series),
                              labels = c(label, comp_label))) %>% 
    group_by(series_id) %>% 
    arrange(date) %>% 
    mutate(change = value/lag(value, 1, na.pad = T) - 1,
           point = case_when(date == max(date) ~ change))
}

cpi_3m <- function(series, label, comp_series = "CUSR0000SA0", comp_label = "All items", df = cpi_data) {
  df %>% 
    filter(series_id %in% c(series, comp_series),
           !is.na(date)) %>% 
    mutate(series_id = factor(series_id, levels = c(series, comp_series),
                              labels = c(label, comp_label))) %>% 
    group_by(series_id) %>% 
    arrange(date) %>% 
    mutate(change = (value/lag(value, 3, na.pad = T))^4 - 1,
           point = case_when(date == max(date) ~ change))
}

cpi_y <- function(series, label, comp_series = "CUUR0000SA0", comp_label = "All items", df = cpi_data) {
  df %>% 
    filter(series_id %in% c(series, comp_series),
           !is.na(date)) %>% 
    mutate(series_id = factor(series_id, levels = c(series, comp_series),
                              labels = c(label, comp_label))) %>% 
    group_by(series_id) %>% 
    arrange(date) %>% 
    mutate(change = value/lag(value, 12, na.pad = T) - 1,
           point = case_when(date == max(date) ~ change))
}

cpi_theme <- function() {
  theme(plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 20),
        plot.background = element_rect(fill = "white"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.4),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey50", size = 16),
        axis.ticks = element_line(colour = "grey", size = 0.4),
        plot.caption = element_text(colour = "grey50", size = 16))
}

# Calculator for special indexes "ex-" an arbitrary number of items. Assumes you have
# `cpi_wts` updated. See below function that doesn't require that.
ex_items <- function(series, startdate, 
                       all_items = "CUSR0000SA0", 
                       df = cpi_data,
                       wt_var = wt,
                       wt_df = cpi_wts) {
  
  
  w <- map_dfr(series, function(x) {
    
    df %>% 
      filter(series_id %in% c(x, all_items),
             date >= ymd(startdate)) %>% 
      left_join(cpi_series, by = "series_id") %>% 
      left_join(wt_df, by = c("item_code", "date")) %>% 
      mutate(series_id = factor(series_id, levels = c(x, all_items),
                                labels = c("item", "all"))) %>% 
      select(series_id, date, value, wt) %>% 
      pivot_wider(names_from = series_id, values_from = c(value, wt)) %>% 
      mutate(contrib = (lag(wt_item, 1, na.pad = T)/wt_all)*(value_item/lag(value_item, 1, na.pad = T)),
             series_id = x)
    
  })
  
  all_items_wt <- w %>% 
    filter(series_id == series[[1]]) %>% 
    select(date, all_items = value_all, all_items_wt = wt_all)
  
  w <- w %>% 
    group_by(date) %>% 
    summarize(wt = sum(wt_item),
              contrib = sum(contrib))
  
  final <- left_join(w, all_items_wt, by = "date") %>%
    mutate(ex_chg = (all_items/lag(all_items, 1, na.pad = T) - contrib) *
             (all_items_wt/(all_items_wt - lag(wt, 1, na.pad = T))) - 1,
           ex_chg = case_when(date == min(date) ~ 0,
                              TRUE ~ ex_chg),
           ex_item = case_when(date == min(date) ~ 100),
           ex_item = case_when(date == min(date) ~ 100,
                               TRUE ~ cumprod(ex_chg + 1)*100))
  
  final %>% 
    select(date, all_items, wt, ex_item, ex_chg)
}

# Calculate given item's share of total CPI growth over given period
item_contrib <- function(item, startdate,
                         all_items = "CUSR0000SA0",
                         df = cpi_data,
                         wt_df = cpi_wts) {
  
  w <-     df %>% 
    filter(series_id %in% c(item, all_items),
           date >= ymd(startdate)) %>% 
    left_join(cpi_series, by = "series_id") %>% 
    left_join(wt_df, by = c("item_code", "date")) %>% 
    mutate(series_id = factor(series_id, levels = c(item, all_items),
                              labels = c("item", "all"))) %>% 
    select(series_id, date, value, wt) %>% 
    pivot_wider(names_from = series_id, values_from = c(value, wt)) %>% 
    mutate(item_chg = first(wt_item) * (value_item/first(value_item)) - first(wt_item),
           all_item_chg = wt_all * (value_all/first(value_all)) - wt_all) %>% 
    arrange(desc(date))
  
  w$item_chg[1]/w$all_item_chg[1]

  }

# Doesn't require 'cpi_wts' calculation:
item_contrib_old <- function(item, rel_imp, startdate, 
                         all_items = "CUSR0000SA0", 
                         all_items_wt = 100,
                         df = cpi_data) {
  w <- df %>% 
    filter(series_id %in% c(item, all_items),
           date >= startdate) %>% 
    mutate(series_id = factor(series_id, levels = c(item, all_items),
                              labels = c("item", "all_items"))) %>% 
    select(date, series_id, value) %>% 
    spread(series_id, value) %>% 
    mutate(item_chg = rel_imp * (item/first(item)) - rel_imp,
           all_item_chg = all_items_wt * (all_items/first(all_items)) - all_items_wt) %>% 
    arrange(desc(date))
  
  w$item_chg[1]/w$all_item_chg[1]
}

# Calculate relative importance
rel_imp <- function(series, rel_imp, startdate, 
                    all_items = "CUSR0000SA0", 
                    all_items_wt = 100,
                    df = cpi_data) {
  w <- df %>% 
    filter(series_id %in% c(series, all_items),
           date >= startdate) %>% 
    mutate(series_id = factor(series_id, levels = c(series, all_items),
                              labels = c("item", "all_items"))) %>% 
    select(date, series_id, value) %>% 
    spread(series_id, value) %>% 
    mutate(wt = rel_imp * (item/first(item)) / (all_items/first(all_items)))
  
  w %>% 
    mutate(series_id = series) %>% 
    select(date, series, wt, item)
  
  # This was to calculate ex-item index, but superseded by `ex_items` function.
  # Preserving for future refence:

  # w %>% 
  #   mutate(series_id = series,
  #          ex_chg = (all_items/lag(all_items, 1, na.pad = T) - 
  #                      (lag(wt, 1, na.pad = T)/all_items_wt)*(item/lag(item, 1, na.pad = T))) *
  #            (all_items_wt/(all_items_wt - lag(wt, 1, na.pad = T))) -1,
  #          ex_chg = case_when(date == min(date) ~ 0,
  #                             TRUE ~ ex_chg),
  #          ex_item = case_when(date == min(date) ~ 100),
  #          ex_item = case_when(date == min(date) ~ 100,
  #                              TRUE ~ cumprod(ex_chg + 1)*100)
  #   )
  
}

# Older calculator for "ex-items" series that doesn't require cpi_wts file. Must enter initial weight
ex_items_old <- function(series, rel_imp, startdate, 
                     all_items = "CUSR0000SA0", 
                     all_items_wt = 100,
                     df = cpi_data) {
  
  series_list <- tibble(series_id = series, wt = rel_imp)
  
  w <- map2_dfr(series_list$series_id, series_list$wt, function(x, y) {
    
    df %>%
      filter(series_id %in% c(x, all_items),
             date >= startdate) %>%
      mutate(series_id = factor(series_id, levels = c(x, all_items),
                                labels = c("item", "all_items"))) %>%
      select(date, series_id, value) %>%
      spread(series_id, value) %>%
      mutate(wt = y * (item/first(item)) / (all_items/first(all_items)),
             series_id = x)
    
  })
  
  w <- w %>% 
    select(series_id, date, wt, item, all_items) %>% 
    group_by(series_id) %>% 
    mutate(contrib = (lag(wt, 1, na.pad = T)/all_items_wt)*(item/lag(item, 1, na.pad = T))) %>% 
    group_by(date) %>% 
    summarize(wt = sum(wt),
              contrib = sum(contrib))
  
  final <- df %>% 
    filter(series_id == all_items,
           date >= startdate) %>% 
    left_join(w, by = "date") %>%
    rename(all_items = value) %>% 
    mutate(ex_chg = (all_items/lag(all_items, 1, na.pad = T) - contrib) *
             (all_items_wt/(all_items_wt - lag(wt, 1, na.pad = T))) - 1,
           ex_chg = case_when(date == min(date) ~ 0,
                              TRUE ~ ex_chg),
           ex_item = case_when(date == min(date) ~ 100),
           ex_item = case_when(date == min(date) ~ 100,
                               TRUE ~ cumprod(ex_chg + 1)*100))
  
  final %>% 
    select(date, all_items, wt, ex_item, ex_chg)
}
