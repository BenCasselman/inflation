# Functions
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

# Calculate given item's share of total CPI growth over given period
item_contrib <- function(item, rel_imp, startdate, 
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

# Calculator for "ex-[item]" series. Works for multiple items. Must enter initial weight
ex_items <- function(series, rel_imp, startdate, 
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


# BROKEN
# rel_imp <- function(series, rel_imp, startdate, 
#                     all_items = "CUSR0000SA0", 
#                     df = cpi_data) {
#   w <- df %>% 
#     filter(series_id %in% c(series, all_items),
#            date >= startdate) %>% 
#     mutate(series_id = factor(series_id, levels = c(series, all_items),
#                               labels = c("item", "all_items"))) %>% 
#     select(date, series_id, value) %>% 
#     spread(series_id, value) %>% 
#     mutate(wt = rel_imp * (item/first(item)) * 
#              1 / (all_items/first(all_items)))
#   
#   w %>% 
#     mutate(series_id = series,
#            contrib = ((item/lag(item, 1, na.pad = T)) * lag(wt, 1, na.pad = T)) - lag(wt, 1, na.pad = T),
#            all_chg = 100*all_items/lag(all_items, 1, na.pad = T) - 100,
#            share = contrib/all_chg,
#            chg_ex_used = (all_items/lag(all_items, 1, na.pad = T) - 1) * (1-share),
#            index_ex_used = case_when(date == min(date) ~ 100),
#            index_ex_used  = case_when(date == min(date) ~ 100,
#                                       TRUE ~ lag(index_ex_used, 1, na.pad = T)*(1+chg_ex_used))) %>% 
#     # chg_ex_used = all_chg - contrib,
#     # index_ex_used = case_when(date == min(date) ~ 100,
#     #                           TRUE ~ 1 + chg_ex_used/100),
#     # index_ex_used = cumprod(index_ex_used)) %>% 
#     select(series_id, date, item, rel_imp = wt, all_items, ex_item = index_ex_used, chg_ex_used, share)
# }
