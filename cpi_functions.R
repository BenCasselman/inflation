# Functions
load("cpi_wts.RData")

cpi_download <- function() {
  
  cpi <- GET(url = 'https://download.bls.gov/pub/time.series/cu/cu.data.0.Current', 
             user_agent('ben.casselman@nytimes.com')) %>% 
    content(as = "text") %>% 
    read_tsv(skip = 1, col_names = c("series_id", "year", "period", "value", "footnote"),
             col_types = "cdcdc") 
  
  # url <- "https://download.bls.gov/pub/time.series/ce/ce.data.0.AllCESSeries"
  # ces <- read_tsv(url, skip = 1, 
  #                 col_names = c("series_id", "year", "period", "value", "footnote"),
  #                 col_types = "cdcdc")
  cpi <- cpi %>% 
    mutate(date = ymd(paste0(year, period, "-01")))
  
  return(cpi)
}

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
# ex_items <- function(series, startdate, 
#                        all_items = "CUSR0000SA0", 
#                        df = cpi_data,
#                        wt_var = wt,
#                        wt_df = cpi_wts) {
#   
#   
#   w <- map_dfr(series, function(x) {
#     
#     df %>% 
#       filter(series_id %in% c(x, all_items),
#              date >= ymd(startdate)) %>% 
#       left_join(cpi_series, by = "series_id") %>% 
#       left_join(wt_df, by = c("item_code", "date")) %>% 
#       mutate(series_id = factor(series_id, levels = c(x, all_items),
#                                 labels = c("item", "all"))) %>% 
#       select(series_id, date, value, wt) %>% 
#       pivot_wider(names_from = series_id, values_from = c(value, wt)) %>% 
#       mutate(contrib = (lag(wt_item, 1, na.pad = T)/wt_all)*(value_item/lag(value_item, 1, na.pad = T)),
#              series_id = x)
#     
#   })
#   
#   all_items_wt <- w %>% 
#     filter(series_id == series[[1]]) %>% 
#     select(date, all_items = value_all, all_items_wt = wt_all)
#   
#   w <- w %>% 
#     group_by(date) %>% 
#     summarize(wt = sum(wt_item),
#               contrib = sum(contrib))
#   
#   final <- left_join(w, all_items_wt, by = "date") %>%
#     mutate(ex_chg = (all_items/lag(all_items, 1, na.pad = T) - contrib) *
#              (all_items_wt/(all_items_wt - lag(wt, 1, na.pad = T))) - 1,
#            ex_chg = case_when(date == min(date) ~ 0,
#                               TRUE ~ ex_chg),
#            ex_item = case_when(date == min(date) ~ 100),
#            ex_item = case_when(date == min(date) ~ 100,
#                                TRUE ~ cumprod(ex_chg + 1)*100))
#   
#   final %>% 
#     select(date, all_items, wt, ex_item, ex_chg)
# }

# New, better version:
ex_items <- function(series, startdate, 
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
      mutate(item_chg = first(wt_item) * (value_item/first(value_item)) - first(wt_item),
             all_item_chg = wt_all * (value_all/first(value_all)) - wt_all,
             contrib_pct = item_chg/all_item_chg,
             contrib_pp = contrib_pct * all_item_chg,
             adj_wt = first(wt_item) * (value_item/first(value_item)),
             ex_item_index = first(wt_all) * (value_all/first(value_all)) - adj_wt, # calc new index value
             ex_item = 100*(ex_item_index/first(ex_item_index) - 1), # calc new change
             series_id = series,
             all_items = 100*(value_all/first(value_all) - 1)) %>% 
    select(date, series_id, all_items, ex_item, wt_item, 
           contrib_pct, contrib_pp, ex_item_index)
    
  }



ex_items_new <- function(series, 
                         startdate, 
                     all_items = "CUSR0000SA0", 
                     df = cpi_data,
                     wt_var = wt,
                     wt_df = cpi_wts) {
  
  df_items <- df %>% 
    filter(series_id %in% series,
           date >= ymd(startdate),
           period != "M13") %>% 
    left_join(cpi_series, by = "series_id") %>% 
    left_join(wt_df, by = c("item_code", "date")) %>% 
    group_by(series_id) %>% 
    mutate(new_wt = (value/first(value)) * first(wt)) %>% 
    group_by(date) %>% 
    summarize(wt_item = sum(new_wt))
  
  df_calc <- df %>% 
    filter(series_id == all_items,
           date >= ymd(startdate),
           period != "M13") %>% 
    left_join(cpi_series, by = "series_id") %>% 
    left_join(wt_df, by = c("item_code", "date")) %>% 
    select(date, value, wt_all = wt) %>% 
    left_join(df_items, by = "date") %>% 
    mutate(chg_all = value/first(value) - 1,
           new_wt = (value/first(value)) * first(wt_all),
           ex_wt = new_wt - wt_item,
           chg_ex_item = ex_wt/first(ex_wt) - 1,
           # chg_all = 100 * chg_all,
           # chg_ex_item = 100 * chg_ex_item,
           ex_wt = ex_wt) %>% 
    select(date, orig_index = value, ex_item_index = ex_wt)

  df_calc

}








ex_items_yy <- function(series, startdate, 
                     all_items = "CUUR0000SA0", 
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
    mutate(item_chg = lag(wt_item, 12, na.pad = T) * (value_item/lag(value_item, 12, na.pad = T)) - lag(wt_item, 12, na.pad = T),
           all_item_chg = wt_all * (value_all/lag(value_all, 12, na.pad = T)) - wt_all,
           contrib_pct = item_chg/all_item_chg,
           contrib_pp = contrib_pct * all_item_chg,
           adj_wt = first(wt_item) * (value_item/first(value_item)),
           ex_item_index = first(wt_all) * (value_all/first(value_all)) - adj_wt, # calc new index value
           ex_item = 100*(ex_item_index/lag(ex_item_index, 12, na.pad = T) - 1), # calc new change
           series_id = series,
           all_items = 100*(value_all/lag(value_all, 12, na.pad = T) - 1)) %>% 
    select(date, series_id, all_items, ex_item, wt_item, 
           contrib_pct, contrib_pp) %>% 
    filter(!is.na(all_items))
  
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
           ex_item_index = first(wt_all) * (value_all/first(value_all)) - adj_wt, # calc new index value
           ex_item = 100*(ex_item_index/lag(ex_item_index, 1, na.pad = T) - 1), # calc new change
           series_id = series,
           all_items = 100*(value_all/lag(value_all, 1, na.pad = T) - 1)) %>% 
    select(date, series_id, all_items, ex_item, wt_item, 
           contrib_pct, contrib_pp) %>% 
    filter(!is.na(all_items))
  
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


hi_since <- function(.data, my = "y") {
  
  if (my == "y") {
    .data %>% 
      # filter(series_id == series) %>% 
      arrange(date) %>% 
      filter(!is.na(date)) %>% 
      mutate(change = value/lag(value, 12, na.pad = T) - 1,
             current = change[date == max(date)]) %>% 
      filter(change >= current) %>% 
      arrange(desc(date))
    
  } else if (my == "m") {
    .data %>% 
      # filter(series_id == series) %>% 
      arrange(date) %>% 
      filter(!is.na(date)) %>% 
      mutate(change = value/lag(value, 1, na.pad = T) - 1,
             current = change[date == max(date)]) %>% 
      filter(change >= current) %>% 
      arrange(desc(date))
  }
}


cpi_full_history <- read_tsv("https://download.bls.gov/pub/time.series/cu/cu.data.2.Summaries") %>% 
  mutate(date = ymd(paste(year, substr(period, 2,3), "01", sep = "-")))
series_list <- cpi_full_history %>% count(series_id) %>% .$series_id
cpi_full_history <- read_tsv("https://download.bls.gov/pub/time.series/cu/cu.data.20.USCommoditiesServicesSpecial") %>% 
  mutate(date = as.Date(paste0(year, period, "-01"), format = "%YM%m-%d")) %>% 
  filter(!series_id %in% series_list) %>% 
  bind_rows(cpi_full_history)
series_list <- cpi_full_history %>% count(series_id) %>% .$series_id
cpi_full_history <- read_tsv("https://download.bls.gov/pub/time.series/cu/cu.data.12.USHousing") %>% 
  mutate(date = as.Date(paste0(year, period, "-01"), format = "%YM%m-%d")) %>% 
  filter(!series_id %in% series_list) %>% 
  bind_rows(cpi_full_history)




test <- function(series, startdate, 
                 all_items = "CUSR0000SA0", 
                 df = cpi_data,
                 wt_var = wt,
                 wt_df = cpi_wts) {
  
  # Items to be removed
  ex_df <- df %>% 
    filter(series_id %in% series,
           date >= ymd(startdate),
           period != "M13") %>% 
    left_join(cpi_series, by = "series_id") %>% 
    left_join(wt_df, by = c("item_code", "date")) %>% 
    group_by(date) %>% 
    mutate(wt_share = wt/sum(wt)) %>%
    group_by(series_id) %>% 
    mutate(m_chg = value/lag(value, 1, na.pad = T) - 1,
           m_chg = m_chg * lag(wt_share, 1, na.pad = T)) %>% 
    group_by(date) %>% 
    summarize(wt = sum(wt),
              m_chg = sum(m_chg, na.rm = T)) %>% 
    mutate(series = "item",
           cumprod = cumprod(1 + m_chg),
           index = case_when(date == first(date) ~ 100,
                             TRUE ~ 100 * cumprod)) %>% 
    select(-m_chg, -cumprod)
  
  ex_df
  
  # calc_df <- df %>%
  #   filter(series_id == all_items,
  #          date >= ymd(startdate),
  #          period != "M13") %>%
  #   left_join(cpi_series, by = "series_id") %>%
  #   left_join(wt_df, by = c("item_code", "date")) %>%
  #   select(date, wt, index = value) %>%
  #   mutate(series = "all")
  
  # calc_df <- calc_df %>%
  #   bind_rows(ex_df) %>%
  #   pivot_wider(names_from = series, values_from = c(index, wt)) %>%
  #   mutate(adj_wt = first(wt_item) * (index_item/first(index_item)),
  #          ex_item_index = first(wt_all) * (index_all/first(index_all)) - adj_wt, # calc new index index
  #          ex_item = 100*(ex_item_index/first(ex_item_index) - 1), # calc new change
  #          all_items = 100*(index_all/first(index_all) - 1)) %>%
  #   select(date, ex_item_index, cum_chg_all = all_items, cum_chg_ex_item = ex_item)
  
  # calc_df <- calc_df %>%
  #   bind_rows(ex_df) %>%
  #   pivot_wider(names_from = series, values_from = c(index, wt)) %>%
  #   mutate(adj_wt = case_when(date == first(date) ~ wt_item,
  #                             TRUE ~ lag(wt_item, 1, na.pad = T) * (index_item/lag(index_item, 1, na.pad = T))),
  #          ex_item_index = case_when(date == first(date) ~ wt_all - wt_item,
  #                                    TRUE ~ lag(wt_all, 1, na.pad = T) * (index_all/lag(index_all, 1, na.pad = T)) - adj_wt), # calc new index index
  #          ex_item = 100*(ex_item_index/first(ex_item_index) - 1), # calc new change
  #          all_items = 100*(index_all/first(index_all) - 1)) %>%
  #   select(date, ex_item_index, cum_chg_all = all_items, cum_chg_ex_item = ex_item)
  # 
  # calc_df
  
}

test(c("CUSR0000SAF1"), startdate = "2022-06-01", all_items = "CUSR0000SAC") %>% 
  mutate(chg = index/lag(index, 1, na.pad = T) - 1,
         chg = 100 * chg)

test(c("CUSR0000SAF1", "CUSR0000SACE"), startdate = "2022-06-01", all_items = "CUSR0000SAC") %>% 
  mutate(chg = index/lag(index, 1, na.pad = T) - 1,
         chg = 100 * chg)

test(c("CUSR0000SETA01", "CUSR0000SETA02"), startdate = "2022-06-01", all_items = "CUSR0000SAC") %>% 
  mutate(chg = index/lag(index, 1, na.pad = T) - 1,
         chg = 100 * chg)