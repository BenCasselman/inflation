library(tidyverse)
library(lubridate)
library(scales)
source("~/jobsday/bls_api.R")
library(directlabels)

# On-time setup
cpi_series <- read_tsv("https://download.bls.gov/pub/time.series/cu/cu.series")
cpi_item <- read_tsv("https://download.bls.gov/pub/time.series/cu/cu.item")

# Get new data
cpi_codes <- c("CUUR0000SA0", "CUSR0000SA0", "CUUR0000SA0L1E", "CUSR0000SA0L1E", "CUSR0000SAH1", "CUUR0000SAH1", "CUSR0000SAR", "CUUR0000SAR", "CUSR0000SAT", "CUUR0000SAT", "CUSR0000SEHA", "CUUR0000SEHA", "CUSR0000SEHC", "CUUR0000SEHC", "CUSR0000SETA01", "CUUR0000SETA01", "CUSR0000SETA02", "CUUR0000SETA02", "CUSR0000SETG01", "CUUR0000SETG01", "CUSR0000SEFV", "CUUR0000SEFV", "CUSR0000SEHB02", "CUUR0000SEHB02") %>% 
  unique

cpi_data_prelim <- new_bls_api(cpi_codes)

cpi_data <- read_tsv("https://download.bls.gov/pub/time.series/cu/cu.data.0.Current") %>% 
  mutate(date = ymd(paste(year, substr(period, 2,3), "01", sep = "-")))

cpi_data %>% 
  arrange(desc(date))


cpi_data %>% 
  filter(series_id == "CUSR0000SETA04") %>% 
  arrange(date) %>% 
  mutate(change = value/lag(value, 1, na.pad = T) - 1) %>% 
  arrange(desc(date))
  

# Charts:
p <- cpi_y("CUUR0000SA0L1E", "Core") %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x - 2.5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Year-over-year change in prices",
       subtitle = "'Core' excludes food and energy prices",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/core.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_m("CUSR0000SA0L1E", "Core") %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x - 2.5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Month-over-month change in prices",
       subtitle = "Seasonally adjusted. 'Core' excludes food and energy prices.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/core_m.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_3m("CUSR0000SA0L1E", "Core") %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x - 2.5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in prices",
       subtitle = "Seasonally adjusted annual rate. 'Core' excludes food and energy prices.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/core_3m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_3m("CUSR0000SEFV", "Restaurants") %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `Restaurants` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2010-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in prices",
       subtitle = "Seasonally adjusted annual rate. `Restaurants` is BLS `food away from home` category.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/food_away_3m.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_y("CUUR0000SEFV", "Restaurants") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `Restaurants` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Restaurant prices",
       subtitle = "Change from a year earlier. `Restaurants` is BLS `food away from home` category.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/food_away_y.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_3m("CUSR0000SETA02", "Used cars") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `Used cars` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in prices",
       subtitle = "Seasonally adjusted annual rate.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/used_cars_3m.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_y("CUUR0000SETA02", "Used cars") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `Used cars` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Used car prices are soaring",
       subtitle = "Change from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/used_cars_y.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_y("CUUR0000SETA", "Cars") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `Cars` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Car prices are soaring",
       subtitle = "New and used car prices, change from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/cars_y.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_y("CUUR0000SETG01", "Airfare") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", Airfare = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Airfares are rebounding",
       subtitle = "Change from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/airfare_y.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_y("CUUR0000SEHB02", "Hotels") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", Hotels = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Hotel rates are rebounding",
       subtitle = "Change from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/hotels_y.png", p, device = "png", width = 14.2, height = 8)



p <- cpi_y("CUUR0000SETA04", "Rental cars") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `Rental cars` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Rental car prices are taking off",
       subtitle = "Change from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/rental_cars_y.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_y("CUUR0000SEHA", "Rent") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `Rent` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Rental inflation remains muted",
       subtitle = "Change from a year earlier in rent of primary residence",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/rent_y.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_3m("CUSR0000SEHA", "Rent") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `Rent` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Rental inflation remains muted",
       subtitle = "Three-month change in rent of primary residence, seasonally adjusted annual rate",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/rent_3m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_y("CUUR0000SEHC01", "OER") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `OER` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Owner's equivalent rent of primary residence",
       subtitle = "Change from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/oer_y.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_3m("CUSR0000SEHC01", "OER") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `OER` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Owner's equivalent rent of primary residence",
       subtitle = "Three-month change, seasonally adjusted annual rate",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/oer_3m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_data %>% 
  filter(series_id %in% c("CUUR0000SETA02", "CUUR0000SA0"),
         !is.na(date)) %>% 
  select(date, series_id, value) %>% 
  mutate(series_id = factor(series_id, levels = c("CUUR0000SETA02", "CUUR0000SA0"),
                            labels = c("used_cars", "all_items"))) %>% 
  spread(series_id, value) %>% 
  mutate(less_cars = all_items - used_cars * (2.504/100)) %>% 
  select(date, `All items` = all_items, `Ex-used cars` = less_cars) %>% 
  gather(series_id, value, -date) %>% 
  group_by(series_id) %>% 
  mutate(change = value/lag(value, 12, na.pad = T) - 1,
         point = case_when(date == max(date) ~ change)) %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(date, change, colour = series_id)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  cpi_theme() +
  xlim(ymd("2019-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Used car prices are pushing up overall inflation",
       subtitle = "Change from a year earlier. Used cars calculated at fixed Feb. 2020 weights.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex_cars.png", p, device = "png", width = 14.2, height = 8)



p <- cpi_data %>% 
  filter(series_id %in% c("CUUR0000SETA", "CUUR0000SA0"),
         !is.na(date)) %>% 
  select(date, series_id, value) %>% 
  mutate(series_id = factor(series_id, levels = c("CUUR0000SETA", "CUUR0000SA0"),
                            labels = c("cars", "all_items"))) %>% 
  spread(series_id, value) %>% 
  mutate(less_cars = all_items - cars * (6.250/100)) %>% 
  select(date, `All items` = all_items, `Ex-cars` = less_cars) %>% 
  gather(series_id, value, -date) %>% 
  group_by(series_id) %>% 
  mutate(change = value/lag(value, 12, na.pad = T) - 1,
         point = case_when(date == max(date) ~ change)) %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(date, change, colour = series_id)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  cpi_theme() +
  xlim(ymd("2019-01-01"), ymd("2021-12-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Used car prices are pushing up overall inflation",
       subtitle = "Change from a year earlier. Used cars calculated at fixed Feb. 2020 weights.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex_cars.png", p, device = "png", width = 14.2, height = 8)





# Check:
rel_imp("CUUR0000SA0E", 6.941, startdate = ymd("2021-04-01"),
        all_items = "CUUR0000SA0") %>% 
  mutate(chg = ex_item/lag(ex_item, 1, na.pad = T) - 1,
         all_chg = all_items/lag(all_items, 1, na.pad = T) - 1)

rel_imp("CUUR0000SA0E", 6.256, startdate = ymd("2020-03-01"),
        all_items = "CUUR0000SA0")


rel_imp("CUUR0000SAH1", 32.857, startdate = ymd("2021-04-01"),
        all_items = "CUUR0000SA0") %>% 
  mutate(chg_all = all_items/lag(all_items, 1, na.pad = T) - 1,
         chg_item = item/lag(item, 1, na.pad = T) - 1)



  
  mutate(contrib = ((value/lag(value, 1, na.pad = T)) * lag(rel_imp, 1, na.pad = T)) - lag(rel_imp, 1, na.pad = T),
         all_chg = 100*all_items/lag(all_items, 1, na.pad = T) - 100,
         chg_ex_used = all_chg - contrib,
         share = contrib/all_chg,
         wt_ex_used = 100-rel_imp,
         index_ex_used = case_when(date == min(date) ~ 100,
                                   TRUE ~ 1 + chg_ex_used/100),
         index_ex_used = cumprod(index_ex_used)) %>% 
  select(date, all_items, ex_used) %>% 
  gather(series_id, value, -date) %>% 
  group_by(series_id) %>% 
  mutate(change = value/first(value) - 1) %>% 
  filter(!is.na(change)) %>% 
  ggplot(aes(date, change, colour = series_id)) + geom_line(size = 1)

rel_imp("CUSR0000SETA", 7.525, startdate = ymd("2020-12-01"),
        all_items = "CUSR0000SA0") %>% 
  mutate(contrib = ((value/lag(value, 1, na.pad = T)) * lag(rel_imp, 1, na.pad = T)) - lag(rel_imp, 1, na.pad = T),
         all_chg = 100*all_items/lag(all_items, 1, na.pad = T) - 100,
         chg_ex_used = all_chg - contrib,
         share = contrib/all_chg,
         wt_ex_used = 100-rel_imp,
         index_ex_used = case_when(date == min(date) ~ 100,
                                   TRUE ~ 1 + chg_ex_used/100),
         index_ex_used = cumprod(index_ex_used),
         chg = index_ex_used/lag(index_ex_used, 1, na.pad = T) - 1,
         check = (all_items/lag(all_items, 1, na.pad = T) - 1)* (1-share)) %>% 
  select(-series_id, -value, -rel_imp, - all_items)

cpi_data %>% 
  filter(series_id %in% c("CUUR0000SETA02", "CUUR0000SA0"),
         !is.na(date),
         date >= ymd("2020-01-01")) %>% 
  select(date, series_id, value) %>% 
  mutate(series_id = factor(series_id, levels = c("CUUR0000SETA02", "CUUR0000SA0"),
                            labels = c("used_cars", "all_items"))) %>% 
  spread(series_id, value) %>% 
  mutate(rel_imp = 2.504 * (used_cars/first(used_cars)) *
           1/(all_items/first(all_items)))


cpi_data %>% 
  filter(series_id %in% c("CUUR0000SETA02", "CUUR0000SA0"),
         !is.na(date),
         date >= ymd("2021-03-01")) %>% 
  select(date, series_id, value) %>% 
  mutate(series_id = factor(series_id, levels = c("CUUR0000SETA02", "CUUR0000SA0"),
                            labels = c("used_cars", "all_items"))) %>% 
  spread(series_id, value) %>% 
  mutate(rel_imp = 2.757 * (used_cars/first(used_cars)) *
           1/(all_items/first(all_items)))




item_contrib("CUSR0000SETA02", 2.998, ymd("2021-04-01"))

item_contrib("CUUR0000SETA02", 2.562, ymd("2020-05-01"), all_items = "CUUR0000SA0")

item_contrib("CUUR0000SA0E", 6.941, ymd("2021-04-01"), all_items = "CUUR0000SA0")

item_contrib("CUSR0000SETA", 7.712, ymd("2021-04-01"), all_items = "CUSR0000SA0L1E", all_items_wt = 79.131)



ex_items(c("CUSR0000SETA", "CUUR0000SA0E"), c(7.712, 6.941),
          startdate = ymd("2021-01-01"))

ex_items("CUUR0000SA0E", 6.256, startdate = ymd("2020-03-01"),
        all_items = "CUUR0000SA0")

# Less food and shelter
ex_items(c("CUUR0000SAH1", "CUUR0000SAF1"), c(33.303, 13.862),
          startdate = ymd("2020-03-01"),
          all_items = "CUUR0000SA0")

item_contrib("CUSR0000SETA", 7.268, startdate = ymd("2020-02-01"),
             all_items = "CUUR0000SA0")

p <- ex_items("CUSR0000SETA", 7.268, startdate = ymd("2020-02-01"),
              all_items = "CUSR0000SA0") %>% 
  select(date, all_items, ex_item) %>% 
  gather(series_id, value, -date) %>% 
  mutate(series_id = factor(series_id, levels = c("all_items", "ex_item"),
                            labels = c("All items", "Ex-autos"))) %>% 
  group_by(series_id) %>% 
  mutate(change = value/first(value) - 1,
         point = case_when(date == max(date) ~ change)) %>% 
  ggplot(aes(date, change, colour = series_id)) + geom_line(size = 1, show.legend = F)

p <- p +
  cpi_theme() +
  xlim(ymd("2020-02-01"), ymd("2021-08-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Car prices are pushing up overall inflation",
       subtitle = "Cumulative change in consumer prices since Feb. 2020, with and without new & used car component",
       caption = "Note: Relative importance calculated from published Feb. 2020 value. | Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex_cars.png", p, device = "png", width = 14.2, height = 8)
