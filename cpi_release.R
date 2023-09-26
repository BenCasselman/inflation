library(tidyverse)
library(lubridate)
library(scales)
library(httr)
# source("~/jobsday/bls_api.R")
library(directlabels)

# On-time setup
# cpi_series <- read_tsv("https://download.bls.gov/pub/time.series/cu/cu.series")
# cpi_item <- read_tsv("https://download.bls.gov/pub/time.series/cu/cu.item")

# Get new data
# cpi_codes <- c("CUUR0000SA0", "CUSR0000SA0", "CUUR0000SA0L1E", "CUSR0000SA0L1E", "CUSR0000SAH1", "CUUR0000SAH1", "CUSR0000SAR", "CUUR0000SAR", "CUSR0000SAT", "CUUR0000SAT", "CUSR0000SEHA", "CUUR0000SEHA", "CUSR0000SEHC", "CUUR0000SEHC", "CUSR0000SETA01", "CUUR0000SETA01", "CUSR0000SETA02", "CUUR0000SETA02", "CUSR0000SETG01", "CUUR0000SETG01", "CUSR0000SEFV", "CUUR0000SEFV", "CUSR0000SEHB02", "CUUR0000SEHB02") %>% 
#   unique
# 
# cpi_data_prelim <- new_bls_api(cpi_codes, startyear = 2018)
# cpi_data <- cpi_data_prelim


cpi_data <- cpi_download()

# url <- "https://download.bls.gov/pub/time.series/cu/cu.data.0.Current"
# temp <- tempfile()
# download.file(url, temp)
# cpi_data <- read_tsv("cu.data.0.Current")
# cpi_data <- cpi_data %>%
#   mutate(date = as.Date(paste0(year, period, "-01"), format = "%YM%m-%d"))

cpi_data %>% 
  arrange(desc(date))

cpi_full_history <- cpi_full_history %>% 
  filter(year < 2020) %>% 
  bind_rows(cpi_data %>% mutate(year = as.numeric(year)) %>% filter(year >= 2020))

# Update weights
cpi_wts <- wt_update()

# Core services ex-housing
ex_items_new(c("CUSR0000SEHA", "CUSR0000SEHC"), "2018-01-01", all_items = "CUSR0000SASLE") %>% 
  mutate(chg_1mo = (ex_item_index/lag(ex_item_index, 1, na.pad = T)) - 1,
         chg_3mo = (ex_item_index/lag(ex_item_index, 3, na.pad = T))^4 - 1,
         chg_12mo = ex_item_index/lag(ex_item_index, 12, na.pad = T) - 1) %>% 
  select(date, chg_1mo, chg_3mo, chg_12mo) %>% 
  arrange(desc(date))

ex_items_new(c("CUUR0000SEHA", "CUUR0000SEHC"), "2018-01-01", all_items = "CUUR0000SASLE") %>% 
  mutate(chg_12mo = (ex_item_index/lag(ex_item_index, 12, na.pad = T)) - 1) %>% 
  select(date, chg_12mo) %>% 
  arrange(desc(date))

ex_items_yy(c("CUUR0000SAS2RS"), "2018-01-01", all_items = "CUUR0000SASLE") %>% 
  arrange(desc(date))


ex_items_old("CUUR0000SEHA", rel_imp = 7.278, all_items_wt = 56.831, all_items = "CUUR0000SASLE", df = cpi_data, startdate = "2022-03-01")


cpi_full_history %>% 
  filter(series_id == "CUSR0000SEHA") %>% 
  hi_since(my = "m")

cpi_full_history %>% 
  filter(series_id == "CUUR0000SA0") %>% 
  hi_since(my = "y")


# Charts:
p <- cpi_y("CUUR0000SA0L1E", "Core") %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .3), "last.points", cex = 1.5)) +
  xlim(ymd("2010-01-01"), ymd("2024-06-01")) +
  scale_color_manual(values = c(Core = "#a6cee3", `All items` = "#1f78b4")) +
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
  xlim(ymd("2010-01-01"), ymd("2024-04-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .3), "last.points", cex = 1.5)) +
  scale_color_manual(values = c(Core = "#a6cee3", `All items` = "#1f78b4")) +
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
  xlim(ymd("2010-01-01"), ymd("2024-06-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  scale_color_manual(values = c(Core = "#a6cee3", `All items` = "#1f78b4")) +
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


p <- cpi_3m("CUSR0000SA0LE", "Ex-energy") %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  xlim(ymd("2010-01-01"), ymd("2024-06-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  scale_color_manual(values = c(`Ex-energy` = "#1f78b4", `All items` = "#a6cee3")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in prices, with and without energy",
       subtitle = "Seasonally adjusted annual rate.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex-energy_3m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_y("CUUR0000SA0LE", "Ex-energy") %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  xlim(ymd("2010-01-01"), ymd("2024-06-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  scale_color_manual(values = c(`Ex-energy` = "#1f78b4", `All items` = "#a6cee3")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "12-month change in prices, with and without energy",
       subtitle = "Not seasonally adjusted.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex-energy.png", p, device = "png", width = 14.2, height = 8)




p <- cpi_3m("CUSR0000SEFV", "Restaurants") %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Restaurants` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2010-01-01"), ymd("2024-06-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.qp", cex = 1.3)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in restaurant prices",
       subtitle = "Seasonally adjusted annual rate. `Restaurants` is BLS `food away from home` category.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/food_away_3m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_m("CUSR0000SEFV", "Restaurants") %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Restaurants` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2010-01-01"), ymd("2024-06-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.qp", cex = 1.3)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Monthly change in restaurant prices",
       subtitle = "Seasonally adjusted annual rate. `Restaurants` is BLS `food away from home` category.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/food_away_1m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_y("CUUR0000SEFV", "Restaurants") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Restaurants` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-06-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.qp", cex = 1.5)) +
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


p <- cpi_m("CUSR0000SETA02", "Used cars") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Used cars` = "#1f78b4")) +  
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-06-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Monthly change in used-car prices",
       subtitle = "Seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/used_cars_1m.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_m("CUSR0000SETA01", "New cars") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `New cars` = "#1f78b4")) +  
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-06-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Monthly change in new car prices",
       subtitle = "Seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/new_cars_1m.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_3m("CUSR0000SETA01", "New cars") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `New cars` = "#1f78b4")) +  
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-04-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in new-car prices",
       subtitle = "Seasonally adjusted annual rate.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/new_cars_3m.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_y("CUUR0000SETA02", "Used cars") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Used cars` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-04-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Year-over-year change in used-car prices",
       subtitle = "Change from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/used_cars_y.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_y("CUUR0000SETA01", "New cars") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `New cars` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-04-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Year-over-year change in new car prices",
       subtitle = "Change from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/new_cars_y.png", p, device = "png", width = 14.2, height = 8)



p <- cpi_y("CUUR0000SETA", "Cars") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Used cars` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-04-01")) +
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
  scale_color_manual(values = c(`All items` = "#a6cee3", Airfare = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-04-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Airfares in the pandemic",
       subtitle = "Change from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/airfare_y.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_m("CUSR0000SETG01", "Airfare") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", Airfare = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-04-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Monthly change in airfares",
       subtitle = "Change from a month earlier, seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/airfare_m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_y("CUUR0000SEHB02", "Hotels") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", Hotels = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-04-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Hotel rates in the pandemic",
       subtitle = "Change from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/hotels_y.png", p, device = "png", width = 14.2, height = 8)



p <- cpi_y("CUUR0000SETA04", "Rental cars") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `Rental cars` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-04-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Rental car prices in the pandemic",
       subtitle = "Change from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/rental_cars_y.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_y("CUUR0000SEHA", "Rent") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Rent` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-06-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Annual change in rents",
       subtitle = "Change from a year earlier in rent of primary residence",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/rent_y.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_3m("CUSR0000SEHA", "Rent") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Rent` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-06-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in rents",
       subtitle = "Change from three months earlier in rent of primary residence, seasonally adjusted annual rate",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/rent_3m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_m("CUSR0000SEHA", "Rent") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Rent` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-06-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Monthly change in rents",
       subtitle = "Change from a month earlier in rent of primary residence, seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/rent_1m.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_y("CUUR0000SEHC01", "OER") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `OER` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-04-01")) +
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
  scale_color_manual(values = c(`All items` = "#a6cee3", `OER` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-04-01")) +
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

p <- cpi_m("CUSR0000SEHC01", "OER") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `OER` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-04-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Owner's equivalent rent of primary residence",
       subtitle = "Monthly change, seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/oer_1m.png", p, device = "png", width = 14.2, height = 8)


# p <- cpi_data %>% 
#   filter(series_id %in% c("CUUR0000SETA", "CUUR0000SA0"),
#          !is.na(date)) %>% 
#   select(date, series_id, value) %>% 
#   mutate(series_id = factor(series_id, levels = c("CUUR0000SETA", "CUUR0000SA0"),
#                             labels = c("cars", "all_items"))) %>% 
#   spread(series_id, value) %>% 
#   mutate(less_cars = all_items - cars * (6.250/100)) %>% 
#   select(date, `All items` = all_items, `Ex-cars` = less_cars) %>% 
#   gather(series_id, value, -date) %>% 
#   group_by(series_id) %>% 
#   mutate(change = value/lag(value, 12, na.pad = T) - 1,
#          point = case_when(date == max(date) ~ change)) %>% 
#   filter(year(date) >= 2019) %>% 
#   ggplot(aes(date, change, colour = series_id)) +
#   geom_line(size = 1, show.legend = F)
# 
# p <- p +
#   cpi_theme() +
#   xlim(ymd("2019-01-01"), ymd("2022-03-01")) +
#   geom_dl(aes(label = series_id),
#           method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
#   scale_y_continuous(labels = percent)  +
#   geom_hline(yintercept = 0) +
#   # geom_hline(yintercept = .02, linetype = "dashed") +
#   geom_point(aes(y = point, colour = series_id),
#              show.legend = F, size = 3) +
#   labs(x = NULL, y = NULL,
#        title = "Used car prices are pushing up overall inflation",
#        subtitle = "Change from a year earlier. Used cars calculated at fixed Feb. 2020 weights.",
#        caption = "Source: Bureau of Labor Statistics")
# 
# ggsave("cpi_charts/ex_cars.png", p, device = "png", width = 14.2, height = 8)





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

item_contrib_old("CUUR0000SA0E", 6.941, ymd("2021-04-01"), all_items = "CUUR0000SA0")
item_contrib("CUUR0000SA0E", ymd("2021-04-01"), all_items = "CUUR0000SA0")

item_contrib_old("CUSR0000SETA", 7.712, ymd("2021-04-01"), all_items = "CUSR0000SA0L1E", all_items_wt = 79.131)
item_contrib("CUSR0000SETA", ymd("2021-06-01"), all_items = "CUSR0000SA0L1E")


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

p <- ex_items("CUSR0000SETA", startdate = ymd("2020-02-01"),
              all_items = "CUSR0000SA0") %>% 
  select(date, all_items, ex_item) %>% 
  gather(series_id, change, -date) %>% 
  mutate(series_id = factor(series_id, levels = c("all_items", "ex_item"),
                            labels = c("All items", "Ex-autos"))) %>% 
  group_by(series_id) %>% 
  mutate(point = case_when(date == max(date) ~ change/100)) %>% 
  ggplot(aes(date, change/100, colour = series_id)) + geom_line(size = 1, show.legend = F)

p <- p +
  cpi_theme() +
  xlim(ymd("2020-02-01"), ymd("2024-04-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Ex-autos` = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Car prices have pushed up overall inflation",
       subtitle = "Cumulative change in consumer prices since Feb. 2020, with and without new & used car component",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex_cars.png", p, device = "png", width = 14.2, height = 8)


ex_items("CUUR0000SETA", 
                startdate = "2020-12-01", 
                all_items = "CUUR0000SA0") %>% 
  arrange(desc(date))


p <- ex_items("CUSR0000SETA02", startdate = ymd("2020-02-01"),
              all_items = "CUSR0000SA0") %>% 
  select(date, all_items, ex_item) %>% 
  gather(series_id, change, -date) %>% 
  mutate(series_id = factor(series_id, levels = c("all_items", "ex_item"),
                            labels = c("All items", "Ex-used cars"))) %>% 
  group_by(series_id) %>% 
  mutate(point = case_when(date == max(date) ~ change/100)) %>% 
  ggplot(aes(date, change/100, colour = series_id)) + 
  geom_line(size = 1, show.legend = F)

p <- p +
  cpi_theme() +
  xlim(ymd("2020-02-01"), ymd("2024-04-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Ex-used cars` = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Used car prices pushed up inflation this year",
       subtitle = "Cumulative change in consumer prices since Feb. 2020, with and without used car component",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex_used.png", p, device = "png", width = 14.2, height = 8)

ex_items("CUSR0000SETA02", "2020-02-01")
ex_items_yy("CUUR0000SETA02", "2020-02-01")

# CALC HERE
ex_items("CUUR0000SETA02", startdate = ymd("2020-12-01"),
         all_items = "CUUR0000SA0") %>% 
  select(date, all_items, ex_item) %>% 
  gather(series_id, value, -date) %>% 
  mutate(series_id = factor(series_id, levels = c("all_items", "ex_item"),
                            labels = c("All items", "Ex-used cars"))) %>% 
  group_by(series_id) %>% 
  mutate(value = value/first(value) - 1) %>% 
  spread(series_id, value)

item_contrib("CUSR0000SETG01", startdate = ymd("2022-06-01"),
             all_items = "CUSR0000SA0")

item_contrib("CUUR0000SETA", startdate = ymd("2021-01-01"),
             all_items = "CUUR0000SA0")

cpi_data %>% 
  filter(series_id %in% c("CUUR0000SETA", "CUUR0000SA0"),
         date >= ymd("2020-02-01")) %>% 
  select(series_id, date, value) %>% 
  spread(series_id, value) %>% 
  left_join(cpi_wts %>% filter(item_code == "SETA"), by = "date") %>%
  mutate(item_chg = first(wt) * (CUUR0000SETA/first(CUUR0000SETA)) - first(wt),
         all_item_chg = 100 * (CUUR0000SA0/first(CUUR0000SA0)) - 100,
         contrib_pct = item_chg/all_item_chg,
         contrib_pp = contrib_pct * all_item_chg,
         ex_item = all_item_chg - contrib_pp) %>%
  select(-item_code, -wt, -w_wt)




ex_items_yy("CUUR0000SETA", startdate = ymd("2016_12_01"),
         all_items = "CUUR0000SA0") %>%
  select(date, all_items, ex_item) %>% 
  gather(series_id, change, -date) %>% 
  mutate(series_id = factor(series_id, levels = c("all_items", "ex_item"),
                            labels = c("All items", "Ex-used cars"))) %>% 
  group_by(series_id) %>% 
  mutate(point = case_when(date == max(date) ~ change)) %>% 
  ggplot(aes(date, change, colour = series_id)) + geom_line(size = 1, show.legend = F)


cpi_data %>% 
  filter(series_id %in% c("CUUR0000SETA", "CUUR0000SA0"),
         date >= ymd("2016-12-01")) %>% 
  select(series_id, date, value) %>% 
  spread(series_id, value) %>% 
  left_join(cpi_wts %>% filter(item_code == "SETA"), by = "date") %>%
  mutate(item_chg = lag(wt, 12, na.pad = T) * (CUUR0000SETA/lag(CUUR0000SETA, 12, na.pad = T)) - lag(wt, 12, na.pad = T),
         all_item_chg = 100 * (CUUR0000SA0/lag(CUUR0000SA0, 12, na.pad = T)) - 100,
         contrib_pct = item_chg/all_item_chg,
         contrib_pp = contrib_pct * all_item_chg,
         ex_item = all_item_chg - contrib_pp) %>%
  select(-item_code, -wt, -w_wt) %>% 
  filter(!is.na(item_chg)) %>% 
  arrange(desc(date))


cpi_data %>% 
  filter(series_id %in% c("CUSR0000SETA02", "CUSR0000SA0"),
         date >= ymd("2020-01-01")) %>% 
  left_join(cpi_series, by = "series_id") %>% 
  left_join(cpi_wts, by = c("item_code", "date")) %>% 
  mutate(series_id = factor(series_id, levels = c("CUSR0000SETA02", "CUSR0000SA0"),
                            labels = c("item", "all"))) %>% 
  select(series_id, date, value, wt) %>% 
  pivot_wider(names_from = series_id, values_from = c(value, wt)) %>% 
  mutate(contrib = (lag(wt_item, 1, na.pad = T)/wt_all)*(value_item/lag(value_item, 1, na.pad = T))) %>% 
  group_by(date) %>% 
  summarize(wt = sum(wt_item),
            contrib = sum(contrib))
  


ex_items_2 <- function(series, startdate, 
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


ex_items("CUSR0000SETA", startdate = "2020-02-01",
         all_items = "CUSR0000SA0") 



p <- cpi_data %>% 
  filter(series_id %in% c("CUSR0000SETA04", "CUSR0000SETG01", "CUSR0000SEHB02"),
         date >= ymd("2020-02-01")) %>% 
  mutate(series_id = factor(series_id,
                            levels = c("CUSR0000SETA04", "CUSR0000SETG01", "CUSR0000SEHB02"),
                            labels = c("Car rental", "Airfare", "Hotels"))) %>% 
  group_by(series_id) %>% 
  mutate(change = value/first(value) - 1,
         point = case_when(date == max(date) ~ change)) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  xlim(ymd("2020-02-01"), ymd("2024-02-01")) +
  labs(x = NULL, y = NULL,
       title = "Travel-related prices in the pandemic",
       subtitle = "Cumulative change since Feb. 2020 in select categories, seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics") +
  scale_colour_manual(values = c(Airfare = "#1f78b4", Hotels = "#33a02c", `Car rental` = "#fb9a99"))

ggsave("cpi_charts/travel.png", p, device = "png", width = 14.2, height = 8)


earnings <- fred_mult(series_id = c("CES0500000003", "CPIAUCSL", "AHETPI", "CES7000000008", "CWSR0000SA0"))

# earnings <- tibble(date = c(ymd("2023-03-01"), ymd("2023-03-01")),
#                             series_id = c("CPIAUCSL", "CWSR0000SA0"),
#                             value = c(301.808, 295.999 )) %>% 
#   bind_rows(earnings) %>% 
#   arrange(date)

earnings <- earnings %>% 
  spread(series_id, value) %>% 
  rename(nominal_prodsup = AHETPI,
         nominal_all = CES0500000003,
         nominal_leis = CES7000000008,
         cpi = CPIAUCSL,
         cpiw = CWSR0000SA0) %>% 
  mutate(real_all = nominal_all * last(cpi)/cpi,
         real_prodsup = nominal_prodsup * last(cpiw)/cpiw,
         real_leis = nominal_leis * last(cpiw)/cpiw) %>% 
  select(-cpiw, -cpi) %>% 
  pivot_longer(-date, names_to = c("adj", "industry"),
               names_sep = "_", values_to = "value")

p <- earnings %>% 
  filter(industry %in% c("all", "prodsup")) %>% 
  mutate(industry = factor(industry, levels = c("all", "prodsup"), labels = c("Total private", "Non-managers")),
         adj = factor(adj, levels = c("nominal", "real"), labels = c("Unadjusted", "Inflation \nadjusted"))) %>% 
  group_by(industry, adj) %>% 
  mutate(change = value/lag(value, 12, na.pad = T) - 1,
         point = case_when(date == max(date) ~ change)) %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(date, change, colour = adj)) +
  geom_line(size = 1, show.legend = F) +
  facet_wrap(~industry)

p <- p +
  cpi_theme() +
  geom_dl(aes(label = adj),
          method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  scale_color_manual(values = c(Unadjusted = "#a6cee3", `Inflation \nadjusted` = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = adj),
             show.legend = F, size = 3) +
  xlim(ymd("2019-01-01"), ymd("2024-07-01")) +
  theme(strip.text = element_text(size = 16)) +
  labs(x = NULL, y = NULL,
       title = "Hourly earnings growth, adjusted and unadjusted for inflation",
       subtitle = "Change from a year earlier",
       caption = "Note: Non-managers series deflated by CPI-W. | Source: Bureau of Labor Statistics")

ggsave("cpi_charts/earnings.png", p, device = "png", width = 14.2, height = 8)

p <- earnings %>% 
  filter(industry %in% c("all", "prodsup")) %>% 
  mutate(industry = factor(industry, levels = c("all", "prodsup"), labels = c("Total private", "Non-managers")),
         adj = factor(adj, levels = c("nominal", "real"), labels = c("Unadjusted", "Inflation adjusted"))) %>% 
  group_by(industry, adj) %>% 
  mutate(change = (value/lag(value, 3, na.pad = T))^4 - 1,
         point = case_when(date == max(date) ~ change)) %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(date, change, colour = adj)) +
  geom_line(size = 1, show.legend = F) +
  facet_wrap(~industry)

p <- p +
  cpi_theme() +
  geom_dl(aes(label = adj),
          method = list(dl.trans(x = x + .2), "last.qp", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  scale_color_manual(values = c(Unadjusted = "#a6cee3", `Inflation adjusted` = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = adj),
             show.legend = F, size = 3) +
  xlim(ymd("2019-01-01"), ymd("2025-05-01")) +
  theme(strip.text = element_text(size = 16)) +
  labs(x = NULL, y = NULL,
       title = "Hourly earnings growth, adjusted and unadjusted for inflation",
       subtitle = "Annualized three-month change",
       caption = "Note: Non-managers series deflated by CPI-W. | Source: Bureau of Labor Statistics")

ggsave("cpi_charts/earnings_3mo.png", p, device = "png", width = 14.2, height = 8)



p <- earnings %>% 
  filter(industry %in% c("all", "prodsup"),
         adj == "real") %>% 
  mutate(industry = factor(industry, levels = c("all", "prodsup"), labels = c("Total private", "Non-managers"))) %>% 
  group_by(industry) %>% 
  mutate(change = value/lag(value, 12, na.pad = T) - 1,
         point = case_when(date == max(date) ~ change)) %>% 
  filter(!is.na(change)) %>% 
  ggplot(aes(date, change, colour = industry)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`Total private` = "#a6cee3", `Non-managers`= "#1f78b4")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  geom_dl(aes(label = industry),
          method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  geom_point(aes(y = point, colour = industry),
             show.legend = F, size = 3) +
  xlim(ymd("1966-01-01"), ymd("2029-01-01")) +
  cpi_theme() +
  labs(x = NULL, y = NULL,
       title = "Change in hourly earnings, adjusted for inflation",
       subtitle = "Change from a year earlier",
       caption = "Note: Non-managers series deflated by CPI-W. | Source: Bureau of Labor Statistics")

ggsave("cpi_charts/earnings_history.png", p, device = "png", width = 14.2, height = 8)


p <- earnings %>% 
  filter(industry %in% c("prodsup", "leis")) %>% 
  group_by(adj, industry) %>% 
  mutate(adj = factor(adj, levels = c("nominal", "real"), labels = c("Unadjusted", "Inflation adjusted")),
         industry = factor(industry, levels = c("prodsup", "leis"), labels = c("All private", "Leisure & \nhospitality"))) %>% 
  mutate(change = value/lag(value, 12, na.pad = T) - 1,
         point = case_when(date == max(date) ~ change)) %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(date, change, colour = industry)) +
  geom_line(size = 1, show.legend = F) +
  facet_wrap(~adj) 

p <- p +
  cpi_theme() +
  geom_dl(aes(label = industry),
          method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = industry),
             show.legend = F, size = 3) +
  scale_color_manual(values = c(`All private` = "#a6cee3", `Leisure & \nhospitality` = "#1f78b4")) +
  xlim(ymd("2019-01-01"), ymd("2023-11-01")) +
  theme(strip.text = element_text(size = 16)) +
  labs(x = NULL, y = NULL,
       title = "Hourly earnings growth in leisure and hospitality",
       subtitle = "Change from a year earlier in earnings of production and nonsupervisory workers",
       caption = "Note: Deflated by CPI-W. | Source: Bureau of Labor Statistics")

ggsave("cpi_charts/leis_earnings.png", p, device = "png", width = 14.2, height = 8)

# Real levels vs trend
p <- earnings %>%
  filter(adj == "real",
         industry %in% c("prodsup", "leis"),
         year(date) >= 2017) %>% 
  mutate(industry = factor(industry, levels = c("prodsup", "leis"),
                           labels = c("Total private", "Leisure & hospitality"))) %>% 
  group_by(industry) %>% 
  mutate(index = (value/first(value)) * 100)

p <- p %>% 
  ggplot(aes(date, index, colour = industry)) +
  geom_line(size = 1, show.legend = F) +
  stat_smooth(data = subset(p, date <= ymd("2020-01-01")),
              method = "glm",
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE,
              method.args = list(family=gaussian(link="log")),
              size = 1.1,
              show.legend = F) +
  scale_colour_manual(values = c(`Total private` = "#1f78b4", `Leisure & hospitality` = "#33a02c"))

p <- p +
  cpi_theme() +
  xlim(ymd("2017-01-01"), ymd("2024-11-01")) +
  geom_dl(aes(label = industry),
          method = list(dl.trans(x = x + .5), "last.qp", cex = 1.4)) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  labs(x = NULL, y = NULL,
       title = "Real average hourly earnings since 2017",
       subtitle = "Average hourly earnings of production and nonsupervisory workers. Jan. 2017 = 100.",
       caption = "Notes: Seasonally adjusted. Deflated using CPI-W. | Source: Bureau of Labor Statistics")


ggsave("cpi_charts/earnings_levels.png", p, device = "png", width = 14.2, height = 8)



# r_all <- earnings %>% 
#   filter(adj == "real",
#          industry == "prodsup",
#          date >= ymd("2019-01-01"),
#          date <= ymd("2020-02-01")) %>% 
#   mutate(chg = value/first(value) - 1) %>% 
#   glm(chg ~ date, data = .)
# 
# r_leis <- earnings %>% 
#   filter(adj == "real",
#          industry == "leis",
#          date >= ymd("2019-01-01"),
#          date <= ymd("2020-02-01")) %>% 
#   mutate(chg = value/first(value) - 1) %>% 
#   lm(chg ~ date, data = .)
# 
# 
# p <- earnings %>% 
#   filter(date >= ymd("2019-01-01"),
#          industry %in% c("prodsup", "leis"),
#          adj == "real") %>% 
#   group_by(industry) %>% 
#   mutate(chg = value/first(value) - 1) %>% 
#   mutate(trend = case_when(industry == "prodsup" ~ map_dbl(date, ~predict(r_all, newdata = tibble(date = .x))),
#                                   industry == "leis" ~ map_dbl(date, ~predict(r_leis, newdata = tibble(date = .x))))) %>% 
#   ungroup() %>% 
#   mutate(industry = factor(industry, levels = c("prodsup", "leis"), labels = c("Total private", "Leisure & \nhospitality"))) %>% 
#   ggplot(aes(date, chg, colour = industry)) +
#   geom_line(size = 1)  +
#   geom_line(aes(y = trend, colour = industry), linetype = "dashed", size = 1,
#             show.legend = F)

p <- earnings %>%
  filter(date >= ymd("2017-01-01"),
         industry %in% c("prodsup", "leis"),
         adj == "real") %>%
  mutate(industry = factor(industry, levels = c("prodsup", "leis"), labels = c("Total private", "Leisure & \nhospitality"))) %>% 
  group_by(industry) %>% 
  mutate(chg = value/nth(value, 38) * 100)

p <- p %>%
  ggplot(aes(date, chg, colour = industry)) +
  geom_line(size = 1)  +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")),
              method = "glm",
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE,
              method.args = list(family=gaussian(link="log")),
              size = 1.1)

p <- p + 
  # geom_hline(yintercept = 0) +
  scale_color_manual(values = c(`Total private` = "#a6cee3", `Leisure & \nhospitality` = "#1f78b4")) +
  labs(title = "Real average hourly earnings vs prepandemic trend",
       subtitle = "Production and nonsupervisory workers, seasonally adjusted. Index, Feb. 2020 = 100.",
       caption = "Note: Deflated by CPI-W. Log-linear trend. | Source: Bureau of Labor Statistics",
       x = NULL, y = NULL) +
  # scale_y_continuous(labels = percent) +
  theme(plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 20),
        plot.background = element_rect(fill = "white"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.4),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey50", size = 16),
        axis.ticks = element_line(colour = "grey", size = 0.4),
        plot.caption = element_text(colour = "grey50", size = 16),
        legend.position = c(.25, .75),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.background = element_blank(),
        legend.key = element_blank())

ggsave("cpi_charts/leis_levels.png", p, device = "png", width = 14.2, height = 8)


p <- ex_items(series = c("CUSR0000SETA", "CUSR0000SEHB02", "CUSR0000SETG01", "CUSR0000SETA04"),
         startdate = "2020-02-01") %>% 
  select(date, all_items, ex_item) %>% 
  gather(series_id, value, -date) %>% 
  mutate(series_id = factor(series_id, levels = c("all_items", "ex_item"),
                            labels = c("All items", "Ex-Covid"))) %>% 
  group_by(series_id) %>% 
  mutate(change = value/first(value) - 1,
         point = case_when(date == max(date) ~ change)) %>% 
  ggplot(aes(date, change, colour = series_id)) + geom_line(size = 1, show.legend = F)

p <- p +
  cpi_theme() +
  xlim(ymd("2020-02-01"), ymd("2023-11-01")) +
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
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex_cars.png", p, device = "png", width = 14.2, height = 8)

item_contrib("CUSR0000SETA02", startdate = "2021-05-01", all_items = "CUSR0000SA0L1E")
item_contrib_old("CUSR0000SETA02", rel_imp = 3.166, startdate = "2021-05-01", all_items = "CUSR0000SA0L1E", all_items_wt = 79.053)



# vs precovid trend

r_cpi <- cpi_data %>% 
  filter(series_id == "CUSR0000SA0",
         date >= ymd("2015-02-01"),
         date <= ymd("2020-02-01")) %>% 
  lm(value ~ date, data = .)

r_cpi_nsa <- cpi_data %>% 
  filter(series_id == "CUUR0000SA0",
         date >= ymd("2015-02-01"),
         date <= ymd("2020-02-01"),
         !is.na(date)) %>% 
  lm(value ~ date, data = .)

# r_cpi_nsa <- cpi_data %>% 
#   filter(series_id == "CUUR0000SA0",
#          date >= ymd("2015-02-01"),
#          date <= ymd("2020-02-01"),
#          !is.na(date)) %>% 
#   glm(value ~ date, data = ., family=poisson())

r_cpi <- cpi_data %>% 
  filter(series_id == "CUSR0000SA0",
         date >= ymd("2015-02-01"),
         date <= ymd("2020-02-01")) %>% 
  glm(value ~ date, data = ., family = gaussian(link = "log"))

p <- cpi_data %>% 
  filter(series_id == "CUSR0000SA0",
         date >= ymd("2017-01-01")) %>% 
  # group_by(series_id) %>% 
  # mutate(chg = value/first(value) - 1) %>% 
  mutate(trend = map_dbl(date, ~predict(r_cpi, newdata = tibble(date = .x)))) %>% 
  ungroup() %>% 
  ggplot(aes(date, value)) +
  geom_line(size = 1, colour = "#1f78b4") +
  geom_line(aes(y = trend), linetype = "dashed", size = 1, colour = "grey25")


p <- cpi_data %>% 
  filter(series_id == "CUSR0000SA0",
         date >= ymd("2017-01-01"))

p <- p %>% 
  ggplot(aes(date, value)) +
  geom_line(size = 1, colour = "#1f78b4") +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "glm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE,
              method.args = list(family=gaussian(link="log")),
              colour = "grey25",
              size = 1) 
p <- p + 
  labs(title = "Consumer prices vs pre-Covid trend",
       subtitle = "Trendline based on five years prior to pandemic. Index values, seasonally adjusted.",
       caption = "Source: Bureau of Labor Statistics",
       x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 20),
        plot.background = element_rect(fill = "white"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.4),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey50", size = 16),
        axis.ticks = element_line(colour = "grey", size = 0.4),
        plot.caption = element_text(colour = "grey50", size = 16),
        legend.position = c(.25, .75),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.background = element_blank(),
        legend.key = element_blank())

ggsave("cpi_charts/vs_trend.png", p, device = "png", width = 14.2, height = 8)


cpi_data %>% 
  filter(series_id == "CUSR0000SA0",
         date == ymd("2020-02-01"))

p <- cpi_data %>% 
  filter(series_id == "CUSR0000SA0",
         date >= ymd("2017-01-01")) %>% 
  mutate(value = 100*value/(cpi_data %>% 
                              filter(series_id == "CUSR0000SA0",
                                     date == ymd("2020-02-01")) %>% .$value))

p <- p %>% 
  ggplot(aes(date, value)) +
  geom_line(size = 1, colour = "#1f78b4") +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "glm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE,
              method.args = list(family=gaussian(link="log")),
              colour = "grey25",
              size = 1) 
p <- p + 
  labs(title = "Consumer prices vs pre-Covid trend",
       subtitle = "Trendline based on five years prior to pandemic. Feb. 2020 = 100. Seasonally adjusted.",
       caption = "Source: Bureau of Labor Statistics",
       x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 20),
        plot.background = element_rect(fill = "white"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.4),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey50", size = 16),
        axis.ticks = element_line(colour = "grey", size = 0.4),
        plot.caption = element_text(colour = "grey50", size = 16),
        legend.position = c(.25, .75),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.background = element_blank(),
        legend.key = element_blank())

ggsave("cpi_charts/vs_trend.png", p, device = "png", width = 14.2, height = 8)



p <- cpi_data %>% 
  filter(series_id == "CUSR0000SA0L1E",
         date >= ymd("2017-01-01"))

p <- p %>% 
  ggplot(aes(date, value)) +
  geom_line(size = 1, colour = "#1f78b4") +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "glm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE,
              method.args = list(family=gaussian(link="log")),
              colour = "grey25",
              size = 1) 
p <- p + 
  labs(title = "Core consumer prices vs pre-Covid trend",
       subtitle = "Trendline based on five years prior to pandemic. Index values, seasonally adjusted.",
       caption = "Source: Bureau of Labor Statistics",
       x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 20),
        plot.background = element_rect(fill = "white"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.4),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey50", size = 16),
        axis.ticks = element_line(colour = "grey", size = 0.4),
        plot.caption = element_text(colour = "grey50", size = 16),
        legend.position = c(.25, .75),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.background = element_blank(),
        legend.key = element_blank())

ggsave("cpi_charts/core_vs_trend.png", p, device = "png", width = 14.2, height = 8)



cpi_data %>% 
  filter(series_id == "CUUR0000SA0",
         date >= ymd("2017-01-01"),
         !is.na(date)) %>% 
  # group_by(series_id) %>% 
  # mutate(chg = value/first(value) - 1) %>% 
  mutate(trend = map_dbl(date, ~predict(r_cpi_nsa, newdata = tibble(date = .x))),
         actual_chg = value/lag(value, 12, na.pad = T) - 1,
         alt_chg = value/lag(trend, 12, na.pad = T) - 1) %>% 
  select(date, actual_chg, alt_chg) %>% 
  arrange(desc(date))

p <- cpi_data %>% 
  filter(series_id == "CUUR0000SA0",
         date >= ymd("2017-01-01"),
         !is.na(date)) %>% 
  # group_by(series_id) %>% 
  # mutate(chg = value/first(value) - 1) %>% 
  mutate(trend = map_dbl(date, ~predict(r_cpi_nsa, newdata = tibble(date = .x))),
         actual_chg = value/lag(value, 12, na.pad = T) - 1,
         alt_chg = value/lag(trend, 12, na.pad = T) - 1) %>% 
  select(date, actual_chg, alt_chg) %>%
  gather(series, value, -date) %>% 
  mutate(series = factor(series, levels = c("actual_chg", "alt_chg"),
                         labels = c("Actual", "Ex-base effect")),
         point = case_when(date == max(date) ~ value)) %>% 
  ggplot(aes(date, value, colour = series)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  scale_color_manual(values = c(`Actual` = "#a6cee3", `Ex-base effect` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2020-02-01"), ymd("2023-04-01")) +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .5), "last.qp", cex = 1.4)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Year-over-year change in prices",
       subtitle = "Not seasonally adjusted. Base effects based on five-year pre-Covid trend",
       caption = "Source: Bureau of Labor Statistics")


ggsave("cpi_charts/base_effects.png", p, device = "png", width = 14.2, height = 8)



cpi_data %>% 
  filter(series_id == "CUUR0000SA0",
         !is.na(date)) %>% 
  mutate(chg = value/lag(value, 12, na.pad = T) - 1) %>% 
  arrange(desc(chg))

p <- cpi_data %>% 
  filter(series_id == "CUUR0000SA0",
         !is.na(date)) %>% 
  mutate(chg = value/lag(value, 12, na.pad = T) - 1,
         chg_2 = (value/lag(value, 24, na.pad = T))^.5 - 1) %>% 
  select(date, chg, chg_2) %>% 
  gather(series, value, -date) %>% 
  mutate(series = factor(series, levels = c("chg", "chg_2"),
                         labels = c("One-year change", "Two-year change")),
         point = case_when(date == max(date) ~ value)) %>% 
  filter( year(date) >= 2018) %>% 
  ggplot(aes(date, value, colour = series)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  scale_color_manual(values = c(`One-year change` = "#a6cee3", `Two-year change` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2018-01-01"), ymd("2024-02-01")) +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .5), "last.qp", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Change in prices over one and two years",
       subtitle = "Two-year change calculated as annualized change from two years earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/two_year.png", p, device = "png", width = 14.2, height = 8)




p <- cpi_y("CUUR0000SA0E", "Energy") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `Energy` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2023-11-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Energy prices are way pu over the past year",
       subtitle = "Change from a year earlier in energy prices",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/energy_y.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_3m("CUSR0000SA0E", "Energy") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "gray50", `Energy` = "#00BFC4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2023-11-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in energy prices",
       subtitle = "Seasonally adjusted annual rate.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/energy_3m.png", p, device = "png", width = 14.2, height = 8)



p <- cpi_y("CUUR0000SETB01", "Gasoline") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Gasoline` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2023-11-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Annual change in gasoline prices",
       subtitle = "Change from a year earlier in retail gasoline prices",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/gas_y.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_3m("CUSR0000SETB01", "Gasoline") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Gasoline` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2023-11-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in gasoline prices",
       subtitle = "Seasonally adjusted annual rate.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/gas_3m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_m("CUSR0000SETB01", "Gasoline") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Gasoline` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2023-11-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Monthly change in gasoline prices",
       subtitle = "Seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/gas_1m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_y("CUUR0000SAH21", "Household energy") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Household energy` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2023-11-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Home heating prices prices are rising",
       subtitle = "Change from a year earlier in household energy prices",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/hhold_energy_y.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_3m("CUSR0000SAH21", "Household energy") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Household energy` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2023-11-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in household energy prices",
       subtitle = "Seasonally adjusted annual rate.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/hhold_energy_3m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_y("CUUR0000SAF11", "Groceries") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Groceries` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-01-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Year-over-year change in grocery prices",
       subtitle = "Change from a year earlier in prices for food at home",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/food_y.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_3m("CUSR0000SAF11", "Groceries") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Groceries` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-01-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in prices for food at home",
       subtitle = "Seasonally adjusted annual rate.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/food_3m.png", p, device = "png", width = 14.2, height = 8)

p <- cpi_m("CUSR0000SAF11", "Groceries") %>% 
  filter(year >= 2016) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Groceries` = "#1f78b4")) +
  cpi_theme() +
  xlim(ymd("2016-01-01"), ymd("2024-01-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "One-month change in prices for food at home",
       subtitle = "Seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/food_1m.png", p, device = "png", width = 14.2, height = 8)


cpi_data %>% 
  filter(series_id %in% c("CUSR0000SEEB01", "CUSR0000SA0"),
         year(date) >= 2010) %>% 
  mutate(series_id = factor(series_id, levels = c("CUSR0000SEEB01", "CUSR0000SA0"),
                            labels = c("Tuition", "Total"))) %>% 
  group_by(series_id) %>% 
  mutate(change = value/first(value) - 1) %>% 
  ggplot(aes(date, change, colour = series_id)) +
  geom_line()



p <- cpi_data %>% 
  filter(series_id %in% c("CUUR0000SEHA", "CUUR0000SEEB01", "CUUR0000SAM")) %>% 
  mutate(series_id = factor(series_id, levels = c("CUUR0000SEHA", "CUUR0000SEEB01", "CUUR0000SAM"),
                            labels = c("Rent", "Tuition & fees", "Medical care"))) %>% 
  arrange(date) %>% 
  group_by(series_id) %>% 
  mutate(change = value/lag(value, 12, na.pad = T) - 1) %>% 
  filter(year(date) >= 2000) %>% 
  ggplot(aes(date, change, colour = series_id)) + 
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y", limits = c(ymd("2000-01-01"), ymd("2024-06-01"))) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  labs(x = NULL, y = NULL,
       title = "Inflation in rent, college costs and health care",
       subtitle = "Change from a year earlier, not seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/millennials.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_data %>% 
  filter(series_id %in% c("CUUR0000SA0LE", "CUUR0000SA0")) %>% 
  mutate(series_id = factor(series_id, levels = c("CUUR0000SA0LE", "CUUR0000SA0"),
                            labels = c("Ex-energy", "All items"))) %>% 
  arrange(date) %>% 
  group_by(series_id) %>% 
  mutate(change = value/lag(value, 12, na.pad = T) - 1) %>% 
  filter(year(date) >= 2000) %>% 
  ggplot(aes(date, change, colour = series_id)) + 
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x - 3.5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  labs(x = NULL, y = NULL,
       title = "Inflation with and without energy prices",
       subtitle = "Change from a year earlier, not seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics") +
  scale_colour_manual(values = c(`Ex-energy` = "#1f78b4", `All items` = "#a6cee3"))

ggsave("cpi_charts/ex_energy.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_full_history %>% 
  filter(series_id %in% c("CUUR0000SA0"),
         !is.na(date)) %>% 
  mutate(chg = value/lag(value, 12, na.pad = T) - 1,
         point = case_when(date == max(date) ~ chg),
         label = case_when(date == max(date) ~ paste0("+", percent(chg, accuracy = .1)))) %>% 
  filter(!is.na(chg),
         year(date) >= 1945) %>% 
  ggplot(aes(date, chg)) +
  geom_line(size = 1.2, colour = "#1f78b4") +
  geom_point(aes(y = point), colour = "#1f78b4", show.legend = F, size = 3) + 
  geom_dl(aes(label = label), colour = "#1f78b4",
          method = list(dl.trans(x = x + .25), "last.points", cex = 1.5)) +
  geom_hline(yintercept = 0) +
  recession_shade("1946-01-01") +
  scale_y_continuous(labels = percent) +
  xlim(ymd("1945-01-01"), ymd("2025-03-01")) +
  labs(x = NULL, y = NULL,
       title = "Inflation since World War II",
       subtitle = "Year-over-year change in Consumer Price Index. Shaded areas denote recessions.",
       caption = "Source: Bureau of Labor Statistics") +
  cpi_theme()

ggsave("cpi_charts/full_history.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_full_history %>% 
  filter(series_id %in% c("CUSR0000SA0"),
         !is.na(date)) %>% 
  mutate(chg = (value/lag(value, 3, na.pad = T))^4 - 1,
         point = case_when(date == max(date) ~ chg),
         label = case_when(date == max(date) ~ paste0("+", percent(chg, accuracy = .1)))) %>% 
  filter(!is.na(chg),
         year(date) >= 1945) %>% 
  ggplot(aes(date, chg)) +
  geom_line(size = 1.2, colour = "#1f78b4") +
  geom_point(aes(y = point), colour = "#1f78b4", show.legend = F, size = 3) + 
  geom_dl(aes(label = label), colour = "#1f78b4",
          method = list(dl.trans(x = x + .25), "last.points", cex = 1.5)) +
  geom_hline(yintercept = 0) +
  recession_shade("1946-01-01") +
  scale_y_continuous(labels = percent) +
  xlim(ymd("1945-01-01"), ymd("2025-03-01")) +
  labs(x = NULL, y = NULL,
       title = "Inflation since World War II, another look",
       subtitle = "Three-month annualized change in Consumer Price Index. Shaded areas denote recessions.",
       caption = "Source: Bureau of Labor Statistics") +
  cpi_theme()

ggsave("cpi_charts/full_history_3mo.png", p, device = "png", width = 14.2, height = 8)


# CPI-U-RS

cpi_rs <- readxl::read_xlsx("r-cpi-u-rs-allitems.xlsx", skip = 5)
cpi_rs <- cpi_rs %>% 
  gather(month, value, -YEAR, -AVG) %>% 
  mutate(date = ymd(paste(YEAR, month, "01", sep = "-"))) %>% 
  arrange(date) %>% 
  filter(!is.na(value)) %>% 
  select(date, value)

p <- cpi_rs %>% 
  mutate(chg_y = value/lag(value, 12, na.pad = 1) - 1,
         series = "Adjusted series") %>% 
  filter(year(date) >= 1979)

p <- cpi_full_history %>% 
  filter(series_id == "CUUR0000SA0",
         !is.na(date)) %>% 
  mutate(chg_y = value/lag(value, 12, na.pad = 1) - 1,
         series = "Official series") %>% 
  filter(year >= 1979) %>% 
  select(date, value, chg_y, series) %>% 
  bind_rows(p)

p <- cpi_full_history %>% 
  filter(series_id == "CUUR0000SA0",
         !is.na(date)) %>% 
  mutate(chg_y = value/lag(value, 12, na.pad = 1) - 1,
         series = "Adjusted series") %>% 
  filter(year >= 2021) %>% 
  select(date, value, chg_y, series) %>% 
  bind_rows(p)

p <- p %>% 
  mutate(series = factor(series, levels = c("Official series", "Adjusted series"))) %>% 
  ggplot(aes(date, chg_y, colour = series)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  recession_shade("1979-01-01") +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "The official CPI overstated inflation in earlier decades",
       subtitle = "Year-over-year change in Consumer Price Index. Adjusted series (CPI-U-RS) incorporates current methodologies to construct consistent series.",
       caption = "Notes: CPI-U-RS extended into 2021 using official CPI data. Shaded areas denote recessions.\nSource: Bureau of Labor Statistics") +
  cpi_theme() +
  scale_color_manual(values = c(`Official series` = "#a6cee3", `Adjusted series` = "#1f78b4")) +
  annotate("text", x = ymd("1982-06-01"), y = .13, label = "bold(`Official`)", parse = TRUE, colour = "#a6cee3", size = 6) +
  annotate("text", x = ymd("2019-10-01"), y = .065, label = "bold(`Adjusted`)", parse = TRUE, colour = "#1f78b4", size = 6) 
  

ggsave("cpi_charts/cpi_rs.png", p, device = "png", width = 14.2, height = 8)



cpi_data %>% 
  filter(series_id %in% c("CUUR0000SEFV", "CUUR0000SEFV02"),
         !is.na(date)) %>% 
  mutate(series_id = factor(series_id, levels = c("CUUR0000SEFV", "CUUR0000SEFV02"),
                            labels = c("Food away", "Limited-service"))) %>% 
  group_by(series_id) %>% 
  mutate(change = value/lag(value, 12, na.pad = T) - 1) %>% 
  filter(!is.na(change)) %>% 
  ggplot(aes(date, change, colour = series_id)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Year-over-year change in restaurant prices")

CUUR0000SA0E
ex_items("CUUR0000SA0E", "2021-01-01", all_items = "CUUR0000SA0")
ex_items_yy("CUUR0000SA0E", "2021-01-01", all_items = "CUUR0000SA0")


ex_items("CUUR0000SAF11", "2022-01-01")
ex_items_yy("CUUR0000SAH1", "2021-02-01", all_items = "CUUR0000SA0")



p <- ex_items("CUSR0000SETB01", startdate = ymd("2020-02-01"),
              all_items = "CUSR0000SA0") %>% 
  select(date, all_items, ex_item) %>% 
  gather(series_id, change, -date) %>% 
  mutate(series_id = factor(series_id, levels = c("all_items", "ex_item"),
                            labels = c("All items", "Ex-gasoline"))) %>% 
  group_by(series_id) %>% 
  mutate(point = case_when(date == max(date) ~ change/100)) %>% 
  ggplot(aes(date, change/100, colour = series_id)) + 
  geom_line(size = 1, show.legend = F)

p <- p +
  cpi_theme() +
  xlim(ymd("2020-02-01"), ymd("2022-11-01")) +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Ex-used cars` = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Used car prices pushed up inflation this year",
       subtitle = "Cumulative change in consumer prices since Feb. 2020, with and without used car component",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex_used.png", p, device = "png", width = 14.2, height = 8)


agg_hours <- fred_mult(series_id = c("CEU0500000017", "CEU3000000035", "CPIAUCNS", "CWUR0000SA0"))

agg_hours <- agg_hours %>% 
  spread(series_id, value) %>% 
  rename(nominal_prodsup = CEU3000000035,
         nominal_all = CEU0500000017,
         cpi = CPIAUCNS,
         cpiw = CWUR0000SA0) %>% 
  mutate(real_all = nominal_all * last(cpi)/cpi,
         real_prodsup = nominal_prodsup * last(cpiw)/cpiw) %>% 
  select(-cpiw, -cpi) %>% 
  pivot_longer(-date, names_to = c("adj", "industry"),
               names_sep = "_", values_to = "value") %>% 
  filter(year(date) >= 2010)

p <- agg_hours %>% 
  filter(industry %in% c("all", "prodsup")) %>% 
  mutate(industry = factor(industry, levels = c("all", "prodsup"), labels = c("Total private", "Non-managers")),
         adj = factor(adj, levels = c("nominal", "real"), labels = c("Unadjusted", "Inflation \nadjusted"))) %>% 
  group_by(industry, adj) %>% 
  mutate(change = value/lag(value, 12, na.pad = T) - 1,
         point = case_when(date == max(date) ~ change)) %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(date, change, colour = adj)) +
  geom_line(size = 1, show.legend = F) +
  facet_wrap(~industry)

p <- p +
  cpi_theme() +
  geom_dl(aes(label = adj),
          method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  scale_color_manual(values = c(Unadjusted = "#a6cee3", `Inflation \nadjusted` = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = adj),
             show.legend = F, size = 3) +
  xlim(ymd("2019-01-01"), ymd("2024-05-01")) +
  theme(strip.text = element_text(size = 16)) +
  labs(x = NULL, y = NULL,
       title = "Aggregate earnings growth, adjusted and unadjusted for inflation",
       subtitle = "Change from a year earlier in indexes of aggregate hours worked",
       caption = "Note: Non-managers series deflated by CPI-W. | Source: Bureau of Labor Statistics")

ggsave("cpi_charts/earnings_agg.png", p, device = "png", width = 14.2, height = 8)



atlanta <- read_csv("atlanta_fed_tracker_by_quartile.csv")

p <- atlanta %>% 
  select(-Overall) %>% 
  gather(quartile, value, -date) %>% 
  mutate(date = mdy(date)) %>% 
  left_join(cpi_data %>% 
              filter(series_id == "CUUR0000SA0") %>% 
              mutate(change = value/lag(value, 12, na.pad = T) -1,
                     roll = zoo::rollmean(change, 12, align = "right", na.pad = T)) %>% 
                       select(date, cpi = roll),
                     by = "date") %>% 
  mutate(real = value - 100*cpi) %>% 
  ggplot(aes(date, real/100, colour = quartile)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  cpi_theme() +
  geom_dl(aes(label = quartile),
          method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  scale_y_continuous(labels = percent)  +
  # scale_color_manual(values = c(Unadjusted = "#a6cee3", `Inflation \nadjusted` = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  xlim(ymd("2000-01-01"), ymd("2025-02-01")) +
  labs(x = NULL, y = NULL,
       title = "Real wage growth by income quartile",
       subtitle = "Change from a year earlier in hourly wages, deflated by CPI-U",
       caption = "Note: Both wages and CPI shown as 12-month rollings average of 12-month changes. | Source: Bureau of Labor Statistics, Atlanta Fed")

ggsave("cpi_charts/earnings_agg.png", p, device = "png", width = 14.2, height = 8)


# Ex-health insurance
p <- ex_items_yy("CUUR0000SEME", startdate = "2019-01-01") %>% 
  select(date, `All items` = all_items, `Ex-insurance` = ex_item) %>% 
  gather(series, value, -date) %>% 
  ggplot(aes(date, value/100, colour = series)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .3), "last.qp", cex = 1.5)) +
  xlim(ymd("2020-01-01"), ymd("2023-05-01")) +
  scale_color_manual(values = c(`Ex-insurance` = "#1f78b4", `All items` = "#a6cee3")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  # geom_point(aes(y = point, colour = series_id),
  #            show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Inflation with and without health insurance",
       subtitle = "Change in consumer prices from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex_insurance.png", p, device = "png", width = 14.2, height = 8)

p <- ex_items_yy("CUUR0000SEME", all_items = "CUUR0000SA0L1E",startdate = "2019-01-01") %>% 
  select(date, `All items` = all_items, `Ex-insurance` = ex_item) %>% 
  gather(series, value, -date) %>% 
  ggplot(aes(date, value/100, colour = series)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .3), "last.qp", cex = 1.5)) +
  xlim(ymd("2020-01-01"), ymd("2023-05-01")) +
  scale_color_manual(values = c(`Ex-insurance` = "#a6cee3", `All items` = "#1f78b4")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  # geom_point(aes(y = point, colour = series_id),
  #            show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Inflation with and without health insurance",
       subtitle = "Change in consumer prices from a year earlier",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex_insurance.png", p, device = "png", width = 14.2, height = 8)



p <- cpi_y("CUUR0000SEME", "Health insurance") %>% 
  filter(year >= 2010) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .3), "last.points", cex = 1.5)) +
  xlim(ymd("2010-01-01"), ymd("2024-06-01")) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Health insurance` = "#1f78b4")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Year-over-year change in health insurance prices",
       subtitle = "Not seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/insurance.png", p, device = "png", width = 14.2, height = 8)


ex_items_mm("CUUR0000SEME", all_items = "CUUR0000SA0L1E",end_date = "2022-10-01")

item_contrib

p <- cpi_full_history %>% 
  filter(series_id %in% c("CUUR0000SA0", "CUUR0000SA0L2"),
         period != "M13") %>% 
  mutate(series_id = factor(series_id, 
                            levels = c("CUUR0000SA0", "CUUR0000SA0L2"),
                            labels = c("All items", "Ex-shelter"))) %>% 
  group_by(series_id) %>% 
  mutate(change = value/lag(value, 12, na.pad = T) - 1) %>% 
  filter(year >= 1945) %>% 
  ggplot(aes(date, change, colour = series_id)) +
  geom_line()



p <- cpi_3m("CUSR0000SA0L2", "Ex-shelter") %>% 
  filter(year >= 2015) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .3), "last.points", cex = 1.5)) +
  xlim(ymd("2015-01-01"), ymd("2024-1-01")) +
  scale_color_manual(values = c(`All items` = "#a6cee3", `Ex-shelter` = "#1f78b4")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Inflation with and without shelter",
       subtitle = "Three-month annualized change, seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex_shelter_3m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_3m("CUSR0000SA0L12E", "Ex-shelter", 
            comp_series = "CUSR0000SA0L1E",
            comp_label = "Core prices") %>% 
  filter(year >= 2015) %>% 
  ggplot(aes(date, change, color = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .3), "last.points", cex = 1.5)) +
  xlim(ymd("2015-01-01"), ymd("2024-01-01")) +
  scale_color_manual(values = c(`Core prices` = "#a6cee3", `Ex-shelter` = "#1f78b4")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Core inflation with and without shelter",
       subtitle = "Three-month annualized change, seasonally adjusted. Both series exclude food and energy.",
       caption = "Source: Bureau of Labor Statistics")

ggsave("cpi_charts/ex_shelter_3m_core.png", p, device = "png", width = 14.2, height = 8)





p <- cpi_full_history %>% 
  filter(series_id %in% c("CUUR0000SA0L1E"),
         !is.na(date)) %>% 
  mutate(chg = value/lag(value, 12, na.pad = T) - 1,
         point = case_when(date == max(date) ~ chg),
         label = case_when(date == max(date) ~ paste0("+", percent(chg, accuracy = .1)))) %>% 
  filter(!is.na(chg),
         year(date) >= 1945) %>% 
  ggplot(aes(date, chg)) +
  geom_line(size = 1.2, colour = "#1f78b4") +
  geom_point(aes(y = point), colour = "#1f78b4", show.legend = F, size = 3) + 
  geom_dl(aes(label = label), colour = "#1f78b4",
          method = list(dl.trans(x = x + .25), "last.points", cex = 1.5)) +
  geom_hline(yintercept = 0) +
  recession_shade("1960-01-01") +
  scale_y_continuous(labels = percent) +
  xlim(ymd("1960-01-01"), ymd("2025-06-01")) +
  labs(x = NULL, y = NULL,
       title = "Core inflation since the 1960s",
       subtitle = "Year-over-year change in Consumer Price Index, excluding food and energy. Shaded areas denote recessions.",
       caption = "Source: Bureau of Labor Statistics") +
  cpi_theme()

ggsave("cpi_charts/full_history_core.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_full_history %>% 
  filter(series_id %in% c("CUSR0000SA0L1E"),
         !is.na(date)) %>% 
  mutate(chg = (value/lag(value, 3, na.pad = T))^4 - 1,
         point = case_when(date == max(date) ~ chg),
         label = case_when(date == max(date) ~ paste0("+", percent(chg, accuracy = .1)))) %>% 
  filter(!is.na(chg),
         year(date) >= 1945) %>% 
  ggplot(aes(date, chg)) +
  geom_line(size = 1.2, colour = "#1f78b4") +
  geom_point(aes(y = point), colour = "#1f78b4", show.legend = F, size = 3) + 
  geom_dl(aes(label = label), colour = "#1f78b4",
          method = list(dl.trans(x = x + .25), "last.points", cex = 1.5)) +
  geom_hline(yintercept = 0) +
  recession_shade("1960-01-01") +
  scale_y_continuous(labels = percent) +
  xlim(ymd("1960-01-01"), ymd("2025-06-01")) +
  labs(x = NULL, y = NULL,
       title = "Core inflation since the 1960s, another look",
       subtitle = "Three-month annualized change in core Consumer Price Index. Shaded areas denote recessions.",
       caption = "Source: Bureau of Labor Statistics") +
  cpi_theme()

ggsave("cpi_charts/full_history_3mo_core.png", p, device = "png", width = 14.2, height = 8)

# Core services (services ex energy services)
ex_items_mm(c("CUSR0000SEHF"), "2022-01-01", all_items = "CUSR0000SAS")

# Services ex-shelter
ex_items(c("CUSR0000SAS2RS"), "2022-11-01", all_items = "CUSR0000SAS")
ex_items_mm(c("CUSR0000SAS2RS"), "2022-01-01", all_items = "CUSR0000SAS")

# Core services ex-shelter
# Subtract shelter from services ex-energy
ex_items(c("CUSR0000SAS2RS"), "2022-11-01", all_items = "CUSR0000SASLE")


ex_items(c("CUSR0000SAS2RS"), "2018-01-01", all_items = "CUSR0000SASLE") %>% 
  mutate(chg = (ex_item_index/lag(ex_item_index, 3, na.pad = T))^4 - 1) %>% 
  ggplot(aes(date, chg)) +
  geom_line() +
  geom_hline(yintercept = 0)


ex_items_mm(c("CUSR0000SAS2RS"), "2018-01-01", all_items = "CUSR0000SASLE") %>% 
  arrange(desc(date))
ex_items_yy(c("CUUR0000SAS2RS"), "2018-01-01", all_items = "CUUR0000SASLE") %>% 
  arrange(desc(date))

# Core services ex-shelter compared to headline and core
# 
# p <- ex_items_yy(c("CUUR0000SAS2RS"), "2018-01-01", all_items = "CUUR0000SASLE") %>% 
#   mutate(change = ex_item/100) %>% 
#   select(date, change) %>% 
#   mutate(series_id = "Core services ex-shelter")
# 
# p <- cpi_y("CUUR0000SA0L1E", "Core") %>% 
#   filter(year >= 2019) %>% 
#   select(date, series_id, change) %>% 
#   bind_rows(p)
# 
# p <- p %>% 
#   mutate(point = case_when(date == max(date) ~ change)) %>% 
#   ggplot(aes(date, change, colour = series_id)) +
#   geom_line(size = 1, show.legend = F) +
#   cpi_theme() +
#   geom_dl(aes(label = series_id),
#           method = list(dl.trans(x = x + .3), "last.qp", cex = 1.5)) +
#   xlim(ymd("2019-01-01"), ymd("2024-01-01")) +
#   scale_color_manual(values = c(`All items` = "grey75", `Core` = "#a6cee3", `Core services ex-shelter` = "#1f78b4")) +
#   scale_y_continuous(labels = percent)  +
#   geom_hline(yintercept = 0) +
#   # geom_hline(yintercept = .02, linetype = "dashed") +
#   geom_point(aes(y = point, colour = series_id),
#              show.legend = F, size = 3) +
#   labs(x = NULL, y = NULL,
#        title = "Three measures of inflation",
#        subtitle = "Change from a year earlier, not seasonally adjusted",
#        caption = "Note: 'Core' excludes food and energy. 'Core services ex-shelter' excludes energy services and rent of shelter.\nSource: Bureau of Labor Statistics")
# 
# ggsave("cpi_charts/core_svces_ex_shelter_yy.png", p, device = "png", width = 14.2, height = 8)
# 
# 
# # 3m annualized
# p <- ex_items(c("CUSR0000SAS2RS"), "2018-01-01", all_items = "CUSR0000SASLE") %>% 
#   mutate(change = (ex_item_index/lag(ex_item_index, 3, na.pad = T))^4 - 1) %>% 
#   select(date, change) %>% 
#   mutate(series_id = "Core services ex-shelter")
# 
# p <- cpi_3m("CUSR0000SA0L1E", "Core") %>% 
#   filter(year >= 2019) %>% 
#   select(date, series_id, change) %>% 
#   bind_rows(p)
# 
# p <- p %>% 
#   mutate(point = case_when(date == max(date) ~ change)) %>% 
#   ggplot(aes(date, change, colour = series_id)) +
#   geom_line(size = 1, show.legend = F) +
#   cpi_theme() +
#   geom_dl(aes(label = series_id),
#           method = list(dl.trans(x = x + .3), "last.qp", cex = 1.5)) +
#   xlim(ymd("2019-01-01"), ymd("2024-01-01")) +
#   scale_color_manual(values = c(`All items` = "grey75", `Core` = "#a6cee3", `Core services ex-shelter` = "#1f78b4")) +
#   scale_y_continuous(labels = percent)  +
#   geom_hline(yintercept = 0) +
#   # geom_hline(yintercept = .02, linetype = "dashed") +
#   geom_point(aes(y = point, colour = series_id),
#              show.legend = F, size = 3) +
#   labs(x = NULL, y = NULL,
#        title = "Three measures of shorter-term inflation",
#        subtitle = "Annualized three-month change, seasonally adjusted",
#        caption = "Note: 'Core' excludes food and energy. 'Core services ex-shelter' excludes energy services and rent of shelter.\nSource: Bureau of Labor Statistics")
# 
# ggsave("cpi_charts/core_svces_ex_shelter_3m.png", p, device = "png", width = 14.2, height = 8)
# 

# HERE HERE HERE


p <- ex_items_new(c("CUUR0000SEHA", "CUUR0000SEHC"), "2018-01-01", all_items = "CUUR0000SASLE") %>% 
  mutate(change = (ex_item_index/lag(ex_item_index, 12, na.pad = T)) - 1) %>% 
  select(date, change) %>% 
  mutate(series_id = "Core services ex-shelter")

p <- cpi_y("CUUR0000SA0L1E", "Core") %>% 
  filter(year >= 2019) %>% 
  select(date, series_id, change) %>% 
  bind_rows(p)

p <- p %>% 
  mutate(point = case_when(date == max(date) ~ change)) %>% 
  ggplot(aes(date, change, colour = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .3), "last.qp", cex = 1.5)) +
  xlim(ymd("2019-01-01"), ymd("2024-08-01")) +
  scale_color_manual(values = c(`All items` = "grey75", `Core` = "#a6cee3", `Core services ex-shelter` = "#1f78b4")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three measures of inflation",
       subtitle = "Change from a year earlier, not seasonally adjusted",
       caption = "Note: 'Core' excludes food and energy. 'Core services ex-shelter' excludes energy services, rent of primary residence and OER.\nSource: Bureau of Labor Statistics")

ggsave("cpi_charts/core_svces_ex_shelter_yy.png", p, device = "png", width = 14.2, height = 8)


# 3m annualized
p <- ex_items_new(c("CUSR0000SEHA", "CUSR0000SEHC"), "2018-01-01", all_items = "CUSR0000SASLE") %>% 
  mutate(change = (ex_item_index/lag(ex_item_index, 3, na.pad = T))^4 - 1) %>% 
  select(date, change) %>% 
  mutate(series_id = "Core services ex-shelter")

p <- cpi_3m("CUSR0000SA0L1E", "Core") %>% 
  filter(year >= 2019) %>% 
  select(date, series_id, change) %>% 
  bind_rows(p)

p <- p %>% 
  mutate(point = case_when(date == max(date) ~ change)) %>% 
  ggplot(aes(date, change, colour = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .3), "last.qp", cex = 1.5)) +
  xlim(ymd("2019-01-01"), ymd("2024-08-01")) +
  scale_color_manual(values = c(`All items` = "grey75", `Core` = "#a6cee3", `Core services ex-shelter` = "#1f78b4")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Three measures of shorter-term inflation",
       subtitle = "Annualized three-month change, seasonally adjusted",
       caption = "Note: 'Core' excludes food and energy. 'Core services ex-shelter' excludes energy services, rent of primary residence and OER.\nSource: Bureau of Labor Statistics")

ggsave("cpi_charts/core_svces_ex_shelter_3m.png", p, device = "png", width = 14.2, height = 8)


  
  

# Goods: CUSR0000SAC
# Core goods:CUSR0000SACL1E
# Services: CUSR0000SAS
# Core services: CUSR0000SASLE
# Shelter: CUSR0000SAS2RS
# Core services ex-shelter: CUSR0000SASLE - CUSR0000SAS2RS

p <- ex_items(c("CUSR0000SAS2RS"), "2018-01-01", all_items = "CUSR0000SASLE") %>% 
  mutate(change = (ex_item_index/lag(ex_item_index, 1, na.pad = T)) - 1) %>% 
  select(date, change) 



p <- cpi_data %>% 
  filter(series_id %in% c("CUSR0000SAC", "CUSR0000SACL1E",
                          "CUSR0000SAS", "CUSR0000SASLE")) %>% 
  mutate(series = case_when(series_id %in% c("CUSR0000SAC", "CUSR0000SACL1E") ~ "Goods",
                            TRUE ~ "Services"),
         core = case_when(series_id %in% c("CUSR0000SAC", "CUSR0000SAS") ~ "Headline",
                          TRUE ~ "Core")) %>% 
  group_by(series, core) %>% 
  mutate(change = (value/lag(value, 1, na.pad = T))^12 -1) %>% 
  filter(year >= 2019)

p <- p %>% 
  filter(date >= ymd("2021-01-01")) %>% 
  ungroup() %>% 
  mutate(series = factor(series, levels = c("Goods", "Services")),
         core = factor(core, levels  = c("Headline", "Core"))) %>% 
  ggplot(aes(date, change, fill = series_id)) +
  geom_col(position = "dodge", show.legend = F) +
  facet_wrap(~core) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c(CUSR0000SAC = "#1f78b4", CUSR0000SACL1E = "#a6cee3",
                               CUSR0000SAS = "#33a02c", CUSR0000SASLE = "#b2df8a")) +
  scale_y_continuous(labels = percent) +
  cpi_theme() +
  labs(x = NULL, y = NULL,
       title = "Monthly inflation in goods and services",
       subtitle = "Change from prior month, seasonally adjusted",
       caption = "Note: 'Core' excludes food and energy. 'Core services ex-shelter' excludes energy services and rent of shelter.\nSource: Bureau of Labor Statistics")

ggsave("cpi_charts/good_vs_svces_bars.png", p, device = "png", width = 14.2, height = 8)


p <- ex_items(c("CUSR0000SAS2RS"), "2018-01-01", all_items = "CUSR0000SASLE") %>% 
  mutate(change = (ex_item_index/lag(ex_item_index, 1, na.pad = T))^12 - 1) %>% 
  select(date, change) %>% 
  mutate(series_id = "Core services ex-shelter")

p <- cpi_data %>% 
  filter(series_id %in% c("CUSR0000SACL1E", "CUSR0000SAS2RS")) %>% 
  mutate(series_id = factor(series_id, levels = c("CUSR0000SACL1E", "CUSR0000SAS2RS"),
                            labels = c("Core goods", "Shelter"))) %>% 
  group_by(series_id) %>% 
  mutate(change = (value/lag(value, 1, na.pad = T))^12 -1) %>% 
  filter(year >= 2019) %>% 
  select(date, series_id, change) %>% 
  bind_rows(p)

p %>% 
  filter(date >= ymd("2021-07-01")) %>% 
  ggplot(aes(date, change, fill = series_id)) +
  geom_col(position = "dodge")

# Core goods vs core services
p <- cpi_data %>% 
  filter(series_id %in% c("CUSR0000SACL1E", "CUSR0000SASLE")) %>% 
  mutate(series_id = factor(series_id,
                            levels = c("CUSR0000SACL1E", "CUSR0000SASLE"),
                            labels = c("Core goods", "Core services"))) %>% 
  group_by(series_id) %>% 
  mutate(chg = (value/lag(value, 3))^4 - 1,
         point = case_when(date == max(date) ~ chg)) %>% 
  ggplot(aes(date, chg, colour = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .3), "last.qp", cex = 1.5)) +
  xlim(ymd("2019-01-01"), ymd("2023-12-01")) +
  scale_color_manual(values = c(`Core services` = "#33a02c", `Core goods` = "#1f78b4")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Core goods vs core services inflation",
       subtitle = "Annualized three-month change, seasonally adjusted",
       caption = "Note: Note: Excludes food and energ. | Source: Bureau of Labor Statistics")

ggsave("cpi_charts/core_goods_vs_svces_3m.png", p, device = "png", width = 14.2, height = 8)


p <- cpi_data %>% 
  filter(series_id %in% c("CUUR0000SACL1E", "CUUR0000SASLE"),
         period != "M13") %>% 
  mutate(series_id = factor(series_id,
                            levels = c("CUUR0000SACL1E", "CUUR0000SASLE"),
                            labels = c("Core goods", "Core services"))) %>% 
  group_by(series_id) %>% 
  mutate(chg = value/lag(value, 12) - 1,
         point = case_when(date == max(date) ~ chg)) %>% 
  ggplot(aes(date, chg, colour = series_id)) +
  geom_line(size = 1, show.legend = F) +
  cpi_theme() +
  geom_dl(aes(label = series_id),
          method = list(dl.trans(x = x + .3), "last.qp", cex = 1.5)) +
  xlim(ymd("2019-01-01"), ymd("2023-12-01")) +
  scale_color_manual(values = c(`Core services` = "#33a02c", `Core goods` = "#1f78b4")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = .02, linetype = "dashed") +
  geom_point(aes(y = point, colour = series_id),
             show.legend = F, size = 3) +
  labs(x = NULL, y = NULL,
       title = "Core goods vs core services inflation",
       subtitle = "Change from a year earlier, not seasonally adjusted",
       caption = "Note: Note: Excludes food and energ. | Source: Bureau of Labor Statistics")

ggsave("cpi_charts/core_goods_vs_svces_yy.png", p, device = "png", width = 14.2, height = 8)




ex_items_mm("CUSR0000SAF1", startdate = "2022-06-01", all_items = "CUSR0000SAC")

test(c("CUSR0000SAF1", "CUSR0000SACE"), startdate = "2022-06-01", all_items = "CUSR0000SAC") %>% 
  mutate(chg = ex_item_index/lag(ex_item_index, 1, na.pad = T) - 1,
         chg = 100 * chg)

test(c("CUSR0000SAF1"), startdate = "2022-06-01", all_items = "CUSR0000SAC") %>% 
  mutate(chg = ex_item_index/lag(ex_item_index, 1, na.pad = T) - 1,
         chg = 100 * chg)

# Commod less food/energy/used
test(c("CUSR0000SAF1", "CUSR0000SACE", "CUSR0000SETA02"), startdate = "2022-06-01", all_items = "CUSR0000SAC") %>% 
  mutate(chg = ex_item_index/lag(ex_item_index, 1, na.pad = T) - 1,
         chg = 100 * chg)



test(c("CUSR0000SEHF", "CUSR0000SAS2RS"), startdate = "2022-06-01", all_items = "CUSR0000SAS") %>% 
  mutate(chg = ex_item_index/lag(ex_item_index, 1, na.pad = T) - 1,
         chg = 100 * chg)


test(c("CUSR0000SAS2RS"), startdate = "2022-06-01", all_items = "CUSR0000SASLE") %>% 
  mutate(chg = ex_item_index/lag(ex_item_index, 1, na.pad = T) - 1,
         chg = 100 * chg)

test(c("CUSR0000SEHF"), startdate = "2022-06-01", all_items = "CUSR0000SASL2RS") %>% 
  mutate(chg = ex_item_index/lag(ex_item_index, 1, na.pad = T) - 1,
         chg = 100 * chg)


item_contrib("CUUR0000SASLE", all_items = "CUUR0000SA0", startdate = "2018-02-01")



n <- cpi_data %>% 
  filter(series_id == "CUUR0000SA0",
         period != "M13") %>% 
  mutate(chg = (value/lag(value, 12, na.pad = T))^(1/12)) %>% 
  filter(date == ymd("2022-12-01")) %>% 
  .$chg

cpi_data %>% 
  filter(series_id == "CUUR0000SA0",
         date >= ymd("2021-12-01"),
         period != "M13") %>% 
  mutate(alt = case_when(year == 2022 ~ first(value) * (n^month(date)),
                         TRUE ~ value),
         chg = value/lag(value, 12, na.pad = T) - 1,
         ex_base = alt/lag(alt, 12, na.pad = T) - 1) %>% 
  # filter(year >= 2022) %>% select(date, value, alt, chg, ex_base)
  filter(year == 2023) %>% 
  select(date, chg, ex_base) %>% 
  gather(series, value, -date) %>% 
  ggplot(aes(date, value, colour = series)) +
  geom_line()
  

cpi_data %>% 
  filter(series_id == "CUSR0000SA0") %>%
  mutate(chg = value/lag(value, 1, na.pad = T) - 1) %>% 
  filter(year >= 2022) %>% 
  ggplot(aes(date, chg)) +
  geom_col()

cpi_data %>% 
  filter(series_id == "CUUR0000SA0",
         year %in% 2022:2023,
         period != "M13") %>% 
  mutate(month = month(date)) %>% 
  filter(month < 8) %>% 
  select(month, year, value) %>% 
  spread(year, value) %>% 
  mutate(chg = `2023`/`2022` - 1) %>% 
  ggplot(aes(month)) +
  geom_line(aes(y = `2022`)) +
  geom_line(aes(y = `2023`)) +
  geom_ribbon(
    aes(ymin = `2022`, ymax = `2023`),
    fill = "#a6cee3", alpha = 0.5)


cpi_data %>% 
  filter(series_id == "CUUR0000SA0",
         date >= ymd("2021-08-01"),
         period != "M13") %>% 
  mutate(yr = case_when(date <= ymd("2022-07-01") ~ "Last year",
                        TRUE ~ "This year"),
         month = as.character(month(date)),
         month = factor(month, levels = c("8", "9", "10", "11", "12", "1", "2", "3", "4", "5", "6", "7"))) %>% 
  select(month, yr, value) %>% 
  spread(yr, value) %>% 
  mutate(rownum = row_number(),
         chg = `This year`/`Last year` - 1) %>% 
  ggplot(aes(rownum)) +
  geom_line(aes(y = `Last year`)) +
  geom_line(aes(y = `This year`)) +
  geom_ribbon(
    aes(ymin = `Last year`, ymax = `This year`),
    fill = "#a6cee3", alpha = 0.5)


p <- cpi_data %>% 
  filter(series_id == "CUSR0000SA0",
         year %in% 2022:2023,
         period != "M13") %>% 
  mutate(month = month(date)) %>% 
  select(month, year, value) 

p <- tibble(month = 7:12,
            year = 2024) %>% 
  mutate(rownum = row_number(),
         value = cpi_data$value[cpi_data$date == ymd("2023-07-01") & cpi_data$series_id == "CUSR0000SA0"]*(1.002^(rownum-1))) %>% 
  select(-rownum) %>% 
  bind_rows(p)
  
p %>% 
  spread(year, value) %>% 
  mutate(type = case_when(month >= 8 ~ "proj",
                          TRUE ~ "actual")) %>% 
  ggplot(aes(month)) +
  geom_line(aes(y = `2022`)) +
  geom_line(aes(y = `2023`)) +
  geom_line(aes(y = `2024`), linetype = "dashed") +
  geom_ribbon(
    aes(ymin = `2022`, ymax = `2023`),
    fill = "#a6cee3", alpha = 0.5) +
  geom_ribbon(
    aes(ymin = `2022`, ymax = `2024`),
    fill = "#b2df8a", alpha = 0.5) 

cpi_y("CUUR0000SEFG01", "Fresh fish") %>% arrange(desc(date))
  