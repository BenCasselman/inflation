# Avg price data

food_prices <- GET(url = 'https://download.bls.gov/pub/time.series/ap/ap.data.3.Food', 
           user_agent('ben.casselman@nytimes.com')) %>% 
  content(as = "text") %>% 
  read_tsv(skip = 1, col_names = c("series_id", "year", "period", "value", "footnote"),
           col_types = "cdcdc") %>% 
  mutate(date = ymd(paste0(year, period, "-01")))

p <- food_prices %>% 
  filter(series_id == "APU0000708111",
         year >= 2000) %>% 
  mutate(point = case_when(date == max(date) ~ value),
         label = case_when(date == max(date) ~ sprintf("$%.2f", value))) %>% 
  ggplot(aes(date, value)) +
  geom_line(colour = "#1f78b4", size = 1) +
  cpi_theme() +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .3), "last.points", cex = 1.5),
          colour = "#1f78b4") +
  scale_y_continuous(labels = dollar)  +
  geom_hline(yintercept = 0) +
  geom_point(aes(y = point),
             show.legend = F, size = 3,
             colour = "#1f78b4") +
  labs(x = NULL, y = NULL,
       title = "Egg prices",
       subtitle = "Avg. price in dollars of a dozen Grade A Large eggs",
       caption = "Source: Bureau of Labor Statistics") +
  xlim(ymd("2000-01-01"), ymd("2024-12-31"))

ggsave("cpi_charts/eggs.png", p, device = "png", width = 14.2, height = 8)



food_prices %>% 
  filter(series_id == "APU0000720222",
         year >= 2000) %>% 
  mutate(point = case_when(date == max(date) ~ value),
         label = case_when(date == max(date) ~ sprintf("$%.2f", value))) %>% 
  ggplot(aes(date, value)) +
  geom_line(colour = "#1f78b4", size = 1) +
  cpi_theme() +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .3), "last.points", cex = 1.5),
          colour = "#1f78b4") +
  scale_y_continuous(labels = dollar)  +
  geom_hline(yintercept = 0) +
  geom_point(aes(y = point),
             show.legend = F, size = 3,
             colour = "#1f78b4") +
  labs(x = NULL, y = NULL,
       title = "Vodka prices",
       subtitle = "Avg. price in dollars of a dozen Grade A Large eggs",
       caption = "Source: Bureau of Labor Statistics") +
  xlim(ymd("2000-01-01"), ymd("2024-12-31"))
