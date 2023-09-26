# HICP calculations
library(tidyverse)
library(lubridate)
library(scales)

hicp_oecd <- read_csv("PRICES_CPI_19102022150316809.csv") %>% 
  mutate(date = ymd(paste0(TIME, "-01")))

hicp_eurostat <- read_csv(unzip("prc_hicp_midx.zip", "prc_hicp_midx_1_Data.csv"))
hicp_eurostat <- hicp_eurostat %>% 
  mutate(date = ymd(paste0(TIME, "M01")),
         Value = as.numeric(Value))

hicp_uk <- read_csv("mm23.csv", skip = 1)
hicp_uk <- hicp_uk %>% 
  slice(6:nrow(hicp_uk)) %>% 
  gather(series, value, -CDID)

hicp_uk <- hicp_uk %>% 
  filter(!is.na(value)) %>% 
  mutate(value = as.numeric(value),
         period = case_when(nchar(CDID) == 4 ~ "A",
                            grepl("Q", CDID) ~ "Q",
                            TRUE ~ "M"),
         date = case_when(period == "A" ~ ymd(paste(CDID, "01", "01", sep = "-")),
                          period == "Q" ~ ymd(paste(CDID, substr(CDID, nchar(CDID), nchar(CDID)), "01", sep = "-")),
                          TRUE ~ ymd(paste(CDID, "01", sep = " "))))

%>% 
  # rename(series_code = `series_code$Title`)
  relocate(Title, series_code) %>% 
  pivot_longer(-c(Title, series_code), names_to = "series", values_to = "value")
  
  gather(series, value, -Title, -series_code)

p <- hicp_oecd %>% 
  filter(Subject == "HICP: All items",
         MEASURE == "GY",
         FREQUENCY == "M",
         LOCATION %in% c("GBR"),
         date >= ymd("2019-01-01")) %>% 
  select(date, LOCATION, Country, Value)

p <- hicp_eurostat %>% 
  arrange(date) %>% 
  filter(grepl("Euro area \\(EA11-1999, EA12-2001", GEO),
         COICOP == "All-items HICP") %>% 
  mutate(change = 100*(Value/lag(Value, 12, na.pad = T) - 1)) %>% 
  filter(date >= ymd("2019-01-01")) %>% 
  select(date, Value = change) %>% 
  mutate(LOCATION = "EA19", Country = "Euro Area") %>% 
  filter(!is.na(Value)) %>% 
  bind_rows(p)

p <- ex_items_yy("CUUR0000SEHC", startdate = "2018-01-01") %>% 
  select(date, Value = ex_item) %>% 
  mutate(LOCATION = "USA", Country = "United States") %>% 
  bind_rows(p)

p %>% 
  group_by(LOCATION) %>% 
  mutate(col = case_when(LOCATION == "USA" ~ "USA", 
                         LOCATION == "EA19" ~ "USA",
                         TRUE ~ "all_other"),
         point = case_when(date == max(date) ~ Value),
         label = case_when(date == max(date) ~ Country)) %>% 
  ggplot(aes(date, Value, group = Country, colour = col)) +
  geom_line(size = 1, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 5, na.rm = T, show.legend = F, size = 6) +
  scale_x_date(expand = expansion(mult = c(0, 0.25))) +
  # geom_dl(aes(label = Country),
  #         method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  geom_point(aes(y = point), size = 3, show.legend = F) +
  scale_colour_manual(values = c(USA = "#1f78b4", all_other = "#a6cee3")) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Inflation in the rich world",
       subtitle = "12-month change in consumer prices, selected countries",
       caption = "Note: European data is HICP. U.S. data is CPI excluding owners' equivalent rant. | Sources: Eurostat, OECD, BLS") +
  cpi_theme() 

p <- p %>% 
  group_by(LOCATION) %>% 
  mutate(col = case_when(LOCATION == "USA" ~ "USA", 
                         LOCATION == "EA19" ~ "USA",
                         TRUE ~ "all_other"),
         point = case_when(date == max(date) ~ Value),
         label = case_when(date == max(date) ~ Country)) %>% 
  ggplot(aes(date, Value, colour = Country)) +
  geom_line(size = 1, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 1, na.rm = T, show.legend = F, size = 6) +
  scale_x_date(expand = expansion(mult = c(0, 0.25))) +
  # geom_dl(aes(label = Country),
  #         method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  geom_point(aes(y = point), size = 3, show.legend = F) +
  scale_colour_manual(values = c(`Euro Area` = "#1f78b4", `United Kingdom` = "#33a02c", `United States` = "#fb9a99")) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Inflation in Europe vs. the U.S.",
       subtitle = "12-month change in consumer prices, using comparable price indexes",
       caption = "Note: European data is HICP. U.S. data is CPI excluding owners' equivalent rent. U.K. data available through August. | Sources: Eurostat, OECD, BLS") +
  cpi_theme() 

ggsave("cpi_charts/hicp.png", p, device = "png", width = 14.2, height = 8)


# Core HICP

p <- hicp_oecd %>% 
  filter(SUBJECT == "CPHPLA01",
         MEASURE == "GY",
         FREQUENCY == "M",
         LOCATION %in% c("GBR"),
         date >= ymd("2019-01-01")) %>% 
  select(date, LOCATION, Country, Value)

p <- hicp_eurostat %>% 
  arrange(date) %>% 
  filter(grepl("Euro area \\(EA11-1999, EA12-2001", GEO),
         COICOP == "Overall index excluding energy, food, alcohol and tobacco") %>%   
  mutate(change = 100*(Value/lag(Value, 12, na.pad = T) - 1)) %>% 
  filter(date >= ymd("2019-01-01")) %>% 
  select(date, Value = change) %>% 
  mutate(LOCATION = "EA19", Country = "Euro Area") %>% 
  filter(!is.na(Value)) %>% 
  bind_rows(p)

p <- ex_items_yy("CUUR0000SEHC", startdate = "2018-01-01", all_items = "CUUR0000SA0L1E") %>% 
  select(date, Value = ex_item) %>% 
  mutate(LOCATION = "USA", Country = "United States") %>% 
  bind_rows(p)


p <- p %>% 
  group_by(LOCATION) %>% 
  mutate(col = case_when(LOCATION == "USA" ~ "USA", 
                         LOCATION == "EA19" ~ "USA",
                         TRUE ~ "all_other"),
         point = case_when(date == max(date) ~ Value),
         label = case_when(date == max(date) ~ Country)) %>% 
  ggplot(aes(date, Value, colour = Country)) +
  geom_line(size = 1, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 1, na.rm = T, show.legend = F, size = 6) +
  scale_x_date(expand = expansion(mult = c(0, 0.25))) +
  # geom_dl(aes(label = Country),
  #         method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  geom_point(aes(y = point), size = 3, show.legend = F) +
  scale_colour_manual(values = c(`Euro Area` = "#1f78b4", `United Kingdom` = "#33a02c", `United States` = "#fb9a99")) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Core inflation in Europe vs. the U.S.",
       subtitle = "12-month change in core consumer prices, using comparable price indexes",
       caption = "Note: European data is HICP ex. food, energy, alcohol and tobacco. U.S. data is CPI excluding owners' equivalent rent, food and energy.\n| Sources: Eurostat, OECD, BLS") +
  cpi_theme() 

ggsave("cpi_charts/core_hicp.png", p, device = "png", width = 14.2, height = 8)


"CUUR0000SA0E","CUUR0000SAF1"
SAF116
SEGA
p %>% 
  group_by(LOCATION) %>% 
  mutate(col = case_when(LOCATION == "USA" ~ "USA", 
                         LOCATION == "EA19" ~ "USA",
                         TRUE ~ "all_other"),
         point = case_when(date == max(date) ~ Value),
         label = case_when(date == max(date) ~ Country)) %>% 
  ggplot(aes(date, Value, group = Country, colour = col)) +
  geom_line(size = 1, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 5, na.rm = T, show.legend = F, size = 6) +
  scale_x_date(expand = expansion(mult = c(0, 0.25))) +
  # geom_dl(aes(label = Country),
  #         method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  geom_point(aes(y = point), size = 3, show.legend = F) +
  scale_colour_manual(values = c(USA = "#1f78b4", all_other = "#a6cee3")) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Inflation in the rich world",
       subtitle = "12-month change in consumer prices, selected countries",
       caption = "Note: European data is HICP. U.S. data is CPI excluding owners' equivalent rant. | Sources: Eurostat, OECD, BLS") +
  cpi_theme() 



p <- ex_items_yy("CUUR0000SEHC", startdate = "2018-01-01") %>% 
  select(date, Value = ex_item) %>% 
  mutate(LOCATION = "USA", Country = "United States") %>% 
  bind_rows(p)