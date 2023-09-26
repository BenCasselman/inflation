# International comparison
library(tidyverse)
library(lubridate)
library(scales)

oecd <- read_csv("PRICES_CPI_11052022213405641.csv")
oecd <- oecd %>% 
  mutate(date = ymd(paste0(TIME, "-01")))


oecd %>% 
  filter(MEASURE == "IXNB",
         SUBJECT == "CPALTT01",
         date >= ymd("2020-01-01"),
         LOCATION != "TUR",
         FREQUENCY == "M") %>% 
  group_by(LOCATION) %>% 
  mutate(change = Value/first(Value) - 1,
         col = case_when(LOCATION == "USA" ~ "USA", 
                         TRUE ~ "all_other")) %>% 
  ggplot(aes(date, change, colour = col, group = LOCATION)) +
  geom_line(size = 1) +
  scale_colour_manual(values = c(USA = "#1f78b4", all_other = "grey75"))

oecd %>% 
  filter(MEASURE == "IXNB",
         SUBJECT == "CPALTT01",
         date >= ymd("2020-01-01"),
         LOCATION %in% c("USA", "CAN", "EA19", "JPN", "GBR")) %>% 
  group_by(LOCATION) %>% 
  mutate(change = Value/first(Value) - 1,
         col = case_when(LOCATION == "USA" ~ "USA", 
                         TRUE ~ "all_other")) %>% 
  ggplot(aes(date, change, colour = LOCATION)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0)



p <- oecd %>% 
  filter(MEASURE == "IXNB",
         SUBJECT == "CPALTT01",
         FREQUENCY == "M",
         LOCATION %in% c("USA", "CAN", "EA19", "JPN", "GBR", "KOR")) %>% 
  group_by(Country) %>% 
  mutate(change = Value/first(Value) - 1,
         col = case_when(LOCATION == "USA" ~ "USA", 
                         LOCATION == "EA19" ~ "USA",
                         TRUE ~ "all_other"),
         point = case_when(date == max(date) ~ change),
         label = case_when(date == max(date) ~ Country)) 

# p <- p %>% 
#   ggplot(aes(date, change, group = Country, colour = col)) +
#   geom_line(size = 1, show.legend = F) +
#   geom_dl(aes(label = Country),
#           method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
#   geom_point(aes(y = point), size = 3, show.legend = F) +
#   stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
#               method = "lm", 
#               fullrange = TRUE,
#               linetype = "dashed",
#               se = FALSE,
#               show.legend = F) +
#   scale_colour_manual(values = c(USA = "#1f78b4", all_other = "#a6cee3")) +
#   scale_y_continuous(labels = percent)  +
#   geom_hline(yintercept = 0) +
#   labs(x = NULL, y = NULL,
#        title = "Inflation in the rich world",
#        subtitle = "Change in consumer prices since Jan. 2015 in selected countries. Dashed lines are prepandemic trends.",
#        caption = "Source: OECD") +
#   cpi_theme() +
#   xlim(ymd("2015-01-01"), ymd("2023-12-01"))

p <- p %>% 
  ggplot(aes(date, change, group = Country, colour = col)) +
  geom_line(size = 1, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 5, na.rm = T, show.legend = F, size = 6) +
  scale_x_date(expand = expansion(mult = c(0, 0.25))) +
  # geom_dl(aes(label = Country),
  #         method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  geom_point(aes(y = point), size = 3, show.legend = F) +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "lm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE,
              show.legend = F) +
  scale_colour_manual(values = c(USA = "#1f78b4", all_other = "#a6cee3")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Inflation in the rich world",
       subtitle = "Change in consumer prices since Jan. 2015 in selected countries. Dashed lines are prepandemic trends.",
       caption = "Note: Data through Feb. 2022. | Source: OECD") +
  cpi_theme() 

ggsave("cpi_charts/oecd.png", p, device = "png", width = 14.2, height = 8)


p %>% 
  ggplot(aes(date, change, colour = Country)) +
  geom_line(size = 1) +
  geom_dl(aes(label = Country),
          method = list(dl.trans(x = x + .5), "last.points", cex = 1.5)) +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "lm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE)


+
  scale_colour_manual(values = c(USA = "#1f78b4", all_other = "grey75"))



cpi_item %>% 
  left_join(cpi_wts %>% filter(date == ymd("2021-12-01")), by = "item_code") %>% 
  mutate(grp = substr(item_code, 1, 3)) %>% 
  group_by(grp) %>% 
  filter(!is.na(wt),
         display_level == max(display_level),
         grp != "SA0") %>% 
  ungroup() %>% 
  summarize(sum(wt))


oecd_ppi <- read_csv("MEI_PRICES_PPI_13042022163936681.csv")
oecd <- oecd %>% 
  mutate(date = ymd(paste0(TIME, "-01")))

hicp <- read_csv("PRICES_CPI_21042022223654126.csv") %>% 
  mutate(date = ymd(paste0(TIME, "-01")))


oecd_emp <- read_csv("STLABOUR_21042022192135765.csv") 
oecd_emp <- oecd_emp %>% 
  mutate(date = as.numeric(substr(Time, 2, 2)),
         date = date * 3 - 2,
         date = ymd(paste(substr(TIME, 1, 4), date, 1)))


oecd_emp %>% 
  count(Subject)


oecd_emp %>% 
  filter(SUBJECT == "LREM64TT",
         FREQUENCY == "Q",
         LOCATION %in% c("USA", "CAN", "EA19", "JPN", "GBR"),
         date >= ymd("2019-10-01")) %>% 
  group_by(Country) %>% 
  mutate(change = Value/first(Value) - 1,
         col = case_when(LOCATION == "USA" ~ "USA", 
                         LOCATION == "EA19" ~ "USA",
                         TRUE ~ "all_other"),
         point = case_when(date == max(date) ~ change),
         label = case_when(date == max(date) ~ Country)) %>% 
  ggplot(aes(date, change, group = Country, colour = col)) +
  geom_line(size = 1, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 5, na.rm = T, show.legend = F, size = 6) +
  scale_x_date(expand = expansion(mult = c(0, 0.25))) +
  # geom_dl(aes(label = Country),
  #         method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  geom_point(aes(y = point), size = 3, show.legend = F) +
  scale_colour_manual(values = c(USA = "#1f78b4", all_other = "#a6cee3")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Employment rates in the rich world",
       subtitle = "Change in employment rates since Q4 2019 in selected countries.",
       caption = "Note: Quarterly data. | Source: OECD") +
  cpi_theme() 


oecd_emp %>% 
  filter(SUBJECT == "LREM64TT",
         FREQUENCY == "Q",
         LOCATION %in% c("USA", "CAN", "EA19", "JPN", "GBR"),
         date >= ymd("2019-10-01")) %>% 
  group_by(Country) %>% 
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
  labs(x = NULL, y = NULL,
       title = "Employment rates in the rich world",
       subtitle = "Employment rates since Q4 2019 in selected countries.",
       caption = "Note: Quarterly data. | Source: OECD") +
  cpi_theme() 

oecd_emp %>% 
  filter(SUBJECT == "LREM64TT",
         FREQUENCY == "Q",
         LOCATION %in% c("USA", "CAN", "EA19", "JPN", "GBR"),
         date >= ymd("2019-10-01")) %>% 
  group_by(Country) %>% 
  mutate(change = 100*(Value/first(Value) - 1)) %>% 
  select(Country, date, change) %>% 
  spread(Country, change) %>% 
  write_csv("dw_epop.csv")

hicp %>% 
  filter(Subject == "HICP: All items",
         MEASURE == "GY",
         FREQUENCY == "M",
         LOCATION %in% c("USA", "CAN", "EA19", "JPN", "GBR"),
         date >= ymd("2019-01-01")) %>% 
  group_by(Country) %>% 
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
       caption = "Note: Quarterly data. | Source: OECD") +
  cpi_theme() 



test <- ex_items_yy("CUUR0000SEHC", startdate = "2018-01-01") %>% 
  select(date, Value = ex_item) %>% 
  mutate(source = "CPI ex-OER")

test <- ex_items_yy("CUUR0000SAH1", startdate = "2018-01-01") %>% 
  select(date, Value = ex_item) %>% 
  mutate(source = "CPI ex-shelter") %>% 
  bind_rows(test)

test <- hicp %>% 
  filter(Subject == "HICP: All items",
         MEASURE == "GY",
         FREQUENCY == "M",
         LOCATION %in% c("USA"),
         date >= ymd("2018-10-01")) %>% 
  select(date, Value) %>% 
  mutate(source = "HICP") %>% 
  bind_rows(test)

test %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(date, Value, colour = source)) +
  geom_line(size = 1) +
  labs(title = "Year-over-year change in consumer prices")




# hicp_eurostat <- read_tsv("prc_hicp_midx.tsv.gz")
# hicp_eurostat <- hicp_eurostat %>% 
#   gather(date, value, -`unit,coicop,geo\\time`)

hicp_eurostat <- read_csv(unzip("prc_hicp_midx.zip", "prc_hicp_midx_1_Data.csv"))
hicp_eurostat <- hicp_eurostat %>% 
  mutate(date = ymd(paste0(TIME, "M01")))

hicp_eurostat %>% 
  filter(date >= ymd("2020-01-01"),
         GEO %in% c("Euro area - 19 countries  (from 2015)",
                    "United Kingdom", "United States")) %>% 
  group_by(GEO) %>% 
  mutate(change = Value/first(Value) - 1,
         point = case_when(date == max(date) ~ change),
         label = case_when(date == max(date) ~ GEO)) %>% 
  ggplot(aes(date, change, group = GEO, colour = GEO)) +
  geom_line(size = 1, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 5, na.rm = T, show.legend = F, size = 6) +
  scale_x_date(expand = expansion(mult = c(0, 0.25))) +
  # geom_dl(aes(label = GEO),
  #         method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  geom_point(aes(y = point), size = 3, show.legend = F) +
  # stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
  #             method = "lm", 
  #             fullrange = TRUE,
  #             linetype = "dashed",
  #             se = FALSE,
  #             show.legend = F) +
  # scale_colour_manual(values = c(USA = "#1f78b4", all_other = "#a6cee3")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Inflation in the rich world",
       subtitle = "Change in consumer prices since Jan. 2015 in selected countries. Dashed lines are prepandemic trends.",
       caption = "Note: Data through Feb. 2022. | Source: OECD") +
  cpi_theme() 



p <- hicp %>% 
  filter(Subject == "HICP: All items",
         MEASURE == "GY",
         FREQUENCY == "M",
         LOCATION %in% c("GBR"),
         date >= ymd("2019-01-01")) %>% 
  select(date, LOCATION, Country, Value)

p <- hicp_eurostat %>% 
  filter(GEO == "Euro area - 19 countries  (from 2015)") %>% 
  mutate(change = 100*(Value/lag(Value, 12, na.pad = T) - 1)) %>% 
  filter(date >= ymd("2019-01-01")) %>% 
  select(date, Value = change) %>% 
  mutate(LOCATION = "EA19", Country = "Euro Area") %>% 
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

p %>% 
  filter(year(date) >= 2020) %>% 
  select(-LOCATION) %>% 
  spread(Country, Value) %>% 
  write_csv("dw_inflation.csv")


hicp_eurostat %>% 
  group_by(GEO) %>% 
  mutate(change = 100*(Value/lag(Value, 12, na.pad = T) - 1)) %>% 
  filter(date == ymd("2022-03-01")) %>% 
  arrange(desc(change)) %>% 
  View("hicp")

oecd_gdp <- read_csv("QNA_22042022013715999.csv")
oecd_gdp <- oecd_gdp %>% 
  mutate(date = as.numeric(substr(Period, 2, 2)),
         date = date * 3 - 2,
         date = ymd(paste(substr(TIME, 1, 4), date, 1)))

oecd_gdp %>% 
  count(MEASURE)

oecd_gdp %>% 
  filter(MEASURE == "LNBQRSA",
         FREQUENCY == "Q",
         LOCATION %in% c("USA", "CAN", "EA19", "JPN", "GBR"),
         date >= ymd("2019-10-01")) %>% 
  group_by(Country) %>% 
  mutate(change = Value/first(Value) - 1,
         col = case_when(LOCATION == "USA" ~ "USA", 
                         LOCATION == "EA19" ~ "USA",
                         TRUE ~ "all_other"),
         point = case_when(date == max(date) ~ change),
         label = case_when(date == max(date) ~ Country)) %>% 
  ggplot(aes(date, change, group = Country, colour = col)) +
  geom_line(size = 1, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 5, na.rm = T, show.legend = F, size = 6) +
  scale_x_date(expand = expansion(mult = c(0, 0.25))) +
  # geom_dl(aes(label = Country),
  #         method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  geom_point(aes(y = point), size = 3, show.legend = F) +
  scale_colour_manual(values = c(USA = "#1f78b4", all_other = "#a6cee3")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "GDP recovery in the rich world",
       subtitle = "Change in inflation-adjusted GDP since Q4 2019 in selected countries.",
       caption = "Note: Quarterly data. | Source: OECD") +
  cpi_theme() 


oecd_pce <- read_csv("HH_DASH_22042022144544176.csv")
oecd_pce <- oecd_pce %>% 
  mutate(date = as.numeric(substr(Time, 2, 2)),
         date = date * 3 - 2,
         date = ymd(paste(substr(TIME, 1, 4), date, 1)))


oecd_pce %>% 
  filter(Indicator == "Real household final consumption expenditure per capita index",
         FREQUENCY == "Q",
         LOCATION %in% c("USA", "CAN", "EMU", "JPN", "GBR"),
         date >= ymd("2019-10-01")) %>% 
  group_by(Country) %>% 
  mutate(change = Value/first(Value) - 1,
         col = case_when(LOCATION == "USA" ~ "USA", 
                         LOCATION == "EMU" ~ "USA",
                         TRUE ~ "all_other"),
         point = case_when(date == max(date) ~ change),
         label = case_when(date == max(date) ~ Country)) %>% 
  ggplot(aes(date, change, group = Country, colour = col)) +
  geom_line(size = 1, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 5, na.rm = T, show.legend = F, size = 6) +
  scale_x_date(expand = expansion(mult = c(0, 0.25))) +
  # geom_dl(aes(label = Country),
  #         method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  geom_point(aes(y = point), size = 3, show.legend = F) +
  scale_colour_manual(values = c(USA = "#1f78b4", all_other = "#a6cee3")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Household spending recovery in the rich world",
       subtitle = "Change in inflation-adjusted consumer spending per capita since Q4 2019 in selected countries.",
       caption = "Note: Quarterly data. | Source: OECD") +
  cpi_theme() 


oecd_pce %>% 
  filter(Indicator == "Real household final consumption expenditure per capita index",
         FREQUENCY == "Q",
         LOCATION %in% c("USA", "CAN", "EMU", "JPN", "GBR"),
         date >= ymd("2019-10-01")) %>% 
  group_by(Country) %>% 
  mutate(change = 100*(Value/first(Value) - 1)) %>% 
  select(date, Country, change) %>% 
  spread(Country, change) %>% 
  write_csv("dw_spending.csv")


oecd_unemp <- read_csv("DP_LIVE_22042022200133919.csv")

oecd_unemp <- oecd_unemp %>% 
  mutate(date = ymd(paste0(TIME, "-01")))

oecd_unemp %>% 
  filter(date >= ymd("2019-12-01")) %>% 
  group_by(LOCATION) %>% 
  mutate(change = Value - first(Value)) %>% 
  arrange(desc(date))


oecd_emp %>% 
  filter(SUBJECT == "LREM64TT",
         FREQUENCY == "Q",
         LOCATION %in% c("USA"),
         date >= ymd("2019-10-01")) %>% 
  arrange(desc(date))


oecd_lfs <- read_csv("STLABOUR_25042022151015514.csv")

oecd_lfs <- oecd_lfs %>% 
  mutate(date = as.numeric(substr(Time, 2, 2)),
         date = date * 3 - 2,
         date = ymd(paste(substr(TIME, 1, 4), date, 1)))


oecd_lfs %>% 
  count(Subject)


p <- oecd_lfs %>% 
  filter(Subject == "Activity rate, Aged 15 and over, All persons",
         FREQUENCY == "Q",
         LOCATION %in% c("USA", "CAN", "EA19", "JPN", "GBR"),
         date >= ymd("2019-10-01")) %>% 
  group_by(Country) %>% 
  mutate(change = Value/first(Value) - 1,
         col = case_when(LOCATION == "USA" ~ "USA", 
                         LOCATION == "EA19" ~ "USA",
                         TRUE ~ "all_other"),
         point = case_when(date == max(date) ~ change),
         label = case_when(date == max(date) ~ Country)) %>% 
  ggplot(aes(date, change, group = Country, colour = col)) +
  geom_line(size = 1, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 5, na.rm = T, show.legend = F, size = 6) +
  scale_x_date(expand = expansion(mult = c(0, 0.25))) +
  # geom_dl(aes(label = Country),
  #         method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  geom_point(aes(y = point), size = 3, show.legend = F) +
  scale_colour_manual(values = c(USA = "#1f78b4", all_other = "#a6cee3")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Labor force recovery in the rich world",
       subtitle = "Change in labor force participation rate, ages 15+, since Q4 2019 in selected countries.",
       caption = "Note: Quarterly data. | Source: OECD") +
  cpi_theme() 


ggsave("cpi_charts/oecd_lfp.png", p, device = "png", width = 14.2, height = 8)




r_oecd_emp_us <- oecd_emp %>% 
  filter(SUBJECT == "LREM64TT",
         FREQUENCY == "Q",
         LOCATION %in% c("USA"),
         date <= ymd("2019-10-01")) %>% 
  lm(Value ~ date, data = .)

r_oecd_emp_ea <- oecd_emp %>% 
  filter(SUBJECT == "LREM64TT",
         FREQUENCY == "Q",
         LOCATION %in% c("EA19"),
         date <= ymd("2019-10-01")) %>% 
  lm(Value ~ date, data = .)


p <- oecd_emp %>% 
  filter(SUBJECT == "LREM64TT",
         FREQUENCY == "Q",
         LOCATION %in% c("USA", "EA19"),
         date >= ymd("2019-10-01")
         ) %>% 
  group_by(Country) %>% 
  mutate(trend = case_when(LOCATION == "USA" ~ map_dbl(date, ~predict(r_oecd_emp_us, newdata = tibble(date = .x))),
                           LOCATION == "EA19" ~ map_dbl(date, ~predict(r_oecd_emp_ea, newdata = tibble(date = .x)))),
         dev = Value - trend, 
         point = case_when(date == max(date) ~ dev),
         label = case_when(date == max(date) ~ Country)) %>% 
  ggplot(aes(date, dev, group = Country, colour = LOCATION)) +
  geom_line(size = 1, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = label), nudge_x = 5, na.rm = T, show.legend = F, size = 6) +
  scale_x_date(expand = expansion(mult = c(0, 0.25))) +
  # geom_dl(aes(label = Country),
  #         method = list(dl.trans(x = x + .2), "last.points", cex = 1.5)) +
  geom_point(aes(y = point), size = 3, show.legend = F) +
  scale_colour_manual(values = c(USA = "#1f78b4", EA19 = "#33a02c")) +
  scale_y_continuous(labels = percent)  +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Employment rates vs trend, U.S. vs Euro area",
       subtitle = "Change in employment rates vs prepandemic trend.",
       caption = "Note: Quarterly data. Trend based on 2015-2019 data. | Source: OECD") +
  cpi_theme() 

ggsave("cpi_charts/oecd_epop_trend.png", p, device = "png", width = 14.2, height = 8)



p <- earnings %>% 
  filter(date >= ymd("2019-01-01"),
         industry %in% c("prodsup", "leis"),
         adj == "real") %>% 
  group_by(industry) %>% 
  mutate(chg = value/first(value) - 1) %>% 
  mutate(trend = case_when(industry == "prodsup" ~ map_dbl(date, ~predict(r_all, newdata = tibble(date = .x))),
                           industry == "leis" ~ map_dbl(date, ~predict(r_leis, newdata = tibble(date = .x))))) %>% 
  ungroup() %>% 
  mutate(industry = factor(industry, levels = c("prodsup", "leis"), labels = c("Total private", "Leisure & \nhospitality"))) %>% 
  ggplot(aes(date, chg, colour = industry)) +
  geom_line(size = 1)  +
  geom_line(aes(y = trend, colour = industry), linetype = "dashed", size = 1,
            show.legend = F)
