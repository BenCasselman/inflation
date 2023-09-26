median_cpi <- read_csv("https://www.clevelandfed.org/~/media/files/webcharts/mediancpi/mcpi_revised.csv?la=en",
                       skip = 1,
                       col_names = c("date", "monthly", "annualized")) %>% 
  mutate(series = "median",
         date = mdy(date))

median_cpi <- read_csv("https://www.clevelandfed.org/~/media/files/webcharts/mediancpi/trim_revised.csv?la=en",
                       skip = 1,
                       col_names = c("date", "monthly", "annualized")) %>% 
  mutate(series = "trimmed_mean",
         date = mdy(date)) %>% 
  bind_rows(median_cpi)

median_cpi <- median_cpi %>% 
  group_by(series) %>% 
  mutate(index = 100 * cumprod((1+monthly))/(1 + first(monthly)),
         annual = index/lag(index, 12, na.pad = T) - 1) 

w <- cpi_full_history %>% 
  filter(series_id %in% c("CUSR0000SA0", "CUSR0000SA0L1E")) %>% 
  mutate(series = factor(series_id, labels = c("headline", "core"))) %>% 
  group_by(series_id) %>% 
  mutate(monthly = value/lag(value, 1, na.pad = T) - 1,
         annualized = (1 + monthly)^12 -1,
         annualized = 100 * annualized) %>% 
  ungroup() %>% 
  select(date, series, index = value, monthly, annualized)

w <- cpi_full_history %>% 
  filter(series_id %in% c("CUUR0000SA0", "CUUR0000SA0L1E"),
         !is.na(date)) %>% 
  mutate(series = factor(series_id, labels = c("headline", "core"))) %>% 
  group_by(series_id) %>% 
  mutate(annual = value/lag(value, 12, na.pad = T) - 1) %>% 
  ungroup() %>% 
  select(date, series, annual) %>% 
  left_join(w, by = c("date", "series"))
  
median_cpi <- median_cpi %>% 
  bind_rows(w)

median_cpi %>% arrange(desc(date))

p <- median_cpi %>% 
  filter(series %in% c("headline", "core", "median"),
         date >= ymd("1990-01-01")) %>% 
  mutate(series = factor(series, levels = c("headline", "core", "median"),
                         labels = c("Headline", "Core", "Median"))) %>% 
  ggplot(aes(date, annual, colour = series)) +
  geom_line(size = 1, show.legend = F) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c(Headline = "#1f78b4", Core = "#a6cee3", Median = "#fb9a99")) +
  geom_hline(yintercept = 0)

p <- p +
  cpi_theme() +
  xlim(ymd("1990-01-01"), ymd("2027-01-01")) +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .3), "last.qp", cex = 1.5)) +
  labs(x = NULL, y = NULL,
       title = "Median CPI compared to official measures",
       subtitle = "Change from a year earlier. 'Core' excludes food and energy prices.",
       caption = "Source: Bureau of Labor Statistics, Cleveland Fed")

ggsave("cpi_charts/median_cpi.png", p, device = "png", width = 14.2, height = 8)


p <- median_cpi %>% 
  group_by(series) %>% 
  mutate(mo3 = (index/lag(index, 3, na.pad = T))^4 - 1) %>% 
  filter(series %in% c("headline", "core", "median"),
         date >= ymd("1990-01-01")) %>% 
  mutate(series = factor(series, levels = c("headline", "core", "median"),
                         labels = c("Headline", "Core", "Median"))) %>% 
  ggplot(aes(date, mo3, colour = series)) +
  geom_line(size = 1, show.legend = F) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c(Headline = "#1f78b4", Core = "#a6cee3", Median = "#fb9a99")) +
  geom_hline(yintercept = 0)

p <- p +
  cpi_theme() +
  xlim(ymd("1990-01-01"), ymd("2027-01-01")) +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .3), "last.qp", cex = 1.5)) +
  labs(x = NULL, y = NULL,
       title = "Median CPI compared to official measures",
       subtitle = "Three-month annualized change. 'Core' excludes food and energy prices.",
       caption = "Source: Bureau of Labor Statistics, Cleveland Fed")

ggsave("cpi_charts/median_cpi_3mo.png", p, device = "png", width = 14.2, height = 8)



p <- median_cpi %>% 
  filter(series == "median") %>% 
  mutate(mo3 = (index/lag(index, 3, na.pad = T))^4 - 1) %>% 
  filter(date >= ymd("2019-01-01")) %>% 
  ggplot(aes(date, annualized/100)) +
  geom_col(fill = "grey80") +
  geom_line(aes(y = mo3), size = 1.2, show.legend = F, colour = "#1f78b4") +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0)

p <- p +
  cpi_theme() +
  labs(x = NULL, y = NULL,
       title = "Cleveland Fed median CPI",
       subtitle = "Bars show annualized monthly change. Line shows annualized three-month change.",
       caption = "Source: Cleveland Fed")

ggsave("cpi_charts/median_cpi2.png", p, device = "png", width = 14.2, height = 8)


p <- median_cpi %>% 
  filter(series == "trimmed_mean") %>% 
  mutate(mo3 = (index/lag(index, 3, na.pad = T))^4 - 1) %>% 
  filter(date >= ymd("2019-01-01")) %>% 
  ggplot(aes(date, annualized/100)) +
  geom_col(fill = "grey80") +
  geom_line(aes(y = mo3), size = 1.2, show.legend = F, colour = "#1f78b4") +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0)

p <- p +
  cpi_theme() +
  labs(x = NULL, y = NULL,
       title = "Cleveland Fed trimmed mean CPI",
       subtitle = "Bars show annualized monthly change. Line shows annualized three-month change.",
       caption = "Source: Cleveland Fed")

ggsave("cpi_charts/mean_cpi2.png", p, device = "png", width = 14.2, height = 8)



median_components <- read_csv("https://www.clevelandfed.org/-/media/files/webcharts/mediancpi/mediancpi_component_table.csv?sc_lang=en&hash=8C06DBF0A4FD0CC60C5FB4350D43CBC9")
