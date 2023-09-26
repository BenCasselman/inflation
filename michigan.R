temp <- tempfile(fileext = ".xls")
download.file("https://data.sca.isr.umich.edu/get-table.php?c=RB&y=2021&m=4&n=37&f=xls&k=90f631a9dc260fd9b753f89c52986a54",
              temp, mode = "wb")

autos <- readxl::read_xls(temp, skip = 8, col_names = c("month", "year", "good_time", 
                                                        "uncertain", "bad_time", "total",
                                                        "relative", "cases")) 

autos %>% 
  mutate(date = ymd(paste(year, month, "01"))) %>% 
  ggplot(aes(date, bad_time)) +
  geom_line()
