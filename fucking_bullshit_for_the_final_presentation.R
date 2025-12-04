library(tidyverse)






df <- data.frame(c("0102 CIN 1-B",
                   "0103 CIN 1-C",
                   "0105 CIN 1-E",
                   "0107 CIN 1-G",
                   "0109 CIN 1-I",
                   "0111 CIN 1-K",
                   "0113 CIN 1-M",
                   "0204 CIN 2-D",
                   "0302 CIN 3-B",
                   "0303 CIN 3-C",
                   "0305 CIN 3-E",
                   "0401 CIN 4-A",
                   "0402 CIN 4-B",
                   "0404 CIN 4-D",
                   "0408 CIN 4-H",
                   "0505 CIN 5-E",
                   "0802 CIN 8-B",
                   "0902 CIN 9-B",
                   "1102 CIN 11-B",
                   "1401 CIN 14-A",
                   "1404 CIN 14-D",
                   "1405 CIN 14-E",
                   "1407 CIN 14-G",
                   "1408 CIN 14-H",
                   "1505 CIN 15-E",
                   "1507 CIN 15-G",
                   "1508 CIN 15-H",
                   "1509 CIN 15-I",
                   "1601 CIN 16-A",
                   "1702 CIN 17-B",
                   "2003 CIN 20-C",
                   "2004 CIN 20-D",
                   "2101 CIN 21-A",
                   "2103 CIN 21-C",
                   "2304 CIN 23-D",
                   "2309 CIN 23-I",
                   "2310 CIN 23-J",
                   "2314 CIN 23-N",
                   "2316 CIN 23-P",
                   "2318 CIN 23-R",
                   "2510 CIN 25-J",
                   "2602 CIN 26-B",
                   "2603 CIN 26-C",
                   "2606 CIN 26-F",
                   "2611 CIN 26-K",
                   "2612 CIN 26-L",
                   "2614 CIN 26-N",
                   "2615 CIN 26-O",
                   "0104 CIN 1-D",
                   "0108 CIN 1-H",
                   "0202 CIN 2-B",
                   "0206 CIN 2-F",
                   "0210 CIN 2-J",
                   "0211 CIN 2-K",
                   "0406 CIN 4-F",
                   "0407 CIN 4-G",
                   "0502 CIN 5-B",
                   "0508 CIN 5-H",
                   "0601 CIN 6-A",
                   "1001 CIN 10-A",
                   "1002 CIN 10-B",
                   "1101 CIN 11-A",
                   "1302 CIN 13-B",
                   "1303 CIN 13-C",
                   "1305 CIN 13-E",
                   "1406 CIN 14-F",
                   "1409 CIN 14-I",
                   "2105 CIN 21-E",
                   "2301 CIN 23-A",
                   "2302 CIN 23-B",
                   "2308 CIN 23-H",
                   "2311 CIN 23-K",
                   "2313 CIN 23-M",
                   "2315 CIN 23-O",
                   "2403 CIN 24-C",
                   "2405 CIN 24-E",
                   "2406 CIN 24-F",
                   "2408 CIN 24-H",
                   "2604 CIN 26-D",
                   "2610 CIN 26-J",
                   "2616 CIN 26-P",
                   "2618 CIN 26-R",
                   "0602 CIN 6-B",
                   "0604 CIN 6-D",
                   "0702 CIN 7-B",
                   "0710 CIN 7-J",
                   "0711 CIN 7-K",
                   "1003 CIN 10-C",
                   "1205 CIN 12-E",
                   "1301 CIN 13-A",
                   "1502 CIN 15-B",
                   "1701 CIN 17-A",
                   "1802 CIN 18-B",
                   "1904 CIN 19-D",
                   "2005 CIN 20-E",
                   "2102 CIN 21-B",
                   "2305 CIN 23-E",
                   "2306 CIN 23-F",
                   "2402 CIN 24-B",
                   "2501 CIN 25-A",
                   "2608 CIN 26-H",
                   "0201 CIN 2-A",
                   "0203 CIN 2-C",
                   "0208 CIN 2-H",
                   "0209 CIN 2-I",
                   "0304 CIN 3-D",
                   '0701 CIN 7-A',
                   "0705 CIN 7-E",
                   "0706 CIN 7-F",
                   "0708 CIN 7-H",
                   "0709 CIN 7-I",
                   "0804 CIN 8-D",
                   "0901 CIN 9-A",
                   "0903 CIN 9-C",
                   "1104 CIN 11-D",
                   "1201 CIN 12-A",
                   "1203 CIN 12-C",
                   "1204 CIN 12-D",
                   "1308 CIN 13-H",
                   "1402 CIN 14-B",
                   "1501 CIN 15-A",
                   "1504 CIN 15-D",
                   "1506 CIN 15-F",
                   "1510 CIN 15-J",
                   "1703 CIN 17-C",
                   "2201 CIN 22-A",
                   "2202 CIN 22-B",
                   "2203 CIN 22-C",
                   "2303 CIN 23-C",
                   "2307 CIN 23-G",
                   "2401 CIN 24-A",
                   "2512 CIN 25-L",
                   "2605 CIN 26-E",
                   "2617 CIN 26-Q",
                   "0301 CIN 3-A",
                   "0603 CIN 6-C",
                   "0703 CIN 7-C",
                   "0704 CIN 7-D",
                   "0801 CIN 8-A",
                   "0904 CIN 9-D",
                   "1103 CIN 11-C",
                   "1503 CIN 15-C",
                   "1801 CIN 18-A",
                   "2409 CIN 24-I",
                   "2507 CIN 25-G",
                   "0405 CIN 4-E",
                   "0707 CIN 7-G",
                   "1304 CIN 13-D",
                   "1306 CIN 13-F",
                   "1307 CIN 13-G",
                   "0403 CIN 4-C",
                   "1202 CIN 12-B"))
df_2 <- read_csv("data/Book1.csv")

total_votes <- read_csv("data/council_filtered.csv")
names(df)[1] <- "Precincts"
names(total_votes)[1] <- "Precincts"

df_3 <- inner_join(df_2, df, by = "Precincts")

df_3 <- left_join(df_3, total_votes, by = "Precincts")

df_4 <- df_3 %>% 
  mutate(james_percent = `R James` / `Total Votes`) %>% 
  mutate(approx_ballots_cast = `Total Votes` / 7.5) %>% 
  mutate(ballots_cast_percent_appox = approx_ballots_cast / Registered) %>%  
  filter(james_percent > .08)


df_5 <- df_3 %>% 
  mutate(james_percent = `R James` / `Total Votes`) %>% 
  mutate(approx_ballots_cast = `Total Votes` / 7.5) %>% 
  mutate(ballots_cast_percent_appox = approx_ballots_cast / Registered) %>% 
  filter(james_percent > .07)


df_6 <- df_3 %>% 
  mutate(james_percent = `R James` / `Total Votes`) %>% 
  mutate(approx_ballots_cast = `Total Votes` / 7.5) %>% 
  mutate(ballots_cast_percent_appox = approx_ballots_cast / Registered) %>%  
  filter(james_percent > .065)



mean(df_5$ballots_cast_percent_appox)


hist(df_5$ballots_cast_percent_appox)


















