library(tidyverse)
library(sf)
library(readxl)
library(RColorBrewer)

map2020 <- st_zm(st_read("data/maps/PRECINCT_052219.shp"))

results2020 <- read_excel("data/election results/G20_Official_Canvass.xlsx", 
                          sheet = "Candidates",skip=1)

mapANDresults2020 <-
  left_join(map2020, results2020, by = c("PRECINCT" = "PRECINCT"))

mapANDresults2020 %>% 
  mutate(Biden.prop = `Biden & Harris  (Dem)`/( `Biden & Harris  (Dem)`+ `Trump & Pence       (Rep)`)) %>%
  mutate(Biden.baseswing = cut(Biden.prop, breaks = c(-0.001, 0.15,.40, 0.60, 0.85,1),labels = c("Very Residual","Residual", "Swing", "Base","Very Base")))%>%
  ggplot(aes(fill=Biden.baseswing)) +
  geom_sf()+
  labs(title = "2020 Presidential Election", 
       subtitle = "Joe Biden vs Donald Trump",
       fill = "Vote for \nBiden (%)", 
       caption = "")+
  scale_fill_manual(
    values = c(
      "Very Residual" = "darkred",
      "Residual" = "red",
      "Base" = "blue",
      "Very Base" = "darkblue",
      
      "Swing" = "yellow"
    )
  )



#### THE 2023 ONE!!!!

results2023 <- read_excel("data/election results/G23_Official_Canvass.xlsx", 
                          sheet = "Judicial",skip=2)
mapANDresults2023 <-
  left_join(acs_interp_judicial, results2023, by = c("PRECINCT" = "PRECINCT"))

mapANDresults2023 %>% 
  mutate(Dem.prop = `Samantha Silverstein`/( `Samantha Silverstein`+ `Curt           Kissinger`)) %>%
  mutate(Dem.baseswing = cut(Dem.prop, breaks = c(-0.001, 0.2,.40,.45,.55, 0.60, 0.8,1),labels = c("Very Residual","Residual","Leaning Residual", "Swing", "Leaning Base", "Base","Very Base")))%>%
  ggplot(aes(fill=Dem.baseswing)) +
  geom_sf()+
  labs(title = "2023 Judicial Election 4", 
       subtitle = "Samantha Silverstein vs Curt Kissinger",
       fill = "Base/Swing for \nSilverstein", 
       caption = "")+
  scale_fill_manual(
    values = c(
      "Very Residual" = "darkred",
      "Residual" = "red",
      "Leaning Residual" = "pink",
      "Leaning Base" = "lightblue",
      "Base" = "blue",
      "Very Base" = "darkblue",
      "Swing" = "yellow"
    )
  )+
  theme_void()

mapANDresults2023 %>% 
  mutate(Dem.prop = `Samantha Silverstein`/( `Samantha Silverstein`+ `Curt           Kissinger`)) %>%
  mutate(Dem.baseswing = cut(Dem.prop, breaks = c(-0.001, 0.2,.40, 0.60, 0.8,1),labels = c("Very Residual","Residual", "Swing", "Base","Very Base")))%>%
  ggplot(aes(fill=Dem.baseswing)) +
  geom_sf()+
  labs(title = "2023 Judicial Election 4", 
       subtitle = "Samantha Silverstein vs Curt Kissinger",
       fill = "Base/Swing for \nSilverstein", 
       caption = "")+
  scale_fill_manual(
    values = c(
      "Very Residual" = "darkred",
      "Residual" = "red",
      "Leaning Residual" = "pink",
      "Leaning Base" = "lightblue",
      "Base" = "blue",
      "Very Base" = "darkblue",
      "Swing" = "yellow"
    )
  )+
  theme_void()
#### THE 2019 ONE


results2019 <- read_excel("data/election results/G19_Official_Canvass.xlsx", 
                          sheet = "Judicial",skip=2)
mapANDresults2019 <-
  left_join(acs_interp_judicial, results2019, by = c("PRECINCT" = "PRECINCT")) 

mapANDresults2019 %>% 
  mutate(Dem.prop = `John Kennedy`/( `John Kennedy`+ `Josh Berkowitz`)) %>%
  mutate(Dem.baseswing = cut(Dem.prop, breaks = c(-0.001, 0.2,.40,.45,.55, 0.60, 0.8,1),labels = c("Very Residual","Residual","Leaning Residual", "Swing", "Leaning Base", "Base","Very Base")))%>%
  ggplot(aes(fill=Dem.baseswing)) +
  geom_sf()+
  labs(title = "2019 Judicial Election 4", 
       subtitle = "John Kennedy vs Josh Berkowitz!",
       fill = "Base/Swing for \nKennedy", 
       caption = "")+
  scale_fill_manual(
    values = c(
      "Very Residual" = "darkred",
      "Residual" = "red",
      "Leaning Residual" = "pink",
      "Leaning Base" = "lightblue",
      "Base" = "blue",
      "Very Base" = "darkblue",
      "Swing" = "yellow"
    )
  ) +
  theme_void()
  

mapANDresults2019 %>% 
  mutate(Dem.prop = `John Kennedy`/( `John Kennedy`+ `Josh Berkowitz`)) %>%
  mutate(Dem.baseswing = cut(Dem.prop, breaks = c(-0.001, 0.2,.40, 0.60, 0.8,1),labels = c("Very Residual","Residual", "Swing", "Base","Very Base")))%>%
  ggplot(aes(fill=Dem.baseswing)) +
  geom_sf()+
  labs(title = "2019 Judicial Election 4", 
       subtitle = "John Kennedy vs Josh Berkowitz!",
       fill = "Base/Swing for \nKennedy", 
       caption = "")+
  scale_fill_manual(
    values = c(
      "Very Residual" = "darkred",
      "Residual" = "red",
      "Base" = "blue",
      "Very Base" = "darkblue",
      "Swing" = "yellow"
    )
  ) +
  theme_void()


mapANDresults2019 %>% 
  mutate(Dem.prop = `John Kennedy`/( `John Kennedy`+ `Josh Berkowitz`)) %>%
  mutate(Dem.baseswing = cut(Dem.prop, breaks = c(-0.001, 0.2,.40, 0.60, 0.8,1),labels = c("Very Residual","Residual", "Swing", "Base","Very Base")))%>%
  ggplot(aes(fill=Dem.baseswing)) +
  geom_sf()+
  labs(title = "2019 Judicial Election 4", 
       subtitle = "John Kennedy vs Josh Berkowitz!",
       fill = "Base/Swing for \nKennedy", 
       caption = "")+
  scale_fill_manual(
    values = c(
      "Very Residual" = "white",
      "Residual" = "white",
      "Base" = "blue",
      "Very Base" = "blue",
      "Swing" = "white"
    )
  ) +
  theme_void()
# BASED ON 2024 ELECTION DATA, WE HAVE 98,131 REGISTERED TOTAL VOTERS


# let's get a grip on, out of how many voters that are registered actually vote
# in these judicial elections

(sum( mapANDresults2019$`John Kennedy` )+
    sum( mapANDresults2019$`Josh Berkowitz` ))/
  sum(mapANDresults2019$`REGISTERED VOTERS TOTAL`)
# in 2019, 30.65% of all registered voters voted in the judicial election

(sum( mapANDresults2023$`Samantha Silverstein` )+
  sum( mapANDresults2023$`Curt           Kissinger` ))/
  sum(mapANDresults2023$`REGISTERED VOTERS TOTAL`)
# in 2023, 48.36% of all registered voters voted in the judicial election
# HOWEVER! This was a big election (issue 1 and 2)


# MAKE FUNKY GRAPH 1

results2013 <- read_excel("data/election results/G13_Official_Canvass.xls", 
                          sheet = "Judicial",skip=2)
mapANDresults2013 <-
  left_join(acs_interp_judicial, results2013, by = c("PRECINCT" = "PRECINCT")) 

results2015 <- read_excel("data/election results/G15_Official_Canvass.xls", 
                          sheet = "Judicial",skip=2)
mapANDresults2015 <-
  left_join(acs_interp_judicial, results2015, by = c("PRECINCT" = "PRECINCT")) 

results2017 <- read_excel("data/election results/G17_Official_Canvass.xls", 
                          sheet = "Judicial",skip=2)
mapANDresults2017 <-
  left_join(acs_interp_judicial, results2017, by = c("PRECINCT" = "PRECINCT")) 

results2021 <- read_excel("data/election results/G21_Official_Canvass.xlsx", 
                          sheet = "Judicial",skip=2)
mapANDresults2021 <-
  left_join(acs_interp_judicial, results2021, by = c("PRECINCT" = "PRECINCT")) 

percent2013 = sum(mapANDresults2013$`BALLOTS CAST TOTAL`) / sum(mapANDresults2013$`REGISTERED VOTERS TOTAL`)
percent2015 = sum(mapANDresults2015$`BALLOTS CAST TOTAL`) / sum(mapANDresults2015$`REGISTERED VOTERS TOTAL`)
percent2017 = sum(mapANDresults2017$`BALLOTS CAST TOTAL`) / sum(mapANDresults2017$`REGISTERED VOTERS TOTAL`)
percent2019 = sum(mapANDresults2019$`BALLOTS CAST TOTAL`) / sum(mapANDresults2019$`REGISTERED VOTERS TOTAL`)
percent2021 = sum(mapANDresults2021$`BALLOTS CAST TOTAL`) / sum(mapANDresults2021$`REGISTERED VOTERS TOTAL`)
percent2023 = sum(mapANDresults2023$`BALLOTS CAST TOTAL`) / sum(mapANDresults2023$`REGISTERED VOTERS TOTAL`)

basicGeomData = data.frame(Date = c(2013,2015,2017,2019,2021,2023),
                           Percent = c(percent2013,percent2015,percent2017,percent2019,percent2021,percent2023) )
predictedGeomData = data.frame(Data = c(2025,2025,2025,2025,2025),Percent = c(.33,.34,.35,.36,.37))

ggplot(data=basicGeomData, aes(x = Date, y=Percent )) +
  geom_point(size = 4, color = "blue") +
  geom_line(size = 2, color = "blue") +
  geom_point(data=predictedGeomData, aes(x=Data,y=Percent), size = 4,color = "purple" ) +
  geom_line() + 
  theme_minimal() +                
  scale_y_continuous(labels = scales::percent_format(), breaks= seq(0,1,by=.05)) +  # Turnout as percentage
  scale_x_continuous(breaks = seq(2012, 2024, by = 1)) +
  ylim(0,1) +
  labs(
    title = "Yearly Voter Turnout Rate",
    y = "Turnout Proportion"
  )

# MAKE FUNKY GRAPH 2


percent2013 = sum(mapANDresults2013$`Megan Shanahan`) / sum(mapANDresults2013$`BALLOTS CAST TOTAL`)
percent2015 = (sum(mapANDresults2015$`Shane Herzner`) + sum(mapANDresults2015$`Curt Kissinger`) 
               +  sum(mapANDresults2015$`Josh Berkowitz`) + sum(mapANDresults2015$`Bob Kelly`)
 )/(sum(mapANDresults2015$`BALLOTS CAST TOTAL`)*2)
percent2017 = (sum(mapANDresults2017$`Curt Kissinger`)+sum(mapANDresults2017$`Darlene Rogers`))/sum(mapANDresults2017$`BALLOTS CAST TOTAL`)
percent2019 = (sum(mapANDresults2019$`John Kennedy`) + sum(mapANDresults2019$`Josh Berkowitz`)) /
  sum(mapANDresults2019$`BALLOTS CAST TOTAL`)
percent2023 = (sum(mapANDresults2023$`Samantha Silverstein`) + sum(mapANDresults2023$`Curt           Kissinger`)) /
  sum(mapANDresults2023$`BALLOTS CAST TOTAL`)

basicGeomData = data.frame(Date = c(2013,2015,2017,2019,2023),
                           Percent = c(percent2013,percent2015,percent2017,percent2019,percent2023) )
predictedGeomData = data.frame(Data = c(2025,2025,2025,2025,2025),Percent = c(.13,.14,.15,.16,.17))

ggplot(data=basicGeomData, aes(x = Date, y=1-Percent )) +
  geom_point(size = 4, color = "blue") +
  geom_line(size = 2, color = "blue") +
  geom_point(data=predictedGeomData, aes(x=Data,y=Percent), size = 4,color = "purple" ) +
  geom_line() + 
  theme_minimal() +                
  scale_y_continuous(labels = scales::percent_format(), breaks= seq(0,1,by=.05)) +  # Turnout as percentage
  scale_x_continuous(breaks = seq(2012, 2024, by = 1)) +
  ylim(0,1) +
  labs(
    title = "Ballot Dropoff Rate",
    y = "Dropoff Proportion"
  )

#Due to the current political climate, but the fact that this is still a normal
# judicial race, I will use 35% as my falloff estimate


results2024 <- read_excel("data/election results/G24_Official_Canvass.xlsx",skip=1)
mapANDresults2024 <-
  left_join(acs_interp_judicial, results2024, by = c("PRECINCT" = "PRECINCT")) 
estimatedVoters = sum(mapANDresults2024$`REGISTERED VOTERS TOTAL`) * 0.32
estimatedVoters

# we estimate that 34345 voters, or around that, will be voting in our election
estimatedVoters/2 - 1
# Our magic number is 17,171... or somewhere around the 17,000 mark.


# TURNOUT
sum(mapANDresults2019$`BALLOTS CAST TOTAL`)/sum(mapANDresults2019$`REGISTERED VOTERS TOTAL`)
#sum(mapANDresults2023$`BALLOTS CAST TOTAL`)/sum(mapANDresults2023$`REGISTERED VOTERS TOTAL`)

# 35%

# DROPOFF
1-(sum(mapANDresults2019$`John Kennedy`) + sum(mapANDresults2019$`Josh Berkowitz`)) /
  sum(mapANDresults2019$`BALLOTS CAST TOTAL`)

1-(sum(mapANDresults2023$`Samantha Silverstein`) + sum(mapANDresults2023$`Curt           Kissinger`)) /
  sum(mapANDresults2023$`BALLOTS CAST TOTAL`)
# 87% ?!

base2023 = mapANDresults2023 %>% 
  mutate(Dem.prop = `Samantha Silverstein`/( `Samantha Silverstein`+ `Curt           Kissinger`)) %>%
  filter(Dem.prop > 0.6)


res2019 = mapANDresults2019 %>% 
  mutate(Dem.prop = `John Kennedy`/( `John Kennedy`+ `Josh Berkowitz`)) %>% 
  filter(Dem.prop < 0.4)

# white population proportion
sum(base2023$whiteE)/sum(base2023$pop_totalE) # 82%
sum(res2019$whiteE)/sum(res2019$pop_totalE) # 87%

# (weighted by population) median incomes
weighted.mean(base2023$med_incomeE,base2023$pop_totalE) # $99,443
weighted.mean(res2019$med_incomeE,res2019$pop_totalE) # 119,672

# (weighted by population) median age
weighted.mean(base2023$median_ageE,base2023$pop_totalE) # 35.8
weighted.mean(res2019$median_ageE,res2019$pop_totalE) # 41.0

