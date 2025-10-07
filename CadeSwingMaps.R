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
  left_join(map2020, results2023, by = c("PRECINCT" = "PRECINCT")) %>% 
  filter( !is.na(`Samantha Silverstein`) )

mapANDresults2023 %>% 
  mutate(Dem.prop = `Samantha Silverstein`/( `Samantha Silverstein`+ `Curt           Kissinger`)) %>%
  mutate(Dem.baseswing = cut(Dem.prop, breaks = c(-0.001, 0.2,.40, 0.60, 0.8,1),labels = c("Very Residual","Residual", "Swing", "Base","Very Base")))%>%
  ggplot(aes(fill=Dem.baseswing)) +
  geom_sf()+
  labs(title = "2023 Judicial Election 4", 
       subtitle = "Samantha Silverstein vs Curt Kissinger",
       fill = "Vote for \nSam (%)", 
       caption = "")+
  scale_fill_manual(
    values = c(
      "Very Residual" = "darkred",
      "Residual" = "red",
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
  left_join(map2020, results2019, by = c("PRECINCT" = "PRECINCT")) %>% 
  filter( !is.na(`Josh Berkowitz`) )

mapANDresults2019 %>% 
  mutate(Dem.prop = `John Kennedy`/( `John Kennedy`+ `Josh Berkowitz`)) %>%
  mutate(Dem.baseswing = cut(Dem.prop, breaks = c(-0.001, 0.2,.40, 0.60, 0.8,1),labels = c("Very Residual","Residual", "Swing", "Base","Very Base")))%>%
  ggplot(aes(fill=Dem.baseswing)) +
  geom_sf()+
  labs(title = "2019 Judicial Election 4", 
       subtitle = "John Kennedy vs Josh Berkowitz!",
       fill = "Vote for \nJohn (%)", 
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
  

