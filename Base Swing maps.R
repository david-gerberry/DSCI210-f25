#run magic number and election results first. 
ThresholdsRes = magic_number_average * 1.5
ThresholdsSwing = magic_number_average * .8



SmallMap %>% 
  ggplot(aes(fill = PrecentPorxy)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo") +
  labs(title = "Votes for Mapp") +
  labs(fill = "Percent of Vote") 

SmallMap %>%
  mutate(MappBaseSwing = cut(PrecentPorxy, breaks = c(-0.001, ThresholdsSwing, ThresholdsRes, 1), label = c('Residual', 'Swing', 'Base'))) %>% 
  ggplot(aes(fill = MappBaseSwing)) +
  geom_sf() +
  scale_fill_manual(
    values = c(
      "Residual" = "red",   
      "Base" = "blue",   
      "Swing" = "gold"   
    )
  )
SmallMap19 %>%
  mutate(MappBaseSwing = cut(PrecentPorxy, breaks = c(-0.001,ThresholdsSwing, ThresholdsRes, 1), label = c('Residual', 'Swing', 'Base'))) %>% 
  ggplot(aes(fill = MappBaseSwing)) +
  geom_sf() +
  scale_fill_manual(
    values = c(
      "Residual" = "red",   
      "Base" = "blue",   
      "Swing" = "gold"   
    )
  )
AvgMap %>% 
  mutate(BaseSwing = cut(AvgProx, breaks = c(-0.001,ThresholdsSwing, ThresholdsRes, 1), label = c('Residual', 'Swing', 'Base'))) %>% 
  ggplot(aes(fill = BaseSwing)) +
  geom_sf() +
  scale_fill_manual(
    values = c(
      "Residual" = "red",   
      "Base" = "blue",   
      "Swing" = "gold"   
))
AvgMap %>% 
  ggplot(aes(fill = AvgProx)) +
  geom_sf() +
  scale_fill_gradientn(colours=brewer.pal(n=10,name="RdBu"),na.value = "transparent",
                       values = c(0,ThresholdsSwing, ThresholdsRes, 1),
                       breaks = c(0, ThresholdsSwing, ThresholdsRes, 1), 
                       labels = c("0%", "22%", "43%", "100%"), 
                       limits=c(0,1))