dd <- read_rds("/Users/andi/Documents/PhD/stats/moorea_longterm/archive/chapter_1_longterm_data_stats/PolynesiaMana/data/clean/dat_pm_benthic_cover.rds")


dd <- dd %>% 
  filter(site %in% c("E2b", "Haapiti"), year >=2016) 

ddS <- dd %>% 
  group_by(year, site, genus) %>% 
  summarise(cover = mean(cover_perc)) %>% 
  pivot_wider(names_from = genus, values_from = cover,values_fill = 0) %>% 
  pivot_longer(3:ncol(.), names_to = "genus", values_to = "cover")

top_5_corals_pmana <- ddS %>% 
  group_by(year, site, genus) %>% 
  summarise(cover = mean(cover)) %>%  #mean coral cover per site 
  group_by(genus) %>% 
  summarise(cover = mean(cover) %>% 
              round(2)) %>% 
  arrange(desc(cover)) %>% 
  slice(1:5) %>% 
  pull(genus)


ddS <- ddS %>%
  mutate(genus = case_when(genus %in% top_5_corals_pmana ~ genus,
                           .default = "other")) %>% 
  group_by(year, site, genus) %>% 
  summarise(cover = sum(cover)) %>% 
  arrange(year, site, desc(cover))



all_mean <- ddS %>% 
  group_by(year, genus) %>% #mean over sites
  summarise(cover = mean(cover)) %>%
  group_by(genus) %>% #mean over years
  summarise(cover = mean(cover)) %>% 
  arrange(desc(cover))
