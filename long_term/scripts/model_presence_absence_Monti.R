test <- dat_genus_delta %>% 
  filter(genus == "Montipora") %>% 
  mutate(present = ifelse(counts > 0, "yes", "no")) %>% 
  select(marine_area, transect, year, coast, present) %>% 
  group_by(marine_area, year, coast) %>% 
  count(present)

m_ccover_M<- brm(n|trials(3) ~ marine_area*factor(year),
                 data = test,
                 family = binomial(),
                 iter = 4000,
                 prior = c(prior(normal(0, 5), class = Intercept),
                           prior(normal(0, 5), class = b)),
                 warmup = 2000,
                 # control = list(adapt_delta = 0.85, 
                 #                max_treedepth = 15),
                 chains = 4,
                 cores = 4,
                 threads = threading(2, static = T),
                 seed = 123,
                 #   file = here("long_term/models/m_ccover_binom_monti"),
                 backend = "cmdstanr")

summary(m_ccover_M)#
pp_check(m_ccover_M, ndraws = 100)
bayes_R2(m_ccover_M)#

new_data_M <- test %>% 
  droplevels() %>% 
  dplyr::select(marine_area, coast, year) %>% 
  distinct()


pred_M <-  m_ccover_M %>% 
  epred_draws(new_data_M) %>% 
  ungroup() %>% 
  select(.draw,marine_area, coast, year, .epred) %>% 
  janitor::clean_names() %>% 
  mutate(prob = epred/3)

pred_M_S <- pred_M %>% 
  group_by(year, coast) %>% 
  summarise(median = median(prob),
            l_66 = quantile(prob, probs = (1-0.66)/2),
            u_66 = quantile(prob, probs = 1 - (1-0.66)/2),
            l_89 = quantile(prob, probs = (1-0.89)/2),
            u_89 = quantile(prob, probs = 1-(1-0.89)/2)
  )



pred_M_S %>%
  mutate(coast = fct_rev(coast)) %>% 
  ggplot(aes(x = year, col = coast))+
  geom_hline(yintercept = c(0,1), col = "grey", linetype = "11")+
  # geom_line(aes(y = median), position = position_dodge(width = .1))+
  geom_errorbar(aes(ymin = l_89, ymax = u_89),width = 0, linewidth = .5, position = position_dodge(width = .1))+
  geom_errorbar(aes(ymin = l_66, ymax = u_66),width = 0, linewidth = .8, position = position_dodge(width = .1))+
  scale_colour_manual(values = col_coast, name = NULL)+
  geom_point(aes(y = median), position = position_dodge(width = .1))+
  labs( y = expression(Delta~"Cover"), x = NULL)+
  theme_andi()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face="bold"),
        strip.text = element_text(face="italic"),
        legend.position = "bottom")

###
#Bernoulli
# Tried out Bernoulli but not appropriate, only if ONE trail, if more trails, it's Binomia2


dat_bern <- dat_genus_delta %>% 
  mutate(absent = 50 - counts,
         present = counts) %>% 
  pivot_longer(absent:present, names_to = "present") %>% 
  uncount(value)


# m_ccover_genus_bern <- brm(present ~ marine_area*genus*factor(year),
#                          data = dat_bern ,
#                          family = bernoulli(),
#                          init = 0,
#                          iter = 4000,
#                          warmup = 2000,
#                          # control = list(adapt_delta = 0.85, 
#                          #                max_treedepth = 15),
#                          chains = 4,
#                          cores = 4,
#                          threads = threading(2, static = T),
#                          seed = 123,
#                         # file = here("long_term/models/m_ccover_genus_binomial"),
#                          backend = "cmdstanr")