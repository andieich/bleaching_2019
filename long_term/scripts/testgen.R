dat_genus_delta <- dat_c %>% 
 # mutate(proportion = ifelse(proportion == 0, 0.001, proportion)) %>% 
#  mutate(proportion = ifelse(proportion == 1, 0.999, proportion)) %>% 
  filter(year %in% 2019:2020) %>% 
  droplevels() %>% 
  mutate(year = as.factor(year))

dat_dhw <- read.csv(here("dhw/data/dat_sites_maxdhw_mur.csv"))



dat_genus_delta <- dat_genus_delta %>% 
  mutate(marine_area = case_when(marine_area == "Entre 2 baies" ~  "Entre 2 baies",
                                 marine_area == "Haapiti" ~  "Ha'apiti",
                                 marine_area == "Maatea" ~  "Ma'atea",
                                 marine_area == "Pihaena" ~ "Piha'ena",
                                 marine_area == "Temae" ~ "Tema'e",
                                 .default = marine_area)) 

dat_dhw <- dat_dhw %>% 
  mutate(site = ifelse(site == "Entre 2 baies", "Entre 2 baies", site ))



dat_genus_delta <- dat_genus_delta %>% 
  select(-coast) %>% 
  left_join(dat_dhw, by = c("marine_area" = "site")) 



dat_delta <- dat_genus_delta %>% 
  select(proportion, marine_area, year, transect) %>% 
  group_by(across(c(-proportion))) %>% 
  summarise(proportion = sum(proportion, na.rm = T))  %>% 
  ungroup()

dat_delta %>% 
  group_by(marine_area, year) %>% 
  summarise(n = n()) %>% 
  filter(n != 3)


library(ordbetareg)


test_gen_obr <- ordbetareg(bf(proportion ~ genus + (1 + year|marine_area),
                              phi ~ genus + (1 + year|marine_area)),
                  data = dat_genus_delta,
                  phi_reg = T,
                  iter = 4000,
                  warmup = 2000,
                  chains = 4,
                  cores = 4,
                  # control = list(adapt_delta = 0.95, 
                  #                max_treedepth = 20),
                  seed = 123,
                  threads = threading(2, static = T),
                  backend = "cmdstanr")

dat_genus_deltaT <- dat_genus_delta %>% 
  mutate(proportion = ifelse(proportion == 0, 0.001, proportion)) %>%
  mutate(proportion = ifelse(proportion == 1, 0.999, proportion)) 



test_gen2 <- brm(bf(proportion ~ 0 + genus + (1 + year|marine_area),
                    phi ~ genus + (1 + year|marine_area)),
                 family = Beta(),
                 data = dat_genus_deltaT,
                 init = 0,
                 iter = 4000,
                 warmup = 2000,
                 chains = 4,
                 cores = 4,
                 # control = list(adapt_delta = 0.95, 
                 #                max_treedepth = 20),
                 seed = 123,
                 threads = threading(2, static = T),
                 backend = "cmdstanr")

summary(test_gen_obr)
pp_check(test_gen_obr, resp = "proportion", ndraws = 100)
bayes_R2(test_gen_obr)


summary(test_gen2)
pp_check(test_gen2, resp = "proportion", ndraws = 100)
bayes_R2(test_gen2)



test_gen_obr <- add_criterion(test_gen_obr, "loo")
test_gen2 <- add_criterion(test_gen2, "loo", moment_match = TRUE)

loo_compare(test_gen_obr, test_gen2)









get_bef_aft_gen_site <- function(model, data){
  
  
  
  years <-  data$year %>% unique()

  if(length(years) != 2){stop(paste("Only two years allowed but", length(years != 2), "provided"))}
  
  year_1 <- sym(as.character(years[1]))
  
  year_2 <- sym(as.character(years[2]))
  
  nd <- expand_grid(genus = data$genus %>% unique(),
                    marine_area = data$marine_area %>% unique(),
                    year = data$year %>% unique()) %>% 
    mutate(row = 1:n())
  
  
  brms_summary <- function(x) {
    posterior::summarise_draws(x, "mean", "sd",  ~quantile(.x, probs = c(0.025, 0.975)))
  }
  
  
  
  
  
  
  post_pred <- fitted(model,
                 newdata = nd,
                 summary = F) %>% 
    # convert the results to a data frame
    data.frame() %>% 
    # rename the columns
    set_names(pull(nd, row)) %>% 
    # add a numeric index for the MCMC draws
    mutate(draw = 1:n()) %>% 
    # convert to the long format
    pivot_longer(-draw) %>% 
    # convert the row column from the character format to the numeric format
    mutate(row = as.double(name)) %>% 
    # join the nd predictor grid to the output
    left_join(nd, by = "row") %>% 
    # drop two of the columns which are now unnecessary
    select(-name, -row) %>% 
    # convert to a wider format so we can compute the contrast
    pivot_wider(names_from = year, values_from = value) %>% 
    # compute the ATE contrast
    mutate(tau = !!year_2 - !!year_1) %>% 
    # compute the average ATE value within each MCMC draw
    group_by(draw, genus, marine_area) %>% 
    summarise(ate = mean(tau)) %>% 
    # remove the draw index column
    ungroup() %>% 
    select(ate, genus, marine_area) #%>% 
    # now summarize the ATE across the MCMC draws
    # group_by(genus, marine_area) %>% 
    # brms_summary()
  
  return(post_pred)
}




pr_obr <- get_bef_aft_gen_site(test_gen_obr, dat_genus_delta) %>% 
  left_join(dat_dhw %>% 
              select(coast, site),
            by = c("marine_area" = "site")) %>% 
  mutate(method = "Ordered Beta Regression")
  
pr_br <- get_bef_aft_gen_site(test_gen2, dat_genus_deltaT) %>% 
  left_join(dat_dhw %>% 
              select(coast, site),
            by = c("marine_area" = "site"))%>% 
  mutate(method = "Beta Regression")

pred <- bind_rows(pr_obr, pr_br)


cols_spec <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73", "darkblue", "grey") %>% 
  make_pastel(n = 0.6)


plot_change_cover <- pred %>%
  ggplot(aes(y = genus, 
             x = ate*100,
             col = method, 
             fill = method)) +
  geom_vline(xintercept = 0, linetype = "11")+
  geom_density_ridges(rel_min_height = 0.02,scale = 1.8, alpha = .5)+
  scale_fill_manual(values = cols_spec, name = NULL)+
  scale_colour_manual(values = cols_spec, name = NULL)+
  
  facet_ragged_cols(rows = vars(marine_area),
                    cols =  vars(coast), )+
  labs(x = "Delta (% per year)", y = NULL)+
  scale_y_discrete(limits=rev)+
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "black", size = 0.2),
    panel.border = element_rect(fill=NA))+
  theme(legend.position = "bottom")
plot_change_cover

ggsave(filename = "delta_cover_sites.pdf",
       plot = plot_change_cover, 
       path = "~/Desktop",
       width = 20, height = 35, units  = "cm")



pred %>%
  ggplot(aes(y = genus, 
             x = ate*100,
             col = method, 
             fill = method)) +
  geom_vline(xintercept = 0, linetype = "11")+
  geom_density_ridges(scale = 1.8, alpha = .5)+
  scale_fill_manual(values = cols_spec, name = NULL)+
  scale_colour_manual(values = cols_spec, name = NULL)+
  
  facet_wrap(~coast)+
  labs(x = "Delta (% per year)", y = NULL)+
  scale_y_discrete(limits=rev)+
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "black", size = 0.2),
    panel.border = element_rect(fill=NA))+
  theme(legend.position = "bottom")


library(patchwork)


p <- p1/p2

ggsave(filename = "t.pdf",plot = p,path = "~/Desktop/",width = 25, height = 20, units = "cm")


test_gen2x <- brm(bf(proportion ~  max_dhw_29 * genus + (1 + year|marine_area),
                    phi ~ genus + (1 + year|marine_area)),
                 family = Beta(),
                 data = dat_genus_delta,
                 init = 0,
                 iter = 4000,
                 warmup = 2000,
                 chains = 4,
                 cores = 4,
                 # control = list(adapt_delta = 0.95, 
                 #                max_treedepth = 20),
                 seed = 123,
                 threads = threading(2, static = T),
                 backend = "cmdstanr")


test_gen3 <- brm(bf(proportion ~  genus + (1 + year|marine_area),
                    phi ~ genus + (1 + year|marine_area)),
                 family = Beta(),
                 data = dat_genus_delta,
                 init = 0,
                 iter = 4000,
                 warmup = 2000,
                 chains = 4,
                 cores = 4,
                 # control = list(adapt_delta = 0.95, 
                 #                max_treedepth = 20),
                 seed = 123,
                 threads = threading(2, static = T),
                 backend = "cmdstanr",
                 file = here("long_term/models/test_gen3"))



summary(test_gen1)
pp_check(test_gen1, resp = "proportion", ndraws = 100)
bayes_R2(test_gen1)

summary(test_gen2)
pp_check(test_gen2, resp = "proportion", ndraws = 100)
bayes_R2(test_gen2)

summary(test_gen2x)
pp_check(test_gen2x, resp = "proportion", ndraws = 100)
bayes_R2(test_gen2x)

test_gen2 <- add_criterion(test_gen2, "loo", moment_match = TRUE)
test_gen2x <- add_criterion(test_gen2x, "loo", moment_match = TRUE)

loo_compare(test_gen2x, test_gen2)


plot_slopes(test_X, variables = "year",type = "response", by = c( "marine_area"))

plot_slopes(test_gen1, variables = "year",type = "response", by = c( "max_dhw_29"))
plot_slopes(test_gen1, variables = "year",type = "response", by = c( "genus"))
plot_slopes(test_gen1, variables = "year",type = "response", by = c( "max_dhw_29", "genus"))


plot_slopes(test_gen2, variables = "year",type = "response", by = c( "max_dhw_29"))
plot_slopes(test_gen2, variables = "year",type = "response", by = c( "marine_area", "genus"))
plot_slopes(test_gen2, variables = "year",type = "response", by = c( "max_dhw_29", "genus"))

plot_slopes(test_gen3, variables = "year",type = "response", by = c( "genus"))
