m1<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area),
                     data = dat_genus_delta,
                     family = binomial(),
                     iter = 4000,
                     prior = c(prior(normal(0, 1), class = b),
                               prior(normal(0, 1), class = Intercept)),
                     warmup = 2000,
                     chains = 4,
                     cores = 4,
                     threads = threading(2, static = T),
                     control = list(adapt_delta = 0.99),
                     seed = 123,
                     backend = "cmdstanr") 

m2<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus),
         data = dat_genus_delta,
         family = binomial(),
         iter = 4000,
         prior = c(prior(normal(0, 1), class = b),
                   prior(normal(0, 1), class = Intercept)),
         warmup = 2000,
         chains = 4,
         cores = 4,
         threads = threading(2, static = T),
         control = list(adapt_delta = 0.99),
         seed = 123,
         backend = "cmdstanr") 

m3<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus) + (1|year),
         data = dat_genus_delta,
         family = binomial(),
         iter = 4000,
         prior = c(prior(normal(0, 1), class = b),
                   prior(normal(0, 1), class = Intercept)),
         warmup = 2000,
         chains = 4,
         cores = 4,
         threads = threading(2, static = T),
         control = list(adapt_delta = 0.99),
         seed = 123,
         backend = "cmdstanr") 

m4<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus) + (1|year:genus),
         data = dat_genus_delta,
         family = binomial(),
         iter = 4000,
         prior = c(prior(normal(0, 1), class = b),
                   prior(normal(0, 1), class = Intercept)),
         warmup = 2000,
         chains = 4,
         cores = 4,
         threads = threading(2, static = T),
         control = list(adapt_delta = 0.99),
         seed = 123,
         backend = "cmdstanr") 

m5<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus) + (1|year:marine_area),
         data = dat_genus_delta,
         family = binomial(),
         iter = 4000,
         prior = c(prior(normal(0, 1), class = b),
                   prior(normal(0, 1), class = Intercept)),
         warmup = 2000,
         chains = 4,
         cores = 4,
         threads = threading(2, static = T),
         control = list(adapt_delta = 0.99),
         seed = 123,
         backend = "cmdstanr") 

m6<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus) + (1|year:marine_area:genus),
         data = dat_genus_delta,
         family = binomial(),
         iter = 4000,
         prior = c(prior(normal(0, 1), class = b),
                   prior(normal(0, 1), class = Intercept)),
         warmup = 2000,
         chains = 4,
         cores = 4,
         threads = threading(2, static = T),
         control = list(adapt_delta = 0.99),
         seed = 123,
         backend = "cmdstanr") 

m7<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus) + (1|year:marine_area) + (1|year:genus),
         data = dat_genus_delta,
         family = binomial(),
         iter = 4000,
         prior = c(prior(normal(0, 1), class = b),
                   prior(normal(0, 1), class = Intercept)),
         warmup = 2000,
         chains = 4,
         cores = 4,
         threads = threading(2, static = T),
         control = list(adapt_delta = 0.99),
         seed = 123,
         backend = "cmdstanr") 
m8<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus) + (1|year:marine_area) ,
         data = dat_genus_delta,
         family = binomial(),
         iter = 4000,
         prior = c(prior(normal(0, 1), class = b),
                   prior(normal(0, 1), class = Intercept)),
         warmup = 2000,
         chains = 4,
         cores = 4,
         threads = threading(2, static = T),
         control = list(adapt_delta = 0.99),
         seed = 123,
         backend = "cmdstanr") 
m9<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus)+ (1|year:genus),
         data = dat_genus_delta,
         family = binomial(),
         iter = 4000,
         prior = c(prior(normal(0, 1), class = b),
                   prior(normal(0, 1), class = Intercept)),
         warmup = 2000,
         chains = 4,
         cores = 4,
         threads = threading(2, static = T),
         control = list(adapt_delta = 0.99),
         seed = 123,
         backend = "cmdstanr") 


m1 <- m1 %>% 
  add_criterion("loo")
m2 <- m2 %>% 
  add_criterion("loo")
m3 <- m3 %>% 
  add_criterion("loo")
m4 <- m4 %>% 
  add_criterion("loo")
m5 <- m5 %>% 
  add_criterion("loo", moment_match = TRUE)
m6 <- m6 %>% 
  add_criterion("loo", moment_match = TRUE)#ca
m7 <- m7 %>% 
  add_criterion("loo")
m8 <- m8 %>% 
  add_criterion("loo", moment_match = TRUE)
m9 <- m9 %>% 
  add_criterion("loo")

loo_compare(m1, m2, m3, m4, m5, m6, m7, m8, m9)

bayes_R2(m1)
bayes_R2(m2)
bayes_R2(m3)
bayes_R2(m4)
bayes_R2(m5)
bayes_R2(m6)#0.888691
bayes_R2(m7)
bayes_R2(m8)
bayes_R2(m9)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)
summary(m9)

m6<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus) + (1|year:marine_area:genus),
         data = dat_genus_delta,
         family = binomial(),
         iter = 4000,
         prior = c(prior(normal(0, 1), class = b),
                   prior(normal(0, 1), class = Intercept)),
         warmup = 2000,
         chains = 4,
         cores = 4,
         threads = threading(2, static = T),
        # control = list(adapt_delta = 0.99),
         seed = 123,
         backend = "cmdstanr") 


m6.1<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus) + (1|year:marine_area:genus),
         data = dat_genus_delta,
         family = binomial(),
         iter = 4000,
         prior = c(prior(normal(0, 10), class = b),
                   prior(normal(0, 10), class = Intercept)),
         warmup = 2000,
         chains = 4,
         cores = 4,
         threads = threading(2, static = T),
        # control = list(adapt_delta = 0.99),
         seed = 123,
         backend = "cmdstanr") 

m6 <- m6 %>% 
  add_criterion("loo", moment_match = TRUE)

m6.1 <- m6.1 %>% 
  add_criterion("loo",moment_match = TRUE)

loo_compare(m6.1, m6)

r2_bayes(m6.1)

r2_bayes(m6)



m6p<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus) + (1|year:marine_area:genus),
         data = dat_genus_delta,
         family = binomial(),
         iter = 4000,
         prior = c(prior(normal(0, 1), class = b),
                   prior(normal(0, 1), class = Intercept)),
         warmup = 2000,
         chains = 4,
         cores = 4,
         threads = threading(2, static = T),
         # control = list(adapt_delta = 0.99),
         seed = 123,
         backend = "cmdstanr", sample_prior = "only") 


m6.1p<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus) + (1|year:marine_area:genus),
           data = dat_genus_delta,
           family = binomial(),
           iter = 4000,
           prior = c(prior(normal(0, 10), class = b),
                     prior(normal(0, 10), class = Intercept)),
           warmup = 2000,
           chains = 4,
           cores = 4,
           threads = threading(2, static = T),
           # control = list(adapt_delta = 0.99),
           seed = 123,
           backend = "cmdstanr", sample_prior = "only") 

m6.2p<- brm(counts|trials(50) ~ max_dhw*factor(year)*genus + (1|marine_area) + (1|genus) + (1|year:marine_area:genus),
            data = dat_genus_delta,
            family = binomial(),
            iter = 4000,
            prior = c(prior(normal(0, 2), class = b),
                      prior(normal(0, 2), class = Intercept)),
            warmup = 2000,
            chains = 4,
            cores = 4,
            threads = threading(2, static = T),
            # control = list(adapt_delta = 0.99),
            seed = 123,
            backend = "cmdstanr", sample_prior = "only") 


pp_check(m6p)
pp_check(m6.1p)
pp_check(m6.2p)
