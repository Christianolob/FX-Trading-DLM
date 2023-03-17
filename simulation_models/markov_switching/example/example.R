rm(list = ls())
options(scipen = 999)
options(max.print=1000000)

# install.packages("data.table")
# install.packages("Rtools")
# install.packages("this.path")
# install.packages("tidyverse")

library(data.table)
library(rstan)
library(this.path)
library(shinystan)
library(tictoc)
# library(Rtool)

library(tidyverse); library(rstan); library(Quandl)
# Now with real data! 
googl <- Quandl("WIKI/GOOGL", collapse = "weekly")

googl <- googl %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date) %>% 
  mutate(l_ac = log(`Adj. Close`),
         dl_ac = c(NA, diff(l_ac))) %>% 
  filter(Date > "2010-01-01")

plot.ts(googl$dl_ac, main = "Weekly changes in Google's adjusted close price")

options(mc.cores = parallel::detectCores())
compiled_model <- stan_model("C:/Users/chris/Documents/FX-Trading-DLM/simulation_models/markov_switching/example/example.stan")

googl_mod <- sampling(compiled_model, data= list(T = nrow(googl), y = googl$dl_ac*100), iter = 1000, chains = 4)

print(googl_mod, pars = c("alpha", "rho", "p", "sigma"))

goog <- as.data.frame(googl_mod, pars = "xi") %>% 
  gather(par, value) %>% 
  filter(grepl(",1", par)) %>%
  mutate(time = readr::parse_number(stringr::str_extract(par, "[0-9]{1,3}[,]"))) %>% 
  group_by(par) %>% 
  summarise(time = first(time), 
            mean = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975)) %>% 
  mutate(date = readr::parse_date(googl$Date)) %>% 
  ggplot(aes(x = date, y = mean)) +
  #geom_point(alpha = 0.3) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3,  fill = "green") +
  labs(title ="Probability of being in random walk state/n vsv momentum state",
       y = "Probability",
       subtitle = "GOOGL")

close <- ggplot(data = googl,aes(x = Date, y = `Adj. Close`)) +
  geom_line(colour = "green")

gridExtra::grid.arrange(goog, close)

launch_shinystan(modelo_reversao)
