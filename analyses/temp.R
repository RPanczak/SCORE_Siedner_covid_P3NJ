p_load(lme4, sjPlot, ggeffects)

nyt_first_clean <- nyt_first %>% 
  filter(is.finite(log_dif))

ggplot(nyt_first_clean, aes(time_to_intervention, cases_day)) +
  geom_line() + 
  facet_wrap(~state, scales = "free_y") +
  geom_vline(xintercept = 0, colour = "red") +
  geom_vline(xintercept = 4, colour = "green")

ggplot(nyt_first_clean, aes(time_to_intervention, cases_day_perc)) +
  geom_line() + 
  facet_wrap(~state, scales = "free_y") +
  geom_vline(xintercept = 0, colour = "red") +
  geom_vline(xintercept = 4, colour = "green")

ggplot(nyt_first_clean, aes(time_to_intervention, log_dif)) +
  geom_line() + 
  facet_wrap(~state, scales = "free_y") +
  geom_vline(xintercept = 0, colour = "red") +
  geom_vline(xintercept = 4, colour = "green")

ggplot(nyt_first_clean, aes(time_to_intervention, cases_day_perc)) +
  geom_point() + 
  geom_vline(xintercept = 0, colour = "red") +
  geom_vline(xintercept = 4, colour = "green")

m11 <- lmer(log_dif ~ time_to_intervention + post_intervention +
             time_to_intervention : post_intervention +
             (1 | state),
           data = nyt_first_clean)

tab_model(m11, digits = 3)
plot_model(m11)

plot_model(m11, type = "pred", terms = "time_to_intervention") +
  geom_vline(xintercept = 0, colour = "red") +
  geom_vline(xintercept = 4, colour = "green")

ggpredict(m11, "time_to_intervention", type = "fixed")
ggpredict(m11, "time_to_intervention", type = "random")

# p_load(nlme)
# m12 <- lme(log_dif ~ time_to_intervention + post_intervention + 
#              time_to_intervention : post_intervention, 
#            random = ~ 1 | state, data = nyt_first_clean)
# 
# intervals(m12, which = "fixed")

nyt_lockdown_clean <- nyt_lockdown %>% 
  filter(is.finite(log_dif)) 

m21 <- lmer(log_dif ~ time_to_lockdown + post_intervention + 
             time_to_lockdown : post_intervention + 
             (1 | state), 
           data = nyt_lockdown_clean)

tab_model(m21, digits = 3)
plot_model(m21)
plot_model(m21, type = "pred", terms = "time_to_lockdown") +
  geom_vline(xintercept = 0, colour = "red") +
  geom_vline(xintercept = 4, colour = "green")


ggpredict(m21, "time_to_lockdown", type = "fixed")
ggpredict(m21, "time_to_lockdown", type = "random")

# m22 <- lme(log_dif ~ time_to_lockdown + post_intervention +
#              time_to_lockdown : post_intervention,
#            random = ~ 1 | state, data = nyt_lockdown_clean)
# 
# intervals(m22, which = "fixed")

