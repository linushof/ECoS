# prep --------------------------------------------------------------------

# packages
pacman::p_load(tidyverse, brms, posterior, tidybayes, scico, patchwork)

# load data
choices <- read_csv('data/clean/choices.csv') |> 
  filter(!participant %in% c('618_4_1', '618_6_20')) # participants did not follow the instructions

s1_choices <- choices |> filter(study=='s1')
s2_choices <- choices |> filter(study=='s2')
s3_choices <- choices |> filter(study=='s3')

# participants ------------------------------------------------------------

participants <- choices |> 
  distinct(study, part_short, .keep_all = T) |> 
  select(study:training)

participants |> 
  group_by(study) |> 
  summarise(n = n(), 
            age.mean = mean(age, na.rm=TRUE) ,
            age.sd = sd(age, na.rm=TRUE) ,
            age.lower = range(age, na.rm=TRUE)[1] , 
            age.upper = range(age, na.rm=TRUE)[2] , 
            women = sum(gender=='w', na.rm = T) , 
            men = sum(gender=='m', na.rm=T) , 
            enby = sum(gender=='enby', na.rm=T) , 
            na = sum(is.na(gender))
            )

# M1: Switch effect -----------------------------------------------------------

'To Dos: 
- How to interpret the random intercept (referring to long-term) for participants in the 
short-term condition
- compare to models with random slopes)
- add priors 
- add complexity
'

## Experiment 1 -----------------------------------------------------------------

s1_m1_dat <- list(
  PART = as.factor(s1_choices$part_short) , 
  PROB = as.factor(s1_choices$problem) , 
  PROB_T = s1_choices$problem_type , 
  G = as.factor(s1_choices$goal) ,
  S = as.double(scale(s1_choices$switch_rate)) , 
  C = s1_choices$correct_ground
)

# PROB_T is to distinguish problems where long- and short-term are same/different
s1_m1_f <- bf(C ~ S*G*PROB_T + (1|PART) + (1|PROB)) 
s1_m1 <- brm(s1_m1_f , 
             data=s1_m1_dat , 
             family = bernoulli(link = "logit") ,
             iter = 1000 ,
             warmup = 500 ,
             chains = 6  ,
             cores = 6 ,
             save_pars = save_pars(all=T) , 
             file='models/s1_m1')
summary(s1_m1)

variables(s1_m1)
s1_m1_posts <-  s1_m1 |>  
  spread_draws(
    b_Intercept, b_S, b_Gshort, b_PROB_TTRUE, # main effects
    `b_S:Gshort` , `b_S:PROB_TTRUE` ,  `b_Gshort:PROB_TTRUE` , # 2-way interaction
    `b_S:Gshort:PROB_TTRUE` # 3-way interaction
    )  |> 
  mutate(
    a_long_neq = b_Intercept,
    a_long_eq = b_Intercept + b_PROB_TTRUE ,  
    a_short_neq = b_Intercept + b_Gshort , 
    a_short_eq = b_Intercept + b_Gshort + b_PROB_TTRUE + `b_Gshort:PROB_TTRUE` , 
    b_long_neq = b_S , # long-term goal, different from short-term
    b_long_eq = b_S + `b_S:PROB_TTRUE` , # long-term goal, same as short-term
    b_short_neq = b_S + `b_S:Gshort` , # short-term goal, different from long-term
    b_short_eq = b_S + `b_S:PROB_TTRUE` + `b_S:Gshort` +  `b_S:Gshort:PROB_TTRUE` # short-term goal, same as long-term goal
  ) |> 
  select(a_long_neq, a_short_neq, a_long_eq, a_short_eq, b_long_neq, b_short_neq, b_long_eq, b_short_eq) 


s1_m1_effects <- s1_m1_posts |>  
  summarise_draws(default_summary_measures())
s1_m1_effects

## Experiment 2 -----------------------------------------------------------------

s2_m1_dat <- list(
  PART = as.factor(s2_choices$part_short) , 
  PROB = as.factor(s2_choices$problem) , 
  PROB_T = s2_choices$problem_type , 
  G = as.factor(s2_choices$goal) ,
  S = as.double(scale(s2_choices$switch_rate)) , 
  C = s2_choices$correct_ground
)

s2_m1_f <- bf(C ~ S*G*PROB_T + (1|PART) + (1|PROB)) 
s2_m1 <- brm(s2_m1_f , 
             data=s2_m1_dat , 
             family = bernoulli(link = "logit") ,
             iter = 1000 ,
             warmup = 500 ,
             chains = 6  ,
             cores = 6 ,
             save_pars = save_pars(all=T) , 
             file='models/s2_m1')
summary(s2_m1)

s2_m1_posts <-  s2_m1 |>  
  spread_draws(
    b_Intercept, b_S, b_Gshort, b_PROB_TTRUE, # main effects
    `b_S:Gshort` , `b_S:PROB_TTRUE` ,  `b_Gshort:PROB_TTRUE` , # 2-way interaction
    `b_S:Gshort:PROB_TTRUE` # 3-way interaction
  )  |> 
  mutate(
    a_long_neq = b_Intercept,
    a_long_eq = b_Intercept + b_PROB_TTRUE ,  
    a_short_neq = b_Intercept + b_Gshort , 
    a_short_eq = b_Intercept + b_Gshort + b_PROB_TTRUE + `b_Gshort:PROB_TTRUE` , 
    b_long_neq = b_S , # long-term goal, different from short-term
    b_long_eq = b_S + `b_S:PROB_TTRUE` , # long-term goal, same as short-term
    b_short_neq = b_S + `b_S:Gshort` , # short-term goal, different from long-term
    b_short_eq = b_S + `b_S:PROB_TTRUE` + `b_S:Gshort` +  `b_S:Gshort:PROB_TTRUE` # short-term goal, same as long-term goal
  ) |> 
  select(a_long_neq, a_short_neq, a_long_eq, a_short_eq, b_long_neq, b_short_neq, b_long_eq, b_short_eq) 


s2_m1_effects <- s2_m1_posts |>  
  summarise_draws(default_summary_measures())
s2_m1_effects

## Experiment 3 ------------------------------------------------------------

s3_m1_dat <- list(
  PART = as.factor(s3_choices$part_short) , 
  PROB = as.factor(s3_choices$problem) , 
  PROB_T = s3_choices$problem_type , 
  G = as.factor(s3_choices$goal) ,
  S = as.double(scale(s3_choices$switch_rate)) , 
  C = s3_choices$correct_ground
)

s3_m1_f <- bf(C ~ S*G*PROB_T + (1|PART) + (1|PROB)) 
s3_m1 <- brm(s1_m1_f , 
             data=s3_m1_dat , 
             family = bernoulli(link = "logit") ,
             iter = 1000 ,
             warmup = 500 ,
             chains = 6  ,
             cores = 6 ,
             save_pars = save_pars(all=T) , 
             file='models/s3_m1')
summary(s3_m1)

s3_m1_posts <-  s3_m1 |>  
  spread_draws(
    b_Intercept, b_S, b_Gshort, b_PROB_TTRUE, # main effects
    `b_S:Gshort` , `b_S:PROB_TTRUE` ,  `b_Gshort:PROB_TTRUE` , # 2-way interaction
    `b_S:Gshort:PROB_TTRUE` # 3-way interaction
  )  |> 
  mutate(
    a_long_neq = b_Intercept,
    a_long_eq = b_Intercept + b_PROB_TTRUE ,  
    a_short_neq = b_Intercept + b_Gshort , 
    a_short_eq = b_Intercept + b_Gshort + b_PROB_TTRUE + `b_Gshort:PROB_TTRUE` , 
    b_long_neq = b_S , # long-term goal, different from short-term
    b_long_eq = b_S + `b_S:PROB_TTRUE` , # long-term goal, same as short-term
    b_short_neq = b_S + `b_S:Gshort` , # short-term goal, different from long-term
    b_short_eq = b_S + `b_S:PROB_TTRUE` + `b_S:Gshort` +  `b_S:Gshort:PROB_TTRUE`  # short-term goal, same as long-term goal
    ) |> 
  select(a_long_neq, a_short_neq, a_long_eq, a_short_eq, b_long_neq, b_short_neq, b_long_eq, b_short_eq) 


s3_m1_effects <- s3_m1_posts |>  
  summarise_draws(default_summary_measures())
s3_m1_effects


## Visualization -----------------------------------------------------------

s1_m1_preds <- s1_m1_posts |> 
  pivot_longer(cols = a_long_neq:b_short_eq, names_to = 'param', values_to = 'estimate') |> 
  separate_wider_delim(param ,delim='_' , names=c('coefficient','goal','target')) |>
  group_by(coefficient, goal,target) |> 
  mutate(iter = row_number()) |> 
  pivot_wider(names_from = 'coefficient', values_from = 'estimate') |> 
  expand_grid(x=seq(min(s1_m1_dat$S),max(s1_m1_dat$S),.01)) |> 
  #expand_grid(x=seq(-25,25,.01)) |>
  mutate(y_pred = plogis(a+b*x)) |> 
  group_by(goal, target, x) |> 
  summarise(m =  mean(y_pred) , 
            q5 = quantile(y_pred, probs = .05) , 
            q95 = quantile(y_pred, probs = .95)) |> 
  mutate(experiment = as.factor('s1'))


s2_m1_preds <- s2_m1_posts |> 
  pivot_longer(cols = a_long_neq:b_short_eq, names_to = 'param', values_to = 'estimate') |> 
  separate_wider_delim(param ,delim='_' , names=c('coefficient','goal','target')) |>
  group_by(coefficient, goal,target) |> 
  mutate(iter = row_number()) |> 
  pivot_wider(names_from = 'coefficient', values_from = 'estimate') |> 
  expand_grid(x=seq(min(s2_m1_dat$S),max(s2_m1_dat$S),.01)) |> 
  #expand_grid(x=seq(-25,25,.01)) |>
  mutate(y_pred = plogis(a+b*x)) |> 
  group_by(goal, target, x) |> 
  summarise(m =  mean(y_pred) , 
            q5 = quantile(y_pred, probs = .05) , 
            q95 = quantile(y_pred, probs = .95)) |> 
  mutate(experiment = as.factor('s2'))

s3_m1_preds <- s3_m1_posts |> 
  pivot_longer(cols = a_long_neq:b_short_eq, names_to = 'param', values_to = 'estimate') |> 
  separate_wider_delim(param ,delim='_' , names=c('coefficient','goal','target')) |>
  group_by(coefficient, goal,target) |> 
  mutate(iter = row_number()) |> 
  pivot_wider(names_from = 'coefficient', values_from = 'estimate') |> 
  expand_grid(x=seq(min(s3_m1_dat$S),max(s3_m1_dat$S),.01)) |> 
  #expand_grid(x=seq(-25,25,.01)) |>
  mutate(y_pred = plogis(a+b*x)) |> 
  group_by(goal, target, x) |> 
  summarise(m =  mean(y_pred) , 
            q5 = quantile(y_pred, probs = .05) , 
            q95 = quantile(y_pred, probs = .95)) |> 
  mutate(experiment = as.factor('s3'))


m1_figure <- bind_rows(s1_m1_preds, s2_m1_preds, s3_m1_preds ) |> 
  filter(target=='neq') |> 
  ggplot(aes(x,m, color = goal, fill=goal)) + 
  facet_wrap(~factor(experiment, levels = c('s1', 's2', 's3'), labels=c('Exp. 1', 'Exp. 2', 'Exp. 3')), nrow=1, scales='free_x') +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.3) +
  geom_line(linewidth=1) +
  #scale_y_continuous(limits = c(.5,1)) +
  labs(x='Normalized Switch Rate' , 
       y='Posterior Predicted Probability\nof Correct Decision', 
       color='Goal',
       fill = 'Goal') +
  theme_bw() +
  scale_color_scico_d(palette='managua', begin=.1, end=.9) +
  scale_fill_scico_d(palette='managua', begin=.1, end=.9)

m1_figure
ggsave('manuscript/figures/switch_effects.jpg', plot=m1_figure, units = 'mm', width = 190, height = 190*.4)


# M2: switch behavior -----------------------------------------------------

## switch rate (OIB) -------------------------------------------------------

### Experiment 1 ------------------------------------------------------------

'To Dos: 
- add priors
'

s1_m2_dat <- list(
  PART = as.factor(s1_choices$part_short) , 
  PROB = as.factor(s1_choices$problem) , 
  PROB_T = s1_choices$problem_type , 
  G = as.factor(s1_choices$goal) ,
  S = as.double(s1_choices$switch_rate)
)


s1_m2_f <- bf(S ~ 1 + G , 
              phi ~ 1 + G ,  # The precision of the 0-1 values, or phi
              zoi ~ 1 + G ,  # The zero-or-one-inflated part, or alpha
              coi ~  0 + offset(Inf) # prior should be centered around a large value (> 4), as no 0 can occur
) 

# s1_m2_p <- c(
#   
#   # expectation mu of continuous (0,1) part
#   prior(beta(1,1), class = "Intercept") , # long term
#   prior(normal(0,2) , class = "b") , # short term
# 
#   # add prior for precision phi of continuous (0,1) part
#   
# 
#   # probability of 0 or 1 
#   prior(normal(-1, 1), class = "Intercept", dpar="zoi") , # long term 
#   prior(normal(0, 2), class = "b", dpar="zoi")  # short term
# 
#   )

s1_m2 <- brm(s1_m2_f , 
             data=s1_m2_dat , 
             family = zero_one_inflated_beta() ,
             #prior = s1_m2_p , 
             iter = 1000 ,
             warmup = 500 ,
             chains = 6  ,
             cores = 6 , 
             file='models/s1_m2_ran')
summary(s1_m2)

s1_m1$prior

check_m$formula


variables(s1_m1)
s1_m1_posts <-  s1_m1 |>  
  spread_draws(
    b_Intercept, b_S, b_Gshort, b_PROB_TTRUE, # main effects
    `b_S:Gshort` , `b_S:PROB_TTRUE` ,  `b_Gshort:PROB_TTRUE` , # 2-way interaction
    `b_S:Gshort:PROB_TTRUE` # 3-way interaction
  )  |> 
  mutate(
    b_long_neq = b_S , # long-term goal, different from short-term
    b_long_eq = b_S + b_PROB_TTRUE , # long-term goal, same as short-term
    b_short_neq = b_S + `b_S:Gshort` , # short-term goal, different from long-term
    b_short_eq = b_S + b_PROB_TTRUE + `b_S:Gshort` +  `b_S:Gshort:PROB_TTRUE` # short-term goal, same as long-term goal
  ) |> 
  select(b_long_neq, b_short_neq, b_long_eq, b_short_eq) 


s1_m1_effects <- s1_m1_posts |>  
  summarise_draws(default_summary_measures())

s1_m1_effects


## switch count (logistic) ------------------------------------------------------------

names(s1_choices)
View(s1_choices)

s1_m2.2_dat <- list(
  PART = as.factor(s1_choices$part_short) , 
  PROB = as.factor(s1_choices$problem) , 
  G = as.factor(s1_choices$goal) ,
  SC = as.double(s1_choices$switch_total) , 
  N = as.double(s1_choices$smp_total)
)


s1_m2.2_f <- bf(SC | trials(N) ~ G + (1|PART)) 
s1_m2.2 <- brm(s1_m2.2_f , 
             data=s1_m2.2_dat , 
             family = binomial(link = "logit") ,
             iter = 1000 ,
             warmup = 500 ,
             chains = 6  ,
             cores = 6 ,
             save_pars = save_pars(all=T) , 
             file='models/s1_m2.2')
summary(s1_m2.2)
pp_check(s1_m2.2)
plogis(-1.26)


## Visualization -----------------------------------------------------------

m2_figure <- 
  bind_rows(s1_choices, s2_choices, s3_choices) |>
  group_by(study, goal, part_short) |> 
  summarise(m_switch = mean(switch_rate)) |> 
  ggplot(aes(goal, m_switch, color=goal, fill = goal, slab_color=goal)) + 
  facet_wrap(~factor(study, levels = c('s1', 's2', 's3'), labels=c('Exp. 1', 'Exp. 2', 'Exp. 3')), nrow=1, scales='free_x') +
  stat_histinterval(adjust = .5, 
                    width = .5 ,
                    .width = .1 ,
                    justification = -.3,
                    breaks = 10,
                    alpha=.3 ,    # border color
                    slab_size  = 1) + 
  geom_boxplot(width = .1, outlier.shape = NA, alpha=.1, linewidth = 1) +
  geom_jitter(width = .05, alpha = .3, size = 3) + 
  labs(x = "Goal", 
       y = "Switch Rate",
       color = "Goal" , 
       fill = 'Goal',
       slab_color='Goal') + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.5)) + 
  scale_x_discrete(labels=c("Long", "Short")) +
  scale_fill_scico_d(palette = "managua" , begin = .1, end = .9) + 
  scale_color_scico_d(palette = "managua", begin = .1, end = .9) +
  theme_bw()
m2_figure
ggsave('manuscript/figures/switch_behavior.jpg', plot=m2_figure, units = 'mm', width = 190, height = 190*.4)


# additional analyses -----------------------------------------------------

## complexity --------------------------------------------------------------

### switch effects ----------------------------------------------------------

s1_choices_diff <- s1_choices |> filter(problem_type==FALSE)

s1_m3_dat <- list(
  PART = as.factor(s1_choices_diff$part_short) , 
  PROB = as.factor(s1_choices_diff$problem) , 
  G = as.factor(s1_choices_diff$goal) ,
  S = as.double(scale(s1_choices_diff$switch_rate)) , 
  CPX = as.factor(s1_choices_diff$complexity) ,
  C = s1_choices_diff$correct_ground
)

# PROB_T is to distinguish problems where long- and short-term are same/different
s1_m3_f <- bf(C ~ S*G*CPX + (1|PART) + (1|PROB)) 
s1_m3 <- brm(s1_m3_f , 
             data=s1_m3_dat , 
             family = bernoulli(link = "logit") ,
             iter = 1000 ,
             warmup = 500 ,
             chains = 6  ,
             cores = 6 ,
             save_pars = save_pars(all=T) , 
             file='models/s1_m3')
summary(s1_m3)

variables(s1_m3)

s1_m3_posts <-  s1_m3 |>  
  spread_draws(
    b_Intercept, b_S, b_Gshort, b_CPXlow, b_CPXmedium,  # main effects
    `b_S:Gshort` , `b_S:CPXlow` , `b_S:CPXmedium`, `b_Gshort:CPXlow` , `b_Gshort:CPXmedium`, # 2-way interaction
    `b_S:Gshort:CPXlow`, `b_S:Gshort:CPXmedium`  # 3-way interactions
  )  |> 
  mutate(
    a_long_H = b_Intercept,
    a_long_M = b_Intercept + b_CPXmedium ,  
    a_long_L = b_Intercept + b_CPXlow , 
    a_short_H = b_Intercept + b_Gshort,
    a_short_M = b_Intercept + b_Gshort + b_CPXmedium +  `b_Gshort:CPXmedium` ,  
    a_short_L = b_Intercept + b_Gshort + b_CPXlow + `b_Gshort:CPXlow` , 
    b_long_H = b_S , 
    b_long_M = b_S + `b_S:CPXmedium` , 
    b_long_L = b_S + `b_S:CPXlow` ,
    b_short_H = b_S + `b_S:Gshort` , 
    b_short_M = b_S + `b_S:Gshort` + `b_S:CPXmedium` + `b_S:Gshort:CPXmedium` , 
    b_short_L = b_S + `b_S:Gshort` + `b_S:CPXlow` + `b_S:Gshort:CPXlow` 
    ) |> 
  select(a_long_H, a_long_M,  a_long_L , 
         a_short_H, a_short_M,  a_short_L , 
         b_long_H, b_long_M,  b_long_L , 
         b_short_H, b_short_M,  b_short_L 
         ) 

s1_m3_effects <- s1_m3_posts |>  
  summarise_draws(default_summary_measures())
s1_m3_effects


s1_m3_preds <- s1_m3_posts |> 
  pivot_longer(cols = a_long_H:b_short_L, names_to = 'param', values_to = 'estimate') |> 
  separate_wider_delim(param ,delim='_' , names=c('coefficient','goal','complexity')) |>
  group_by(coefficient, goal,complexity) |> 
  mutate(iter = row_number()) |> 
  pivot_wider(names_from = 'coefficient', values_from = 'estimate') |> 
  expand_grid(x=seq(min(s1_m3_dat$S),max(s1_m3_dat$S),.01)) |> 
  #expand_grid(x=seq(-25,25,.01)) |>
  mutate(y_pred = plogis(a+b*x)) |> 
  group_by(goal, complexity, x) |> 
  summarise(m =  mean(y_pred) , 
            q5 = quantile(y_pred, probs = .05) , 
            q95 = quantile(y_pred, probs = .95)) |> 
  mutate(experiment = as.factor('s1'))

complexity_SE <- s1_m3_preds |> 
  ggplot(aes(x,m, color = goal, fill=goal)) + 
  facet_wrap(~factor(complexity, levels = c('L', 'M', 'H'), labels=c('Low', 'Medium', 'High')), nrow=1, scales='free_x') +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.3) +
  geom_line(linewidth=1) +
  #scale_y_continuous(limits = c(.5,1)) +
  labs(x='Normalized Switch Rate' , 
       y='Posterior Predicted Probability\nof Correct Decision', 
       color='Goal',
       fill = 'Goal') +
  theme_bw() +
  scale_color_scico_d(palette='managua', begin=.1, end=.9) +
  scale_fill_scico_d(palette='managua', begin=.1, end=.9)
complexity_SE


### switch behavior ---------------------------------------------------------------

complexity_SB <- s1_choices |>
  group_by(goal, complexity, part_short) |> 
  summarise(m_switch = mean(switch_rate)) |> 
  ggplot(aes(goal, m_switch, color=goal, fill = goal, slab_color=goal)) + 
  facet_wrap(~factor(complexity, levels=c('low', 'medium','high')), nrow=1) +
  stat_histinterval(adjust = .5, 
                    width = .5 ,
                    .width = .1 ,
                    justification = -.3,
                    breaks = 10,
                    alpha=.3 ,    # border color
                    slab_size  = 1) + 
  geom_boxplot(width = .1, outlier.shape = NA, alpha=.1, linewidth = 1) +
  geom_jitter(width = .05, alpha = .3, size = 3) + 
  labs(x = "Goal", 
       y = "Switch Rate",
       color = "Goal" , 
       fill = 'Goal',
       slab_color='Goal') + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.5)) + 
  scale_x_discrete(labels=c("Long term", "Short term")) +
  scale_fill_scico_d(palette = "managua" , begin = .1, end = .9) + 
  scale_color_scico_d(palette = "managua", begin = .1, end = .9) +
  theme_bw()
complexity_SB

# switch_means <- statSub %>% group_by(aim) %>% 
#   summarise(m = mean(meanSwitching) ,
#             sd = sd(meanSwitching) , 
#             n = n(), 
#             se = sd(meanSwitching)/sqrt(n()))


### accuracy ----------------------------------------------------------------

acc_s <- choices |> 
  filter(study=='s1', problem_type==0) |> 
  group_by(goal, complexity) |> 
  summarise(acc = mean(correct_sampled, na.rm=T)) 

complexity_acc <- choices |> 
  filter(study=='s1', problem_type==0) |> 
  group_by(goal, complexity) |> 
  summarise(acc = mean(correct_ground)) |> 
  ggplot(aes(x=goal, y=acc, 
             colour=factor(complexity, levels = c('low', 'medium', 'high')), 
             fill = factor(complexity, levels = c('low', 'medium', 'high')))) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_bar(data=acc_s, alpha=.5, stat = 'identity', position = 'dodge') + 
  scale_fill_scico_d(palette = "tokyo" , begin = .1, end = .9) + 
  scale_color_scico_d(palette = "tokyo", begin = .1, end = .9) +
  geom_hline(yintercept = .5, linetype='dashed', linewidth=1) +
  labs(x = 'Goal' , 
       y = 'Accuracy' , 
       colour = 'complexity',
       fill = 'complexity') +
  theme_bw()
complexity_acc

## training effects --------------------------------------------------------


### switch behavior ---------------------------------------------------------

training_SB <- 
  s3_choices |> 
  group_by(participant) |> 
  mutate(trial2 = row_number()) |> 
  group_by(goal, trial2) |> 
  summarise(n = n() , 
            mean_switch = mean(switch_rate)) |> 
  ggplot(aes(x=trial2, y=mean_switch, group = goal, color=goal)) +
  geom_line(linewidth = 1) +
  #geom_point(size = 2) +
  geom_vline(xintercept = c(20.5,40.5), linetype="dashed") +
  scale_color_scico_d(palette = "managua", begin = .1, end = .9) +
  theme_bw() +
  labs(x='Trial', 
       y='Switch Rate',
       color='Goal')
training_SB


### accuracy ----------------------------------------------------------------

acc_s <- choices |> 
  filter(phase=='test', complexity=='high', problem_type==0) |> 
  group_by(goal, study) |> 
  summarise(acc = mean(correct_sampled, na.rm=T)) 

training_acc <- choices |> 
  filter(phase=='test', complexity=='high', problem_type==0) |> 
  group_by(goal, study) |> 
  summarise(acc = mean(correct_ground)) |> 
  ggplot(aes(x=goal, y=acc, 
             colour=factor(study, levels = c('s1', 's2', 's3')), 
             fill = factor(study, levels = c('s1', 's2', 's3')))) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_bar(data=acc_s, alpha=.5, stat = 'identity', position = 'dodge') + 
  scale_fill_scico_d(palette = "acton" , begin = .1, end = .9) + 
  scale_color_scico_d(palette = "acton", begin = .1, end = .9) +
  geom_hline(yintercept = .5, linetype='dashed', linewidth=1) +
  labs(x = 'Goal' , 
       y = 'Accuracy' , 
       colour = 'Experiment',
       fill = 'Experiment') +
  theme_bw()
training_acc


## combined figure ---------------------------------------------------------

mixed_figure <- 
  (complexity_SE / 
      complexity_SB / 
      (complexity_acc + training_SB + training_acc)
   ) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = 'collect')
  
mixed_figure
ggsave('manuscript/figures/mixed.jpg', plot=mixed_figure, units = 'mm', width = 190, height = 190)

# supplements ----------------------------------------------------------------
