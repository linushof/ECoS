# Preparation --------------------------------------------------------------------

## packages ----------------------------------------------------------------

pacman::p_load(tidyverse,
               brms, posterior, tidybayes, # Bayes
               scico, patchwork, ggbeeswarm, # plots
               knitr, kableExtra) # tables

## data --------------------------------------------------------------------

participants_to_exclude <- c('618_4_1', '618_6_20') # did not follow the instructions

problems <- read_csv('data/clean/problems.csv')

sampling <- read_csv('data/clean/sampling.csv') |> 
  filter(!participant %in% participants_to_exclude)

choices <- read_csv('data/clean/choices.csv') |> 
  filter(!participant %in% participants_to_exclude) 

s1_choices <- choices |> filter(study=='s1')
s2_choices <- choices |> filter(study=='s2')
s3_choices <- choices |> filter(study=='s3')

## tables ------------------------------------------------------------------

col_names <- c("Coef." , 
               paste0("Mean", footnote_marker_symbol(1, format = "latex")) , 
               paste0("2.5\\%", footnote_marker_symbol(1, format = "latex")) , 
               paste0("97.5\\%", footnote_marker_symbol(1, format = "latex")) , 
               "Median", "SD" , 
               paste0("$\\hat{R}$", footnote_marker_symbol(2, format = "latex")) , 
               paste0("$\\text{ESS}_{\\text{bulk}}$", footnote_marker_symbol(3, format = "latex")) , 
               "$\\text{ESS}_{\\text{tail}}$")

col_names_pp <- c("Exp.", "Short", "Long", 
                  paste0("Difference", footnote_marker_symbol(1, format = "latex")))

## plotting ----------------------------------------------------------------

two_cols <- scico(n=2, begin = .1, end=.9, palette = 'managua')



# Data set -----------------------------------------------------------------

## participants ------------------------------------------------------------

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
            na = sum(is.na(gender)))

## problems ----------------------------------------------------------------

# get outcome range
problems |> 
  pivot_longer(cols = o1_p1:o2_3 , 
               values_to = 'value',
               names_to = 'feature') |> 
  separate_wider_delim(
    feature, delim = '_' ,
    names = c('option', 'feature')
  ) |> 
  mutate(number = str_extract(feature, '\\d'), 
         feature = if_else(str_detect(feature, 'p'),'probability', 'outcome')) |> 
  pivot_wider(names_from = feature, values_from = value) |> 
  filter(study=='s1', probability!=0) |> 
  summarise(min_out = min(outcome),
            max_out = max(outcome),
  )


## data points --------------------------------------------------------------------

n_choices <- choices |> filter(study=='s1') |> nrow() # 7320
n_samples <- sampling |> filter(study=='s1') |> nrow() # 163573



# M1: Switch effects (Binomial) -----------------------------------------------------------

# PROB_T is to distinguish problems where long- and short-term are same/different
m1_f <- bf(C ~ 1 + G*S*PROB_T + (1|PART) + (1|PROB)) 

# priors
m1_prior <- 
  # fixed effects
  ## intercepts
  prior(student_t(3,.75,.5), class = 'Intercept') + # intercept long-term (diff)
  prior(student_t(3,.25,.1), class = 'b', coef = 'PROB_TTRUE') + # : dev intercept long-term (same)
  prior(student_t(3,0,.1), class = 'b', coef = 'Gshort') + # dev intercept short-term (diff)
  prior(student_t(3,0,.1), class = 'b', coef = 'Gshort:PROB_TTRUE') + # : dev intercept short-term (same)
  ## slopes
  prior(normal(0,.5), class = 'b', coef = 'S') + # slope long-term (diff)
  prior(normal(0,.5), class = 'b', coef = 'Gshort:S') + # dev slope short-term (diff)
  prior(normal(0,.5), class = 'b', coef = 'S:PROB_TTRUE') + # dev slope long-term (same)
  prior(normal(0,.5), class = 'b', coef = 'Gshort:S:PROB_TTRUE') # dev slope short-term (same)


make_posts_m1 <- function(m1_fit){
  m1_fit |> 
    spread_draws(
      b_Intercept, b_S, b_Gshort, b_PROB_TTRUE, # main effects
      `b_Gshort:S` , `b_S:PROB_TTRUE` ,  `b_Gshort:PROB_TTRUE` , # 2-way interaction
      `b_Gshort:S:PROB_TTRUE`,  # 3-way interaction
      `sd_PROB__Intercept` , 
      `sd_PART__Intercept`
    )  |> 
    rename(beta_0 = b_Intercept , 
           beta_1 = b_Gshort ,
           beta_2 = b_S , 
           beta_3 = `b_Gshort:S` , 
           beta_4 = b_PROB_TTRUE , 
           beta_5 = `b_Gshort:PROB_TTRUE` , 
           beta_6 = `b_S:PROB_TTRUE` , 
           beta_7 = `b_Gshort:S:PROB_TTRUE` ,
           sigma_u = `sd_PROB__Intercept` , 
           sigma_v = `sd_PART__Intercept`
    ) |> 
    mutate(beta_long = beta_2 , 
           beta_short = beta_2 + beta_3 , 
           beta_delta = beta_3) |> 
    select(beta_long, beta_short, beta_delta, beta_0, beta_1, beta_2, beta_3, beta_4, beta_5, beta_6, beta_7, sigma_u:sigma_v) 
  
}


## Exp. 1 -----------------------------------------------------------------

# data
s1_m1_dat <- list(PART = as.factor(s1_choices$part_short) , 
                  PROB = as.factor(s1_choices$problem) , 
                  PROB_T = s1_choices$problem_type , 
                  G = as.factor(s1_choices$goal) ,
                  S = as.double(scale(s1_choices$switch_rate)) , 
                  C = s1_choices$correct_ground)

s1_m1 <- brm(m1_f , 
             data=s1_m1_dat , 
             prior = s1_m1_prior ,
             family = bernoulli(link = "logit") ,
             iter = 2000 ,
             warmup = 1000 ,
             chains = 6  ,
             cores = 6 ,
             save_pars = save_pars(all=T) ,
             file='fits/s1_m1')
s1_m1_posts <- make_posts_m1(s1_m1)



## Exp. 2 -----------------------------------------------------------------

s2_m1_dat <- list(PART = as.factor(s2_choices$part_short) , 
                  PROB = as.factor(s2_choices$problem) , 
                  PROB_T = s2_choices$problem_type , 
                  G = as.factor(s2_choices$goal) ,
                  S = as.double(scale(s2_choices$switch_rate)) , 
                  C = s2_choices$correct_ground)

s2_m1 <- brm(m1_f ,
             data=s2_m1_dat ,
             prior = m1_prior ,
             family = bernoulli(link = "logit") ,
             iter = 2000 ,
             warmup = 1000 ,
             chains = 6  ,
             cores = 6 ,
             save_pars = save_pars(all=T) ,
             file='fits/s2_m1')
s2_m1_posts <- make_posts_m1(s2_m1)


## Exp. 3 ------------------------------------------------------------

s3_m1_dat <- list(PART = as.factor(s3_choices$part_short) , 
                  PROB = as.factor(s3_choices$problem) , 
                  PROB_T = s3_choices$problem_type , 
                  G = as.factor(s3_choices$goal) ,
                  S = as.double(scale(s3_choices$switch_rate)) , 
                  C = s3_choices$correct_ground)

s3_m1 <- brm(m1_f ,
             data=s3_m1_dat ,
             prior = s1_m1_prior ,
             family = bernoulli(link = "logit") ,
             iter = 2000 ,
             warmup = 1000 ,
             chains = 6  ,
             cores = 6 ,
             save_pars = save_pars(all=T) ,
             file='fits/s3_m1')
s3_m1_posts <- make_posts_m1(s3_m1)

## Tables ------------------------------------------------------------------

make_custom_m1_TeX_table <- function(m1_posts, lower=0.025, upper=0.975,  digits=3){
  
  m1_posts |> 
    summarise_draws('mean', 
                    ~quantile(.x, probs = lower) ,
                    ~quantile(.x, probs = upper) ,
                    'median', 'sd', 'rhat', 'ess_bulk', 'ess_tail') |> 
    mutate(bold = `2.5%` > 0 | `97.5%` < 0 ,
           mean = ifelse(bold & variable %in%  c("beta_long", "beta_short", "beta_delta") , paste0("\\textbf{", round(mean,digits), "}"), round(mean, digits)) , 
           `2.5%` = ifelse(bold & variable %in%  c("beta_long", "beta_short", "beta_delta"), paste0("\\textbf{", round(`2.5%`, digits), "}"), round(`2.5%`,digits)) , 
           `97.5%` = ifelse(bold& variable %in%  c("beta_long", "beta_short", "beta_delta"), paste0("\\textbf{", round(`97.5%`, digits), "}"), round(`97.5%`,digits))) |>
    select(-bold) |> 
    rename(Coefficient = variable , 
           Mean = mean ,
           Median = median , 
           SD = sd ,
           R = rhat ,
           ESS_bulk = ess_bulk , 
           ESS_tail = ess_tail)
  
}

m1_coef_names <- c("$\\beta_{\\text{long}}$", "$\\beta_{\\text{short}}$", "$\\beta_{\\Delta}$" ,
                   "$\\beta_0$", "$\\beta_1$", "$\\beta_2$", "$\\beta_3$" , 
                   "$\\beta_4$", "$\\beta_5$", "$\\beta_6$", "$\\beta_7$" ,
                   "$\\sigma_u$", "$\\sigma_v$")


posteriors <- list(s1_m1_posts, s2_m1_posts, s3_m1_posts)
for(i in seq_along(1:length(posteriors))){
  
  effects <- make_custom_m1_TeX_table(posteriors[[i]])
  effects$Coefficient <- m1_coef_names
  colnames(effects) <- col_names
  
  effects |>
    kbl(format = "latex",
        booktabs = TRUE,
        caption = paste0("Study ", i, " Posterior Summaries of the Logistic Regression Model"),
        label = paste0("s",i,"_m1"),
        align = c("l", rep("r", 8)),
        escape = FALSE, 
        digits=3) |>
    pack_rows("Target estimates", 1, 3, bold=F, italic=T) |>
    pack_rows("Fixed effects", 4, 11, bold=F, italic=T) |>
    pack_rows("Random effects (Hyperparameters)", 12, 13, bold=F, italic=T) |>
    footnote(general = "",
             general_title = "Note. ",
             escape = FALSE,
             threeparttable = TRUE,
             symbol = c(
               "Only \\\\textit{target estimates} with 95\\\\% credible interval excluding zero are bold.", 
               "Scale reduction factor", 
               "Effective sample size")) |>  
    save_kable(paste0("manuscript/tables/", paste0("s",i,"_m1"),".tex"))
  
} 

## Figures -----------------------------------------------------------

s1_m1_preds <- s1_m1_posts |> 
  mutate(alpha_long = beta_0 , 
         alpha_short = beta_0 + beta_1) |> 
  select(alpha_long, alpha_short, beta_long, beta_short) |> 
  pivot_longer(cols = alpha_long:beta_short, names_to = 'param', values_to = 'estimate') |> 
  separate_wider_delim(param ,delim='_' , names=c('coefficient','goal')) |>
  group_by(coefficient, goal) |> 
  mutate(iter = row_number()) |> 
  pivot_wider(names_from = 'coefficient', values_from = 'estimate') |> 
  expand_grid(S_tilde=seq(min(s1_m1_dat$S),max(s1_m1_dat$S),.01)) |> 
  mutate(y_pred = plogis(alpha+beta*S_tilde)) |> 
  group_by(goal, S_tilde) |> 
  summarise(m =  mean(y_pred) , 
            q5 = quantile(y_pred, probs = .05) , 
            q95 = quantile(y_pred, probs = .95)) |> 
  mutate(experiment = as.factor('s1') , 
         prior = as.factor('Informative'))

s2_m1_preds <- s2_m1_posts |> 
  mutate(alpha_long = beta_0 , 
         alpha_short = beta_0 + beta_1) |> 
  select(alpha_long, alpha_short, beta_long, beta_short) |> 
  pivot_longer(cols = alpha_long:beta_short, names_to = 'param', values_to = 'estimate') |> 
  separate_wider_delim(param ,delim='_' , names=c('coefficient','goal')) |>
  group_by(coefficient, goal) |> 
  mutate(iter = row_number()) |> 
  pivot_wider(names_from = 'coefficient', values_from = 'estimate') |> 
  expand_grid(S_tilde=seq(min(s2_m1_dat$S),max(s2_m1_dat$S),.01)) |> 
  mutate(y_pred = plogis(alpha+beta*S_tilde)) |> 
  group_by(goal, S_tilde) |> 
  summarise(m =  mean(y_pred) , 
            q5 = quantile(y_pred, probs = .05) , 
            q95 = quantile(y_pred, probs = .95)) |> 
  mutate(experiment = as.factor('s2'),
         prior = as.factor('Informative'))

s3_m1_preds <- s3_m1_posts |> 
  mutate(alpha_long = beta_0 , 
         alpha_short = beta_0 + beta_1) |> 
  select(alpha_long, alpha_short, beta_long, beta_short) |> 
  pivot_longer(cols = alpha_long:beta_short, names_to = 'param', values_to = 'estimate') |> 
  separate_wider_delim(param ,delim='_' , names=c('coefficient','goal')) |>
  group_by(coefficient, goal) |> 
  mutate(iter = row_number()) |> 
  pivot_wider(names_from = 'coefficient', values_from = 'estimate') |> 
  expand_grid(S_tilde=seq(min(s3_m1_dat$S),max(s3_m1_dat$S),.01)) |> 
  mutate(y_pred = plogis(alpha+beta*S_tilde)) |> 
  group_by(goal, S_tilde) |> 
  summarise(m =  mean(y_pred) , 
            q5 = quantile(y_pred, probs = .05) , 
            q95 = quantile(y_pred, probs = .95)) |> 
  mutate(experiment = as.factor('s3'),
         prior = as.factor('Informative'))



m1_figure <- bind_rows(s1_m1_preds, s2_m1_preds, s3_m1_preds) |> 
  ggplot(aes(S_tilde,m, color = goal, fill=goal)) + 
  facet_wrap(~factor(experiment, levels = c('s1', 's2', 's3'), labels=c('Exp. 1', 'Exp. 2', 'Exp. 3')), scales='free_x', drop=T) +
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
ggsave('manuscript/figures/switch_effects.eps', plot=m1_figure, units = 'mm', width = 190, height = 190*.4)



# M2: Switch behavior (BEOI) -----------------------------------------------------


m2_f <- bf(S ~ 1 + G + (1 |i| PART) + (1 | PROB)  , 
           phi ~ 1 + G + (1 |i| PART) + (1 | PROB) , 
           zoi ~ 1 + G + (1 |i| PART) + (1 | PROB) , 
           coi ~ 0 + offset(1e2))

m2_prior <- 
  
  # fixed effects
  
  ## intercepts (distribution of long-term group)
  
  prior(student_t(3, -.75,.5), class = 'Intercept') + # mu: mean over (0,1)
  prior(normal(1,.5), class = 'Intercept', dpar= 'phi') + # phi: precision over (0,1) 
  prior(logistic(0,1), class = 'Intercept', dpar='zoi') + # pi: probability of 1
  
  ## slopes (group differences: divergence of short-term from long-term)
  
  prior(normal(0,.5), class = 'b', coef = 'Gshort') + # mu: mean over (0,1)
  prior(normal(0,.25), class = 'b', coef = 'Gshort', dpar= 'phi') + # phi: precision over (0,1) 
  prior(normal(0,.5), class = 'b', coef = 'Gshort', dpar='zoi') + # pi: probability of 1
  
  # random effects 
  
  ## participant-specific
  prior(exponential(10), class = 'sd', coef='Intercept', group='PART') + 
  prior(exponential(10), class = 'sd', coef='Intercept', group='PART', dpar='zoi') +
  prior(exponential(10), class = 'sd', coef='Intercept', group='PART', dpar='phi') +
  prior(lkj(4), class = "cor") +
  
  ## problem-specific
  prior(exponential(5), class = 'sd', coef='Intercept', group='PROB') + 
  prior(exponential(5), class = 'sd', coef='Intercept', group='PROB', dpar='zoi') +
  prior(exponential(5), class = 'sd', coef='Intercept', group='PROB', dpar='phi') 
  

make_posts_m2 <- function(m2_fit){
  
  m2_fit |> 
    spread_draws(
      # distribution of long-term condition
      b_zoi_Intercept, b_Intercept, b_phi_Intercept , 
      # deviations of the short-term condition
      b_zoi_Gshort, b_Gshort, b_phi_Gshort , 
      # problem-specific random effects
      sd_PROB__zoi_Intercept, sd_PROB__Intercept, sd_PROB__phi_Intercept , 
      # participant-specific random effects
      sd_PART__zoi_Intercept, sd_PART__Intercept, sd_PART__phi_Intercept ,
      # correlations between participant-specific random effects
      cor_PART__Intercept__phi_Intercept, cor_PART__Intercept__zoi_Intercept, cor_PART__phi_Intercept__zoi_Intercept)  |> 
    rename(gamma_0_pi = b_zoi_Intercept , 
           gamma_0_mu = b_Intercept , 
           gamma_0_phi = b_phi_Intercept ,
           gamma_1_pi = b_zoi_Gshort , 
           gamma_1_mu = b_Gshort , 
           gamma_1_phi = b_phi_Gshort ,
           sigma_u_pi = sd_PROB__zoi_Intercept , 
           sigma_u_mu = sd_PROB__Intercept ,
           sigma_u_phi = sd_PROB__phi_Intercept ,
           sigma_v_pi = sd_PART__zoi_Intercept , 
           sigma_v_mu = sd_PART__Intercept ,
           sigma_v_phi = sd_PART__phi_Intercept ,
           rho_mu_phi = cor_PART__Intercept__phi_Intercept , 
           rho_mu_pi = cor_PART__Intercept__zoi_Intercept , 
           rho_phi_pi = cor_PART__phi_Intercept__zoi_Intercept) |> 
    select(gamma_1_pi, gamma_1_mu, gamma_1_phi , 
           gamma_0_pi, gamma_0_mu, gamma_0_phi ,
           sigma_u_pi, sigma_u_mu, sigma_u_phi , 
           sigma_v_pi, sigma_v_mu, sigma_v_phi , 
           rho_mu_phi, rho_mu_pi, rho_phi_pi)
}
post_draws <- 1e3



## Exp. 1 ------------------------------------------------------------
s1_m2_dat <- list(PART = as.factor(s1_choices$part_short) , 
                  PROB = as.factor(s1_choices$problem) ,
                  G = as.factor(s1_choices$goal) ,
                  S = as.double(s1_choices$switch_rate))
s1_m2_newdat <- s1_choices |> distinct(G=goal, PART=part_short, PROB=problem) # for posterior predictions

s1_m2 <- brm(m2_f , 
                data=s1_m2_dat , 
                prior = m2_prior ,
                family = zero_one_inflated_beta() ,
                iter = 15000 ,
                warmup = 10000 ,
                chains = 6  ,
                cores = 6,
                file = 'fits/s1_m2')

s1_m2_posts <- make_posts_m2(s1_m2)
s1_m2_post_pred <- s1_m2 |> predicted_draws(newdata=s1_m2_newdat, re_formula=NULL, ndraws=post_draws)

# run additional diagnostics
# variables(s1_m2_e1)
# summary(s1_m2_e1)
# pp_check(s1_m2_e1)
# mcmc_plot(s1_m2_e1, type='trace', variable = "^b_", regex = TRUE)
# s1_m2_e1$prior


## Exp. 2 ------------------------------------------------------------------
s2_m2_dat <- list(PART = as.factor(s2_choices$part_short) , 
                  PROB = as.factor(s2_choices$problem) ,
                  G = as.factor(s2_choices$goal) ,
                  S = as.double(s2_choices$switch_rate))
s2_m2_newdat <- s2_choices |> distinct(G=goal, PART=part_short, PROB=problem) # for posterior predictions

s2_m2 <- brm(m2_f , 
             data=s2_m2_dat , 
             prior = m2_prior ,
             family = zero_one_inflated_beta() ,
             iter = 15000 ,
             warmup = 10000 ,
             chains = 6  ,
             cores = 6,
             file = 'fits/s2_m2')

s2_m2_posts <- make_posts_m2(s2_m2)
s2_m2_post_pred <- s2_m2 |> predicted_draws(newdata=s2_m2_newdat, re_formula=NULL, ndraws=post_draws)

## Exp. 3 ------------------------------------------------------------------
s3_choices_t <- s3_choices |> filter(phase=='test')
s3_m2_dat <- list(PART = as.factor(s3_choices_t$part_short) , 
                  PROB = as.factor(s3_choices_t$problem) ,
                  G = as.factor(s3_choices_t$goal) ,
                  S = as.double(s3_choices_t$switch_rate))
s3_m2_newdat <- s3_choices_t |> distinct(G=goal, PART=part_short, PROB=problem) # for posterior predictions

s3_m2 <- brm(m2_f , 
             data=s3_m2_dat , 
             prior = m2_prior ,
             family = zero_one_inflated_beta() ,
             iter = 15000 ,
             warmup = 10000 ,
             chains = 6  ,
             cores = 6,
             file = 'fits/s3_m2')

s3_m2_posts <- make_posts_m2(s3_m2) # posterior distributions of model parameters
s3_m2_post_pred <- s3_m2 |> predicted_draws(newdata=s3_m2_newdat, re_formula=NULL, ndraws=post_draws)
  
# model-implied posterior predictions
#tidy_epred <- s3_m2 |> epred_draws(newdata=newdat, re_formula=NULL, ndraws=1e3)


# run additional diagnostics
variables(s3_m2)
summary(s3_m2)
pp_check(s3_m2)
mcmc_plot(s3_m2, type='trace', variable = "^sd_", regex = TRUE)


## Tables ------------------------------------------------------------------

### posterior distributions -------------------------------------------------

make_custom_m2_TeX_table <- function(m2_posts, lower=0.025, upper=0.975,  digits=3){
  
  m2_posts |> 
    summarise_draws('mean', 
                    ~quantile(.x, probs = lower) ,
                    ~quantile(.x, probs = upper) ,
                    'median', 'sd', 'rhat', 'ess_bulk', 'ess_tail') |> 
    mutate(bold = `2.5%` > 0 | `97.5%` < 0 ,
           mean = ifelse(bold & variable %in%  c("gamma_1_pi", "gamma_1_mu", "gamma_1_phi") , paste0("\\textbf{", round(mean,digits), "}"), round(mean, digits)) , 
           `2.5%` = ifelse(bold & variable %in%  c("gamma_1_pi", "gamma_1_mu", "gamma_1_phi"), paste0("\\textbf{", round(`2.5%`, digits), "}"), round(`2.5%`,digits)) , 
           `97.5%` = ifelse(bold& variable %in%  c("gamma_1_pi", "gamma_1_mu", "gamma_1_phi"), paste0("\\textbf{", round(`97.5%`, digits), "}"), round(`97.5%`,digits))) |>
    select(-bold) |> 
    rename(Coefficient = variable , 
           Mean = mean ,
           Median = median , 
           SD = sd ,
           R = rhat ,
           ESS_bulk = ess_bulk , 
           ESS_tail = ess_tail)
  
}

m2_coef_names <- c("$\\gamma_1^{(\\pi)}$", "$\\gamma_1^{(\\mu)}$", "$\\gamma_1^{(\\phi)}$" ,
                   "$\\gamma_0^{(\\pi)}$", "$\\gamma_0^{(\\mu)}$", "$\\gamma_0^{(\\phi)}$" ,
                   "$\\sigma_{u^{(\\pi)}}$", "$\\sigma_{u^{(\\mu)}}$", "$\\sigma_{u^{(\\phi)}}$" ,
                   "$\\sigma_{v^{(\\pi)}}$", "$\\sigma_{v^{(\\mu)}}$", "$\\sigma_{v^{(\\phi)}}$" ,
                   "$\\rho_{v^{(\\mu \\phi)}}$", "$\\rho_{v^{(\\mu \\pi)}}$", "$\\rho_{v^{(\\phi \\pi)}}$")



posteriors <- list(s1_m2_posts, s2_m2_posts, s3_m2_posts)

for(i in seq_along(1:length(posteriors))){
  
  effects <- make_custom_m2_TeX_table(posteriors[[i]])
  effects$Coefficient <- m2_coef_names
  colnames(effects) <- col_names
  
  effects |>
    kbl(format = "latex",
        booktabs = TRUE,
        caption = paste0("Study ", i, " Posterior Summaries of the Full BEOI Model"),
        label = paste0("s",i,"_m2"),
        align = c("l", rep("r", 8)),
        escape = FALSE, 
        digits=3) |>
    pack_rows("Target estimates", 1, 3, bold=F, italic=T) |>
    pack_rows("Fixed effects", 4, 6, bold=F, italic=T) |>
    pack_rows("Random effects (Hyperparameters)", 7, 12, bold=F, italic=T) |>
    pack_rows("Correlations", 13, 15, bold=F, italic=T) |>
    footnote(general = "",
             general_title = "Note. ",
             escape = FALSE,
             threeparttable = TRUE,
             symbol = c(
               "Only \\\\textit{target estimates} with 95\\\\% credible interval excluding zero are bold.", 
               "Scale reduction factor", 
               "Effective sample size")) |>  
    save_kable(paste0("manuscript/tables/", paste0("s",i,"_m2"),".tex"))
  
} 

### posterior predictions ---------------------------------------------------
'summary table for posterior predictions'

# mean posterior prediction and 95% posterior predicted interval

post_preds <- list(s1_m2_post_pred, s2_m2_post_pred, s3_m2_post_pred)
post_preds_summary <- vector('list', length=length(post_preds))

for(i in seq_along(1:length(post_preds))){ 
  
  post_preds_summary[[i]] <- post_preds[[i]] |>
    group_by(G, .draw) |> 
    summarise(mean.pred = mean(.prediction)) |>
    pivot_wider(names_from = G, values_from = mean.pred) |> 
    mutate(diff=short-long) |> 
    mean_hdi() |> 
    mutate(Experiment=i, 
           Short = paste0(round(short, digits), " [", round(short.lower, digits), ", ", round(short.upper, digits), "]") ,
           Long = paste0(round(long, digits), " [", round(long.lower, digits), ", ", round(long.upper, digits), "]") , 
           bold = diff.lower > 0 | diff.upper < 0 ,
           `(Short-Long)` = if_else(bold, 
                                    paste0("\\textbf{" , round(diff, digits), " [", round(diff.lower, digits), ", ", round(diff.upper, digits), "]","}" ),
                                    paste0(round(diff, digits), " [", round(diff.lower, digits), ", ", round(diff.upper, digits), "]"))) |> 
    select(Experiment, Short, Long, `(Short-Long)`)
  }

post_preds_table <- bind_rows(post_preds_summary)
colnames(post_preds_table) <- col_names_pp

post_preds_table |>
  kbl(format = "latex",
      booktabs = TRUE,
      caption = "Average Predicted Switch Rate for the Short-Term Goal and Long-Term Goal Condition",
      label = paste0("BEOI_pred"),
      align = c("l", rep("r", 3)),
      escape = FALSE, 
      digits=3) |>
  footnote(general = paste0("Posterior predictions are derived from the hierarchical BEOI model (see Appendix \\\\ref{app:models}) and incorporate participant- and problem-specific random effects. ",
                            "Values in squared brackets are the lower and upper boundary of the 95\\\\% prediction interval."),
           general_title = "Note. ",
           escape = FALSE,
           threeparttable = TRUE,
           symbol = c(
             "Only posterior predicted mean differences with 95\\\\% prediction interval excluding zero are bold.")) |>  
  save_kable("manuscript/tables/posterior_predictions.tex")


## Figures -----------------------------------------------------------

### prepare ------------------------------------------------------------

# data 
binwidth <- 0.05  # histogram bins are [0, binwidth), [binwidth, 2*binwidth), ...  

m2_figure_dat <- choices |> 
  filter(!(study=='s3' & phase!='test')) |>  # (rm data not used in BEOI model)
  mutate(bin = cut(switch_rate, breaks = seq(0, 1.05, binwidth) , 
                   include.lowest = T, labels=F, right=F) , 
         bin.upper = (bin*binwidth)-.001
         ) |> 
  group_by(study, goal, bin.upper) |> 
  summarise(n = n()) |> 
  pivot_wider(names_from = goal, values_from = n, values_fill = 0) |>
  mutate(long_rel = long/sum(long) , # = bin_count(long)/total_count(long)
         short_rel = short/sum(short) ,
         diff=long_rel - short_rel)

m2_figure_means <- choices |> 
  filter(!(study=='s3' & phase!='test')) |>
  group_by(study, goal) |> 
  summarise(m=mean(switch_rate)) |> 
  pivot_wider(names_from = goal, values_from = m)

# labels
label_border <- tibble(study = rep('s1', 2) ,
                       x = c(.91,1.09) ,
                       y = rep(0.18, 2) , 
                       label = c("S < 1", "S = 1")
                       )

label_note <- tibble(study = 's1' , 
                     x = (((m2_figure_means[[1,'short']]+m2_figure_means[[1, 'long']])/2) + 1.025)/2 ,
                     y = -.21 , 
                     label = 'driven by'
                     )

label_exp <- c('Exp. 1', 'Exp. 2', 'Exp. 3')

label_1 <- data.frame(
  study = "s1",   # <-- first facet level
  x = 1.025,
  y = -0.04
)

### plot --------------------------------------------------------------------

m2_figure <- m2_figure_dat |> 
  ggplot(aes(x=bin.upper)) + 
  facet_wrap(~factor(study, levels=c('s1', 's2', 's3'), labels=label_exp)) +
  
  # mirror plots
  geom_bar(aes(y=long_rel, fill=factor(diff>0, labels = c("Short", "Long"))), stat = 'identity', fill=cols[1], alpha = .3, just = 1) +
  geom_bar(aes(y=-short_rel, fill=factor(diff>0, labels = c("Short", "Long"))), stat = 'identity', fill=cols[2], alpha = .3, just=1) +
  geom_bar(aes(y=diff, fill=factor(diff>0, labels = c("Short", "Long"))), stat='identity', just=1) +
  
  # condition means
  geom_segment(data=m2_figure_means, aes(x = long, y = 0, xend = long, yend = -.1), color=cols[1], linewidth = .5, linetype = 'dotted') +
  geom_segment(data=m2_figure_means, aes(x = short, y = 0, xend = short, yend = -.1), color=cols[2], linewidth = .5, linetype = 'dotted') +
   
  scale_x_continuous(limits=c(0,1.2), breaks=seq(0,1,length.out=3)) +
  scale_y_continuous(limits=c(-.4,.4)) +

  scale_color_scico_d(palette='managua', begin=.9, end=.1) +
  scale_fill_scico_d(palette='managua', begin=.9, end=.1) +
  
  # labels 
  theme_bw() +
  labs(x="Switch Rate (S)",
       y="Rel. Frequency",
       fill = "Goal",
       color='Goal') +
  
  # labels
  geom_segment(data = subset(m2_figure_dat, study == "s1"),
               aes(x = 1, y = .16, xend = .8, yend = .16),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
               color="black", linewidth=.3) +
  geom_segment(data = subset(m2_figure_dat, study == "s1"),
               aes(x = 1, y = .16, xend = 1.2, yend = .16),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
               color="black", linewidth=.3) +
  geom_segment(data = subset(m2_figure_dat, study == "s1"),
               aes(x = 1, y = .15, xend = 1, yend = .17), 
               color="black", linewidth=.3) +
  
  #annotate("point", x = 1.025, y = -0.04, size = 15, shape = 1, color = "black", stroke=.5) +
  geom_point(data = label_1,
             aes(x = x, y = y),
             size = 15,
             shape = 1,
             color = "black",
             stroke = 0.3,
             inherit.aes = FALSE) +
  geom_curve(data = subset(m2_figure_means, study == "s1") ,
             aes(x = (long+short)/2, y = -.1, xend = 1.025, yend = test[[(nrow(test)/3), ncol(test)]]),
             color="black", linewidth=.3,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"))  +
  geom_text(
    data = label_border,
    aes(x = x, y = y, label = label),
    size=2, 
    color='black',
    inherit.aes = FALSE
  ) + 
  geom_text(
    data = label_note,
    aes(x = x, y = y, label = label),
    size=2, 
    color='black',
    inherit.aes = FALSE
  ) 

m2_figure 
ggsave('manuscript/figures/switch_behavior.jpg', plot=m2_figure, units = 'mm', width = 190, height = 190*.4)



# Complexity -----------------------------------------------------

## M3: Switch effects (binomial) ----------------------------------------------------------

m3_f <- bf(C ~ G*S*CPX + (1|PART) + (1|PROB))

m3_prior <- 
  # fixed effects
  ## intercepts
  prior(student_t(3,.75,.5), class = 'Intercept') + # intercept long-term (high complexity)
  prior(student_t(3,.3,.1), class = 'b', coef = 'CPXlow') + # : dev intercept long-term (low complexity)
  prior(student_t(3,.15,.1), class = 'b', coef = 'CPXmedium') + # : dev intercept long-term (medium complexity)
  prior(student_t(3,0,.1), class = 'b', coef = 'Gshort') + # dev intercept short-term (diff)
  prior(student_t(3,0,.1), class = 'b', coef = 'Gshort:CPXlow') + # : dev intercept short-term (low complexity)
  prior(student_t(3,0,.1), class = 'b', coef = 'Gshort:CPXmedium') + # : dev intercept short-term (medium complexity)
  ## slopes
  prior(normal(0,.5), class = 'b', coef = 'S') + # slope long-term (high complexity)
  prior(normal(0,.5), class = 'b', coef = 'S:CPXlow') + # dev slope long-term (low complexity)
  prior(normal(0,.5), class = 'b', coef = 'S:CPXmedium') + # dev slope long-term (medium complexity)
  prior(normal(0,.5), class = 'b', coef = 'Gshort:S') + # dev slope short-term (high complexity)
  prior(normal(0,.5), class = 'b', coef = 'Gshort:S:CPXlow') + # dev slope short-term (low complexity)
  prior(normal(0,.5), class = 'b', coef = 'Gshort:S:CPXmedium') # dev slope short-term (medium complexity)

s1_choices_diff <- s1_choices |> filter(problem_type==FALSE)
s1_m3_dat <- list(
  PART = as.factor(s1_choices_diff$part_short) , 
  PROB = as.factor(s1_choices_diff$problem) , 
  G = as.factor(s1_choices_diff$goal) ,
  S = as.double(scale(s1_choices_diff$switch_rate)) , 
  CPX = as.factor(s1_choices_diff$complexity) ,
  C = s1_choices_diff$correct_ground
)

s1_m3 <- brm(m3_f , 
             data=s1_m3_dat , 
             prior = m3_prior ,
             family = bernoulli(link = "logit") ,
             iter = 2000 ,
             warmup = 1000 ,
             chains = 6  ,
             cores = 6 ,
             file='fits/s1_m3')

         
## M4: Switch behavior (BEOI Model) ---------------------------------------------------------------



m4_f <- bf(S ~ 1 + G*CPX + (1 |i| PART) + (1 | PROB)  , 
           phi ~ 1 + G*CPX + (1 |i| PART) + (1 | PROB) , 
           zoi ~ 1 + G*CPX + (1 |i| PART) + (1 | PROB) , 
           coi ~ 0 + offset(1e2))

m4_prior <- 
  
  # fixed effects
  
  ## intercepts (distribution of long-term group)
  
  prior(student_t(3, -.75,.5), class = 'Intercept') + # mu: mean over (0,1)
  prior(normal(1,.5), class = 'Intercept', dpar= 'phi') + # phi: precision over (0,1) 
  prior(logistic(0,1), class = 'Intercept', dpar='zoi') + # pi: probability of 1
  
  ## slopes (divergences from long-term--high complexity)

  ## mu: mean over (0,1)
  prior(normal(0,.5), class = 'b', coef = 'Gshort') + 
  prior(normal(0,.5), class = 'b', coef = 'CPXlow') + 
  prior(normal(0,.5), class = 'b', coef = 'CPXmedium') + 
  prior(normal(0,.5), class = 'b', coef = 'Gshort:CPXlow') + 
  prior(normal(0,.5), class = 'b', coef = 'Gshort:CPXmedium') + 
  
  ## phi: precision over (0,1) 
  prior(normal(0,.25), class = 'b', coef = 'Gshort', dpar= 'phi') + 
  prior(normal(0,.25), class = 'b', coef = 'CPXlow', dpar= 'phi') + 
  prior(normal(0,.25), class = 'b', coef = 'CPXmedium', dpar= 'phi') + 
  prior(normal(0,.25), class = 'b', coef = 'Gshort:CPXlow', dpar= 'phi') + 
  prior(normal(0,.25), class = 'b', coef = 'Gshort:CPXmedium', dpar= 'phi') + 
  
  ## pi: probability of 1
  prior(normal(0,.5), class = 'b', coef = 'Gshort', dpar='zoi') + 
  prior(normal(0,.5), class = 'b', coef = 'CPXlow', dpar= 'zoi') + 
  prior(normal(0,.5), class = 'b', coef = 'CPXmedium', dpar= 'zoi') + 
  prior(normal(0,.5), class = 'b', coef = 'Gshort:CPXlow', dpar= 'zoi') + 
  prior(normal(0,.5), class = 'b', coef = 'Gshort:CPXmedium', dpar= 'zoi') +

  # random effects 
  
  ## participant-specific
  prior(exponential(10), class = 'sd', coef='Intercept', group='PART') + 
  prior(exponential(10), class = 'sd', coef='Intercept', group='PART', dpar='zoi') +
  prior(exponential(10), class = 'sd', coef='Intercept', group='PART', dpar='phi') +
  prior(lkj(4), class = "cor") +
  
  ## problem-specific
  prior(exponential(5), class = 'sd', coef='Intercept', group='PROB') + 
  prior(exponential(5), class = 'sd', coef='Intercept', group='PROB', dpar='zoi') +
  prior(exponential(5), class = 'sd', coef='Intercept', group='PROB', dpar='phi') 


post_draws <- 1e3


s1_m4_dat <- list(PART = as.factor(s1_choices$part_short) , 
                  PROB = as.factor(s1_choices$problem) ,
                  G = as.factor(s1_choices$goal) ,
                  S = as.double(s1_choices$switch_rate) ,
                  CPX = as.factor(s1_choices$complexity))
s1_m4_newdat <- s1_choices |> distinct(G=goal, CPX=complexity, PART=part_short, PROB=problem) # for posterior predictions

s1_m4 <- brm(m4_f , 
             data=s1_m4_dat , 
             prior = m4_prior ,
             family = zero_one_inflated_beta() ,
             iter = 15000 ,
             warmup = 10000 ,
             chains = 6  ,
             cores = 6,
             file = 'fits/s1_m4')
s1_m4_post_pred <- s1_m4 |> predicted_draws(newdata=s1_m4_newdat, re_formula=NULL, ndraws=post_draws)


m4_reduced_f <- bf(S ~ 1 + G*CPX  , 
                   phi ~ 1 + G*CPX , 
                   zoi ~ 1 + G*CPX , 
                   coi ~ 0 + offset(1e2))

m4_reduced_prior <- 
  
  # fixed effects
  
  ## intercepts (distribution of long-term group)
  
  prior(student_t(3, -.75,.5), class = 'Intercept') + # mu: mean over (0,1)
  prior(normal(1,.5), class = 'Intercept', dpar= 'phi') + # phi: precision over (0,1) 
  prior(logistic(0,1), class = 'Intercept', dpar='zoi') + # pi: probability of 1
  
  ## slopes (divergences from long-term--high complexity)
  
  ## mu: mean over (0,1)
  prior(normal(0,.5), class = 'b', coef = 'Gshort') + 
  prior(normal(0,.5), class = 'b', coef = 'CPXlow') + 
  prior(normal(0,.5), class = 'b', coef = 'CPXmedium') + 
  prior(normal(0,.5), class = 'b', coef = 'Gshort:CPXlow') + 
  prior(normal(0,.5), class = 'b', coef = 'Gshort:CPXmedium') + 
  
  ## phi: precision over (0,1) 
  prior(normal(0,.25), class = 'b', coef = 'Gshort', dpar= 'phi') + 
  prior(normal(0,.25), class = 'b', coef = 'CPXlow', dpar= 'phi') + 
  prior(normal(0,.25), class = 'b', coef = 'CPXmedium', dpar= 'phi') + 
  prior(normal(0,.25), class = 'b', coef = 'Gshort:CPXlow', dpar= 'phi') + 
  prior(normal(0,.25), class = 'b', coef = 'Gshort:CPXmedium', dpar= 'phi') + 
  
  ## pi: probability of 1
  prior(normal(0,.5), class = 'b', coef = 'Gshort', dpar='zoi') + 
  prior(normal(0,.5), class = 'b', coef = 'CPXlow', dpar= 'zoi') + 
  prior(normal(0,.5), class = 'b', coef = 'CPXmedium', dpar= 'zoi') + 
  prior(normal(0,.5), class = 'b', coef = 'Gshort:CPXlow', dpar= 'zoi') + 
  prior(normal(0,.5), class = 'b', coef = 'Gshort:CPXmedium', dpar= 'zoi')


#s1_m4_reduced_newdat <- s1_choices |> distinct(G=goal, CPX=complexity, PART=part_short, PROB=problem) # for posterior predictions

s1_m4_reduced <- brm(m4_reduced_f , 
                     data=s1_m4_dat , 
                     prior = m4_reduced_prior ,
                     family = zero_one_inflated_beta() ,
                     iter = 10000 ,
                     warmup = 8000 ,
                     chains = 6  ,
                     cores = 6,
                     file = 'fits/s1_m4_reduced')



## Tables ------------------------------------------------------------------

# switch effects

s1_m3_posts <-  s1_m3 |>  
  spread_draws(b_Intercept, b_Gshort, b_S, b_CPXlow, b_CPXmedium,  # main effects
               `b_Gshort:S` ,`b_Gshort:CPXlow` , `b_Gshort:CPXmedium`, `b_S:CPXlow` , `b_S:CPXmedium`, # 2-way interaction
               `b_Gshort:S:CPXlow`, `b_Gshort:S:CPXmedium`) |>   # 3-way interactions
  mutate(intercept_long_H = b_Intercept,
         intercept_long_M = b_Intercept + b_CPXmedium ,  
         intercept_long_L = b_Intercept + b_CPXlow , 
         intercept_short_H = b_Intercept + b_Gshort,
         intercept_short_M = b_Intercept + b_Gshort + b_CPXmedium +  `b_Gshort:CPXmedium` ,  
         intercept_short_L = b_Intercept + b_Gshort + b_CPXlow + `b_Gshort:CPXlow` , 
         beta_long_H = b_S , 
         beta_long_M = b_S + `b_S:CPXmedium` , 
         beta_long_L = b_S + `b_S:CPXlow` ,
         beta_short_H = b_S + `b_Gshort:S` , 
         beta_short_M = b_S + `b_Gshort:S` + `b_S:CPXmedium` + `b_Gshort:S:CPXmedium` , 
         beta_short_L = b_S + `b_Gshort:S` + `b_S:CPXlow` + `b_Gshort:S:CPXlow`, 
         beta_Delta_H = beta_short_H - beta_long_H , 
         beta_Delta_M = beta_short_M - beta_long_M ,
         beta_Delta_L = beta_short_L - beta_long_L)

s1_m3_effects <- s1_m3_posts |>
  select(beta_short_L, beta_long_L, beta_Delta_L ,
         beta_short_M, beta_long_M, beta_Delta_M ,
         beta_short_H, beta_long_H, beta_Delta_H) |> 
  summarise_draws('mean', 
                  ~quantile(.x, probs = lower) ,
                  ~quantile(.x, probs = upper) ,
                  'median', 'sd', 'rhat', 'ess_bulk', 'ess_tail') |> 
  mutate(bold = `2.5%` > 0 | `97.5%` < 0 ,
         mean = ifelse(bold, paste0("\\textbf{", round(mean,digits), "}"), round(mean, digits)) , 
         `2.5%` = ifelse(bold, paste0("\\textbf{", round(`2.5%`, digits), "}"), round(`2.5%`,digits)) , 
         `97.5%` = ifelse(bold, paste0("\\textbf{", round(`97.5%`, digits), "}"), round(`97.5%`,digits))) |>
  select(-bold) |> 
  rename(Coefficient = variable , 
         Mean = mean ,
         Median = median , 
         SD = sd ,
         R = rhat ,
         ESS_bulk = ess_bulk , 
         ESS_tail = ess_tail)


s1_m3_effects$Coefficient <- rep(c("$\\beta_{short}$", "$\\beta_{long}$", "$\\beta_{\\Delta}$"), 3)
colnames(s1_m3_effects) <- col_names
  
s1_m3_effects |>
  kbl(format = "latex",
      booktabs = TRUE,
      caption = "Study 1 Posterior Summaries of the Logistic Regression Model's Target Estimates for Different Complexity Levels",
      label = "cpx_switch_effects",
      align = c("l", rep("r", 8)),
      escape = FALSE, 
      digits=3) |>
    pack_rows("Low complexity", 1, 3, bold=F, italic=T) |>
    pack_rows("Medium complexity", 4, 6, bold=F, italic=T) |>
    pack_rows("High complexity", 7, 9, bold=F, italic=T) |>
    footnote(general = "",
             general_title = "Note. ",
             escape = FALSE,
             threeparttable = TRUE,
             symbol = c(
               "Only \\\\textit{target estimates} with 95\\\\% credible interval excluding zero are bold.", 
               "Scale reduction factor", 
               "Effective sample size")) |>  
    save_kable(paste0("manuscript/tables/cpx_switch_effects.tex"))


# switch behavior

  
m4_differences <- s1_m4_post_pred  |>
  group_by(G, CPX, .draw) |> 
  summarise(mean.pred = mean(.prediction)) |>
  pivot_wider(names_from = CPX:G, values_from = mean.pred) |> 
  mutate(diff_low = low_short - low_long , 
         diff_medium = medium_short - medium_long ,
         diff_high = high_short - high_long, 
         diff_medium_low = diff_medium - diff_low , 
         diff_high_low = diff_high - diff_low ,
         diff_high_medium = diff_high - diff_medium) |> 
  select(diff_low, diff_medium, diff_high, 
         diff_medium_low, diff_high_low, diff_high_medium) |> 
  pivot_longer(cols = everything(), names_to = "comparison", values_to = "value") |> 
  group_by(comparison) |> 
  summarise(mean = mean(value, na.rm = TRUE), 
            lower = quantile(value, 0.025, na.rm = TRUE), 
            upper = quantile(value, 0.975, na.rm = TRUE),
            .groups = "drop") |> 
  arrange(factor(comparison, levels=c('diff_low', 'diff_medium', 'diff_high', 
                                      'diff_medium_low','diff_high_low', 'diff_high_medium'))) |>
  mutate(bold = lower > 0 , 
         mean = ifelse(bold, paste0("\\textbf{", round(mean,digits), "}"), round(mean, digits)), 
         lower = ifelse(bold, paste0("\\textbf{", round(lower,digits), "}"), round(lower, digits)), 
         upper = ifelse(bold, paste0("\\textbf{", round(upper,digits), "}"), round(upper, digits))) |> 
  select(-bold)
  
m4_differences$comparison <- c("$\\Delta_{low}$", "$\\Delta_{medium}$", "$\\Delta_{high}$" , 
                               "$\\Delta_{medium}-\\Delta_{low}$", "$\\Delta_{high}-\\Delta_{low}$", "$\\Delta_{high}-\\Delta_{medium}$")

colnames(m4_differences) <- c('Comparison', 'Mean', '2.5\\%', '97.5\\%')

m4_differences |>
  kbl(format = "latex",
      booktabs = TRUE,
      caption = "Differences in the Average Predicted Switch Rate for the Short-Term Goal and Long-Term Goal Condition Under Low, Medium, and High Complexity",
      label = paste0("BEOI_pred_cpx"),
      align = c("l", rep("r", 3)),
      escape = FALSE, 
      digits=3) |>
  pack_rows("Short - Long", 1, 3, bold=F, italic=T) |>
  pack_rows("Differences of differences", 4, 6, bold=F, italic=T) |> 
  footnote(general = paste0("Posterior predictions are derived from the hierarchical BEOI model (see Appendix \\\\ref{app:models}) and incorporate participant- and problem-specific random effects. ",
                            "Values in squared brackets are the lower and upper boundary of the 95\\\\% prediction interval."),
           general_title = "Note. ",
           escape = FALSE,
           threeparttable = TRUE,
           symbol = c(
             "Only posterior predicted mean differences with 95\\\\% prediction interval excluding zero are bold.")) |>  
  save_kable("manuscript/tables/posterior_predictions_cpx.tex")




## Figures ------------------------------------------------------------------

# switch effects

s1_m3_preds <- s1_m3_posts |> 
  select(intercept_long_H, intercept_long_M, intercept_long_L, 
         intercept_short_H, intercept_short_M, intercept_short_L, 
         beta_long_H, beta_long_M, beta_long_L, 
         beta_short_H, beta_short_M, beta_short_L) |> 
  pivot_longer(cols = everything(), names_to = 'param', values_to = 'estimate') |> 
  separate_wider_delim(param ,delim='_' , names=c('coefficient','goal','complexity')) |>
  group_by(coefficient, goal,complexity) |> 
  mutate(iter = row_number()) |> 
  pivot_wider(names_from = 'coefficient', values_from = 'estimate') |> 
  expand_grid(x=seq(min(s1_m3_dat$S),max(s1_m3_dat$S),.01)) |> 
  mutate(y_pred = plogis(intercept+beta*x)) |> 
  group_by(goal, complexity, x) |> 
  summarise(m =  mean(y_pred) , 
            q5 = quantile(y_pred, probs = .05) , 
            q95 = quantile(y_pred, probs = .95)) |> 
  mutate(experiment = as.factor('s1'))

label_cpx <- c('Low Complexity', 'Medium Complexity', 'High Complexity')
complexity_SE <- s1_m3_preds |>
  mutate(goal = if_else(goal=='long', 'Long', 'Short')) |> 
  ggplot(aes(x,m, color = goal, fill=goal)) + 
  facet_wrap(~factor(complexity, levels = c('L', 'M', 'H'), labels=label_cpx), nrow=1, scales='free_x') +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.3) +
  geom_line(linewidth=1) +
  #scale_y_continuous(limits = c(.5,1)) +
  labs(x='Normalized Switch Rate' , 
       y='Posterior Predicted Probability\nof Correct Decision', 
       color='Goal',
       fill = 'Goal') +
  theme_bw() +
  scale_color_scico_d(palette='managua', begin=.1, end=.9) +
  scale_fill_scico_d(palette='managua', begin=.1, end=.9) +
  theme(legend.position = 'none')


# switch behavior

# data 
binwidth <- 0.05  # histogram bins are [0, binwidth), [binwidth, 2*binwidth), ...  

complexity_SB_dat <- choices |> 
  filter(study=='s1') |>
  mutate(bin = cut(switch_rate, breaks = seq(0, 1.05, binwidth) , 
                   include.lowest = T, labels=F, right=F) , 
         bin.upper = (bin*binwidth)-.001) |> 
  group_by(study, goal, complexity, bin.upper) |> 
  summarise(n = n()) |> 
  pivot_wider(names_from = goal, values_from = n, values_fill = 0) |>
  mutate(long_rel = long/sum(long) , # = bin_count(long)/total_count(long)
         short_rel = short/sum(short) ,
         diff=long_rel - short_rel)

complexity_SB_means <- choices |> 
  filter(study=='s1') |>
  group_by(study, goal, complexity) |> 
  summarise(m=mean(switch_rate)) |> 
  pivot_wider(names_from = goal, values_from = m)

# labels
label_border <- tibble(complexity = rep('low', 2) ,
                       x = c(.91,1.09) ,
                       y = rep(0.18, 2) , 
                       label = c("S < 1", "S = 1")
)


label_1 <- data.frame(
  study = "low",   # <-- first facet level
  x = 1.025,
  y = -0.04
)

# plot

complexity_SB <- complexity_SB_dat |> 
  ggplot(aes(x=bin.upper)) + 
  facet_wrap(~factor(complexity, levels=c('low', 'medium', 'high'), labels=label_cpx)) +
  
  # mirror plots
  geom_bar(aes(y=long_rel, fill=factor(diff>0, labels = c("Short", "Long"))), stat = 'identity', fill=two_cols[1], alpha = .3, just = 1) +
  geom_bar(aes(y=-short_rel, fill=factor(diff>0, labels = c("Short", "Long"))), stat = 'identity', fill=two_cols[2], alpha = .3, just=1) +
  geom_bar(aes(y=diff, fill=factor(diff>0, labels = c("Short", "Long"))), stat='identity', just=1) +
  
  # condition means
  geom_segment(data=complexity_SB_means, aes(x = long, y = 0, xend = long, yend = -.1), color=two_cols[1], linewidth = .5, linetype = 'dotted') +
  geom_segment(data=complexity_SB_means, aes(x = short, y = 0, xend = short, yend = -.1), color=two_cols[2], linewidth = .5, linetype = 'dotted') +
  
  scale_x_continuous(limits=c(0,1.2), breaks=seq(0,1,length.out=3)) +
  scale_y_continuous(limits=c(-.4,.4)) +
  
  scale_color_scico_d(palette='managua', begin=.9, end=.1) +
  scale_fill_scico_d(palette='managua', begin=.9, end=.1) +
  
  # labels 
  theme_bw() +
  labs(x="Switch Rate (S)",
       y="Rel. Frequency",
       fill = "Goal",
       color='Goal') +
  
  # labels
  geom_segment(data = subset(complexity_SB_dat, complexity == "low"),
               aes(x = 1, y = .16, xend = .8, yend = .16),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
               color="black", linewidth=.3) +
  geom_segment(data = subset(complexity_SB_dat, complexity == "low"),
               aes(x = 1, y = .16, xend = 1.2, yend = .16),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
               color="black", linewidth=.3) +
  geom_segment(data = subset(complexity_SB_dat, complexity == "low"),
               aes(x = 1, y = .15, xend = 1, yend = .17), 
               color="black", linewidth=.3) +
  geom_text(
    data = label_border,
    aes(x = x, y = y, label = label),
    size=2, 
    color='black',
    inherit.aes = FALSE
  )

mixed_figure <- 
  (complexity_SE / complexity_SB) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = 'collect')

ggsave('manuscript/figures/complexity.jpg', plot=mixed_figure, units = 'mm', width = 190, height = 190*.75)
ggsave('manuscript/figures/complexity.eps', plot=mixed_figure, units = 'mm', width = 190, height = 190*.75)


# Training ----------------------------------------------------------------

test_dat <- s3_choices |> 
  mutate(trial2 = case_when(phase=='training1' ~ trial , 
                            phase=='training2' ~ trial+20, 
                            phase=='test' ~ trial+40) 
  ) |> 
  group_by(goal, trial2) |> 
  summarise(n = n() , 
            mean_switch = mean(switch_rate)) |> 
  ungroup() 



scale_min_max <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


m4_dat <- list(
  G = as.factor(test_dat$goal) ,
  S = as.double(test_dat$mean_switch),
  Tr = as.double(scale_min_max(test_dat$trial2))
)

m4_f <- bf(S ~ s(Tr, by=G)) 

m4 <- brm(m4_f , 
          data=m4_dat ,
          chains=6,
          cores=6
)

m4_2 <- brm(m4_f , 
            data=m4_dat ,
            family = Beta(),
            chains=6,
            cores=6, 
            control = list(adapt_delta = 0.95)
)



summary(m4)          
pp_check(m4_2)

loo_1 <- loo(m4)
loo_2 <- loo(m4_2)
loo_compare(loo_1, loo_2)


# visualization
effects_plot <- conditional_effects(m4_2, effects = "Tr:G")
p <- plot(effects_plot)[[1]]


t1 <- (20.5 - 1) / 79
t2 <- (40.5 - 1) / 79


m4_figure <- p + 
  geom_point(dat=test_dat, aes(x=scale_min_max(trial2), y=mean_switch, color=goal), inherit.aes=F) +
  geom_vline(xintercept = c(t1,t2), linetype="dashed") +
  scale_color_scico_d(palette = "managua", begin = .1, end = .9) +
  scale_fill_scico_d(palette = "managua", begin = .1, end = .9) +
  labs(x='Trials (scaled)',
       y='Average Switch Rate') +
  theme_bw() 

m4_figure
ggsave('manuscript/figures/switch_rate_change.jpg', plot=m4_figure, units = 'mm', width = 140, height = 140*.60)

# Appendix ----------------------------------------------------------------


## Inter-Individual Differences --------------------------------------------


### mean switch rates -------------------------------------------------------

plot_ind_switch_mean <- bind_rows(s1_choices, s2_choices, s3_choices) |> 
  filter(phase=='test') |> 
  group_by(study, goal, participant) |>
  summarise(switchM = mean(switch_rate)) |> 
  ungroup() |> 
  arrange(switchM) |> 
  mutate(participant=factor(participant,participant)) |> 
  ggplot(aes(x=participant, y=switchM, color=factor(goal, levels=c('long', 'short'), labels=c('Long', 'Short')))) +
  facet_wrap(~factor(study, levels=c('s1', 's2', 's3'), labels = c('Exp. 1',  'Exp. 2', 'Exp. 3')), nrow=1, scales = 'free') + 
  geom_segment(aes(x=participant, xend=participant, y=0, yend=switchM), size=0.5) +
  geom_point(size= 1) +
  coord_flip() +
  scale_color_manual(values=two_cols) +
  theme_bw() + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank() , 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x="Participant", 
       y="Mean Switch Rate",
       color="Goal")

plot_ind_switch_mean_box <- bind_rows(s1_choices, s2_choices, s3_choices) |> 
  filter(phase=='test') |> 
  group_by(study, goal, participant) |>
  summarise(switchM = mean(switch_rate)) |> 
  ggplot(aes(x=factor(goal, levels=c('long', 'short'), labels=c('Long', 'Short')), 
             y=switchM, 
             fill=factor(goal, levels=c('long', 'short'), labels=c('Long', 'Short')))) +
  facet_wrap(~factor(study, levels=c('s1', 's2', 's3'), labels = c('Exp. 1',  'Exp. 2', 'Exp. 3')), nrow=1, scales = 'free') + 
  geom_boxplot() +
  #geom_beeswarm(shape=1) +
  scale_fill_manual(values=two_cols) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x="Goal", 
       y="Mean Switch Rate",
       fill="Goal")


figure_ind_switch_mean <- plot_ind_switch_mean / plot_ind_switch_mean_box + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'A')
figure_ind_switch_mean


ggsave('manuscript/figures/switch_rates_ind_mean.jpg', plot=figure_ind_switch_mean, units = 'mm', width = 190, height = 190)
ggsave('manuscript/figures/switch_rates_ind_mean.eps', plot=figure_ind_switch_mean, units = 'mm', width = 190, height = 190)


### switch rates = 1 ---------------------------------------------------------


plot_ind_switch_ones <- bind_rows(s1_choices, s2_choices, s3_choices) |> 
  filter(phase=='test') |> 
  mutate(switch1 = switch_rate==1) |> 
  group_by(study, goal, participant) |> 
  summarise(trials = sum(switch1)) |> 
  ungroup() |> 
  arrange(trials) |> 
  mutate(participant=factor(participant,participant)) |> 
  ggplot(aes(x=participant, y=trials, color=factor(goal, levels=c('long', 'short'), labels=c('Long', 'Short')))) +
  facet_wrap(~factor(study, levels=c('s1', 's2', 's3'), labels = c('Exp. 1',  'Exp. 2', 'Exp. 3')), nrow=1, scales = 'free') + 
  geom_segment(aes(x=participant, xend=participant, y=0, yend=trials), size=0.5) +
  geom_point(size= 1) +
  coord_flip() +
  scale_color_manual(values=two_cols) +
  theme_bw() + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank() , 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x="Participant", 
       y="Trials with Switch Rate = 1",
       color="Goal")



plot_ind_switch_ones_box <- bind_rows(s1_choices, s2_choices, s3_choices) |> 
  filter(phase=='test') |> 
  mutate(switch1 = switch_rate==1) |> 
  group_by(study, goal, participant) |> 
  summarise(trials = sum(switch1)) |> 
  ggplot(aes(x=factor(goal, levels=c('long', 'short'), labels=c('Long', 'Short')), 
             y=trials, 
             fill=factor(goal, levels=c('long', 'short'), labels=c('Long', 'Short')))) +
  facet_wrap(~factor(study, levels=c('s1', 's2', 's3'), labels = c('Exp. 1',  'Exp. 2', 'Exp. 3')), nrow=1, scales = 'free') + 
  geom_boxplot() +
  #geom_beeswarm(shape=1) +
  scale_fill_manual(values=two_cols) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x="Goal", 
       y="Trials with Switch Rate = 1",
       fill="Goal")


figure_ind_switch_ones <- plot_ind_switch_ones / plot_ind_switch_ones_box + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'A')
figure_ind_switch_ones


ggsave('manuscript/figures/switch_rates_ind_ones.jpg', plot=figure_ind_switch_ones, units = 'mm', width = 190, height = 190)
ggsave('manuscript/figures/switch_rates_ind_ones.eps', plot=figure_ind_switch_ones, units = 'mm', width = 190, height = 190)



## Reduced Switch Rate model -----------------------------------------------------------


m2_reduced_prior <- 
  # fixed effects
  ## intercepts (distribution of long-term group)
  prior(student_t(3, -.75,.5), class = 'Intercept') + # mu: mean over (0,1)
  prior(normal(1,.5), class = 'Intercept', dpar= 'phi') + # phi: precision over (0,1) 
  prior(logistic(0,1), class = 'Intercept', dpar='zoi') + # pi: probability of 1
  # ## slopes (group differences: divergence of short-term from long-term)
  prior(normal(0,.5), class = 'b', coef = 'Gshort') + # mu: mean over (0,1)
  prior(normal(0,.25), class = 'b', coef = 'Gshort', dpar= 'phi') + # phi: precision over (0,1) 
  prior(normal(0,.5), class = 'b', coef = 'Gshort', dpar='zoi') # pi: probability of 1


# (base) model
m2_reduced_f <- bf(S ~ 1 + G , # mean over (0,1) 
                   phi ~ 1 + G , # precision/variance over (0-1) 
                   zoi ~ 1 + G , # probability of 0 or 1
                   coi ~ 0 + offset(1e2) # (conditional) probability of 1 given 0 or 1
                   )

make_posts_m2_reduced <- function(m2_reduced_fit){
  
  m2_reduced_fit |> 
    spread_draws(
      # distribution of long-term condition
      b_zoi_Intercept, b_Intercept, b_phi_Intercept , 
      # deviations of the short-term condition
      b_zoi_Gshort, b_Gshort, b_phi_Gshort)  |> 
    rename(gamma_0_pi = b_zoi_Intercept , 
           gamma_0_mu = b_Intercept , 
           gamma_0_phi = b_phi_Intercept ,
           gamma_1_pi = b_zoi_Gshort , 
           gamma_1_mu = b_Gshort , 
           gamma_1_phi = b_phi_Gshort) |> 
    select(gamma_1_pi, gamma_1_mu, gamma_1_phi , 
           gamma_0_pi, gamma_0_mu, gamma_0_phi)
}



### Exp. 1 ------------------------------------------------------------

s1_m2_reduced <- brm(m2_reduced_f ,
                     data=s1_m2_dat , 
                     prior = m2_reduced_prior ,
                     family = zero_one_inflated_beta() ,
                     iter = 2000 ,
                     warmup = 1000 ,
                     chains = 6  ,
                     cores = 6 , 
                     file = 'fits/s1_m2_reduced')
s1_m2_reduced_posts <- make_posts_m2_reduced(s1_m2_reduced)

### Exp. 2 ------------------------------------------------------------


s2_m2_reduced <- brm(m2_reduced_f ,
                     data=s2_m2_dat , 
                     prior = m2_reduced_prior ,
                     family = zero_one_inflated_beta() ,
                     iter = 2000 ,
                     warmup = 1000 ,
                     chains = 6  ,
                     cores = 6 , 
                     file = 'fits/s2_m2_reduced')
s2_m2_reduced_posts <- make_posts_m2_reduced(s2_m2_reduced)

### Exp. 3 ------------------------------------------------------------

s3_m2_reduced <- brm(m2_reduced_f ,
                     data=s3_m2_dat , 
                     prior = m2_reduced_prior ,
                     family = zero_one_inflated_beta() ,
                     iter = 2000 ,
                     warmup = 1000 ,
                     chains = 6  ,
                     cores = 6 , 
                     file = 'fits/s3_m2_reduced')
s3_m2_reduced_posts <- make_posts_m2_reduced(s3_m2_reduced)


### Tables ------------------------------------------------------------------

make_custom_m2_reduced_TeX_table <- function(m2_reduced_posts, lower=0.025, upper=0.975,  digits=3){
  
  m2_reduced_posts |> 
    summarise_draws('mean', 
                    ~quantile(.x, probs = lower) ,
                    ~quantile(.x, probs = upper) ,
                    'median', 'sd', 'rhat', 'ess_bulk', 'ess_tail') |> 
    mutate(bold = `2.5%` > 0 | `97.5%` < 0 ,
           mean = ifelse(bold & variable %in%  c("gamma_1_pi", "gamma_1_mu", "gamma_1_phi") , paste0("\\textbf{", round(mean,digits), "}"), round(mean, digits)) , 
           `2.5%` = ifelse(bold & variable %in%  c("gamma_1_pi", "gamma_1_mu", "gamma_1_phi"), paste0("\\textbf{", round(`2.5%`, digits), "}"), round(`2.5%`,digits)) , 
           `97.5%` = ifelse(bold& variable %in%  c("gamma_1_pi", "gamma_1_mu", "gamma_1_phi"), paste0("\\textbf{", round(`97.5%`, digits), "}"), round(`97.5%`,digits))) |>
    select(-bold) |> 
    rename(Coefficient = variable , 
           Mean = mean ,
           Median = median , 
           SD = sd ,
           R = rhat ,
           ESS_bulk = ess_bulk , 
           ESS_tail = ess_tail)
  
}

m2_reduced_coef_names <- m2_coef_names[1:6]

m2_col_names <- c("Coef.", 
                  paste0("Mean", footnote_marker_symbol(1, format = "latex")), 
                  paste0("2.5\\%", footnote_marker_symbol(1, format = "latex")), 
                  paste0("97.5\\%", footnote_marker_symbol(1, format = "latex")), 
                  "Median", "SD", 
                  paste0("$\\hat{R}$", footnote_marker_symbol(2, format = "latex")) , 
                  paste0("$\\text{ESS}_{\\text{bulk}}$", footnote_marker_symbol(3, format = "latex")) , 
                  "$\\text{ESS}_{\\text{tail}}$")

posteriors <- list(s1_m2_reduced_posts, s2_m2_reduced_posts, s3_m2_reduced_posts)

for(i in seq_along(1:length(posteriors))){
  
  effects <- make_custom_m2_reduced_TeX_table(posteriors[[i]])
  effects$Coefficient <- m2_reduced_coef_names
  colnames(effects) <- m2_col_names
  
  effects |>
    kbl(format = "latex",
        booktabs = TRUE,
        caption = paste0("Study ", i, " Posterior Summaries of the Reduced BEOI Model"),
        label = paste0("s",i,"_m2_reduced"),
        align = c("l", rep("r", 8)),
        escape = FALSE, 
        digits=3) |>
    pack_rows("Target estimates", 1, 3, bold=F, italic=T) |>
    pack_rows("Fixed effects", 4, 6, bold=F, italic=T) |>
    footnote(general = "",
             general_title = "Note. ",
             escape = FALSE,
             threeparttable = TRUE,
             symbol = c(
               "Only \\\\textit{target estimates} with 95\\\\% credible interval excluding zero are bold.", 
               "Scale reduction factor", 
               "Effective sample size")) |>  
    save_kable(paste0("manuscript/tables/", paste0("s",i,"_m2_reduced"),".tex"))
  
} 