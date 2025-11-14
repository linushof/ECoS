# prep -------------------------------------------------------------

# pkgs 
pacman::p_load(tidyverse, readxl, mirai)

# functions
source('code/helpers/fun_rwp.R')
source('code/helpers/fun_compute_cumulative_stats.R')

# problems
s1_problems <- read_xlsx('data/raw/problems/study_1_problems.xlsx')
s2_problems <- read_xlsx('data/raw/problems/study_2_problems.xlsx')

# choice data
s1_files <- list.files('data/raw/study_1/', pattern = '.csv', full.names = T) # detect individual data sets
s2_files <- list.files('data/raw/study_2/', pattern = '.csv', full.names = T) # detect individual data sets

daemons(6)
s1_data <- s1_files |> map(in_parallel(\(x) readr::read_csv(x)))     
s2_data <- s2_files |> map(in_parallel(\(x) readr::read_csv(x)))     
daemons(0)

# tidy dat  -----------------------------------------------------------------

## problems ----------------------------------------------------------------

# study 1
s1_problems <- s1_problems |> filter(ID <= 120)

# study 2
s2_problems <- s2_problems |> 
  filter(phase!='practice') |> 
  mutate(ID = case_when(phase=='test' ~ ID + 37 , 
                        phase=='training1' | phase=='training2'  ~ ID + 117)
         ) |> 
  select(!c(phase:correct_FW))

# combine and prepare
problems <- bind_rows(s1_problems, s2_problems) |> 
  distinct(ID, .keep_all = T) |> 
  rename(problem = 'ID' , 
         complexity = 'CP' , 
         o1_p1 = 'p_o_1_r_1' , 
         o1_1 = 'o_1_r_1' , 
         o1_p2 = 'p_o_1_r_2' , 
         o1_2 = 'o_1_r_2' ,
         o1_p3 = 'p_o_1_r_3' , 
         o1_3 = 'o_1_r_3' ,
         o2_p1 = 'p_o_2_r_1' , 
         o2_1 = 'o_2_r_1' , 
         o2_p2 = 'p_o_2_r_2' , 
         o2_2 = 'o_2_r_2' ,
         o2_p3 = 'p_o_2_r_3' , 
         o2_3 = 'o_2_r_3' ,
         o1_ev = 'EV_o_1' , 
         o2_ev = 'EV_o_2' , 
         long = 'EV' , 
         short = 'FW'
         ) |> 
  mutate(complexity = case_match(complexity, 'LC' ~ 'low', 'MC' ~ 'medium', 'HC' ~ 'high') , 
         long = case_match(long, 'O1' ~ 'o1', 'O2' ~ 'o2') , 
         short = case_match(short, 'O1' ~ 'o1', 'O2' ~ 'o2')) |> 
  rowwise() |> 
  mutate(o1_rwp = rwp(o1_O = c(o1_1, o1_2, o1_3) , 
                      o1_P = c(o1_p1, o1_p2, o1_p3) ,
                      o2_O = c(o2_1, o2_2, o2_3) , 
                      o2_P = c(o2_p1, o2_p2, o2_p3)
                      ) , 
         o2_rwp = rwp(o1_O = c(o1_1, o1_2, o1_3) , 
                      o1_P = c(o1_p1, o1_p2, o1_p3) ,
                      o2_O = c(o2_1, o2_2, o2_3) , 
                      o2_P = c(o2_p1, o2_p2, o2_p3), 
                      direction = 'smaller')
         ) |> 
  ungroup() |> 
  select(problem, complexity, o1_p1:o1_3, o2_p1:o2_3, o1_ev, o2_ev, long, o1_rwp, o2_rwp, short)

## choices -----------------------------------------------------------------

# study 1 
s1_dat <- s1_data |> 
  map(\(part) part |>
        rename(goal = 'aim' ,
               complexity = 'CP' ,
               problem = 'ID' ,
               trial = 'Trialround' ,
               o1 = 'O1' , 
               o2 = 'O2' , 
               choice = 'selected') |> 
        mutate(study = 's1' , 
               mouse = 'last' , 
               phase = 'test' ,
               attended = case_when(is.na(choice) & is.na(o1) ~ 'o2' , 
                                    is.na(choice) & is.na(o2) ~ 'o1') ,
               participant = as.character(participant)) |> 
        select(study , 
               participant ,
               goal , # decision goal
               complexity , # complexity
               mouse , 
               phase , 
               trial , 
               problem , 
               attended , 
               o1 , 
               o2 , 
               choice) |> 
        filter(!is.na(problem)) |> 
        group_by(problem) |> 
        mutate(goal = last(goal) , 
               trial = last(trial), 
               choice = if_else(is.na(choice), last(choice), choice)) |> 
        ungroup() |> 
        separate_wider_delim(participant, delim = '_', 
                             names = c('date', 'session', 'computer', 'training'), 
                             too_few = 'align_start',
                             too_many = 'drop', 
                             cols_remove = F)
      ) 

# study 2

s2_dat <- s2_data |>  
  map(\(part) part |>
        rename( 
          goal = 'aim' ,
          complexity = 'CP' , 
          problem = 'ID' , 
          phase = 'phase...19' , 
          trial = 'round' , 
          o1 = 'O1' , 
          o2 = 'O2' , 
          choice = 'response') |> 
        mutate(
          goal = nth(goal, 2) ,
          problem = if_else(is.na(problem), lag(problem), problem) ,
          study = 's2' ,
          mouse = 'neutral', 
          attended = case_match(attended, 'O1' ~ 'o1', 'O2' ~ 'o2')) |> 
        select(study , 
               participant ,
               goal , # decision goal
               complexity , # complexity
               mouse , 
               phase , 
               trial , 
               problem , 
               attended , 
               o1 , 
               o2 , 
               choice) |> 
        filter(! ( is.na(phase) | phase == 'practice') ) |> # remove empty rows
        group_by(phase, problem) |> 
        mutate(trial = if_else(is.na(trial), last(trial), trial) , 
               complexity = if_else(is.na(complexity), lag(complexity), complexity) ,
               choice = if_else(is.na(choice), last(choice), choice)) |>
        ungroup() |> 
        separate_wider_delim(participant, delim = '_', 
                             names = c('date', 'session', 'computer', 'training'), 
                             too_few = 'align_start',
                             too_many = 'drop', 
                             cols_remove = F) |> 
        mutate(goal = if_else(as.integer(computer) %% 2 == 1, "EV", "FW"), # psychopy did not always code the goal
               problem = case_when(phase=='test' ~ problem + 37 , 
                                   phase=='training1' | phase=='training2'  ~ problem + 117)) 
  )

# prepare dat -----------------------------------------------------

dat <- bind_rows(s1_dat, s2_dat) |> 
  mutate(training = case_match(training, NA ~ 'no', 'T' ~ 'yes') , 
         goal = case_match(goal, 'EV' ~ 'long', 'FW' ~ 'short') , 
         complexity = case_match(complexity, 'LC' ~ 'low', 'MC' ~ 'medium', 'HC' ~ 'high') ,
         choice = case_match(choice, 'O1' ~ 'o1', 'O2' ~ 'o2' )
         ) |> 
  filter(!is.na(attended)) |> # remove reduntant row (only contains final choice)
  group_by(study, participant, phase, trial) |> 
  mutate(smp = row_number() ,
         smp_total = max(smp) , 
         switch = ifelse(attended != lag(attended), 1, 0) , 
         switch_total = sum(switch, na.rm = TRUE) ,
         switch_rate = round(switch_total/(smp_total - 1), 3), 
         o1_cm = mean(o1, na.rm=T) ,
         o1_sm = cummean2(o1, na.rm=T) , 
         o2_sm = mean(o2, na.rm=T) ,
         o2_cm = cummean2(o2, na.rm=T)) |> 
  ungroup() 


dat <- dat |> left_join(problems, join_by(problem, complexity)) |>
  group_by(study, participant, phase, trial) |> 
  mutate(o1_smp_total = sum(is.na(o2)) , # sample size option 1
         o2_smp_total = smp_total - o1_smp_total , # sample size option 2
         o1_sp1 = round(sum(if_else(o1 == o1_1, 1, 0), na.rm = TRUE)/o1_smp_total, 3) , # sampled probability option 1 outcome 1
         o1_sp2 = round(sum(if_else(o1 == o1_2, 1, 0), na.rm = TRUE)/o1_smp_total, 3) ,
         o1_sp3 = round(sum(if_else(o1 == o1_3, 1, 0), na.rm = TRUE)/o1_smp_total, 3) ,
         o2_sp1 = round(sum(if_else(o2 == o2_1, 1, 0), na.rm = TRUE)/o2_smp_total, 3) , 
         o2_sp2 = round(sum(if_else(o2 == o2_2, 1, 0), na.rm = TRUE)/o2_smp_total, 3) ,
         o2_sp3 = round(sum(if_else(o2 == o2_3, 1, 0), na.rm = TRUE)/o2_smp_total, 3)) |> 
  rowwise() |> 
  mutate(
    o1_srwp = rwp(o1_O = c(o1_1, o1_2, o1_3) , 
                  o1_P = c(o1_sp1, o1_sp2, o1_sp3) ,
                  o2_O = c(o2_1, o2_2, o2_3) ,
                  o2_P = c(o2_sp1, o2_sp2, o2_sp3)) ,
    o2_srwp = rwp(o1_O = c(o1_1, o1_2, o1_3) , 
                  o1_P = c(o1_sp1, o1_sp2, o1_sp3) , 
                  o2_O = c(o2_1, o2_2, o2_3) ,
                  o2_P = c(o2_sp1, o2_sp2, o2_sp3) ,
                  direction = 'smaller')) |>
  ungroup() |> 
  mutate(long_s = case_when(o1_sm > o2_sm ~'o1' , 
                                 o1_sm < o2_sm ~'o2' ,
                                 o1_sm == o2_sm ~ NA ) ,
         short_s = case_when(o1_srwp > o2_srwp ~'o1' ,
                             o1_srwp < o2_srwp ~'o2' ,
                             o1_srwp == o2_srwp ~ NA )
         )

# continue here -----------------------------------------------------------
dat |> 
  filter(smp==smp_total) |> 
  select(study, long_s, long, short_s, short) |> 
  mutate(long_agree = long_s == long ,
         short_agree = short_s == short) |> 
  group_by(study) |> 
  summarise(p_long_agree = mean(long_agree, na.rm=T) , 
            p_short_agree = mean(short_agree, na.rm=T))


# sampled probabilities

write_csv(choices, 'data/Study_1_Follow_up/clean/choices.csv')
write_csv(data, 'data/Study_1_Follow_up/clean/sampling.csv')






#aggregate all PsychoPy exports from "data" folder into one data frame

# format gender column

# dat.input$gender <- as.factor(revalue(dat.input$gender, c("FALSE" = "f")))
# 
# dat.input$gender <- ifelse(dat.input$gender=="female","f",
#                            ifelse(dat.input=="f\n", NA ,
#                                   ifelse(dat.input=="Female", "f",
#                                          ifelse(dat.input=="FEMALE","f",
#                                                 ifelse(dat.input$gender=="fermale","f",
#                                                        ifelse(dat.input$gender=="M","m",
#                                                               ifelse(dat.input$gender=="m\n",NA,
#                                                                      ifelse(dat.input$gender=="male","m",
#                                                                             ifelse(dat.input$gender=="Male","m",
#                                                                                    ifelse(dat.input$gender=="f","f",
#                                                                                           ifelse(dat.input$gender=="m","m", NA)
#                                                                                    )
#                                                                             )
#                                                                      )
#                                                               )
#                                                        )
#                                                 )
#                                          )
#                                   )
#                            )
# )


## select relevant columns to form data frames for usage

# dat.sampling for record of sampling process, this contains the full trial data
dat.sampling <- dat.input %>%                                                                    
  select(participant ,
         aim , # decision goal
         CP , # complexity
         ID , # problem identifier
         Trialround , # choice trial (sequence in which ID/problems appeared to the participant)
         O1 , # sampled outcome option 1 / left option
         O2 , # sampled outcome option 2 / right option
         selected , # chosen option 
         attractive , # option with better EV
         freqwinner # option with better FW
  ) %>% 
  filter(!is.na(CP))
dat.sampling <- as_tibble(dat.sampling)

## archiving data for later assessment
write_csv(dat.sampling,'data/Study 2/clean/study_2_samples.csv')


# dat2 for hypothesis testing, this contains all choices (one choice is one line item)

dat2 <- dat.input %>%                                                                         
  select(participant , 
         age , 
         gender ,
         aim , 
         CP , 
         ID ,
         Trialround ,
         effort , # number of sampled outcomes in trial
         switching , # number of switches in trial
         selected ,
         attractive ,
         freqwinner
  ) %>% 
  filter(!is.na(selected)) %>% 
  distinct()

## define factor levels / data types

# complexity as ordinal factor
dat2$CP <- factor(dat2$CP, levels=c("LC","MC","HC"))    

# search aim (decision criterion) as unordered factor
dat2$aim <- factor(dat2$aim, levels=c("EV","FW"))  

# participant gender as unordered factor
dat2$gender <- factor(dat2$gender, levels=c("m","f","d"))

# age as double
dat2$age <- as.double(dat2$age) 


## prepare additional columns for analysis

# switching probability
dat2$switchprob <- (dat2$switching/(dat2$effort-1))                                           

# choice matching criterion (binomial)
dat2$rightchoice <- ifelse(dat2$aim=='EV',    
                           ifelse(dat2$selected==dat2$attractive,1,0),
                           ifelse(dat2$aim=='FW',
                                  ifelse(dat2$selected==dat2$freqwinner,1,0),
                                  NA))

# clean participant numeration from 1 onwards
id <- c(1:(length(dat2$participant)/40))
partID <- rep(id,each=40)
dat2$partID <- partID
dat2 <- as_tibble(dat2)

## archiving data for later assessment

# write.csv2(dat2,"data/choices.csv",col.names=T,row.names=F)
write_csv(dat2,'data/Study 2/clean/study_2_choices.csv')






data <- data |> 
  mutate(participant2 = participant, 
         training = str_detect(participant2, "_T")) |> 
  separate(participant2, into = c("date", "session", "computer"), sep = "_", fill = "right") |> 
  mutate(
    computer = as.integer(computer),
    group = if_else(computer %% 2 == 1, "EV", "FW"))

data <- data |> select(participant, training, group, aim, everything() ) |> 
  select(!c(date, session, computer))

# create and store data sets
names(data)
choices <- data |> 
  distinct(participant, phase, ID, .keep_all = TRUE) |> 
  select(!c(p_o_1_r_1:o_2_r_3, attended, O1, O2, smp, switch))

