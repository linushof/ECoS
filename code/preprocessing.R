# prep -------------------------------------------------------------

# pkgs 
pacman::p_load(tidyverse, readxl, mirai)

# helper functions
source('code/helpers/fun_rwp.R')
source('code/helpers/fun_compute_cumulative_stats.R')

# data 

# problems
s1_problems <- read_xlsx('data/raw/problems/study_1_problems.xlsx')
s2_problems <- read_xlsx('data/raw/problems/study_2_problems.xlsx')

# sampling and choice
s1_files <- list.files('data/raw/study_1/', pattern = '.csv', full.names = T) # detect individual data sets
s2_files <- list.files('data/raw/study_2/', pattern = '.csv', full.names = T) 

daemons(6) # to parallelize map() computations 
s1_data <- s1_files |> map(in_parallel(\(x) readr::read_csv(x)), .progress = T)     
s2_data <- s2_files |> map(in_parallel(\(x) readr::read_csv(x)), .progress = T)     
daemons(0)

# tidy data  -----------------------------------------------------------------

## problems ----------------------------------------------------------------

# study 1
s1_problems <- s1_problems |> filter(ID <= 120) # remove practice trials

# study 2
s2_problems <- s2_problems |> 
  filter(phase!='practice') |> # remove practice trials 
  mutate(ID = case_when(phase=='test' ~ ID + 37 , # assign test problems the same IDs as in study 1 (81-120)
                        phase=='training1' | phase=='training2'  ~ ID + 117) # assign new training problems IDs (121-160)
         ) |> 
  select(!c(phase:correct_FW))


# combine and prepare
problems <- bind_rows(s1_problems, s2_problems) |> 
  distinct(ID, .keep_all = T) |> # remove duplicates (problems 81-120 included in both studies)
  rename(# assign descriptive, short names
    problem = 'ID' , 
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
  mutate(# use descriptive short values
    complexity = case_match(complexity, 'LC' ~ 'low', 'MC' ~ 'medium', 'HC' ~ 'high') ,
    long = case_match(long, 'O1' ~ 'o1', 'O2' ~ 'o2') , 
    short = case_match(short, 'O1' ~ 'o1', 'O2' ~ 'o2')
    ) |>
  rowwise() |> 
  mutate(# compute roundwise winning probabilities
    o1_rwp = rwp(o1_O = c(o1_1, o1_2, o1_3) , 
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
  imap(\(part, i) part |>
        rename(# assign short descriptive variable names
          goal = 'aim' ,
          complexity = 'CP' ,
          problem = 'ID' ,
          trial = 'Trialround' ,
          o1 = 'O1' , 
          o2 = 'O2' , 
          choice = 'selected'
          ) |> 
         mutate(
           age = as.double(ifelse(length(unique(c(age, age.text)))==2, na.omit(unique(c(age, age.text))), NA)) ,
           gender = as.character(ifelse(length(unique(c(gender, gender.text)))==2, na.omit(unique(c(gender, gender.text))), NA)) ,
           study = 's1' , 
           mouse = 'last' , 
           phase = 'test' , 
           attended = case_when(is.na(choice) & is.na(o1) ~ 'o2' , # sampled option
                                is.na(choice) & is.na(o2) ~ 'o1') ,
           participant = as.character(participant) , # coding error: two participants have same participant ID
           part_short = paste0('part',i) # assign each participant a unique, short ID (1-185)
           ) |> 
        select(
          study , 
          participant ,
          part_short ,
          age ,
          gender ,
          goal , 
          complexity , 
          mouse , 
          phase , 
          trial , 
          problem , 
          attended , 
          o1 , 
          o2 , 
          choice
          ) |> 
        filter(!is.na(problem)) |> # remove rows without sampling and choice data
        group_by(problem) |> 
        mutate(# complete missing values
          goal = last(goal) , 
          trial = last(trial), 
          choice = if_else(is.na(choice), last(choice), choice)
          ) |> 
        ungroup() |> 
        separate_wider_delim(# obtain session/condition information from participant ID
          participant, delim = '_' ,
          names = c('date', 'session', 'computer', 'training'), 
          too_few = 'align_start',
          too_many = 'drop', 
          cols_remove = F
          )
       ) 

# study 2
s2_dat <- s2_data |>  
  imap(\(part, i) part |>
        rename(# assign short descriptive variable names (as above) 
          goal = 'aim' ,
          complexity = 'CP' , 
          problem = 'ID' , 
          phase = 'phase...19' , # duplicated 'phase' variable
          trial = 'round' , 
          o1 = 'O1' , 
          o2 = 'O2' , 
          choice = 'response'
          ) |> 
        mutate(
          age = as.double(ifelse(length(unique(c(age, age.text)))==2, na.omit(unique(c(age, age.text))), NA)) ,
          gender = as.character(ifelse(length(unique(c(gender, gender.text)))==2, na.omit(unique(c(gender, gender.text))), NA)) ,
          goal = nth(goal, 2) , # copy goal value from 2nd row to all 
          problem = if_else(is.na(problem), lag(problem), problem) , # copy problem information to last row
          study = 's2_3' ,
          mouse = 'neutral', 
          attended = case_match(attended, 'O1' ~ 'o1', 'O2' ~ 'o2') , 
          part_short = paste0('part',i+length(s1_dat))
          ) |> 
        select(
          study ,
          participant ,
          part_short ,
          age ,
          gender ,
          goal , 
          complexity , 
          mouse , 
          phase , 
          trial , 
          problem , 
          attended , 
          o1 , 
          o2 , 
          choice
          ) |> 
        filter(! ( is.na(phase) | phase == 'practice') ) |> # remove practice trials and rows without sampling/choice data
        group_by(phase, problem) |> 
        mutate(
          trial = if_else(is.na(trial), last(trial), trial) , 
          complexity = if_else(is.na(complexity), lag(complexity), complexity) ,
          choice = if_else(is.na(choice), last(choice), choice)
          ) |>
        ungroup() |> 
        separate_wider_delim(# obtain session/condition information from participant ID
          participant, delim = '_', 
          names = c('date', 'session', 'computer', 'training'), # participant codes including '_T' string indicate training condition
          too_few = 'align_start',
          too_many = 'drop', 
          cols_remove = F
          ) |> 
        mutate(
          study = if_else(is.na(training), 's2', 's3') ,
          goal = if_else(as.integer(computer) %% 2 == 1, "EV", "FW"), # psychopy did not always code the goal: EV condition were run on odd computer numbers 
          problem = case_when(phase=='test' ~ problem + 37 , # reasssign problem IDs (as above)
                              phase=='training1' | phase=='training2'  ~ problem + 117)
          )
       )

# prepare dat -----------------------------------------------------

dat <- bind_rows(s1_dat, s2_dat) |> 
  mutate(
    gender = case_when(gender %in% c('m' , 'M', 'male', 'm\n') ~ 'm' , 
                       gender %in% c('FALSE' , 'female', 'Female', 'FEMALE', 'fermale', 'f\n') ~ 'w' ,
                       gender == 'd' ~ 'enby') , 
    training = case_match(training, NA ~ 'no', 'T' ~ 'yes') , 
    goal = case_match(goal, 'EV' ~ 'long', 'FW' ~ 'short') , 
    complexity = case_match(complexity, 'LC' ~ 'low', 'MC' ~ 'medium', 'HC' ~ 'high') ,
    choice = case_match(choice, 'O1' ~ 'o1', 'O2' ~ 'o2' )
    ) |>
  filter(!is.na(attended)) |> # remove rows that only encode final choice (redundant)
  group_by(study, part_short, phase, trial) |> 
  mutate(# compute key sampling variables for each participant and trial separately
    smp = row_number() , # running (cumulative) sample size
    smp_total = max(smp) , # sample size trial  
    switch = ifelse(attended != lag(attended), 1, 0) , # switch indicator
    switch_total = sum(switch, na.rm = TRUE) , # number of switches trial 
    switch_rate = round(switch_total/(smp_total - 1), 3) , # switch rate trial
    o1_cm = mean(o1, na.rm=T) , # running (cumulative) mean option 1
    o1_sm = cummean2(o1, na.rm=T) , # sampled mean trial option 1
    o2_sm = mean(o2, na.rm=T) ,
    o2_cm = cummean2(o2, na.rm=T)
         ) |> 
  ungroup() |> 
  left_join(problems, join_by(problem, complexity)) |> # add problem info to each sample
  group_by(study, part_short, phase, trial) |> 
  mutate(# compute key sampling variables for each participant and trial separately
    o1_smp_total = sum(is.na(o2)) , # sample size option 1
    o2_smp_total = smp_total - o1_smp_total , # sample size option 2
    o1_sp1 = round(sum(if_else(o1 == o1_1, 1, 0), na.rm = TRUE)/o1_smp_total, 3) , # sampled probability option 1 outcome 1
    o1_sp2 = round(sum(if_else(o1 == o1_2, 1, 0), na.rm = TRUE)/o1_smp_total, 3) ,
    o1_sp3 = round(sum(if_else(o1 == o1_3, 1, 0), na.rm = TRUE)/o1_smp_total, 3) ,
    o2_sp1 = round(sum(if_else(o2 == o2_1, 1, 0), na.rm = TRUE)/o2_smp_total, 3) , 
    o2_sp2 = round(sum(if_else(o2 == o2_2, 1, 0), na.rm = TRUE)/o2_smp_total, 3) ,
    o2_sp3 = round(sum(if_else(o2 == o2_3, 1, 0), na.rm = TRUE)/o2_smp_total, 3)) |> 
  rowwise() |> 
  mutate(# roundwise winning probability based on sampled relative frequencies
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
  mutate(# compute short-run and long-run winner based on sampled information 
    long_s = case_when(o1_sm > o2_sm ~'o1' ,
                       o1_sm < o2_sm ~'o2' ,
                       o1_sm == o2_sm ~ NA ) ,
    short_s = case_when(o1_srwp > o2_srwp ~'o1' ,
                        o1_srwp < o2_srwp ~'o2' ,
                        o1_srwp == o2_srwp ~ NA )
    )

# store data --------------------------------------------------------------

write_csv(dat, 'data/clean/sampling.csv')

## choices -----------------------------------------------------------------
## choices contains the trial summaries (without sampled outcomes)

choices <- dat |> 
  filter(smp == smp_total) |> # only keep last row of each trial 
  mutate(# encode whether a correct choice according to the decision goal was made (based on ground truth and sampled information)
    correct_ground = case_when(goal=='long' & choice==long ~ 1 , 
                               goal=='long' & choice!=long ~ 0 ,
                               goal=='short' & choice==short ~ 1 , 
                               goal=='short' & choice!=short ~ 0 
                               ) ,
    correct_sampled = case_when(goal=='long' & choice==long_s ~ 1 , 
                                goal=='long' & choice!=long_s ~ 0 ,
                                goal=='short' & choice==short_s ~ 1 ,
                                goal=='short' & choice!=short_s ~ 0
                                ) , 
    problem_type = long==short
    ) |> 
  select(
    study, participant, part_short, age, gender, goal, complexity, mouse, training, phase, 
    trial, problem,
    smp_total, switch_total, switch_rate, choice, correct_ground, correct_sampled , 
    long, short, problem_type,
    long_s, short_s
    )
write_csv(choices, 'data/clean/choices.csv')

