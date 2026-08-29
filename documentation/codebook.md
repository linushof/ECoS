### The Ecological and Cognitive Search Hypothesis: Adaptive Organization of Information Sampling in Experience-Based Risky Choice

#### Codebook Clean Data Files

*Linus Hof, Julian K. Schäfer, Tamara A. Boschetto, and Thorsten Pachur*


| Variable                                 | Description                                                                                                                                           |
|------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------|
| *Participants and Experimental Setup*    |                                                                                                                                                       |
| `study`                                  | experiment identifier<br/><br/>`s1` = Experiment 1<br/>`s2` = Experiment 2<br/>`s3` = Experiment 3                                                    |
| `participant`,`part_short`               | unique participant identifier                                                                                                                         |
| `age`,`gender`                           | participant demographics                                                                                                                              |
| `goal`                                   | goal condition of the participant<br/><br/>`short` = short-term<br/>`long` = long-term                                                                |
| `complexity`                             | complexity condition of the participant / complexity of problem<br/><br/>`low`/`LC`= low complexity<br/>`medium`/`MC` = medium complexity<br/>`high`/`HC` = high complexity |
| `mouse`                                  | indicates whether mouse cursor remained on the `last` sampled option or was reset to the `neutral` position                                           |
| `training`                               | indicates whether experiment involved a training phase with feedback<br/><br/>`yes`: with training (Exp. 3)<br/>`no`: without training (Exp. 1 & 2)   |
| *Choice problems (hidden distributions)* |                                                                                                                                                       |
| `problem`                                | choice problem identifier                                                                                                                             |
| `o#_#`                                   | possible outcomes of the options 1 and 2<br/><br/>`o1_1` = option 1, outcome 1<br/>`o1_2` = option 1, outcome 2<br/>`o2_1` = option 2, outcome 1      |
| `o#_p#`                                  | outcome probabilities                                                                                                                                 |
| `o#_ev`                                  | expected value (EV) of option \#                                                                                                                      |
| `o#_rwp`                                 | option \#'s probability of winning a pairwise comparison against the other option (aka roundwise winning probability, short-term winning probability) |
| `long`, `short`                          | indicates which option is the long-term winner (higher ev) or short-term winner (higher rwp)                                                          |
| `problem_type`                           | indicates whether one of the options is the short-term and long-term winner at the same time<br/><br/>`TRUE` = same option<br/>`FALSE` = different options |
| *Trials*                                 |                                                                                                                                                       |
| `phase`                                  | indicates the phase of the experiment<br/><br/>`practice` = instruction (3 trials per participant)<br/>`test` = test trial (40 trials)<br/>`block1` = training (20 trials)<br/>`block2` (optional) = training (20 trials)|                                                                                                         |
| `trial`                                  | trial number (indicates the randomized order of occurrence of problems during the experiment)                                                         |
| `smp`                                    | cumulative number (counter) of drawn samples in a given trial                                                                                         |
| `attended`                               | indicates which option was sampled from<br/><br/>`o1` = option 1<br/>`o2` = option 2                                                                  |
| `switch`                                 | indicates whether participant switched options to draw the current sample                                                                             |
| `o1`, `o2`                               | observed outcome for option 1 and option 2<br/><br/>`integer` if option was sampled<br/>`NA` if option was not sampled                                |
| `smp_total`,\                            | number of sampled outcomes across options,                                                                                                           |
| `o#_smp_total`                           | number of sampled outcomes per option (`o#`)                                                                                                          |
| `switch_total`                           | number of option switches during the sampling phase of a given trial                                                                                  |
| `switch_rate`                            | relative switching frequency in a given trial                                                                                                         |
| `choice`                                 | indicates which option was chosen<br/><br/>`o1` = option 1<br/>`o2` = option 2                                                                        |
| `correct_ground`                         | indicates whether participants chose the option that matched their decision goal<br/><br/>`0` = no match / false decision<br/>`1` = match / correct decision |
| *Sampled distributions (not used)*       |                                                                                                                                                       |
| `o#_sp#`                                 | sampled outcome probabilities                                                                                                                         |
| `o#_sm`                                  | sampled mean (considering all sampled outcomes of option \#)                                                                                          |
| `o#_cm`                                  | sampled cumulative mean (considering only outcomes sampled thus far)                                                                                  |
| `o#_srwp`                                | sampled roundwise probability                                                                                                                         |
| `long_s`, `short_s`                      | indicates the long-term and short-term winner when only considering the sampled outcomes                                                              |
| `correct_sampled`                        | alternative evaluation of goal attainment which considers only sampled outcomes                                                                       |
