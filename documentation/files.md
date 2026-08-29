## File Descriptions

### code/ 


| File                             | Description                                                                                                    |
|----------------------------------|----------------------------------------------------------------------------------------------------------------|
| `preprocessing.R`                | Code for reading, merging, and pre-processing the individual participants' raw data sets from [data/raw/](https://github.com/linushof/ECoS/tree/main/data/raw). Generates data sets in [data/clean/](https://github.com/linushof/ECoS/tree/main/data/clean).|                                                                                                                         |
| `analyses.R`                     | Code underlying all descriptive and inferential results (incl. figures and tables) reported in the manuscript. Generates model outputs in [fits/](https://github.com/linushof/ECoS/tree/main/fits). |



### data/


| Folder   | Subfolder   | Files                                           | Description                                                                                                                                                                                         | Repository | Release  |
|----------|-------------|-------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------|----------|
| `clean/` |             | `choices.csv`                                   | Trial summaries (switch rate, choice, etc.) from all experiments.<br/><br/>Each row is a single trial from one participant on one problem in one experiment.<br/> The data set entails all relevant data for the reported analyses.<br/><br/>[Codebook](https://github.com/linushof/ECoS/blob/main/documentation/codebook.md) has details | [X]        | [X]      |
|          |             | `problems.csv`                                  | Description of all choice problems (discrete payoff distributions) used in the experiments.<br/><br/>[Codebook](https://github.com/linushof/ECoS/blob/main/documentation/codebook.md)                                                                          | [X]        | [X]      |
|          |             | `sampling.csv`                                  | All data (including sampling decisions and sampled outcomes) collected in all experiments.<br/><br/>Each row is a single sampling decision within one trial from one participant in one experiment (each trial comprises of multiple sampling decisions/rows).<br/><br/>[Codebook](https://github.com/linushof/ECoS/blob/main/documentation/codebook.md)  | [X]        | [X]      |
| `raw/`   | `study_1/`  | `#DATE_#SESSION_#MACHINE_Testing_timestemp.csv` | Individual participants' raw data sets (PsychoPy output) from Experiment 1.<br/><br/>Each `.csv` file entails all data collected for a single participant, with the file name indicating the test date, test session and lab machine the participant worked on (participant identifier). |            | [x]      |
|          | `study_2/`  | `#DAY_#SESSION_#MACHINE*_Testing_timestemp.csv` | Individual participants' raw data set from Experiments 2 and 3<br/><br/>`* = blank`: no training (Exp. 2)<br/>`* = _T`: with training (Exp. 3)                                                                                                                                    |            | [x]      |
|          | `problems/` | `study_*_problems.csv`                          | Description of choice problems used in the practice trials, test trials and the training phase.<br/><br/>`* = 1`: Exp. 2<br/>`* = 2`: Exps. 2 and 3                                                                                                      |            | [x]      |


### fits/


| File         | Description                                             | Repository           | Release              |
|--------------|---------------------------------------------------------|----------------------|----------------------|
| `s*_m1.rds`  | Logistic regression models for Experiments 1-3.         |                      | [x]                  |
| `s*_m2*.rds` | One-inflated beta models for Experiments 1-3.<br/><br/> `* = blank`: full model with random effects<br/> `* = _reduced`: reduced model without random effects   |                      | [x]                  |
| `s1_m*.rds`  | Models for the complexity analysis of Experiment 1.<br/><br/>`*=3`: Logistic regression model<br/>`*=4`: One-inflated beta model     |                      | [x]                  |
| `s3_m5.rds`  | Spline model for the training analysis of Experiment 3. |                      | [x]                  |


### PsychoPy.7z

Archive containing the three self-contained PsychoPy experiments (`.psyexp` file + materials). 
Experiments were build and run in the PsychoPy builder version `v.2024.2.4`
