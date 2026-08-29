## File Descriptions

### R code 


| File                             | Description                                                                                                    |
|----------------------------------|----------------------------------------------------------------------------------------------------------------|
| `preprocessing.R`                | Code for reading, merging, and pre-processing the individual participants' raw data sets from data/raw/. <br/><br/> Generates data sets in [data/clean/](https://github.com/linushof/ECoS/tree/main/data/clean).|                                                                                                                         |
| `analyses.R`                     | Code underlying all descriptive and inferential results (incl. figures and tables) reported in the manuscript. |



### Data


+----------+-------------+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+------------+----------+
| Folder   | Subfolder   | Files                                           | Description                                                                                                                                                                                         | Repository | Release  |
+==========+=============+=================================================+=====================================================================================================================================================================================================+============+==========+
| `clean/` |             | `choices.csv`                                   | Trial summaries (switch rate, choice, etc.) from all experiments.                                                                                                                                   | [X]        | [X]      |
|          |             |                                                 |                                                                                                                                                                                                     |            |          |
|          |             |                                                 | Each row is a single trial from one participant on one problem in one experiment. The data set entails all relevant data for the reported analyses.                                                 |            |          |
|          |             |                                                 |                                                                                                                                                                                                     |            |          |
|          |             |                                                 | Codebook has details                                                                                                                                                                                |            |          |
+----------+-------------+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+------------+----------+
|          |             | `problems.csv`                                  | Description of all choice problems (discrete payoff distributions) used in the experiments.                                                                                                         | [X]        | [X]      |
|          |             |                                                 |                                                                                                                                                                                                     |            |          |
|          |             |                                                 | Codebook has details                                                                                                                                                                                |            |          |
+----------+-------------+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+------------+----------+
|          |             | `sampling.csv`                                  | All data (including sampling decisions and sampled outcomes) collected in all experiments.                                                                                                          | [X]        | [X]      |
|          |             |                                                 |                                                                                                                                                                                                     |            |          |
|          |             |                                                 | Each row is a single sampling decision within one trial from one participant in one experiment (each trial comprises of multiple sampling decisions/rows).                                          |            |          |
|          |             |                                                 |                                                                                                                                                                                                     |            |          |
|          |             |                                                 | Codebook has details                                                                                                                                                                                |            |          |
+----------+-------------+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+------------+----------+
| `raw/`   | `study_1/`  | `#DATE_#SESSION_#MACHINE_Testing_timestemp.csv` | Individual participants' raw data sets (PsychoPy output) from Experiment 1.                                                                                                                         |            | [x]      |
|          |             |                                                 |                                                                                                                                                                                                     |            |          |
|          |             |                                                 | Each `.csv` file entails all data collected for a single participant, with the file name indicating the test date, test session and lab machine the participant worked on (participant identifier). |            |          |
+----------+-------------+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+------------+----------+
|          | `study_2/`  | `#DAY_#SESSION_#MACHINE*_Testing_timestemp.csv` | Individual participants' raw data set from Experiments 2 and 3                                                                                                                                      |            | [x]      |
|          |             |                                                 |                                                                                                                                                                                                     |            |          |
|          |             |                                                 | `* = blank`: no training (Exp. 2)                                                                                                                                                                   |            |          |
|          |             |                                                 |                                                                                                                                                                                                     |            |          |
|          |             |                                                 | `* = _T`: with training (Exp. 3)                                                                                                                                                                    |            |          |
+----------+-------------+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+------------+----------+
|          | `problems/` | `study_*_problems.csv`                          | Description of choice problems used in the practice trials, test trials and the training phase.                                                                                                     |            | [x]      |
|          |             |                                                 |                                                                                                                                                                                                     |            |          |
|          |             |                                                 | `* = 1`: Exp. 2                                                                                                                                                                                     |            |          |
|          |             |                                                 |                                                                                                                                                                                                     |            |          |
|          |             |                                                 | `* = 2`: Exps. 2 and 3                                                                                                                                                                              |            |          |
+----------+-------------+-------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+------------+----------+

### `brmsfit`

+--------------+---------------------------------------------------------+----------------------+----------------------+
| File         | Description                                             | Repository           | Release              |
+==============+=========================================================+======================+======================+
| `s*_m1.rds`  | Logistic regression models for Experiments 1-3.         |                      | [x]                  |
+--------------+---------------------------------------------------------+----------------------+----------------------+
| `s*_m2*.rds` | One-inflated beta models for Experiments 1-3.           |                      | [x]                  |
|              |                                                         |                      |                      |
|              | `* = blank`: full model with random effects             |                      |                      |
|              |                                                         |                      |                      |
|              | `* = _reduced`: reduced model without random effects    |                      |                      |
+--------------+---------------------------------------------------------+----------------------+----------------------+
| `s1_m*.rds`  | Models for the complexity analysis of Experiment 1.     |                      | [x]                  |
|              |                                                         |                      |                      |
|              | `*=3`: Logistic regression model                        |                      |                      |
|              |                                                         |                      |                      |
|              | `*=4`: One-inflated beta model                          |                      |                      |
+--------------+---------------------------------------------------------+----------------------+----------------------+
| `s3_m5.rds`  | Spline model for the training analysis of Experiment 3. |                      | [x]                  |
+--------------+---------------------------------------------------------+----------------------+----------------------+
