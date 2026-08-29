# Ecological and Cognitive Search (ECoS)

<!-- Preprint: <DOI> -->

In experience-based decision making under risk, people often actively sample information from the available options before making a choice. 
The functional role of switching between options during such predecisional sampling, however, remains poorly understood.
We propose the ecological and cognitive search hypothesis (ECoS), which distinguishes between ecological search (sampling information from the external environment) and cognitive search (processing sampled information in memory to construct goal-relevant mental representations). 
ECoS posits that different decision goals require different mental representations and that the cognitive costs of building these representations depend on how ecological search is organized over options over time. 
Consequently, people should adapt their switching behavior to the current decision goal. 
We tested ECoS in three preregistered experiments (total N = 305) in which participants made decisions from experience in the sampling paradigm and were instructed either to identify the option with the higher expected value (long-term goal) or the option that is more likely to yield the better outcome on the next draw (short-term goal). 
Consistent with ECoS, Experiments 1 and 2 showed that frequent switching was associated with better performance under the short-term goal but impaired performance under the long-term goal. 
Moreover, participants switched more frequently when pursuing the short-term goal, indicating adaptive organization of ecological search. 
Experiment 3 further showed that explicit feedback on goal performance during a training phase selectively increased switching under the short-term goal, highlighting how adaptivity in ecological search can be enhanced.

## How to reproduce this project?

To ensure full reproducibility, recreate the software setup using the following steps:

-   Install and select `R version 4.3.3` in `RStudio` under `Tools > Global Options > General`
-   Download the repo via Fork & Clone or ZIP Download (`<> Code` button) and initialize it as R project
-   Run `renv::restore()` to install all required packages/dependencies with the same version
-   Rerun analyses

All data sets in the [data folder](https://github.com/linushof/ECoS/tree/main/data) can be reproduced using the code scripts in the [code folder](https://github.com/linushof/sampling-strategies/tree/main/code). You can reproduce all data sets from scratch or just specific data sets. Browse the [documentation](https://github.com/linushof/sampling-strategies/tree/main/documentation) for details about each data set, code script, and relevant file dependencies.
