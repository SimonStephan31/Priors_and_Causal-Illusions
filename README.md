# Manipulating Prior Causal Beliefs Affects the Formation of Apparent Causal Illusions

This repository contains all materials for the paper "Manipulating Prior Causal Beliefs Affects the Formation of Apparent Causal Illusions".

## Repository structure

```
+-- Exp_1
|   +-- data_and_analyses
|   |   +-- renv
|   |   +-- .Rprofile
|   |   +-- data.csv
|   |   +-- data_and_analyses.Rproj
|   |   +-- Exp1_Analysis.qmd
|   |   +-- README.Rmd
|   |   +-- renv.lock
|   |   \-- SI_model_fun.R
|   +-- experiment_files
|   |   +-- img
|   |   +-- jspsych
|   |   \-- Exp.html
|   \-- Pilot-Study-of-Exp-1
|       +-- data_and_analyses
|       +-- experiment_files
|       \-- PilotStudy_Summary.pdf
+-- Exp_2
|   +-- data_and_analyses
|   |   +-- Figures
|   |   +-- renv
|   |   +-- .Rprofile
|   |   +-- data.csv
|   |   +-- data_and_analyses.Rproj
|   |   +-- Exp2_Analysis.qmd
|   |   +-- README.Rmd
|   |   +-- renv.lock
|   |   \-- SI_model_fun.R
|   \-- experiment_files
|       +-- img
|       +-- jspsych
|       \-- Exp.html
+-- Exp_3
|   +-- data_and_analyses
|   |   +-- Figures
|   |   +-- renv
|   |   +-- .Rprofile
|   |   +-- data.csv
|   |   +-- data_and_analyses.Rproj
|   |   +-- Exp3_Analysis.qmd
|   |   +-- README.Rmd
|   |   +-- renv.lock
|   |   \-- SI_model_fun.R
|   \-- experiment_files
|       +-- img
|       +-- jspsych
|       \-- Exp.html
+-- Exp_4
|   +-- data_and_analyses
|   |   +-- Figures
|   |   +-- renv
|   |   +-- .Rprofile
|   |   +-- Analysis.qmd
|   |   +-- data.csv
|   |   +-- data_and_analyses.Rproj
|   |   +-- Exp4_Analysis.qmd
|   |   +-- README.Rmd
|   |   \-- renv.lock
|   \-- experiment_files
|       +-- img
|       +-- jspsych
|       \-- Exp.html
+-- Exp_Supp
|   +-- data_and_analyses
|   |   +-- Figures
|   |   +-- renv
|   |   +-- .Rprofile
|   |   +-- data.csv
|   |   +-- data_and_analyses.Rproj
|   |   +-- Exp_Supp_Analysis.qmd
|   |   +-- README.Rmd
|   |   \-- renv.lock
|   +-- experiment_files
|   |   +-- img
|   |   +-- jspsych
|   |   \-- Exp.html
|   \-- Supplementary-Study-Summary.pdf
+-- SI_Model
    +-- SI_model_fun.R
    \-- SI_predictions_generator.qmd
```

# Experiment folders (Exp_1, Exp_2, Exp_3, Exp_4, Exp_Supp)
    # data_and_analysis
        Contains data analysis files, including the function code used to generate the normative model predictions. Look at the included README.Rmd files for more details on the data analysis and how to run it. 
    # experiment_files 
        Contains the experiment files (JsPsych). Download that folder to run a demo the experiment offline in your browser. 

# SI_Model
    Contains the function code (SI_model_fun.R) used to generate the normative model predictions, as well as a QMD file (SI_predictions_generator.qmd) that can be used to generate the predictions for different data sets as well as different causal structure and parameter priors.


