# adult_capture_IR
Adult-capture for insecticide resistance: model code

All analyses and figures done in R.

Mosquito_population.R: mosquito population model
fit_gompertz.R: fit gompertz-makeham mortality function to daily mortality data for each mosquito genotype/exposure
sampling_functions.R: functions to simulate larval or adult sampling from mosquito population
analysis_dataset.R: restricts data and runs sampling functions to construct analysis dataset
analysis.R: builds and evaluates performance of adult sampling correction model
Manuscript Figures.R : code for manuscript figures 
