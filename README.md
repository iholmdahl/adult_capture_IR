# adult_capture_IR
Model code for "Adult-capture assays as an effective tool to measure insecticide resistance in Anopheles malaria vectors: a modeling comparison with larval-capture assays"

All analyses and figures run in R.  

##Full workflow: 
###Setting up parameters
`fit_gompertz.R`: fit gompertz-makeham mortality function to daily mortality data for each mosquito genotype/exposure

###Run Model
`Mosquito_population.R`: mosquito population model: this constructs and runs the simulated mosquito population
`save_data.R`: reads and saves files constructed with mosquito_population.R

###Simulate sampling from the model
`sampling_functions.R`: functions to simulate larval or adult sampling from mosquito population  

`analysis_dataset.R`: restricts data and runs sampling functions to construct analysis dataset  
`analysis.R`: builds and evaluates performance of adult sampling correction model  

`test_performance_function.R` : sets up sensitivity, specificity, NPV and PPV tests
`Manuscript Figures.R` : code for manuscript figures  
