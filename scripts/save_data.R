
## load / save baseline data 

filepath <- "archive/baseline_data/"
filenames <- list.files(filepath)

data <- c()

for(file in filenames){
  data <- read_csv(paste0(filepath, file), show_col_types = FALSE) %>%
    bind_rows(data)
}

write_rds(data, paste0("results/population_main.rds"))

## load / save other sensitivity analyses

filepath <- "archive/5.10_files/"
filenames <- list.files(filepath)

sensitivity_analyses = c("int_mortality", 
                         "larvae_0.01", 
                         "larvae_0.05", 
                         "longevity")

for(a in sensitivity_analyses){
  data <- c()
  files_subset <- dplyr::filter(data.frame(filenames), grepl(a, filenames))
  for(file in files_subset){
    data <- read_csv(paste0(filepath, file), show_col_types = FALSE) %>%
      bind_rows(data)
  }
  write_rds(data, paste0("results/population_",a,".rds"))
}

## load / save other sensitivity analyses
filepath <- "archive/3.12_files/"
filenames <- list.files(filepath)
other_sensitivity_analyses <- c("three", 
                                "mortality")
for(a in other_sensitivity_analyses){
  
  data <- c()
  files_subset <- dplyr::filter(data.frame(filenames), grepl(a, filenames))
  for(file in files_subset){
    data <- read_csv(paste0(filepath, file), show_col_types = FALSE) %>%
      bind_rows(data)
  }
  write_rds(data, paste0("results/population_",a,".rds"))
  
}

## load in dominance sensitivity analysis 

filepath <- "archive/dominance/"
filenames <- list.files(filepath)

data <- c()

for(file in filenames){
  
  file_string <- str_extract(file, '.*(?=\\.csv$)')
  d <- stringr::str_extract(file_string, "(\\d+$)")
  
  data <- read_csv(paste0(filepath, file), show_col_types = FALSE) %>%
    mutate(dominance = d) %>%
    bind_rows(data)
}

data <- data %>%
  mutate(dominance = case_when(dominance == "5" ~ 0.5, 
                               TRUE ~ as.numeric(dominance)/100))

for(d in c(0.25, 0.5, 0.75)){
  
  if(d == 0.25){
    label <- "low"
  } else if(d == 0.75){
    label <- "high"
  } else(){
    label <- d
  }
  write_rds(filter(data, dominance == d), 
            paste0("results/population_dominance_", label, ".rds"))
}
