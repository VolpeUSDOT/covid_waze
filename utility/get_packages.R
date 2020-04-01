# Get all necessary packages across data prep and analysis scripts 

loadpacks <- c(
  "kableExtra",
  "knitr",
  "lmer",
  "plotly",
   "tidyverse",
  "usmap"
  )

for(i in loadpacks){
  if(length(grep(i, (.packages(all.available=T))))==0) install.packages(i, dependencies =TRUE)
  }
rm(i, loadpacks)
