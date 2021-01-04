# Grab data from auto-export bucket and save to both local machine and network drive (for BTS outputs)
# Depends on proper setup of SDC authentication script. Permissions are granted by SDC administrators.
# Buckets updated for ECS

# The auth script is a Python script. We can call it in R using reticulate. This depends on a miniconda installation at 
# C:/Users/{user.name}/AppData/Local/r-miniconda, or you can choose another version of Python installed. See miniconda_path().
# See https://rstudio.github.io/reticulate/articles/python_packages.html


library(reticulate)

# First time: Install the conda environment with necessary packages

if(!dir.exists(
      file.path(dirname(path.expand('~/')),
              'AppData', 'Local', 'r-miniconda'))) {
  conda_create('r-reticulate')
  conda_install('requests')
  conda_install(envname = 'r-reticulate', packages = c('requests', 'configparser'))
  }

use_virtualenv("r-reticulate")

# Setup the paths to work in

auto_export_bucket = 's3://prod-sdc-waze-autoexport-004118380849/alert/'

volpe_drive = '//vntscex.local/DFS/Projects/PROJ-OS62A1/SDI Waze Phase 2/Output/COVID'

code_loc = '~/git/covid_waze'
local_dir = file.path(code_loc, 'Output', Sys.Date())

if (!dir.exists(local_dir)) {
  dir.create(local_dir) 
  }

# Fetch data by refreshing token in Python using Reticulate ----

if(!file.exists(file.path(path.expand(code_loc),
                          'utility',
                          'auto_export_waze.py'))){
  stop('Contact sdc-support@dot.gov to set up auto-export permissions and be given the appropriate authentication script.')
}

# Refresh credentials 
reticulate::source_python(file = file.path(path.expand(code_loc),
            'utility',
           'auto_export_waze.py'))

system(
  paste0('aws --profile sdc-token s3 ls ', auto_export_bucket, Sys.Date(), '/'))

# Files to get

get_files = c('Waze_2020_MSA_day.csv',
              'Waze_2020_MSA_week.csv',
              'Waze_2020_National_day.csv',
              'Waze_2020_National_week.csv',
              paste0('Waze_Covid_joined_', Sys.Date(), '.csv'))

for(file in get_files){
  
  if(grepl(Sys.Date(), file)){
    out_file = 'Waze_Full.csv'
  } else {
    out_file = file
  }
  
  system(
    paste0('aws --profile sdc-token s3 cp ', 
           auto_export_bucket, 
           Sys.Date(), '/',
           file,
           ' ',
           path.expand(local_dir), '/',
           out_file)
  )
  }


# Produce weekly index calculations ----

source('Analysis/Waze_Index_Calcs_Check.R')

# Replace files on Volpe shared drive for Tableau ----

# Too slow. Just copy manually. 
# # Check to see if on the VPN 
# if(dir.exists(volpe_drive)){
#   
#   for(file in get_files){
#     print(paste('Copying', file))
#     
#     file.copy(from = file.path(path.expand(local_dir), file),
#               to = file.path(volpe_drive, file),
#               overwrite = TRUE)
#     
#     print('... done \n')
#     }
#   
#   } else {
#   
#     print('Connect to VPN and verify access to Volpe shared drive')
#   
#   }
