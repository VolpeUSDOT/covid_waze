# Grab data from auto-export bucket and upload to box
# Depends on proper setup of SDC authentication script. Permissions are granted by SDC administrators.

# The auth script is a Python script. We can call it in R using reticulate. This depends on a miniconda installation at 
# C:/Users/{user.name}/AppData/Local/r-miniconda, or you can choose another version of Python installed. See miniconda_path().
# See https://rstudio.github.io/reticulate/articles/python_packages.html

# First time:
# conda_create('r-reticulate')
# conda_install('requests)
# conda_install(envname = 'r-reticulate', packages = c('requests', 'configparser'))

library(reticulate)

use_virtualenv("r-reticulate")

auto_export_bucket = 's3://prod-sdc-waze-autoexport-911061262852-us-east-1-bucket/alert/'


MANUAL = FALSE

code_loc = '~/git/covid_waze'
local_dir = file.path(code_loc, 'Output', Sys.Date())

if (!dir.exists(local_dir)) {
  dir.create(local_dir) 
  }

# path.expand(local_dir)

# using Reticulate ----

# Refresh credentials 
reticulate::source_python(file = file.path(path.expand(code_loc),
            'utility',
           'waze_token_refresh.py'),
           envir = 'r-reticulate')

system(
  paste0('aws --profile sdc-token s3 ls ', auto_export_bucket, Sys.Date(), '/'))

system(
  paste0('aws --profile sdc-token s3 cp ', 
         auto_export_bucket, 
         Sys.Date(), '/',
         'Waze_2020_MSA_day.csv',
         ' ',
         path.expand(local_dir), '/',
         'Waze_2020_MSA_day.csv')
)

system(
  paste0('aws --profile sdc-token s3 cp ', 
         auto_export_bucket, 
         Sys.Date(), '/',
         'Waze_2020_MSA_week_county.csv',
         ' ',
         path.expand(local_dir), '/',
         'Waze_2020_MSA_week_county.csv')
)

system(
  paste0('aws --profile sdc-token s3 cp ', 
         auto_export_bucket, 
         Sys.Date(), '/',
         'Waze_2020_MSA_day_county.csv',
         ' ',
         path.expand(local_dir), '/',
         'Waze_2020_MSA_day_county.csv')
)

system(
  paste0('aws --profile sdc-token s3 cp ', 
         auto_export_bucket, 
         Sys.Date(), '/',
         'Waze_2020_MSA_week.csv',
         ' ',
         path.expand(local_dir), '/',
         'Waze_2020_MSA_week.csv')
)

system(
  paste0('aws --profile sdc-token s3 cp ', 
         auto_export_bucket, 
         Sys.Date(), '/',
         'Waze_2020_National_day.csv',
         ' ',
         path.expand(local_dir), '/',
         'Waze_2020_National_day.csv')
)

system(
  paste0('aws --profile sdc-token s3 cp ', 
         auto_export_bucket, 
         Sys.Date(), '/',
         'Waze_2020_National_week.csv',
         ' ',
         path.expand(local_dir), '/',
         'Waze_2020_National_week.csv')
)

system(
  paste0('aws --profile sdc-token s3 cp ', 
         auto_export_bucket, 
         Sys.Date(), '/',
         'Waze_2020_Index_cleaned.csv',
         ' ',
         path.expand(local_dir), '/',
         'Waze_2020_Index_cleaned.csv')
)


# Also get joined data for full data dashboard
system(  
  paste0('aws --profile sdc-token s3 cp ', 
         auto_export_bucket, 
         Sys.Date(), '/',
         'Waze_Covid_joined_', Sys.Date(),'.csv',
         ' ',
         path.expand(local_dir), '/',
         'Waze_Covid_joined.csv')
)


# Using Anaconda Prompt ----

if(MANUAL){
  
  # # Contintue to paste this into Anaconda Prompt
  # writeClipboard(
  # paste0('python ',
  #                file.path(path.expand(code_loc), 'utility', 'DF_samlapi_formauth_adfs3_windows.py'))
  #         )
  
  # Trying new version
  writeClipboard(
    paste0('python ',
           file.path(path.expand(code_loc), 'utility', 'waze_token_refresh.py'))
  )
  
  system(paste0('aws --profile sdc s3 ls ', auto_export_bucket))
  
  writeClipboard(
  paste0('aws --profile sdc-token s3 ls ', auto_export_bucket))
  
  writeClipboard(
    paste0('aws --profile sdc s3 ls ', auto_export_bucket, Sys.Date(), '/'))
  
  # If get 'config profile could not be found', then re-run profile setup script.
  # TODO: figure out why this works in Git Bash or Anaconda Prompt but not in R
  # This does require installaiton of AWS CLI for Windows. 
  # https://docs.aws.amazon.com/cli/latest/userguide/install-windows.html#install-msi-on-windows
  # 
  # writeClipboard(
  #   paste0('aws --profile sdc s3 cp ', 
  #          auto_export_bucket, 
  #          'Compiled_county_counts_2020-06-11.zip',
  #          ' ',
  #          path.expand(local_dir), '/',
  #          'Compiled_county_counts_2020-06-11.zip')
  # )
  
  writeClipboard(
    paste0('aws --profile sdc s3 cp ', 
           auto_export_bucket, 
           Sys.Date(), '/',
           'Waze_2020_MSA_day.csv',
           ' ',
           path.expand(local_dir), '/',
           'Waze_2020_MSA_day.csv')
  )
  
  writeClipboard(
    paste0('aws --profile sdc s3 cp ', 
           auto_export_bucket, 
           Sys.Date(), '/',
           'Waze_2020_MSA_week.csv',
           ' ',
           path.expand(local_dir), '/',
           'Waze_2020_MSA_week.csv')
  )
  
  writeClipboard(
    paste0('aws --profile sdc s3 cp ', 
           auto_export_bucket, 
           Sys.Date(), '/',
           'Waze_2020_National_day.csv',
           ' ',
           path.expand(local_dir), '/',
           'Waze_2020_National_day.csv')
  )
  
  writeClipboard(
    paste0('aws --profile sdc s3 cp ', 
           auto_export_bucket, 
           Sys.Date(), '/',
           'Waze_2020_National_week.csv',
           ' ',
           path.expand(local_dir), '/',
           'Waze_2020_National_week.csv')
  )
  
  writeClipboard(
    paste0('aws --profile sdc s3 cp ', 
           auto_export_bucket, 
           Sys.Date(), '/',
           'Waze_2020_Index_cleaned.csv',
           ' ',
           path.expand(local_dir), '/',
           'Waze_2020_Index_cleaned.csv')
  )
    
    
  # Also get joined data for full data dashboard
  # Assumed this script is run on same day as reresh script within SDC.
  writeClipboard(  
    paste0('aws --profile sdc s3 cp ', 
           auto_export_bucket, 
           Sys.Date(), '/',
           'Waze_Covid_joined_', Sys.Date(),'.csv',
           ' ',
           path.expand(local_dir), '/',
           'Waze_Covid_joined.csv')
    )
}