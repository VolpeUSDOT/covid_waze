# Grab data from auto-export bucket and upload to box

auto_export_bucket = 's3://prod-sdc-waze-autoexport-911061262852-us-east-1-bucket/alert/'


local_dir = paste0('~/git/covid_waze/Output/', Sys.Date())

if (!dir.exists(local_dir)) {
  dir.create(local_dir) 
  }

path.expand(local_dir)

system(paste0('aws --profile sdc s3 ls ', auto_export_bucket))

paste0('aws --profile sdc s3 ls ', auto_export_bucket)

paste0('aws --profile sdc s3 ls ', auto_export_bucket, Sys.Date(), '/')

# If get 'config profile could not be found', then re-run profile setup script.
# TODO: figure out why this works in Git Bash but not in R

system(
  paste0('aws --profile sdc s3 cp ', 
         auto_export_bucket, 
         Sys.Date(), '/',
         'Waze_2020_Index_cleaned.csv',
         ' ',
         path.expand(local_dir), '/',
         'Waze_2020_Index_cleaned.csv')
)
