# Week of year calculation using CDC epidemiological weeks

dates = c(paste0('2020-12-', 20:31),
          paste0('2021-01-', 1:10))

epiweeks = lubridate::epiweek(strptime(dates, format="%Y-%m-%d"))

isoweeks = lubridate::isoweek(strptime(dates, format="%Y-%m-%d"))

simpleweeks = lubridate::week(strptime(dates, format="%Y-%m-%d"))



data.frame(dates, epiweeks, isoweeks, simpleweeks)
