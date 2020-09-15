# covid_waze project

Some simplifying assumptions:

-	We are currently using counties as our spatial unit, and days as our time unit. Hourly Waze data has also been prepared. 


## Data Sources

- Waze: Compiled counts of Waze alerts by type, by county, by day and hour, since inception of SDC dataset.

- covid-19 cases: NYT counts of cases by county and day from their [GitHub repository](https://github.com/nytimes/covid-19-data)

- State declarations: Manually compiled set of emergency declarations. For the restrictions tracking, we use the New York Times page “See Which States are Reopening and Which are Still Shut Down” (https://www.nytimes.com/interactive/2020/us/states-reopen-map-coronavirus.html). This page is updated daily and is broken down with a list of states that are easing restrictions with links attached to other relevant articles. For instance, this article is linked under Alabama’s reopening update (https://www.al.com/news/2020/04/gov-kay-ivey-press-conference-on-plans-for-reopening-alabama-watch-live.html). Each day we check the articles that are linked for updated information and also check the information in the data sheet to ensure it is correct. If there is a new state opening or new information on a state that has been opened, that information is put into the appropriate column. 

## Data end points

- Box.com: Master copies of data files are being saved to [Box](https://volpe.app.box.com/folder/109014615387)

- internal.explore.dot.gov: Prototype dashboard/s are being published to the internal DOT Tableau server.

## Code Repositories

- SDC GitLab: `covid_waze` repository inside SDC is accessible to anyone with an SDC account. Code that deals with Redshift / curated Waze data lives here. This currently is set up to pull data from Redshift, apply spatial filtering to assign Waze events to your, apply time zones to each Waze event, and aggregate to county/day/hour. 

- GitHub: This `covid_waze` repository. Currently for all other work.

## How to use this code

Launch RStudio using the `covid_waze.Rproj` file.

1. Run `Prep/Join_Data.R`. This will create the daily, aggregated counts of Waze alerts by count, fill in 0's as appropriate for Waze alert types where at least one other alert type was noted in that county, for that day, and join with nytimes covid data.

2a. `Analysis/Generate_expected_counts.R`
  - Calls `Analysis/RandomForest_WazeGrid_Fx.R`.
  - This can be skipped to just use Jan-Feb mean counts as baseline, or used to generate expected counts of Waze alerts by county and day with a RF model for each state. Counties are dummy variables. Estimates are close for just using mean counts versus RF models.

2b. `Analysis/Impact_index.calc.R`
 - Calculates mean values for baseline period and calculates impact index values.

3. `Analysis/Index_Outlier_Cleaning.R`
