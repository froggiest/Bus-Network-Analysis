# Bus connection probability

In this repository I create a binomial probability model that calculates the probability of arriving at a station and catching a bus. Specifically, this model does the following:
1. Find the nearest stations to each given station and select which out of these has the highest number of unique routes ("desired" station): `2.0-desired-stations.R`
2. Take each given station and a time span (peak vs off-peak hours) and search for departures from the given toward the "desired" station within the next 10 minutes of every minute within the time span: `3.0-probability-model.R`
3. Count every minute where there is at least one departure meeting this criteria, as a "success", and divide the total number of successes by the total number of minutes tested: `3.0-probability-model.R`

### Tools Used

* R (RStudio)

### Project Structure

This is the workflow structure for this repository:

* data/ raw and processed datasets
* ingestion/ data loading and cleaning
* eda/ exploratory data analysis
* analysis/ modeling and results
* reports/ final outputs and presentations
