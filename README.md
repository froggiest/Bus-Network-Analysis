# Bus connection success rate

In this repository I create a model that calculates the success rate for, in a given time span, randomly arriving at a station and catching a bus to a "useful" station within 10 minutes. Specifically, this model does the following:
1. Find the nearest stations to each given station and select which out of these has the highest number of unique routes ("desirable direction")
2. Take each given station and a time span (peak vs off-peak hours) and search for departures from the given station toward the "desirable direction" station within the next 10 minutes of every minute within the time span (1 + 2: [2.0-desirable-directions.R](https://github.com/froggiest/Bus-Network-Analysis/blob/main/01-ingestion/scripts/2.0-desirable-directions.R))
3. Count every minute where there is at least one departure meeting this criteria as a "success", and divide the total number of successes by the total number of minutes tested ([`3.0-success-rate-calculation.R`](https://github.com/froggiest/Bus-Network-Analysis/blob/main/03-analysis/scripts/3.0-success-rate-calculation.R))

### Tools Used

* R (RStudio)

### Project Structure

This is the workflow structure for this repository:

* data/ raw and processed datasets
* ingestion/ data loading and cleaning
* eda/ exploratory data analysis
* analysis/ modeling and results
* reports/ final outputs and presentations
