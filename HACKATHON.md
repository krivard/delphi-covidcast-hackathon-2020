# CMU Delphi Hackathon 2020: Survey Processing Machinery

In partnership with the Information Networking Institute

June 26-28, 2020

## Starter Kit

If you have not used R before, we recommend [R Studio](https://rstudio.com/products/rstudio/download/) (the free version is good)

Some tutorials, which were not written by us:

* [Getting started in R and R Studio](https://ourcodingclub.github.io/tutorials/intro-to-r/) - a nice walkthrough of the IDE that also introduces the `dplyr` library
* [Quick-R](https://www.statmethods.net/r-tutorial/index.html) - a more old-school approach
* [Profiling in R Studio](https://support.rstudio.com/hc/en-us/articles/218221837-Profiling-with-RStudio) - a nice walkthrough of identifying speed and memory problems in the IDE
* [Measuring Performance in R](https://adv-r.hadley.nz/perf-measure.html) - a more old-school approach to speed and memory optimization

### Code

We have provided you with an R package in the `surveys` directory that can do the following:

* Read in raw survey data
* Filter and clean raw data
* Compute two statistics:
  * Household CLI
  * Community CLI
* Aggregate data into larger geographical regions by zip code
* Smooth data over a 7-day moving window
* Generate output files formatted for the COVIDcast API

It can also test itself against a small synthetic dataset included in the
`surveys/delphiSurveys/tests/testthat` directory.

To get started, follow the directions in `surveys/README.md`.

### Data

We have provided you with the following datasets in the `data` directory:

* `survey-questions/` - A copy of the survey questions for all language locales currently running in the US
* `survey-data.tgz` - Synthetic data for a period from April 6, 2020 to June 8, 2020. This file is too big for git, but you can fetch it from the [v1.0.1 release](https://github.com/krivard/delphi-covidcast-hackathon-2020/releases/tag/1.0.1-hackathon). Package includes:
  * `survey-data/input/` - Raw survey data
  * `survey-data/weights_in/` - Accompanying weights
  * `survey-data/receiving_gold/` - Correct output files for Household CLI and Community CLI
  * `survey-data/params.json.medium` - A params file to process the last 8 days of synthetic data; baseline running time 86 minutes on an 8-core machine with 32G of memory
  * `survey-data/params.json.large` - A params file to process all available synthetic data; baseline running time 60 hours on an 8-core machine with 32G of memory
* `covidcast_upload_spec.md` - Spec for the COVIDcast API file format
* `signal_descriptions.pdf` - Technical documentation for all computations


## Task

### Coverage

1. Compute a statistic for one of the 18 questions not already being analyzed
2. Aggregate the statistic into larger geographical regions by zip code
3. Smooth the data over a 7-day moving window
4. Generate output files formatted for the COVIDcast API

Repeat with as many questions as you like. Any statistic is fine (sum, average, the fraction choosing “Yes”, etc).

At judging time, we will test your code on synthetic raw survey data from April 6, 2020 to June 9, 2020 (so, an additional day of recorded data from what we have given you). We will check whether the outputs are in the proper format, and contain no invalid entries according to the spec.

Extra credit will be awarded if the choice of statistic is easy to modify. If you implement multiple additional questions, extra credit will be awarded if it is easy to select at runtime which question(s) to output.

### Speed

* Compute Household CLI and Community CLI as quickly as possible.

You may modify any part of the code to improve the speed, but the output must match the provided correct outputs. You may cache any data you like to disk, including staged output files. The time it takes to generate cached data will not be included in the final running time of your code.

At judging time, we will test your code on synthetic raw survey data from April 6, 2020 to June 9, 2020 (so, an additional day of recorded data from what we have given you). We will measure how long it takes your code to generate output files from April 6, 2020 to June 9, 2020, and we will check whether the outputs are correct.

Be careful! Survey responses can be submitted several days after they were started. This means the raw data file for June 9, 2020 must be able to affect the output for earlier days.

## Judging

The deadline to submit your solution is Sunday at 3pm US Eastern time (UTC-04:00).

To submit, fork this repository and submit a pull request on GitHub. Make sure your README file includes the following information:

* The names of all team members
* Which task(s) you attempted
* Any results you are proud of
* Any special instructions for running your code

No matter what task you chose, extra credit will be awarded for:

* Documentation - If you wrote new functions, are they commented? If your code supports new configuration options, did you describe how to use them? 
* Readability - Is your source code easy to understand? Did you divide it into meaningful functions?
* Usability - Is it easy to run your code? Is it easy to predict what will happen when it is run? 
