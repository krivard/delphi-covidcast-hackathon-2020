# CMU Delphi Hackathon 2020: Survey Processing Machinery

In partnership with the Information Networking Institute

June 26-28, 2020

* DRAFT COPY - EXPECT CHANGES UNTIL 26 JUNE *

## Starter Kit

### Code

We have provided you with R code that can do the following:

* Read in raw survey data
* Filter and clean raw data
* Compute two statistics:
  * Household CLI
  * Community CLI
* Aggregate data into larger geographical regions by zip code
* Smooth data over a 7-day moving window
* Generate output files formatted for the COVIDcast API

### Data

We have provided you with the following datasets:

* [x] A copy of the survey questions
* [ ] Synthetic raw survey data from April 6, 2020 to $Month $Day, 2020
* [ ] Correct output files for Household CLI and Community CLI using the synthetic data
* [x] Spec for the COVIDcast API file format
* [ ] Spec for the definition of Household CLI and Community CLI

## Task

### Coverage

1. Compute a statistic for one of the 18 questions not already being analyzed
2. Aggregate the statistic into larger geographical regions by zip code
3. Smooth the data over a 7-day moving window
4. Generate output files formatted for the COVIDcast API

Repeat with as many questions as you like. Any statistic is fine (sum, average, the fraction choosing “Yes”, etc).

At judging time, we will test your code on synthetic raw survey data from April 6, 2020 to $Month $Day+1, 2020. We will check whether the outputs are in the proper format, and contain no invalid entries according to the spec.

Extra credit will be awarded if the choice of statistic is easy to modify. If you implement multiple additional questions, extra credit will be awarded if it is easy to select at runtime which question(s) to output.

### Speed

* Compute Household CLI and Community CLI as quickly as possible.

You may modify any part of the code to improve the speed, but the output must match the provided correct outputs. You may cache any data you like to disk, including staged output files. The time it takes to generate cached data will not be included in the final running time of your code.

At judging time, we will test your code on synthetic raw survey data from April 6, 2020 to $Month $Day+1, 2020. We will measure how long it takes your code to generate output files from April 6, 2020 to $Month $Day+1, 2020, and we will check whether the outputs are correct.

Be careful! Survey responses can be submitted up to four days after they were started. This means the raw data file for $Month $Day+1, 2020 may affect the output files for earlier days.

## Judging

The deadline to submit your solution is Sunday at $Time. 

To submit, send us a link to your code on GitHub. Make sure your README file includes the following information:

* The names of all team members
* Which task(s) you attempted
* Any special instructions for running your code

No matter what task you chose, extra credit will be awarded for:

* Documentation - If you wrote new functions, are they commented? If your code supports new configuration options, did you describe how to use them? 
* Readability - Is your source code easy to understand? Did you divide it into meaningful functions?
* Usability - Is it easy to run your code? Is it easy to predict what will happen when it is run? 
