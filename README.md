## Purpose
This repository contains anonymized raw data and code needed to reproduce the paper "Lives Saved vs Time Lost: Direct Societal Benefits of Probabilistic Tornado Warnings"

## Data
Original data is in Input/Data_Mail.csv (mail sample) and in Input/Tornado_Warnings_Qualtrics.csv.
File PilotData.csv also contains the data for the mail pilot.
Questionnaires used for different waves are in the folder Insruments/

## Code:
All the code used is in the folder Code/
Most code is written for R and worked in R-studio Version 1.3.959. The small script used to generate the distribution of probabilistic forecasts is written in Matlab.

Order of execution for R code matters:
1. import_pilot.R
2. import_qual_surv.R
3. import_mail_surv.R
4. qual_general_analysis.R
5. mail_general_analysis.R
6. direct_costs_estim.R