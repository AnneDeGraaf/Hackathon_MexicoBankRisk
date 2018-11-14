# Hackathon_MexicoBankRisk
My entry for the mini-hackathon on Mexican Bank Risk, organized by MatrixDS and Bajaware.

I won 1st prize for the hackathon.

The assignment was to give a 6 month forecast on loan interest rate and non performing loans 
for different states and corporate sizes in Mexico. The results had to be presented in an 
R Shiny app.

I did the project using the MatrixDS platform. (original link to project: https://community.platform.matrixds.com/project/5be4f07efd1bbd4073390cf7)

### My approach
I made a simple time-dependent linear regression model for every state and corporate size combination.
The models were very rough and I didn't have enough time to tune the hyperparameters or anything,
but the simple approach prooved effective enough.

### Files
##### Data
Data is available through the project on MatrixDS: 
https://community.platform.matrixds.com/project/5be4f07efd1bbd4073390cf7

##### Code
Forecasting.ipynb:  Prediction model

app.R:              Shiny app for presenting the results

##### Results
prediction1.csv
