/* Reading the dataset churn from library*/

data a1;
set work.ch;
proc print data=a1(obs=10); run;

/* We have 11 missing values in TotalCharges column the data as per our finding in the data preprocessing, 
   dropping those records*/

data a2;
set a1;
where TotalCharges ne .;
run;

/* On comparing the percentage difference between the means of all the features for churners and non-churners, we have
	finalised below 10 go to columns for our analysis. 
1. Contract
2. OnlinesSecurity
3. Tenure
4. Tech Support
5. Payment
6. Total Charges
7. Internet service
8. Dependents
9. Senior Citizen
10. Monthly Charges

We have picked all the numeric columns which are Tenure, Monthly Charges and Total Charges and we have picked 7 
categorical features that had the highest percentage difference in mean across churners and non-churners. Priority
is given to numeric columns beacuse of the simple reason that dummy variables have less predictive power
*/

/*Reducing our data set to the most important features*/
data a3;
set a2 (keep= customerID Contract OnlineSecurity tenure TechSupport PaymentMethod InternetService Dependents
SeniorCitizen MonthlyCharges TotalCharges Churn);
run;

proc print data=a3(obs=10); run;

/* Saving as permanent SAS dataset*/
libname q 'H:\';
data q.churn;
set a3;
run;

/* Creating dummy variables for categorical features */
data a4; set a3;

if Dependents= "Yes" then ddependents=1; else ddependents=0;

if Churn= "Yes" then dchurn=1; else dchurn=0;

if InternetService= "DSL" then dinternet_dsl=1; else dinternet_dsl=0;
if InternetService= "Fiber optic" then dinternet_fo=1; else dinternet_fo=0;

if OnlineSecurity= "Yes" then donlinesecurity_Y=1; else donlinesecurity_Y=0;

if TechSupport= "Yes" then dtech_Y=1; else dtech_Y=0;

if Contract= "Month-to-month" then dcontract_M=1; else dcontract_M=0;
if Contract= "One year" then dcontract_O=1; else dcontract_O=0;

if PaymentMethod= "Electronic check" then dpayment_e=1; else dpayment_e=0;
if PaymentMethod= "Mailed check" then dpayment_m=1; else dpayment_m=0;
if PaymentMethod= "Credit card (automatic)" then dpayment_cc=1; else dpayment_cc=0;

run;

proc print data=a4(obs=10); run;

/* Dataset is ready 
We have using “No internet service” as “No” for following columns, they are
“OnlineSecurity” and “TechSupport”. We are using No Internet Service and No as the base for dummy variables as
both of these mean almost one and the same thing, and having 2 instead of 1 dummy variables for each of these
categorical features would introduce linear dependency among dummy variables which will inturn lead to multicollinearity
*/

/* Question 1-4  MODEL-1*/
proc logistic data=a4 outest=betas covout;
model dchurn(event='1')= ddependents dinternet_dsl dinternet_fo donlinesecurity_Y dtech_Y dcontract_M dcontract_O 
dpayment_e dpayment_m dpayment_cc SeniorCitizen tenure MonthlyCharges TotalCharges/ expb stb;

output out=pred p=phat lower=lcl upper=ucl predprob=(individual crossvalidate); 
ods output Association=Association; 
run; 


/*
#Model-2
•	Removing insignificant variables from above model, we are dropping Monthly Charges from the model. 
•	Also removing payment_method_creditcard and payment_method_mailedCheck, which means now the base for payment 

method dummy variable becomes credit card, mailed check and Automatic bank transfer.
*/

proc logistic data=a4 outest=betas covout;
model dchurn(event='1')= ddependents dinternet_dsl dinternet_fo donlinesecurity_Y dtech_Y dcontract_M dcontract_O 
dpayment_e SeniorCitizen tenure TotalCharges/ expb stb;

output out=pred p=phat lower=lcl upper=ucl predprob=(individual crossvalidate); 
ods output Association=Association; 
run; 

/* 
AIC and BIC have reduced on dropping insignificant variables, which means our model has improved on dropping few variables. 
So, Model# 2 would be our logistic regression model to predict which customers are likely to churn */

/*



/* Question -5 Compute Hit Ratio */

proc print data=betas; 
   title2 'Parameter Estimates and Covariance Matrix'; 
run; 

/* Above code prints the betas along with the covariance among them*/ 

proc print data=pred; 
   title2 'Predicted Probabilities and 95% Confidence Limits'; 
run; 

/* phat is the predicted probability of y=1 in data pred, i.e. probability of a customer to churn by our model*/


/* Creating a dataset which has original churn column from telecom data and predicted probability phat from our model*/
proc sql;
create table hit_ratio1 as
select d1.dchurn, d2.phat from a4 as d1 join pred as d2 on 
d1.customerID = d2.customerID;
run;

/* Predicting churn based on the probability*/
data hit_ratio;
set hit_ratio1;
if phat gt 0.5 then pred_churn=1; else pred_churn=0;
run;

/* Creating a column hit that tells us whether the prediction is correct ot not*/
data hit;
set hit_ratio;
if dchurn=pred_churn then hit=1; else hit=0;
run;

/* Computing the mean of that column to compute the hit ratio*/
proc means data=hit;
var hit;run;

/* Got 80% of the predictions correct */


/*
### Using all variables just for curiousity if the variables chosen are correct 

**Code**
data a5; set a2;

if Dependents= "Yes" then ddependents=1; else ddependents=0;

if Churn= "Yes" then dchurn=1; else dchurn=0;

if InternetService= "DSL" then dinternet_dsl=1; else dinternet_dsl=0;
if InternetService= "Fiber optic" then dinternet_fo=1; else dinternet_fo=0;

if OnlineSecurity= "Yes" then donlinesecurity_Y=1; else donlinesecurity_Y=0;
if OnlineSecurity= "No" then donlinesecurity_N=1; else donlinesecurity_N=0;

if TechSupport= "Yes" then dtech_Y=1; else dtech_Y=0;
if TechSupport= "No" then dtech_N=1; else dtech_N=0;

if Contract= "Month-to-month" then dcontract_M=1; else dcontract_M=0;
if Contract= "One year" then dcontract_O=1; else dcontract_O=0;

if PaymentMethod= "Electronic check" then dpayment_e=1; else dpayment_e=0;
if PaymentMethod= "Mailed check" then dpayment_m=1; else dpayment_m=0;
if PaymentMethod= "Credit card (automatic)" then dpayment_cc=1; else dpayment_cc=0;
 
if PaperlessBilling= "Yes" then dpaperless=1; else dpaperless=0;

if DeviceProtection= "Yes" then dprotection_y=1; else dprotection_y=0;
if DeviceProtection= "No" then dprotection_n=1; else dprotection_n=0;
 
if StreamingTV= "Yes" then dtv_y=1; else dtv_y=0;
if StreamingTV= "No" then dtv_n=1; else dtv_n=0;

if StreamingMovies= "Yes" then dmovies_y=1; else dmovies_y=0;
if StreamingMovies= "No" then dmovies_n=1; else dmovies_n=0;

if OnlineBackup= "Yes" then dbackup_Y=1; else dbackup_Y=0;
if OnlineBackup= "No" then dbackup_N=1; else dbackup_N=0;

if MultipleLines= "Yes" then dlines_y=1; else dlines_y=0;
if MultipleLines= "No" then dlines_n=1; else dlines_n=0;

if gender= "Female" then dgender=1; else dgender=0;

if Partner= "Yes" then dpartner=1; else dpartner=0;

if PhoneService= "Yes" then dphoneservice=1; else dphoneservice=0;

run;

proc logistic data=a5;
model dchurn(event='1')= ddependents dinternet_dsl dinternet_fo donlinesecurity_Y dtech_Y 
dcontract_M dcontract_O dpayment_e dpayment_m dpayment_cc SeniorCitizen tenure MonthlyCharges TotalCharges
dpaperless dprotection_y dtv_y dmovies_y dbackup_Y dlines_y dlines_n dgender
dpartner;
output out=pred p=phat lower=lcl upper=ucl 
          predprob=(individual crossvalidate); 
   ods output Association=Association; 
run; 

**code**

## Looks like only Paperless billing is a feature that we have missed using the trick we used to find the important 
features

*/

