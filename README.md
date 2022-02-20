# UniProject_NonParametricModel

# Project scope: 
Prediction if the bank's client will or will not leave the bank.

# Dataset: 
The set of data used in the project does not apply to customers of a certain bank. It contains some features that characterize the client.
The data comes from https://www.kaggle.com/mathchi/churn-for-bank-customers.

# Conclusions: 
Each of the models was used to estimate the value of the monthly salary depending on the credit rating, the balance on the bank account and the age of the bank's customer.

The first analyzed model turned out to be not the best in terms of quality. It was characterized by very high values of generalized cross-checking and standard errors, however, it was the second model that had the highest GCV values.

When comparing models with each other and considering them with regard to the degree of explanation of variability, the first model turns out to be the best with the value of 0.0471% of explaining the total variability of the model.

It is worth mentioning that none of the variables in each of the models was statistically significant. This proves that the explanatory variables CreditScore, Balance as well as Age do not have a significant influence on the dependent variable EstimatedSalary.

Comparing the models with each other shows that the second model significantly simplifies the relationship between the variables. If there is such a dependence at all. 
# Based only on the available data, the first model turns out to be the best, both qualitatively and in terms of explaining the variability.

---------------------
# Presentation 
is prepared in polish language. Sorry for any inconvinience, but translation of the presantation is not the main point of the whole repository.
# Code 
contains comments in english, for better understanding of each step.
