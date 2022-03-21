# Loss functions comparasion

### Folder corresponding to section 3.7 of the dissertation. It contains all the images, R code of the generated images and the results presented in the section.

1. All images are [here](https://github.com/AlbertoRodrigues/estimating_knots_regression_splines_model/tree/main/loss_functions_comparison/images)

2. All the results of estimating the number of knots and predictive performance for each model studied are [here](https://github.com/AlbertoRodrigues/estimating_knots_regression_splines_model/tree/main/loss_functions_comparison/outputs).

There are four lists in R that represent the results: number_knots1(first cost function), lambda_knots1(first cost function), number_knots2(second cost function) and lambda_knots2(second cost function).

The results are in a specific .txt format. An example be explained below:

#Proportion of the estimated number of knots for n=300

| Number of Knots  | 1 | 2  |  3 | 4  | 5  |  
|---|---|---|---|---|
| Proportion  | 0.08 | 0.34 | 0.28 | 0.08 | 0.22  |


#Proportion of the estimated number of knots for n=900

| Number of Knots  | 2  |  3 | 4  | 5  |  
|---|---|---|---|---|
| Proportion  | 0.30 | 0.28 | 0.17 | 0.25|


#Proportion of the estimated number of knots for n=1500

| Number of Knots  | 2  |  3 | 4  | 5  |  
|---|---|---|---|---|
| Proportion  | 0.57 | 0.21 | 0.09 | 0.13  | 


#Proportion of the estimated number of knots for n=3000

| Number of Knots  | 2  |  3 | 4  | 5  |  
|---|---|---|---|---|
| Proportion  | 0.66 | 0.15 | 0.08 | 0.11  | 


#Proportion of the lambda chosen for n=300

| Number of Knots  | 0 | 0.01 | 0.03 | 0.1 | 0.3  | 
|---|---|---|---|---|
| Proportion  | 0.14 | 0.29 | 0.23 | 0.18 | 0.16  | 


#Proportion of the lambda chosen for n=900

| Number of Knots  | 0 | 0.01 | 0.03 | 0.1 | 
|---|---|---|---|---|
| Proportion  | 0.31 | 0.35 | 0.24 | 0.10  | 


#Proportion of the lambda chosen for n=1500

| Number of Knots  | 0 | 0.01 | 0.03 | 0.1 | 
|---|---|---|---|---|
| Proportion  | 0.29 | 0.50 | 0.15 | 0.06 | 


#Proportion of the lambda chosen for n=3000

[[4]]

lambda_knots1

| Number of Knots  | 0 | 0.01 | 0.03 | 0.1 | 
|---|---|---|---|---|
| Proportion  | 0.25 | 0.53 | 0.21 | 0.01 | 

### For the second cost function, the way is exactly the same.

# MSE, Standart deviation of the mean squared of the two cost functions and generated model for all size samples.

[1] 4.173393 4.108796 4.032435 4.022507
[1] 5.859874 5.801372 5.734170 5.689517
[1] 4.185749 4.108241 4.032888 4.022667
[1] 5.863372 5.795202 5.731216 5.691542
[1] 3.975401 4.020725 4.000325 4.007024
[1] 5.577649 5.690516 5.687020 5.667225

