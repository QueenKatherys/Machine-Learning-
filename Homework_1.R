# Kathleen Ashbaker 
# BIOSTAT 546 Machine Learning 
# Homework 1 


#######################PROBLEM 1:##############################################

# In this problem, we will make use of the dataset Medical_Cost_2.RData 


#  1a. Load Data Frame from local files, and view it, checking for missing data
#      load("local file pathway")

View(df)

#   1b. Omit any missing data 

df <- na.omit(df)

#   1c. Make a scatterplot with bmi on the x-axis, charges on the y-axis; 
#       the color of each dot representing whether the subject is a smoker or not


# Load the ggplot2 package

library(ggplot2)

# Assuming your data frame is named df and it has columns 'charges', 'bmi', and 'smoker'
# with 'smoker' being a factor or character vector with values 'yes' and 'no'


# Create the scatterplot with different colors for points and trend lines
ggplot(df, aes(x = bmi, y = charges)) +
  geom_point(aes(color = smoker)) +  # Color points by smoker status
  geom_smooth(method = "lm", se = FALSE, aes(group = smoker), data = subset(df, smoker == "yes"), color = "blue") +  # Blue trend line for smokers
  geom_smooth(method = "lm", se = FALSE, aes(group = smoker), data = subset(df, smoker == "no"), color = "black") +  # Black trend line for non-smokers
  labs(title = "Scatterplot of Charges vs BMI by Smoking Status",
       x = "Body Mass Index (BMI)",
       y = "Charges (Individual Medical Costs)",
       color = "Smoker") +
  theme_minimal()

# 1d. Fit a least-squares linear model, with intercept, in order to predict: 

# d1. charges using bmi as the only predictor;
        
# Fit the linear model
model_non_smoker<- lm(formula = charges ~ bmi, data = df)

# Display the summary of the model
summary(model_non_smoker)


# d2. charges using bmi and smoker as predictors;


# Fit the linear model with both bmi and smoker as predictors
model_smoker_bmi <- lm(charges ~ bmi+smoker, data = df)

# Display the summary of the model
summary(model_smoker_bmi)

  
# d3. charges using bmi and smoker as in the previous model; but allowing
#     for an interaction term between the variables bmi and smoker;



# Fit the linear model with bmi, smoker, and their interaction as predictors
model_smokebmi_int <- lm(charges ~ bmi * smoker, data = df)

# Display the summary of the model
summary(model_smokebmi_int)


# 1d. Plot of all three models in the same scatter plot 

# Create a new variable for the groups
df$group <- with(df, ifelse(smoker == "no", "Non-smoker", ifelse(bmi < 30, "Smoker: BMI < 30", "Smoker: BMI >= 30")))



# Create the scatter plot with smoothed trend lines
ggplot(df, aes(x = bmi, y = charges, color = group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE ) +
  labs(title = "Charges vs BMI by Group",
       x = "Body Mass Index (BMI)",
       y = "Charges (in USD($))",
       color = "Group")+
  theme_minimal()

# 1d. For each model, present your results in the form of a table where you
# report the estimated regression coefficients and their interpretation;
# Report the 95% confidence interval for the coefficient of the variable
# bmi, and provide a sentence explaining the meaning of this confidence
# interval.



# Create a table of estimated regression coefficients
coefficients_table <- data.frame(
  Model = c("Model Non-Smoker", "Model Smoker + BMI", "Model Smoke-BMI Interaction"),
  Intercept = c(coef(model_non_smoker)[1], coef(model_smoker_bmi)[1], coef(model_smokebmi_int)[1]),
  BMI = c(coef(model_non_smoker)[2], coef(model_smoker_bmi)[2], coef(model_smokebmi_int)[2]),
  Smoker = c(NA, coef(model_smoker_bmi)[3], coef(model_smokebmi_int)[3]),
  BMI_Smoker_Interaction = c(NA, NA, coef(model_smokebmi_int)[4])
)

# Print the table
print(coefficients_table)

# Alternatively, View 'coefficients_table'in source pane.  
View(coefficients_table)

# In the coefficients table, each row represents a different model,
# and the columns include the estimated coefficients for the intercept, BMI, 
# smoker status, and the interaction between BMI and smoker status, where applicable.

# Report 95% Confidence Interval for BMI Coefficient
conf_int_non_smoker <- confint(model_non_smoker, "bmi", level = 0.95)
conf_int_smoker_bmi <- confint(model_smoker_bmi, "bmi", level = 0.95)
conf_int_smokebmi_int <- confint(model_smokebmi_int, "bmi", level = 0.95)

# Print the confidence intervals
print(conf_int_non_smoker)
print(conf_int_smoker_bmi)
print(conf_int_smokebmi_int)

# COMMENT ON RESULTS:

# The confidence intervals provide a range of values within which we can be
# 95% confident that the true coefficient of BMI lies. 
# The interpretation is that if we were to repeat the study many times, 
# 95% of the time, the true coefficient would fall within this range.

# For instance, conf_int_non_smoker represents the 95% confidence interval for the 
# BMI coefficient in a model where the sample consists of non-smokers, with a range 
# from 297.0107 to 508.2841.

# conf_int_smoker_bmi shows the 95% confidence interval for the BMI coefficient in a
# model that considers smokers, with the interval ranging from 334.7816 to 462.1514.

# conf_int_smokebmi_int indicates the 95% confidence interval for the BMI coefficient in 
# a model that possibly includes an interaction between smoking and BMI, with the interval
# spanning from 27.16986 to 151.7778.


# 1d. Report the (training set) mean squared error of the model

# Calculate and report training set MSE
mse_non_smoker <- mean((df$charges - predict(model_non_smoker))^2)
mse_smoker_bmi <- mean((df$charges - predict(model_smoker_bmi))^2)
mse_smokebmi_int <- mean((df$charges - predict(model_smokebmi_int))^2)

# Print the MSE values
mse_non_smoker
mse_smoker_bmi
mse_smokebmi_int



# 1d. Predict the medical costs billed by the health insurance company to
# a smoker with a bmi that is 29 and 31.5.

# New data frame for prediction
new_data <- data.frame(bmi = c(29, 31.5), smoker = factor(c("yes", "yes"), levels = levels(df$smoker)))

# Predict charges using the model
predicted_charges <- predict(model_smokebmi_int, newdata = new_data)

# Display the predictions
predicted_charges

# 1d. Compute the predicted difference in charges between a smoker with
# bmi 31.5 and one with bmi 29. Do the same for non-smokers

# Create a new data frame for prediction
new_data_smoker <- data.frame(bmi = c(29, 31.5), smoker = factor(c("yes", "yes"), levels = levels(df$smoker)))
new_data_nonsmoker <- data.frame(bmi = c(29, 31.5), smoker = factor(c("no", "no"), levels = levels(df$smoker)))

# Predict charges for smokers
predicted_charges_smoker <- predict(model_smokebmi_int, newdata = new_data_smoker)

# Predict charges for non-smokers
predicted_charges_nonsmoker <- predict(model_smokebmi_int, newdata = new_data_nonsmoker)

# Calculate the differences
difference_smoker <- predicted_charges_smoker[2] - predicted_charges_smoker[1]
difference_nonsmoker <- predicted_charges_nonsmoker[2] - predicted_charges_nonsmoker[1]

# Display the differences
difference_smoker
difference_nonsmoker

# COMMENT ON RESULTS: 
# The predicted difference in charges between smokers with BMIs of 31.5 and 29 is $3748.82, 
# while for non-smokers with the same BMI values, the predicted difference is much smaller at $223.68.



# e.  Now define and add to the dataset a new boolean variable smoker_bmi30p
#     that is True only if the subject is a smoker and has a bmi greater than 30.


# Create new data frames for prediction
new_data_smokerbool <- data.frame(bmi = c(29, 31.5), smoker = factor(c("yes", "yes"), levels = c("no", "yes")))
new_data_nonsmokerbool <- data.frame(bmi = c(29, 31.5), smoker = factor(c("no", "no"), levels = c("no", "yes")))

# Add smoker_bmi30p to new data frames
new_data_smokerbool$smoker_bmi30p <- new_data_smokerbool$smoker == "yes" & new_data_smokerbool$bmi > 30
new_data_nonsmokerbool$smoker_bmi30p <- new_data_nonsmokerbool$smoker == "yes" & new_data_nonsmokerbool$bmi > 30



# Add smoker_bmi30p to your dataset
df$smoker_bmi30p <- df$smoker == "yes" & df$bmi > 30

# Fit the linear model
modelbool <- lm(charges ~ bmi * smoker + smoker_bmi30p, data = df)

# Display the summary of the model
summary(modelbool)

# e. Present your results in the form of one table where you report the 
#     estimated coefficients of the model.For each predictor, comment on whether 
#     you can reject the null hypothesis that there is no (linear) association between that predictor
#     and charges, conditional on the other predictors in the model.

# Load the broom package
if (!require(broom)) {
  install.packages("broom")
  library(broom)
}

# Fit the model (assuming you already have the 'df' data frame)
model <- lm(charges ~ bmi * smoker + smoker_bmi30p, data = df)

# Use broom to tidy the model and get a data frame of coefficients
model_summary <- broom::tidy(model)

# Select relevant columns
model_table <- model_summary[, c("term", "estimate", "std.error", "statistic", "p.value")]

# Rename columns for presentation
colnames(model_table) <- c("Predictor", "Estimate", "Std. Error", "t value", "Pr(>|t|)")

# Print the table
print(model_table)

# Alternatively, View 'model_table'in source pane.  
View(model_table)

# e. Comments on Null Hypothesis:

# Intercept: The intercept is significant, indicating a baseline level of charges when all predictors are zero.
# bmi: There is a significant linear association between BMI and charges, independent of smoking status.
# smokeryes: The coefficient for smokers is not statistically significant, suggesting no additional charge for being a smoker when not considering BMI.
# smoker_bmi30pTRUE: The significant positive coefficient for the interaction term indicates a substantial increase in charges for smokers with BMI over 30.
# bmi:smokeryes: The significant interaction between BMI and smoking status indicates that the effect of BMI on charges differs between smokers and non-smokers.






# e. Explain how Figure 1( The "Charges vs BMI by Group scatter plot in Part 1.d) would change if we were to discard those variables, i.e. perform variable selection.

# If we were to discard the non-significant variables in the modelbool
#(i.e., lm(charges ~ bmi * smoker + smoker_bmi30p, data = df)), specifically 
#the smokeryes variable which is not significant, and compare it to the other models 
#(model_non_smoker, model_smoker_bmi, model_smokebmi_int), here's how the comparison might look:

#model_non_smoker (lm(charges ~ bmi, data = df)): This model only includes BMI as a predictor for charges and is significant, indicating a linear relationship between BMI and charges. Removing smokeryes from modelbool would not change this model as it already does not include smoking status.

#model_smoker_bmi (lm(charges ~ bmi + smoker, data = df)): This model includes both BMI and smoking status as predictors. Discarding smokeryes from modelbool would make modelbool similar to model_smoker_bmi in terms of predictors, but model_smoker_bmi treats smoking status as a significant factor, whereas it's non-significant in modelbool.

# model_smokebmi_int (lm(charges ~ bmi * smoker, data = df)): This model considers the interaction between BMI and smoking status. If smokeryes were discarded from modelbool, the remaining variables would be similar to model_smokebmi_int, but modelbool also includes the smoker_bmi30p variable, which is significant and captures additional variation related to smokers with BMI over 30.

# In summary, removing the non-significant smokeryes variable from modelbool would 
# simplify the model, potentially making it more similar to model_smoker_bmi and 
# model_smokebmi_int. However, the presence of the smoker_bmi30p interaction in 
# modelbool still distinguishes it from these models by capturing an additional 
# aspect of the relationship between smoking, BMI, and charges. 
# This variable selection process would lead to a model that emphasizes the interaction 
# of BMI with smoking status and the specific effect for smokers with BMI over 30.


# e.  Explain the interpretation of the non-significant variables in the model(p > 0.05)


# In the model summary, the smokeryes variable has a p-value greater than 0.05 (p = 0.70267), 
# indicating that it is not statistically significant in this model. The interpretation of 
# this non-significant result is as follows:
# smokeryes: The coefficient for smokeryes is not statistically significant, which suggests that,
# within this model framework, being a smoker does not have a statistically significant 
# independent effect on charges, when controlling for BMI, the interaction between BMI and
# smoking status, and the specific effect of being a smoker with BMI over 30
# (as captured by smoker_bmi30pTRUE). This could mean that the main effect of being a 
# smoker on charges is either not present or is captured by the interaction terms and the smoker_bmi30pTRUE variable.
# It's important to note that the lack of significance does not imply that
# smoking status has no effect on charges; rather, it suggests that the effect of 
# being a smoker is more nuanced and possibly dependent on other factors in the model, 
# such as BMI and the additional impact on smokers with a BMI over 30. The significant 
# interaction terms indicate that the effect of smoking on charges is conditional on BMI values.


# e. Predict charges for smokers
predicted_charges_smokerbool <- predict(modelbool, newdata = new_data_smokerbool)

# e. Predict charges for non-smokers
predicted_charges_nonsmokerbool <- predict(modelbool, newdata = new_data_nonsmokerbool)

# e. Calculate the differences
difference_smokerbool <- predicted_charges_smokerbool[2] - predicted_charges_smokerbool[1]
difference_nonsmokerbool <- predicted_charges_nonsmokerbool[2] - predicted_charges_nonsmokerbool[1]

# e. Display the differences
difference_smokerbool
difference_nonsmokerbool

# e. Compare with the analogous results in point (d) and comment on the results.

# The difference in predicted charges between smokers with BMIs of 31.5 and 29 
# using the model with a smoker_bmi30p interaction term is 
# significantly higher ($15956.21) compared to the previous model ($3748.82), 
# while the difference for non-smokers remains the same ($223.68) in both models.

#######################PROBLEM 2:##############################################

# This problem has to do with the notion of bias-variance trade-off.

# a. Plot of Squared Bias, Variance, Irreducible Error, and Expected Prediction Error:

# Simulate some data
flexibility <- seq(0.1, 1, length.out = 100)
bias_squared <- (1 - flexibility)^2  # Example: bias decreases as flexibility increases
variance <- flexibility^2            # Example: variance increases as flexibility increases
irreducible_error <- rep(0.2, 100)   # Constant irreducible error

# Calculate expected prediction error
expected_error <- bias_squared + variance + irreducible_error

# Data frame for plotting
df_error <- data.frame(flexibility, bias_squared, variance, irreducible_error, expected_error)

# Plot
ggplot(df_error, aes(x = flexibility)) +
  geom_line(aes(y = bias_squared, color = "Bias Squared")) +
  geom_line(aes(y = variance, color = "Variance")) +
  geom_line(aes(y = irreducible_error, color = "Irreducible Error")) +
  geom_line(aes(y = expected_error, color = "Expected Prediction Error")) +
  labs(title = "Bias-Variance Tradeoff", y = "Error", color = "Component") +
  theme_minimal()


# 2a.  Indicate which level of flexibility is “best”.

# In the context of the bias-variance tradeoff graph  the "best" level of flexibility
# is the one that minimizes the Expected Prediction Error (EPE), 
# which is the sum of Bias Squared, Variance, and Irreducible Error. 

# Looking at the graph, the "best" flexibility level is where the 
# Expected Prediction Error curve (green line) reaches its minimum. 
# This point represents the optimal balance between bias (red line) and
# variance (purple line), or the point where the bias and variance lines intersect at 
# flexibility( green) estimated at 0.5 ( errorestimated at  0.25). 
# resulting in the lowest possible prediction error for 
# the given model complexity.
# Look for the flexibility level at which the green line is at its lowest point 
# on the graph. This is typically where the decrease in bias is balanced by the 
# increase in variance, or vice versa.




# b. Plot of Training Error and Test Error

# Simulate some data
flexibility <- seq(0.1, 1, length.out = 100)
training_error <- (1 - flexibility)^2    # Example: training error decreases as flexibility increases
test_error <- (0.5 - flexibility)^2 + 0.1  # Example: test error decreases then increases

# Data frame for plotting
df_model_error <- data.frame(flexibility, training_error, test_error)

# Plot
ggplot(df_model_error, aes(x = flexibility)) +
  geom_line(aes(y = training_error, color = "Training Error")) +
  geom_line(aes(y = test_error, color = "Test Error")) +
  labs(title = "Training vs Test Error", y = "Error", color = "Error Type") +
  theme_minimal()


# 2b.  Indicate which level of flexibility is “best”.

#In the training vs test error graph, the "best" level of flexibility is the one 
#that minimizes the test error while maintaining a reasonable level of training error. 
# It is where the test error curve (red line) reaches its minimum( estimated error at 0.1)
# This point represents the optimal balance between fitting the training data well 
# and maintaining the model's ability to generalize to new, unseen data.

# You should look for the flexibility level where the red line is at its lowest point
# before it starts to increase( estimated to be between 0.5 and 0.6) 
# which indicates the point of overfitting where the model is too complex and 
# begins to fit the noise in the training data rather than the underlying relationship. 
# This is typically the ideal point to prevent both underfitting and overfitting.


#######################PROBLEM 3:############################################## 

# To complete this problem, you will need to follow these steps 

# Set the seed for reproducibility
set.seed(0)

# 3a. Generate the predictor vector X using rnorm() 
# Use the rnorm() function to generate a predictor vector X of length n =
# 30, and use runif() to generate a noise vector ϵ of length n = 30.
n <- 30
X <- rnorm(n)

# Generate the noise vector ϵ using runif()
epsilon <- runif(n)

# Define the true function f_true(X)
f_true <- function(X) {
  3 + 2 * X + 3 * X^3
}

# 3b. Generate the response vector Y according to the model Y = ftrue(X) + ϵ,
# with ftrue(X) = 3 + 2X + 3 ∗ X3Y <- f_true(X) + epsilon

# Generate the response vector Y
Y <- f_true(X) + epsilon

# 3c.  Fit the model Y = f(X) +ϵ to the data (using the lm() function), for the following choices of f:
# f(X) = β0 + β1 ∗ X
# f(X) = β0 + β1 ∗ X + β2 ∗ X2
# f(X) = β0 + β1 ∗ X + β2 ∗ X2 + β3 ∗ X3 + β4 ∗ X4
# f(X) = β0 + β1 ∗ X + β3 ∗ X3


# To fit the models to the training data using the lm() function in R, you will create four different linear models with varying degrees of complexity:

# A linear model with only the first power of X.
# A quadratic model including up to the second power of X.
# A quartic model including up to the fourth power of X.
# A model with the first and third powers of X.


# Fit the linear model
model_linear <- lm(Y ~ X)

# Fit the quadratic model
model_quadratic <- lm(Y ~ X + I(X^2))

# Fit the quartic model
model_quartic <- lm(Y ~ X + I(X^2) + I(X^3) + I(X^4))

# Fit the model with first and third powers of X
model_first_third <- lm(Y ~ X + I(X^3))

# 3d. For each of the models above compute the training mean squared error (MSE). Comment on the results

#To compute the training Mean Squared Error (MSE) for each of the models, you can follow these steps:


# Predicted values from each model
pred_linear <- predict(model_linear)
pred_quadratic <- predict(model_quadratic)
pred_quartic <- predict(model_quartic)
pred_first_third <- predict(model_first_third)

# Compute MSE for each model
mse_linear <- mean((Y - pred_linear)^2)
mse_quadratic <- mean((Y - pred_quadratic)^2)
mse_quartic <- mean((Y - pred_quartic)^2)
mse_first_third <- mean((Y - pred_first_third)^2)

# Print the MSE values
mse_linear
mse_quadratic
mse_quartic
mse_first_third

# COMMENT ON THE RESULTS: 
#The training Mean Squared Error (MSE) results suggest that as the 
# complexity of the model increases, the MSE decreases. 
#The linear model has the highest MSE, indicating poor fit. 
# The quadratic model reduces MSE significantly, suggesting a better fit. 
# The quartic and the first-third power models provide the lowest and nearly 
# identical MSEs, which are much lower than the linear and quadratic models, 
# indicating the best fit among the models considered. 
# However, the quartic model might be overfitting due to including an 
# unnecessary fourth power term, as the true function is cubic, 
# and the first-third power model already captures the true relationship well.


# 3e. Now generate 10K (new) test observations following steps 3(a) and 3(b).
# Compute the test MSE of the models fitted in 3(c) on these test observations. Report and comment on the 
# results 


# Set the seed for reproducibility
set.seed(0)

# Generate the predictor vector X for test data
n_test <- 10000
X_test <- rnorm(n_test)

# Generate the noise vector ϵ for test data
epsilon_test <- runif(n_test)

# Define the true function f_true(X)
f_true_test <- function(X) {
  3 + 2 * X + 3 * X^3
}

# Generate the response vector Y for test data
Y_test <- f_true_test(X_test) + epsilon_test

# Next, we'll predict the responses for the test data and compute the MSE for each model:

# Predicted values from each model on test data
pred_linear_test <- predict(model_linear, newdata = data.frame(X = X_test))
pred_quadratic_test <- predict(model_quadratic, newdata = data.frame(X = X_test))
pred_quartic_test <- predict(model_quartic, newdata = data.frame(X = X_test))
pred_first_third_test <- predict(model_first_third, newdata = data.frame(X = X_test))

# Compute MSE for each model on test data
mse_linear_test <- mean((Y_test - pred_linear_test)^2)
mse_quadratic_test <- mean((Y_test - pred_quadratic_test)^2)
mse_quartic_test <- mean((Y_test - pred_quartic_test)^2)
mse_first_third_test <- mean((Y_test - pred_first_third_test)^2)

# Print the MSE values
mse_linear_test
mse_quadratic_test
mse_quartic_test
mse_first_third_test

# COMMENT ON THE RESULTS: 

# The Mean Squared Error (MSE) results for the test data show that the models 
# perform differently on the test set compared to the training set. The linear model 
# has a lower MSE than the quadratic model on the test data, indicating that the quadratic 
# model may be overfitting to the noise in the training data. The quartic model and the 
# first-third power model, which include the correct cubic term present in the true function, 
# have the lowest MSEs, with the first-third power model performing slightly better. 
# This suggests that these models are capturing the underlying relationship more effectively 
# than the linear or quadratic models. However, the quartic model may still be slightly 
# overfitting, as indicated by its marginally higher MSE compared to the first-third power model, 
# which aligns more closely with the true cubic function.

# Consider how the complexity of the model affects its performance on unseen data. 
# A good model should have a low test MSE, indicating that it generalizes well to new data. 
# However, if a model has a much lower training MSE compared to its test MSE, 
# it might be overfitting the training data. 
# In contrast, a model that performs similarly on both training and test data is likely to be more robust.


# 3f.  Compute training and test MSEs of the true regression function ftrue. 
# Compare to those of the models fitted in 3(c). Comment on the results


# Predicted values using f_true for training data
pred_true_training <- f_true(X)

# Predicted values using f_true for test data
pred_true_test <- f_true_test(X_test)


# Next, compute the MSE for both training and test data:

# Compute MSE for f_true on training data
mse_true_training <- mean((Y - pred_true_training)^2)

# Compute MSE for f_true on test data
mse_true_test <- mean((Y_test - pred_true_test)^2)

# Print the MSE values
mse_true_training
mse_true_test

# COMMENT ON THE RESULTS: 
# The Mean Squared Error (MSE) results for the f_true function on both the training 
# and test data are very low, indicating that the function provides an excellent fit 
# to the data. 
# The MSE for the training data is 0.3381281, and for the test data, it is 0.3285228. 
# These values are consistent and very close to each other, suggesting that the f_true function 
# generalizes well from the training data to the test data. Since f_true represents 
# the true underlying function that generated the data (plus noise), these low MSE 
# values are expected and indicate that the noise added to the data is consistently 
# around the same level in both the training and test datasets.


#######################EXTRA CREDIT:##############################################

#  Now we want to estimate the bias and variance of each one of the three models in 3(c). 
# To this purpose, we generate 40 datasets of 30 observations repeating 
# points 1(a) and 1(b) 40 times. For each model below: 
# f(X) = β0 + β1 ∗ X
# f(X) = β0 + β1 ∗ X + β2 ∗ X2
# f(X) = β0 + β1 ∗ X + β2 ∗ X2 + β3 ∗ X3 + β4 ∗ X4
# f(X) = β0 + β1 ∗ X + β3 ∗ X3


# Estimate its coefficients fitting a separate model to each of the 40
# datasets and compute the associated 40 predictions at x0 = 0.3, i.e.,
# ˆf(x0), with x0 = 0.3

set.seed(0)
n_datasets <- 40
n <- 30
x0 <- 0.3

# Initialize vectors to store predictions for each model at x0
preds_linear <- numeric(n_datasets)
preds_quadratic <- numeric(n_datasets)
preds_quartic <- numeric(n_datasets)

# Loop over the datasets
for (i in 1:n_datasets) {
  X <- rnorm(n)
  epsilon <- runif(n)
  Y <- 3 + 2 * X + 3 * X^3 + epsilon
  
  # Fit the models
  model_linear <- lm(Y ~ X)
  model_quadratic <- lm(Y ~ X + I(X^2))
  model_quartic <- lm(Y ~ X + I(X^2) + I(X^3) + I(X^4))
  
  # Predict at x0 for each model
  preds_linear[i] <- predict(model_linear, newdata = data.frame(X = x0))
  preds_quadratic[i] <- predict(model_quadratic, newdata = data.frame(X = x0))
  preds_quartic[i] <- predict(model_quartic, newdata = data.frame(X = x0))
}


# Next, calculate the average prediction and variance, and then 
# compute the bias and variance for each model


# Compute the true value at x0
f_true_x0 <- 3 + 2 * x0 + 3 * x0^3

# Calculate average predictions
ave_pred_linear <- mean(preds_linear)
ave_pred_quadratic <- mean(preds_quadratic)
ave_pred_quartic <- mean(preds_quartic)

# Calculate variances
var_pred_linear <- var(preds_linear)
var_pred_quadratic <- var(preds_quadratic)
var_pred_quartic <- var(preds_quartic)

# Calculate biases
bias_linear <- (ave_pred_linear - f_true_x0)^2
bias_quadratic <- (ave_pred_quadratic - f_true_x0)^2
bias_quartic <- (ave_pred_quartic - f_true_x0)^2

# Print biases and variances
bias_linear
var_pred_linear
bias_quadratic
var_pred_quadratic
bias_quartic
var_pred_quartic

# Comment on the results and relate to the test MSEs computed in 3(e). 
# The results from the bias and variance calculations provide valuable insights into the 
# performance of the three polynomial regression models (linear, quadratic, and quartic) 
# at a specific point x0= 0.3
# These results can be related to the mean squared error (MSE) values obtained from testing
# the models on a large test dataset. 

# Bias and Variance of Models:
  # Linear Model: High bias (8.092797) and moderate variance (1.661679).
  # Quadratic Model: Moderate bias (5.375305) and higher variance (2.966996).
  # Quartic Model: Low bias (0.2345732) and low variance (0.008418345).

# Interpretation:

  # The linear model, being the simplest, has the highest bias, suggesting it is not
  # complex enough to capture the true relationship,  ftrue(X) = 3 + 2X + 3 ∗ X3

  # The quadratic model, with an additional term, reduces bias but at the cost of increased variance. 
  # This indicates an improvement in capturing the relationship but with less consistency 
  # across different datasets.
  # The quartic model shows both low bias and low variance, suggesting it is well-suited 
  # for modeling the relationship at x0=0.3
  #  This model's complexity allows it to capture the underlying relationship more 
  # accurately and consistently.

# Comparison with Test MSE (3e):

  # Linear Model Test MSE: 46.80163
  # Quadratic Model Test MSE: 78.24657
  # Quartic Model Test MSE: 0.09918218
  # Comparing the bias and variance with test MSEs:
  
  # The linear model's high bias and moderate variance are consistent with its relatively high test MSE, indicating underfitting.
  # The quadratic model's moderate bias and higher variance result in the highest test MSE, suggesting that the model's increased complexity does not align well with the true relationship.
  # The quartic model's low bias and low variance correlate with its low test MSE, indicating that this model provides the best fit and generalization among the three.


# In summary, the bias-variance analysis at x0=0.3 aligns well with the test MSE results. 
# The quartic model, with its ability to capture higher-order relationships, outperforms the 
# simpler linear and quadratic models both in terms of bias-variance at a specific 
# point and overall test MSE. These results demonstrate the importance of model selection in achieving a 
# balance between bias and variance for optimal predictive performance.


