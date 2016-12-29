# Restaurant-revenue-prediction
## **Objective**
The objective of TFI project is to develop a model and a set of preprocess procedures to accurately predict a cross-sectional sample of Turkish restaurant revenues collected in a year.

## **Data**
The dataset consists of a training and test set with 137 and 100,000 samples respectively. This is interesting in itself since such a small training set is presented relative to the test set. 

There are 42 potential explanatory variables, including ID, open date, city, city group, type and 37 obfuscated variables (P1,P2,...,P37) within 3 categories: demographic data, commerical data and real estate data. The response variable is annual revenue.

## **Challenge**
The main challenge is that the data set is peculiar in some aspects. Thus in this case, we need to spend a lot of time on data engineering.

There are seven interesting problems with the data set and we proposed potential solutions to all of them.
#### 1. Distribution
The first problem is that the revenue variable is not normally distributed. 

Solution: We took the ln of the revenue variable and obtained something that is more similar to normal distribution. 

#### 2. Parse Date
The second problem is that the opening date variable cannot simply be assumed to be a factor. 

Solution: We proposed to create two additional features where one is the month that they opened and the other one is the year that they opened. These two factors can also help proxy seasonality difference. 

#### 3. Unaccounted value
The third problem is the unaccounted problem. There is disparity between the training set and the test set. For example, the mobile type restaurants are in the test set but not in the training set. 

Solution: We proposed a k nearest neighbor treatment for this problem. That is, we match each mobile type restaurant with non-mobile type restaurant by finding the most similar features and impute the mobile type values with similar ones. 

#### 4. Categorical vs. continous
The fourth problem is the categorical vs. continuous problem. That is, for certain features, we need to decide whether they are categorical or continuous. 

Solution: We thought that the benefits of assuming continuous values far exceed the cost. This is because, including more categorical features would result in more serious unaccounted problem. This would need treatments like knn or kmeans and would increase errors. 

#### 5. Zero
The fifth problem is the zero problem. For certain features, a large number of samples contain zero values. 

Solution: We investigated the issue carefully and based on our observation, we created the assumption that the zeros are placeholders for missing values. We used the same treatment of K nearest neighbor for treatment.

#### 6. Dimension reduction
The sixth problem is dimension reduction problem in features since the number of features is large. 

Solution: We used PCA to reduce the dimension of the data. 

#### 7. Training set size
When we have just 137 data points and some of them even look like outliers, we should be very careful with overfitting.

Solution: We use cross-validation(CV) to fit models and test models on a subset of test set. We choose the model that gives stable CV scores and decent test accuracy. 

## **Models**
#### Multiple linear regression
Multiple linear regression is used to explain the relationship between one continuous response variable from multiple explanatory variables.

#### Multiple polynomial regression
Multiple polynomial regression is a form of linear regression in which the relationship between multiple explanatory variables and the response variable is modelled as an nth degree polynomial. 

#### Local linear model 
It locally fits a weighted least squares regression on the data. 

#### Additive model
The additive model lies somewhere between fully parametric and fully non-parametric setting. Starting with a multiple linear model, we can simply replace each linear term with a general nonlinear one. 

#### Support Vector Machine (SVM)
SVM performs classification by separating classes with hyperplanes. 

#### Random forest (RF)
A random forest is an ensemble of decision trees created using random variable selection and bootstrap bagging. 

Procedure
- Create a group of decision trees.
- Each tree is created with a bootstrap sample.
- At each node of the tree, the split is created by looking at a random subset of the variables. A commonly used number for each split is the square root of the number of variables. 
- The prediction is made by averaging the predictions of all the individual trees.

#### Emsemble method
Ensemble learning involves creating base prediction algorithms and then combine them in a way that leads to better accuracy.


## **Result**
The ensembled model with SVM and RF gives the lowest RMSE 1,699,123.




