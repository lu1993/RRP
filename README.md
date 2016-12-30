# Restaurant-revenue-prediction
## **Objective**
The objective of TFI project is to develop a model and a set of preprocess procedures to accurately predict a cross-sectional sample of Turkish restaurant revenues collected in a year.

## **Data**
The dataset consists of a training and test set with 137 and 100,000 samples respectively. This is interesting in itself since such a small training set is presented relative to the test set.(there are some junk data in the test set to avoid overfitting.) 

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
The third problem is the unaccounted problem. There is disparity between the training set and the test set. For "type", the mobile type restaurants are in the test set but not in the training set. For "city", there are 34 unique cities in the training set and 57 unique cities in the test set. 

Solution: We proposed different approaches for "type" and "city". 

(1) For "type", we apply a k nearest neighbor treatment. That is, we match each mobile type restaurant with non-mobile type restaurant by finding the most similar features and impute the mobile type values with similar ones. 

(2) For "city", we apply k-means treatment. Since p-variables include geographical data, we plot each p-variable over city to identify which could be a proxy for geographical information. We use k-means clustering to classify cities into clusters based on these variables and use clustering centroid to represent city type. 

#### 4. Zeros
The fourth problem is the zero problem. For certain features, a large number of samples contain zero values. By counting the number of zeros for each p-variable and each row, we find:

(1) The number of zeros are quite frequent and consistent for 17 p-variables(p14-18,p24-27,p30-37). 88 out of 137 are zeros for each of these variables.

(2) We think that these variables are dependent because these variables take on zero values together. 

Solution: We used KNN treatment, trying to impute these variables with their nearest neighbours. But the result didn't improve after applying the KNN imputation and it's quite computationally expensive. Actually, we created the assumption that these zeros are placeholders for missing values. For these variables with many missing values, we can choose to delete them or treat them as categorical variables with missing values as one condition. The later one seems safer since we don't want to delete any useful information. 

#### 5. Categorical vs. continous
For the rest 20 p-variables, we need to decide whether they are categorical or continuous. 

Solution: Unaccounted problem persists if we treat them as categorical data, thus we thought that treating them as continous variable would be better unless there're only 2 unique values for the variable. 

#### 6. Dimension reduction
The sixth problem is dimension reduction problem in features since the number of features is large. 

Solution: We tried two methods to reduce dimension and choose the one with better performance. 

(1) remove variables with high correlation. 

(2) use PCA to reduce the dimension of the data.

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
The RF gives the lowest RMSE 1,699,123.
