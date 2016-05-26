***

## Problem description



Problem we are trying to handle consist of combining historical usage patterns with weather data in order to forecast bike rental demand in the [Capital Bikeshare](http://www.capitalbikeshare.com/system-data) program in Washington, D.C. Bike sharing systems are a means of renting bicycles in an automated way via a network of kiosk locations throughout a city. Using these systems, people are able rent a bike from a one location and return it to a different place on an as-needed basis.

The problem has been posted on [Kaggle](https://www.kaggle.com) and more information about the comptetion you can find [here](https://www.kaggle.com/c/bike-sharing-demand). 


Data set is comprised of the first 19 days of each month from 2011 to 2012. The objective of the comptetion was to predict the total number of bike rents for each  hours.

**Data Fields**

* datetime - hourly date + timestamp;
* season -  1 = spring, 2 = summer, 3 = fall, 4 = winter;
* holiday - whether the day is considered a holiday;
* workingday - whether the day is neither a weekend nor holiday;
* weather:
    + 1: Clear, Few clouds, Partly cloudy, Partly cloudy;
    + 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist;
    + 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds;
    + 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog.
* temp - temperature in Celsius;
* atemp - "feels like" temperature in Celsius;
* humidity - relative humidity;
* windspeed - wind speed;
* **casual** - number of non-registered user rentals initiated;
* **registered** - number of registered user rentals initiated;
* **count** - number of total rentals.


***

## Setting Up: Load the packages and prepare input data

First of all, we are going to install all the required packages unless you didn't do this before.

```{r warning=FALSE, eval=FALSE}
packs = c("e1071", "PerformanceAnalytics", "Hmisc", "corrplot", "DMwR",
          "maggrittr", "tidyr", "dplyr", "randomForest")
install.packages(packs, dependencies = TRUE)
```

Once installed the required packages we need to load it to currant environment.

```{r warning=FALSE, eval=TRUE, message=FALSE}
library(e1071)  # for skewness
library(PerformanceAnalytics)  # for correlogram (chart.Correlation)
library(Hmisc)  # for missing value treatement (impute)
library(corrplot)  # for correlogram (corrplot)
library(DMwR)  # for knnImputation
library(magrittr)  # pipeline operation
library(tidyr)  # data tiding
library(dplyr)  # data processing functions
library(randomForest)  # apply random forest algorithm
```

There are a lot of ways to import data into R. The most popular one is reading from  a `csv` file. We can do it as follow:

```{r echo=FALSE}
# invisible code to set working directory
# set_path_to_your_working_directory = "/home/togrul/Dropbox/Data Challenge/CEDAWI Presentation/Bike problem"

set_path_to_your_working_directory = "C:\\Users\\t.jafarov\\Dropbox\\Data Challenge\\CEDAWI Presentation\\Bike problem"
```


```{r eval=TRUE, warning=FALSE}
# set your working directory
setwd(set_path_to_your_working_directory)
inputData = read.csv("data/train.csv", header = TRUE, stringsAsFactors = FALSE)
```

Now, we have imported data into R, first thing to do as a data analyst is to try to have some idea about data. `R` has very usefull functions to handle that. Let's first check the `summary` function, which will give us `descriptive statistics` of data set.

```{r warning=FALSE}
summary(inputData)
```

Another important information about data set is `data types`:

```{r warning=FALSE}
str(inputData)
```


***

## Creating derived variables

Before moving on to exploratory analysis, it is a good idea to derive as many new variables from your predictors, that you think would be relevant for your dependent variable. In this way we can capture all the nuances that might be an explanatory cause for the dependent variable.

Along this analysis we are going to use `%>%` pipeline operation from `magrittr` package, which makes code easy to read and easy to write. We are going to split `datetime` variable in two differant ways.

Column splitting with `default operations` :

```{r warning=FALSE, eval=FALSE}
inputData = separate(inputData, col = datetime, into = c("date", "time"), sep = " ", remove = FALSE)
inputData = separate(inputData, col = date, into = c("year", "month", "day"), sep = "-", remove = TRUE)
inputData = separate(inputData, col = time, into = c("hour", "minute", "second"), sep = ":", remove = TRUE)
```

Column splitting with `pipeline operation`:

```{r warning=FALSE}
inputData = inputData %>%
  separate(col = datetime, into = c("date", "time"), sep = " ", remove = FALSE) %>%
  separate(col = date, into = c("year", "month", "day"), sep = "-", remove = TRUE) %>%
  separate(col = time, into = c("hour", "minute", "second"), sep = ":", remove = TRUE)
```

Another usefull function in `R` is `head`, which shows you first (6) rows of data.

```{r}
head(inputData)
```


**Differant types of variables.**  
A **variable** is a property that can take on many values. For example: `temp` is variable. It can take on many different values, such as 2.4, 3.0 so on. `season` is a variable. It can take 4 different values, 1,2,3 or 4. Two kinds of varaibles:


* **continue** variables are measured numerically. With continue variables we can doo things like add, substract, multiply and divide and get meaningful information (in `temp` example 4.0 is higher than 3.0 by 1.0).
* **categorical** variables allow for classification based on some characteristic. We can not do thinks that we could do with continious variables (in the example of `season` we can not say that season 2 is two times than season 1).


Data types are important in R.

```{r warning=FALSE}
# convert string to datetime data type
inputData$datetime = strptime(x = inputData$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# get categorical variables
fcts = c("month", "hour", "season", "holiday", "workingday", "day", "weather")

# set string to categorical data type
for(i in 1:length(fcts)) inputData[,fcts[i]] = as.factor(inputData[,fcts[i]])
```


Let's look at the `categorical varaibales` in detail.
```{r}
sapply(inputData[,c("month", "hour", "season", "holiday", "workingday", "day", "weather")], summary)  # get summary statistics for all columns
```

We can mention that `holiday=1` is occurs only `<3%` of the time in the data set. On the other hand, we can explain holidays with workingdays as well. So we can eliminate it from further analysis.

```{r eval=FALSE}
inputData_cat$holiday = NULL
```

Another interesting point is `weather` variable. `weather=1` takes an important part of data set and other 3 seasons are minority so we can regroup them into `season=1` and `season=2` as follows:

```{r}
inputData = within(inputData, {
  weather_grouped=ifelse(weather == 1, 1, 2)
})
# set string to categorical data type
inputData$weather_grouped = as.factor(inputData$weather_grouped)
```


Segregate all continuous and categorical variables into differant variables.

```{r warning=FALSE, message=FALSE}
# Place all continuous vars in inputData_cont
inputData_cont <- inputData[, c("temp", "humidity", "windspeed")]

# Place all categorical variables in inputData_cat
inputData_cat <- inputData[, c("month", "hour", "season", "workingday", "weather_grouped")]

# Place day variable seperately
inputData_day = inputData$day
```

During this presentation we are going to forecast `registered` users only.

```{r warning=FALSE, message=FALSE}
# create the response data frame
inputData_response <- data.frame(registered=inputData[, "registered"])
response_name <- "registered"  # name of response variable
response <- inputData[, response_name]  # response variable as a vector
```


***

## Exploratory analysis

Now, let's generate different types of plots: Density, Scatter, Box plots in order to have more information about continious variables itself and interaction with response variables, which is`registered` bike users. Following code allows you automaticly generate three types of plots and save it into seperate `.png` files.

```{r warning=FALSE, message=FALSE}
for (k in names(inputData_cont)){
  png(file=paste(k,"_dens_scatter_box" ,".png", sep=""), width=900, height=550)
  x <- as.numeric (inputData_cont[, k])
  Skewness <- round(skewness(x), 2)  # calc skewness
  dens <- density(x, na.rm=T)  # density func
  par(mfrow=c(1, 3))  # setup plot-area in 3 columns

  # Density plot
  plot(dens, type="l", col="red", ylab="Frequency", xlab = k, main = paste(k, ": Density Plot"), sub=paste("Skewness: ", Skewness))
  polygon(dens, col="red")
  
  # scatterplot
  plot(x, response, col="blue", ylab="Response", xlab = k, main = paste(k, ": Scatterplot"), pch=20)
  abline(response ~ x)
  
  # boxplot
  boxplot(x, main=paste(k, ": Box Plot"), sub=paste("Outliers: ", paste(boxplot.stats(x)$out, collapse=" ")))
  
  dev.off()
}
```

The above code will create the density plot, scatter plot and box-plot, all within one frame, for all the variables in input data and store it in your working directory. It is preferable for density plot to have a bell shaped curve aligned to the center of chart area. Scatterplot reveals if there is a underlying linear relationship between the predictor and response while the box-plot reveals any outliers in the variable (in which case, dots will be present outside the top and bottom lines (IQR)).


***

## Outlier analysis

Outlier analysis is one the most important steps of data analysis and there are a lot of methods to handle that. There are differant reasons to have outliers on the data set.

One way of addressing outlier problem is eliminating from data set. In R you can remove outliers as follow:

```{r eval=FALSE}
x <- x[!x %in% boxplot.stats(x)$out]
```

But we are not going to remove outliers, instead of that we will replace those with `NA` and treate them as missing values.

```{r warning=FALSE, message=FALSE}
# Function to replace outliers with NA
replace_outlier_with_missing <- function(x, na.rm = TRUE, ...) {

  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)  # get %iles
  H <- 1.5 * IQR(x, na.rm = na.rm)  # outlier limit threshold
  y <- x
  y[x < (qnt[1] - H)] <- NA  # replace values below lower bounds
  y[x > (qnt[2] + H)] <- NA  # replace values above higher bound
  y  # returns treated variable
  
}
```

After replacing outliers in continious variables with `NA`s we can combine them together:

```{r warning=FALSE, message=FALSE}
inputData = as.data.frame (sapply(inputData_cont, replace_outlier_with_missing)) %>%
  cbind(inputData_response, ., inputData_cat)
```

We can find number of missing values per column as well:

```{r message=FALSE, warning=FALSE}
apply(inputData, 2, function(x) sum(is.na(x)))
```

***

## Missing value treatement

**Approaches for handling missing values**

**Eliminating the variable**: If a particular variable has more than 30% missing values, it is advisable to consider removing that variable altogether.
**Eliminating the observations**: If there are missing values scattered through out your dataset and you have a good number of observations in your sample, you may choose to eliminate those observations with missing values. After doing this, if you find losing more than 30% of sample size, then you probably need to look for any particular variable that is contributing the most number of missing values.
**Imputation**: Alternatively, you may also impute the missing values with one of these place holders.


There are several methods to handle missing values:

* Method 1: Replace missing values with `mean` / `median` / `most frequent` value(s);

```{r eval=FALSE}
# impute missing values with the mean using 'impute' func from 'Hmisc' package
inputdata_cont_matrix <- sapply(inputData_cont, FUN=function(x){impute(x, mean)})
```

* Method 2: Predict the missing values by modelling it as a dependent variable with a regression model based approach;

* Method 3: k-Nearest neighbor imputation method.

```{r message=FALSE, warning=FALSE}
if( anyNA(inputData) ) {
  # k = 10
  inputData[, !names(inputData) %in% response_name] <- knnImputation(inputData[, !names(inputData) %in% response_name])
  
}
```


***

## Correlation analysis

The correlogram is a graphical representation of correlation amongst all combinations of response and predictor variables. The investigator may choose to pick variables that have high correlation with the response based on a pre-determined cutoff. But we do not do that filtering here, since it is possible that a variable with relatively lower correlation can prove significant and valuable in regression models to capture unexplained variations.

Let's see the correlation between variables:

```{r message=FALSE, warning=FALSE}
inputData_response_cont_noNA <- na.omit(cbind(inputData_response, inputData_cont))

chart.Correlation(inputData_response_cont_noNA, histogram=TRUE, pch=19)
```

***

## Data modelling

How to find the best model?


Split data set into validation and training data sets.

```{r eval=TRUE}

inputData = cbind(day=inputData_day, inputData)

test = inputData %>% filter(day %in% c(17,18,19))
train = inputData %>% filter(! day %in% c(17,18,19))
```


Size of the data sets:

```{r eval=TRUE}
dim(test); dim(train)
```


**Cross-validation**  

We are going to use `Random Forest` algorithm. In order to find the best parameter for algorithm we will tune on `ntree` parameter using `5-fold cross validation`.


```{r eval=TRUE}
k = 5
# tuning parameter: number of trees.
trees = c(30, 50, 70)
frml = "registered ~ .-day"
errors = matrix(data = NA, nrow = k, ncol = length(trees))

for(i in 1:length(trees)){
  
  for(j in seq(k)){
    
    index = sample(1:16, 3, replace = FALSE)
    test.cv = filter(train, day %in% index)
    train.cv = filter(train, ! day %in% index)
    
    # fit the model
    fit = randomForest(as.formula(frml), data = train.cv, ntree = trees[i])
    pred = predict(fit, test.cv)
    
    # Root-mean-square error - RMSE
    errors[j,i] = sqrt(sum((test.cv$registered - pred)^2) / length(test.cv))
    
  }
  
}

```

Let's find out the best parameter for `random forest`.

```{r}
## plot the results
mean.cv.errors=apply(errors,2,mean)
mean.cv.errors
mean.cv.errors[which.min(mean.cv.errors)]
plot(trees, mean.cv.errors,type='b', main="Parameter tuning",
     xlab="Number of trees", ylab="Mean RMSE"); grid()
points(trees[which.min(mean.cv.errors)], mean.cv.errors[which.min(mean.cv.errors)], col="red", pch=20)

```

Once we find out the best parameter, now we can calculate the test error of model on `test` data.

```{r warning=FALSE, message=FALSE}
# Train model on `training` data set
model = randomForest(as.formula(frml), data = train, ntree = which.min(mean.cv.errors))

# Get the test error
pred = predict(model, test)
```



```{r warning=FALSE, message=FALSE}
# Root-mean-square error
sqrt(sum((test$registered - pred)^2) / length(test))
```


## Follow me
Twitter [toghrul_jafarov](https://twitter.com/toghrul_jafarov)  
GitHub [jtoghrul](https://github.com/jtoghrul)

## Referance

* [Advanced Linear Regression](http://rstatistics.net/linear-regression-advanced-modelling-algorithm-example-with-r/).
* [An Introduction to Statistical Learning](http://www-bcf.usc.edu/~gareth/ISL/).
* [SciPY 2015](http://scipy2015.scipy.org/ehome/index.php?eventid=115969&).
