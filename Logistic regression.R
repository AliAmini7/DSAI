library(ggplot2)
library(cowplot)

address <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data <- read.csv(address, header = FALSE)

head (data)

colnames(data) <- c(
  "age",
  "sex",
  "cp", # chest pain
  # 1 = typical angina,
  # 2 = atypical angina,
  # 3 = non-anginal pain,
  # 4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
  # 1 = normal
  # 2 = having ST-T wave abnormality
  # 3 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment
  # 1 = upsloping
  # 2 = flat
  # 3 = downsloping
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
  # 3 = normal (no cold spots)
  # 6 = fixed defect (cold spots during rest and exercise)
  # 7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)

head(data)

str(data)
# there are some data recorded as ?. we will substitute NA 
data[data == '?'] <- NA

data[data$sex == 0,]$sex <- 'F'
data[data$sex == 1,]$sex <- 'M'

data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

data$hd <- ifelse(test = data$hd == 0, yes = "healthy", no = "unhealthy")
data$hd <- as.factor(data$hd)

str(data)

nrow(data[is.na(data$ca) | is.na(data$thal),])
data[is.na(data$ca) | is.na(data$thal),]

nrow(data)
data <- data[!(is.na(data$ca) | is.na(data$thal)),]
nrow(data)
# we want to make sure that both males and females have HD in
# this sample. we use xtabs to create a contingency table.
xtabs(~ hd + sex, data = data)
# we should do the same for the other categorical variables too
xtabs(~ hd + cp, data = data)
xtabs(~ hd + fbs, data = data)
xtabs(~ hd + restecg , data = data)
xtabs(~ hd + slope , data = data)
xtabs(~ hd + ca, data = data)

# now let's do a simple modeling by only considering sex as the predictor.

logistic <- glm(hd ~ sex, data = data, family = "binomial")
summary(logistic)
# now let's have a full model
logistic <- glm(hd ~ ., data = data, family = "binomial")
summary(logistic)

