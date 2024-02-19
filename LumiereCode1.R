library(dplyr)
library(haven)
library(labelled)
library(coop)
library(stringr)
library(digitTests)

setwd("~/Desktop/GitHub/Supply_Chain-/Data/NEW")
url <- "~/Desktop/GitHub/Supply_Chain-/Data/NEW/New_Comprehensive_June_13_2022.dta"
try2 <- haven::read_dta(url, encoding="latin1")

#Understanding Data set

#1 Finding how many countries and years?

unique <- as.vector(unique(try2$country))
length(unique(try2$country)) #305 data sets

n_last <- 4
countries <- unique(substr(unique, 1, nchar(unique) - n_last)) #154 countries 

#2 Subset Dataset

df <- try2[c(1, 3, 5, 6, 8, 15, 27, 29, 47, 49, 50, 51, 53, 54, 57, 68, 71, 72, 
             76, 80, 83, 84, 87, 97, 98, 100, 101, 106, 124, 127, 129, 130, 132, 134,
             135, 136, 139, 140, 145, 157, 158, 159, 163, 165, 187, 190, 192, 
             194, 195, 196, 197, 198, 199, 217, 220, 241, 261, 266, 267, 268, 
             272, 273, 274, 286, 287, 289, 293, 292, 295, 296, 298, 315, 316, 
             317, 337, 341, 342, 355, 356, 357)]

#3 Finding the column descriptors as vectors

colnames <- as.vector(var_label(try2)) #variable name matched to descriptor/label
dfcols <- as.vector(var_label(df)) #same as above, but for subsetted df

#4 Finding the sparsity of the data set

#### IMPP!!! Should we clean the data by ourselves? or use CSR/CSC?? 

#4a Finding rows with % of NAs
#### IMP: How to decide percentage??
vals <- as.matrix(sapply(df, function(x) (sum(is.na(x))/178373)*100))
length(vals) #80
vals2Less40 <- subset(vals, vals[ , 1] < 40)
length(vals2Less40) #40 have <40% of NAs

# Creating a matrix with main analysis variables
analysisVals <- as.matrix(vals2Less40[c(-1, -2, -3, -4, -5, -6, -7, -8, -9, -10, 
                                                -11, -12, -13, -14, -15), ])
# Finding how many rows have values in all columns                              
df_less40 <- df[c(row.names(vals2Less40))]
NA_Locations1 <- as.matrix(which(is.na(df_less40), arr.ind = TRUE))
rows_with0NA <- as.vector(unique(NA_Locations1[, 1]))
No_NA_vals_dfless40 <- df_less40[-rows_with0NA, ] #the dataset with 0 NA vales
any(is.na(No_NA_vals_dfless40)) #around 85000 observations

df_analysis <- df[c(row.names(analysisVals))]
NA_Locations2 <- as.matrix(which(is.na(df_analysis), arr.ind = TRUE))
rows_with0NA2 <- as.vector(unique(NA_Locations2[, 1]))
No_NA_vals_dfanalysis <- df_analysis[-rows_with0NA2, ] #the dataset with 0 NA vales
any(is.na(No_NA_vals_dfanalysis)) #around 88000 observations

# makes sense to go ahead with df_less50, as the difference between the two is 
# minimal
"
#3 Subset Dataset
# Remove "-9" or "dont know" cells
# this dataset does not have any negative vales OR NA values!
neg_rows <- as.matrix(which(No_NA_vals_dfless50 < 0))
No_negativeRows <- No_NA_vals_dfless50[-neg_rows, ]

# kind of, this is the final dataset!

#4 Add inflation data based on months' and countries

setwd("~/Desktop/GitHub/Supply_Chain-/Data/Inflation Data")

monthly_inflation <- read_dta("monthly.dta")
vector <- as.vector(No_negativeRows$a14m)

for(i in vector) {
  var = i %% 10
  ifelse(var == 0, 
          No_negativeRows$yearmonth <- str_c(No_negativeRows$a14y, "0", No_negativeRows$a14m),
          No_negativeRows$yearmonth <- str_c(No_negativeRows$a14y, No_negativeRows$a14m))
}

vector2 <- as.vector(as.numeric(monthly_inflation$months))
vector2
try = 1
monthly_inflation$newmonth <- NA
for(i in vector2) {
  var2 = i %% 100
  monthly_inflation[try, "newmonth"] <- as.character(var2)
  try = try + 1
}

monthly_inflation$year <- NA
try3 = 1
for(i in vector2) {
  var3 = i %/% 100
  monthly_inflation[try2, "year"] <- as.character(var3)
  try3 = try3 + 1
}

monthly_inflation$yearmonth <- paste(monthly_inflation$year, monthly_inflation$newmonth)
monthly_inflation$yearmonth <- gsub(" ", "", monthly_inflation$yearmonth)

inflation <- monthly_inflation[-c(1, 2, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 
                                  20, 21)]

#remove the last 4 digits from 'country' in no_neg dataset

No_negativeRows$countryname <- substr(as.vector(No_negativeRows$country), 
                                      1, nchar(as.vector(No_negativeRows$country)) - n_last)

#join pure country name and yearmonth as 'key'

No_negativeRows$key <- paste(No_negativeRows$countryname, No_negativeRows$yearmonth)
No_negativeRows$key <- gsub(" ", "", No_negativeRows$key)

inflation$key <- paste(inflation$country, No_negativeRows$yearmonth)
inflation$key <- gsub(" ", "", inflation$key)

#merge the two datasets using 'key'

data_frame <- merge(No_negativeRows, inflation, by = "key")
data_frame <- data_frame[-c(2, 46, 47, 48, 56)]

data_frame <- data_frame[-c(44)]

data_frame <- data_frame[data_frame$f1 != -9, ]
data_frame <- data_frame[data_frame$f1 != -8, ]

"






