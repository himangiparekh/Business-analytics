library(dplyr)
library(haven)
library(labelled)
library(coop)
library(stringr)
library(digitTests)
library(naniar)

#1 Loading Full Dataset

setwd("~/Desktop/GitHub/Supply_Chain-/Data/NEW")
url <- "~/Desktop/GitHub/Supply_Chain-/Data/NEW/New_Comprehensive_June_13_2022.dta"
full_df <- haven::read_dta(url, encoding="latin1")
rm("url")

#2 Finding how many countries and years?

unique <- as.vector(unique(full_df$country))
length(unique(full_df$country)) #305 data sets

n_last <- 4
countries <- unique(substr(unique, 1, nchar(unique) - n_last)) #154 countries 

rm("countries")
rm("n_last")
rm("unique")

#3 Subset relevant columns

full_df <- full_df[-c(4, 7:24, 26, 28, 30:46, 48, 52:56, 58:70, 73:82, 85:89, 
                      92, 99, 102, 104, 105, 107:110, 112:123, 125:128, 131,
                      133, 134, 141:154, 156, 158:162, 164, 167:172, 174:183,
                      185:191, 195, 200:204, 208:209, 211:213, 215:219, 221:240, 
                      242:265, 269:280, 282:285, 287:294, 296:301, 308:311, 
                      313:314, 320:334, 336:340, 342:354, 356:369, 371:374)]

#4 Finding the column descriptors as vectors

full_df_colnames <- as.vector(var_label(full_df))

#5 Replace NEG values with NAs

for (var2 in -1:-10) {
  full_df[full_df == var2] = NA
}
any(full_df == -7)

#6 Add Inflation Dataset

setwd("~/Desktop/GitHub/Supply_Chain-/Data/Inflation Data")

monthly_inflation <- read_dta("monthly.dta") #hcpi is the only one with <40 NAs

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
  monthly_inflation[try3, "year"] <- as.character(var3)
  try3 = try3 + 1
}

monthly_inflation$yearmonth <- paste(monthly_inflation$year, monthly_inflation$newmonth)
monthly_inflation$yearmonth <- gsub(" ", "", monthly_inflation$yearmonth)

inflation <- monthly_inflation[c(3, 4, 7, 20, 21, 22)]

#remove the last 4 digits from 'country' in no_neg dataset

n_last <- 4
full_df$countryname <- substr(as.vector(full_df$country), 
                              1, nchar(as.vector(full_df$country)) - n_last)

#join pure country name and yearmonth as 'key'

full_df$yearmonth <- paste(full_df$a14y, full_df$a14m)
full_df$yearmonth <- gsub(" ", "", full_df$yearmonth)

full_df$key <- paste(full_df$countryname, full_df$yearmonth)
full_df$key <- gsub(" ", "", full_df$key)

inflation$key <- paste(inflation$country, inflation$yearmonth)
inflation$key <- gsub(" ", "", inflation$key)

#merge the two datasets using 'key'

final_df <- merge(full_df, inflation, by = "key")
final_df <- final_df[-c(2, 3, 7, 90, 91, 92, 86, 87)]

#7 Remove columns with NAs

final_df_colsNA <- as.matrix(sapply(final_df, function(x) (sum(is.na(x))/118300)
                                    *100))
#percentage of NAs in each column in final_df
final_df_cols_less40NA <- subset(final_df_colsNA, 
                                 final_df_colsNA[ , 1] < 40)
length(final_df_cols_less40NA) #48 columns from final_df have <40% of NAs

rm("final_df_colsNA")

#8 Remove all rows with NAs

final_df_less40NA <- final_df[c(row.names(final_df_cols_less40NA))]
NA_Locations1 <- as.matrix(which(is.na(final_df_less40NA), arr.ind = TRUE))
rows_with0NA <- as.vector(unique(NA_Locations1[, 1]))
No_NA_vals_dfless40 <- final_df_less40NA[-rows_with0NA, ]#the dataset with 0 NA vales
any(is.na(No_NA_vals_dfless40)) #around 20000 observations

rm(final_df_cols_less40NA)
rm(NA_Locations1)
rm(rows_with0NA)
rm(try)
rm(try3)
rm(var)
rm(var2)
rm(var3)
rm(vector)
rm(vector2)
rm(y)
rm(i)
rm(n_last)

"
#8.1 Impute NA values in columns (-9 and other NA values)
"

#9 Convert LCUs into USD

No_NA_colnames <- as.vector(var_label(No_NA_vals_dfless40))
#LCUs: d2, n2a, n2b, n3

length(as.vector(unique(No_NA_vals_dfless40$countryname))) #92 countries
unique(No_NA_vals_dfless40$countryname)

length(unique(No_NA_vals_dfless40$a14y)) #9 years
unique(No_NA_vals_dfless40$a14y)

length(unique(No_NA_vals_dfless40$key))

setwd("~/Desktop/GitHub/Supply_Chain-/Data/NEW")
exchange_rates1 <- read.csv("FINAL_LCU.csv")
length(unique(exchange_rates1$Country.Name)) #267 countries

exchange_rates2 <- exchange_rates1[c(1, 2, 55:63)]
length(unique(exchange_rates2$Country.Name)) #267 countries

#make a vector with all rownames for new table

all_countries <- as.vector(unique(No_NA_vals_dfless40$countryname))
exchange_rates2 <- exchange_rates2[order(exchange_rates2[ , 1]), ] 
vector <- as.vector(unique(exchange_rates2$Country.Name))
vector_rownames_lcu <- intersect(all_countries, vector)
length(vector_rownames_lcu)

exchangerates <- exchange_rates2
exchangerates$row <- 1:267

lcu_conversion <- 
  exchangerates[exchangerates$Country.Name %in% vector_rownames_lcu, ]

#so data for 87 out of 92 countries is there! 

No_NA_vals_dfless40$keylcu <- paste(No_NA_vals_dfless40$countryname, 
                                    No_NA_vals_dfless40$a14y)
No_NA_vals_dfless40$keylcu <- gsub(" ", "", No_NA_vals_dfless40$keylcu)

names(lcu_conversion)[names(lcu_conversion) == 'X2010'] <- "2010"
names(lcu_conversion)[names(lcu_conversion) == 'X2011'] <- "2011"
names(lcu_conversion)[names(lcu_conversion) == 'X2012'] <- "2012"
names(lcu_conversion)[names(lcu_conversion) == 'X2013'] <- "2013"
names(lcu_conversion)[names(lcu_conversion) == 'X2014'] <- "2014"
names(lcu_conversion)[names(lcu_conversion) == 'X2015'] <- "2015"
names(lcu_conversion)[names(lcu_conversion) == 'X2016'] <- "2016"
names(lcu_conversion)[names(lcu_conversion) == 'X2017'] <- "2017"
names(lcu_conversion)[names(lcu_conversion) == 'X2018'] <- "2018"

lcu_conversion <- lcu_conversion[-c(12)]

lcu_conversion <- lcu_conversion[-87, ]
vector_rownames_lcu <- vector_rownames_lcu[-87]

vectorcountries <- as.vector(rep(vector_rownames_lcu, 9))

vector2010 <- as.vector(rep(2010, 86))
vector2011 <- as.vector(rep(2011, 86))
vector2012 <- as.vector(rep(2012, 86))
vector2013 <- as.vector(rep(2013, 86))
vector2014 <- as.vector(rep(2014, 86))
vector2015 <- as.vector(rep(2015, 86))
vector2016 <- as.vector(rep(2016, 86))
vector2017 <- as.vector(rep(2017, 86))
vector2018 <- as.vector(rep(2018, 86))

vectoryears <- c(vector2010, vector2011, vector2012, vector2013, vector2014, 
                 vector2015, vector2016, vector2017, vector2018)

lcu_conversionvector <- paste(vectorcountries, 
                              vectoryears)
####!!!

lcu_conversionvector <- gsub(" ", "", lcu_conversionvector)

#make a vector which removes the conversion value in lcu_conversion for 
#specific year and country: has to be country1Y1, country2Y1, country3Y1...

lcu_conversionvalue <- c(lcu_conversion$`2010`, lcu_conversion$`2011`, 
                         lcu_conversion$`2012`, lcu_conversion$`2013`, 
                         lcu_conversion$`2014`, lcu_conversion$`2015`, 
                         lcu_conversion$`2016`, lcu_conversion$`2017`,
                         lcu_conversion$`2018`)

lcu_conversion_matrix <- cbind(lcu_conversionvector, lcu_conversionvalue)
lcu_conversion_matrix <- as.data.frame(lcu_conversion_matrix)
lcu_conversion_matrix$keylcu <- lcu_conversionvector

names(lcu_conversion_matrix)[names(lcu_conversion_matrix) == 
                               'lcu_conversionvalue'] <- "value"

try <- merge(No_NA_vals_dfless40, lcu_conversion_matrix, by = "keylcu")

try <- try[c(-45)]
try$value <- as.double(try$value)

try$d2 <- try$d2 / try$value
try$n2a <- try$n2a / try$value
try$n2b <- try$n2b / try$value

#LCUs: d2(16), n2a(42), n2b(43)

#!!!! FINAL DF = try

Final <- try

#10 Adding income information

setwd("~/Desktop")
income_country <- read.csv("country_incomecsv.csv")

inc_countries <- as.vector(unique(income_country$X.1))
vector_Final <- as.vector(unique(Final$countryname))
vector_rownames_inc <- intersect(inc_countries, vector_Final)

income_country <- income_country[c(2:11)]

names(income_country)[names(income_country) == 'FY10'] <- "2010"
names(income_country)[names(income_country) == 'FY11'] <- "2011"
names(income_country)[names(income_country) == 'FY12'] <- "2012"
names(income_country)[names(income_country) == 'FY13'] <- "2013"
names(income_country)[names(income_country) == 'FY14'] <- "2014"
names(income_country)[names(income_country) == 'FY15'] <- "2015"
names(income_country)[names(income_country) == 'FY16'] <- "2016"
names(income_country)[names(income_country) == 'FY17'] <- "2017"
names(income_country)[names(income_country) == 'FY18'] <- "2018"

vectorcountries <- as.vector(rep(vector_rownames_inc, 9))

income_country <- 
  income_country[income_country$X.1 %in% vector_rownames_lcu, ]
income_country$keyinc <- lcu_conversionvector

inc_conversioninc <- c(income_country$`2010`, income_country$`2011`, 
                       income_country$`2012`, income_country$`2013`, 
                       income_country$`2014`, income_country$`2015`, 
                       income_country$`2016`, income_country$`2017`,
                       income_country$`2018`)
inc_conversion_matrix <- cbind(lcu_conversionvector, inc_conversioninc)
inc_conversion_matrix <- as.data.frame(inc_conversion_matrix)

Final2 <- merge(Final, inc_conversion_matrix, by.x = "keylcu", 
                by.y = "lcu_conversionvector")

Final <- Final2

Final <- Final[-c(16)]

#11 Lasso

library(glmnet)
library(gamlr)

Final <- Final[-c(1:2, 46, 48:49)]
Final <- Final[-c(1)]

x <- data.matrix(Final[, colnames(Final[-c(13)])])
y <- log(Final$d2)

cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model)

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#remove b1, d30a

"
#12 Double Lasso

xDL <- data.matrix(Final[, colnames(Final[-c(32)])])
yDL <- log(Final$k8)

cv_model_DL <- cv.glmnet(xDL, yDL, alpha = 1)
best_lambda_DL <- cv_model_DL$lambda.min
best_lambda_DL

plot(cv_model_DL)

best_model_DL <- glmnet(xDL, yDL, alpha = 1, lambda = best_lambda_DL)
coef(best_model_DL)
"

Final <- Final[-c(5, 14)]

"
#13 Split data into test and train
"

#14 X-Learners for Line of Credit

Final$k8[Final$k8 == 2] <- 0

loan_feat <- Final[-c(12, 31)]
loan_tr <- unlist(as.vector(Final[, 31]))
loan_yobs <- as.vector(Final[, 12])

# Make factor
lfactor <- c(5:7, 26:28, 31:32, 35:36, 39)
loan_feat[, -lfactor] <- lapply(loan_feat[, -lfactor], as_factor)

xl_rf_loan <- X_RF(feat = loan_feat, tr = loan_tr, yobs = loan_yobs)
cate_rf_loan <- EstimateCate(xl_rf_loan, loan_feat)
summary(cate_rf_loan)
plot(cate_rf_loan)

#15 code for unfounding yearsmonths

lyears <- seq(2010, 2018, 1)
lmonths <- seq(1, 12, 1)

df_conf_yrm <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                       a14m = lmonths, a14y = lyears, b2a = 50, b2b = 50, b2c = 50, 
                       b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                       c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                       e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                       h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                       i30 = "Major Obstacle", j30a = "Major Obstacle", 
                       j30b = "Major Obstacle", j30c = "Major Obstacle", 
                       j30e = "Major Obstacle", j30f = "Major Obstacle", k2c = 50,
                       k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                       l1 = 50, l2 = 50, l10 = "No", l30a = "Major Obstacle", 
                       n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                       countryname = "Afghanistan", hcpi_m = 50, 
                       inc_conversioninc = "L")

df_conf_yrm[, -lfactor] <- lapply(df_conf_yrm[, -lfactor], as_factor)

cate_rf_loan_yrm <- EstimateCate(xl_rf_loan, df_conf_yrm)

plot(cate_rf_loan_yrm, type = "l")

#16 stra_sector

lsector <- as.vector(unique(loan_feat$stra_sector))

df_conf_sector <- expand.grid(stra_sector = lsector, size = "Medium(20-99)",
                           a14m = 5, a14y = 2013, b2a = 50, b2b = 50, b2c = 50, 
                           b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                           c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                           e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                           h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                           i30 = "Major Obstacle", j30a = "Major Obstacle", 
                           j30b = "Major Obstacle", j30c = "Major Obstacle", 
                           j30e = "Major Obstacle", j30f = "Major Obstacle", k2c = 50,
                           k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                           l1 = 50, l2 = 50, l10 = "No", l30a = "Major Obstacle", 
                           n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                           countryname = "Afghanistan", hcpi_m = 50, 
                           inc_conversioninc = "L")

df_conf_sector[, -lfactor] <- lapply(df_conf_sector[, -lfactor], as_factor)

cate_rf_loan_sector <- EstimateCate(xl_rf_loan, df_conf_sector)

plot(cate_rf_loan_sector, type = "l")

#17 domestic

ldom <- seq(0, 100, 1)

df_conf_dom <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                              a14m = 5, a14y = 2013, b2a = ldom, b2b = 50, b2c = 50, 
                              b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                              c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                              e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                              h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                              i30 = "Major Obstacle", j30a = "Major Obstacle", 
                              j30b = "Major Obstacle", j30c = "Major Obstacle", 
                              j30e = "Major Obstacle", j30f = "Major Obstacle", k2c = 50,
                              k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                              l1 = 50, l2 = 50, l10 = "No", l30a = "Major Obstacle", 
                              n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                              countryname = "Afghanistan", hcpi_m = 50, 
                              inc_conversioninc = "L")

df_conf_dom[, -lfactor] <- lapply(df_conf_dom[, -lfactor], as_factor)

cate_rf_loan_dom <- EstimateCate(xl_rf_loan, df_conf_dom)

plot(cate_rf_loan_dom, type = "l")

#18 international

lint <- seq(0, 100, 1)

df_conf_int <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                           a14m = 5, a14y = 2013, b2a = 50, b2b = lint, b2c = 50, 
                           b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                           c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                           e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                           h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                           i30 = "Major Obstacle", j30a = "Major Obstacle", 
                           j30b = "Major Obstacle", j30c = "Major Obstacle", 
                           j30e = "Major Obstacle", j30f = "Major Obstacle", k2c = 50,
                           k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                           l1 = 50, l2 = 50, l10 = "No", l30a = "Major Obstacle", 
                           n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                           countryname = "Afghanistan", hcpi_m = 50, 
                           inc_conversioninc = "L")

df_conf_int[, -lfactor] <- lapply(df_conf_int[, -lfactor], as_factor)

cate_rf_loan_int <- EstimateCate(xl_rf_loan, df_conf_int)

plot(cate_rf_loan_int, type = "l")

#19 gov

lgov <- seq(0, 100, 1)

df_conf_gov <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                           a14m = 5, a14y = 2013, b2a = 50, b2b = 50, b2c = lgov, 
                           b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                           c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                           e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                           h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                           i30 = "Major Obstacle", j30a = "Major Obstacle", 
                           j30b = "Major Obstacle", j30c = "Major Obstacle", 
                           j30e = "Major Obstacle", j30f = "Major Obstacle", k2c = 50,
                           k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                           l1 = 50, l2 = 50, l10 = "No", l30a = "Major Obstacle", 
                           n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                           countryname = "Afghanistan", hcpi_m = 50, 
                           inc_conversioninc = "L")

df_conf_gov[, -lfactor] <- lapply(df_conf_gov[, -lfactor], as_factor)

cate_rf_loan_gov <- EstimateCate(xl_rf_loan, df_conf_gov)

plot(cate_rf_loan_gov, type = "l")

#20 start year

lstart_op <- seq(1785, 2014, 1)

df_conf_start_op <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                           a14m = 5, a14y = 2013, b2a = 50, b2b = 50, b2c = 50, 
                           b5 = lstart_op, c22b = "No", c30a = "Major Obstacle", 
                           c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                           e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                           h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                           i30 = "Major Obstacle", j30a = "Major Obstacle", 
                           j30b = "Major Obstacle", j30c = "Major Obstacle", 
                           j30e = "Major Obstacle", j30f = "Major Obstacle", k2c = 50,
                           k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                           l1 = 50, l2 = 50, l10 = "No", l30a = "Major Obstacle", 
                           n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                           countryname = "Afghanistan", hcpi_m = 50, 
                           inc_conversioninc = "L")

df_conf_start_op[, -lfactor] <- lapply(df_conf_start_op[, -lfactor], as_factor)

cate_rf_loan_start_op <- EstimateCate(xl_rf_loan, df_conf_start_op)

plot(df_conf_start_op$b5, cate_rf_loan_start_op, type = "l")

#21 sales paid aft delivery

lcr_sales <- seq(0, 100, 1)

df_conf_cr_sales <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                                a14m = 5, a14y = 2013, b2a = 50, b2b = 50, b2c = 50, 
                                b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                                c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                                e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                                h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                                i30 = "Major Obstacle", j30a = "Major Obstacle", 
                                j30b = "Major Obstacle", j30c = "Major Obstacle", 
                                j30e = "Major Obstacle", j30f = "Major Obstacle", 
                                k2c = lcr_sales,
                                k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                                l1 = 50, l2 = 50, l10 = "No", l30a = "Major Obstacle", 
                                n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                                countryname = "Afghanistan", hcpi_m = 50, 
                                inc_conversioninc = "L")

df_conf_cr_sales[, -lfactor] <- lapply(df_conf_cr_sales[, -lfactor], as_factor)

cate_rf_loan_cr_sales <- EstimateCate(xl_rf_loan, df_conf_cr_sales)

plot(df_conf_cr_sales$k2c, cate_rf_loan_cr_sales, type = "l")

#22 working capital

lwc <- seq(0, 100, 1)

df_conf_wc <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                                a14m = 5, a14y = 2013, b2a = 50, b2b = 50, b2c = 50, 
                                b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                                c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                                e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                                h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                                i30 = "Major Obstacle", j30a = "Major Obstacle", 
                                j30b = "Major Obstacle", j30c = "Major Obstacle", 
                                j30e = "Major Obstacle", j30f = "Major Obstacle", 
                                k2c = 50,
                                k3bc = lwc, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                                l1 = 50, l2 = 50, l10 = "No", l30a = "Major Obstacle", 
                                n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                                countryname = "Afghanistan", hcpi_m = 50, 
                                inc_conversioninc = "L")

df_conf_wc[, -lfactor] <- lapply(df_conf_wc[, -lfactor], as_factor)

cate_rf_loan_wc <- EstimateCate(xl_rf_loan, df_conf_wc)

plot(df_conf_wc$k3bc, cate_rf_loan_wc, type = "l")

#23 credit purchases

lcr_purchases <- seq(0, 100, 1)

df_conf_cr_purchases <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                          a14m = 5, a14y = 2013, b2a = 50, b2b = 50, b2c = 50, 
                          b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                          c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                          e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                          h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                          i30 = "Major Obstacle", j30a = "Major Obstacle", 
                          j30b = "Major Obstacle", j30c = "Major Obstacle", 
                          j30e = "Major Obstacle", j30f = "Major Obstacle", 
                          k2c = 50,
                          k3bc = 50, k3f = lcr_purchases, k4 = "No", k30 = "Major Obstacle", 
                          l1 = 50, l2 = 50, l10 = "No", l30a = "Major Obstacle", 
                          n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                          countryname = "Afghanistan", hcpi_m = 50, 
                          inc_conversioninc = "L")

df_conf_cr_purchases[, -lfactor] <- lapply(df_conf_cr_purchases[, -lfactor], as_factor)

cate_rf_loan_cr_purchases <- EstimateCate(xl_rf_loan, df_conf_cr_purchases)

plot(df_conf_cr_purchases$k3f, cate_rf_loan_cr_purchases, type = "l")

#24 employees

lemployees <- seq(0, 21955, 1)

df_conf_employees <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                                    a14m = 5, a14y = 2013, b2a = 50, b2b = 50, b2c = 50, 
                                    b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                                    c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                                    e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                                    h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                                    i30 = "Major Obstacle", j30a = "Major Obstacle", 
                                    j30b = "Major Obstacle", j30c = "Major Obstacle", 
                                    j30e = "Major Obstacle", j30f = "Major Obstacle", 
                                    k2c = 50,
                                    k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                                    l1 = lemployees, l2 = 50, l10 = "No", 
                                 l30a = "Major Obstacle", 
                                    n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                                    countryname = "Afghanistan", hcpi_m = 50, 
                                    inc_conversioninc = "L")

df_conf_employees[, -lfactor] <- lapply(df_conf_employees[, -lfactor], as_factor)

cate_rf_loan_employees <- EstimateCate(xl_rf_loan, df_conf_employees)

plot(df_conf_employees$l1, cate_rf_loan_employees, type = "l")

#25 countryname

lcountry <- as.vector(unique(loan_feat$countryname))

df_conf_country <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                                 a14m = 5, a14y = 2013, b2a = 50, b2b = 50, b2c = 50, 
                                 b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                                 c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                                 e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                                 h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                                 i30 = "Major Obstacle", j30a = "Major Obstacle", 
                                 j30b = "Major Obstacle", j30c = "Major Obstacle", 
                                 j30e = "Major Obstacle", j30f = "Major Obstacle", 
                                 k2c = 50,
                                 k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                                 l1 = 50, l2 = 50, l10 = "No", 
                                 l30a = "Major Obstacle", 
                                 n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                                 countryname = lcountry, hcpi_m = 50, 
                                 inc_conversioninc = "L")

df_conf_country[, -lfactor] <- lapply(df_conf_country[, -lfactor], as_factor)

cate_rf_loan_lcountry <- EstimateCate(xl_rf_loan, df_conf_country)

plot(cate_rf_loan_lcountry, type = "l")

#26 inflation

linfl <- seq(65, 6752, 1)

df_conf_infl <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                               a14m = 5, a14y = 2013, b2a = 50, b2b = 50, b2c = 50, 
                               b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                               c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                               e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                               h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                               i30 = "Major Obstacle", j30a = "Major Obstacle", 
                               j30b = "Major Obstacle", j30c = "Major Obstacle", 
                               j30e = "Major Obstacle", j30f = "Major Obstacle", 
                               k2c = 50,
                               k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                               l1 = 50, l2 = 50, l10 = "No", 
                               l30a = "Major Obstacle", 
                               n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                               countryname = "Afghanistan", hcpi_m = linfl, 
                               inc_conversioninc = "L")

df_conf_infl[, -lfactor] <- lapply(df_conf_infl[, -lfactor], as_factor)

cate_rf_loan_infl <- EstimateCate(xl_rf_loan, df_conf_infl)

plot(cate_rf_loan_infl, type = "l")

#27 size

lsize <- as.vector(unique(loan_feat$size))

df_conf_size <- expand.grid(stra_sector = "Manufacturing", size = lsize,
                            a14m = 5, a14y = 2013, b2a = 50, b2b = 50, b2c = 50, 
                            b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                            c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                            e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                            h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                            i30 = "Major Obstacle", j30a = "Major Obstacle", 
                            j30b = "Major Obstacle", j30c = "Major Obstacle", 
                            j30e = "Major Obstacle", j30f = "Major Obstacle", 
                            k2c = 50,
                            k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                            l1 = 50, l2 = 50, l10 = "No", 
                            l30a = "Major Obstacle", 
                            n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                            countryname = "Afghanistan", hcpi_m = 50, 
                            inc_conversioninc = "L")

df_conf_size[, -lfactor] <- lapply(df_conf_size[, -lfactor], as_factor)

cate_rf_loan_size <- EstimateCate(xl_rf_loan, df_conf_size)

plot(df_conf_size$size, cate_rf_loan_size, type = "b")

#28 website

lwebsite <- as.vector(unique(loan_feat$c22b))

df_conf_website <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                            a14m = 5, a14y = 2013, b2a = 50, b2b = 50, b2c = 50, 
                            b5 = 2010, c22b = lwebsite, c30a = "Major Obstacle", 
                            c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                            e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                            h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                            i30 = "Major Obstacle", j30a = "Major Obstacle", 
                            j30b = "Major Obstacle", j30c = "Major Obstacle", 
                            j30e = "Major Obstacle", j30f = "Major Obstacle", 
                            k2c = 50,
                            k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                            l1 = 50, l2 = 50, l10 = "No", 
                            l30a = "Major Obstacle", 
                            n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                            countryname = "Afghanistan", hcpi_m = 50, 
                            inc_conversioninc = "L")

df_conf_website[, -lfactor] <- lapply(df_conf_website[, -lfactor], as_factor)

cate_rf_loan_website <- EstimateCate(xl_rf_loan, df_conf_website)

plot(df_conf_website$c22b, cate_rf_loan_website, type = "l")

#29 income

lincome <- as.vector(unique(loan_feat$inc_conversioninc))

df_conf_income <- expand.grid(stra_sector = "Manufacturing", size = "Medium(20-99)",
                               a14m = 5, a14y = 2013, b2a = 50, b2b = 50, b2c = 50, 
                               b5 = 2010, c22b = "No", c30a = "Major Obstacle", 
                               c30b = "Major Obstacle", d30b = "Major Obstacle", e6 = "No",
                               e11 = "No", e30 = "Major Obstacle", g30a = "Major Obstacle",
                               h5 = "No", h8 = "No", h30 = "Major Obstacle", 
                               i30 = "Major Obstacle", j30a = "Major Obstacle", 
                               j30b = "Major Obstacle", j30c = "Major Obstacle", 
                               j30e = "Major Obstacle", j30f = "Major Obstacle", 
                               k2c = 50,
                               k3bc = 50, k3f = 50, k4 = "No", k30 = "Major Obstacle", 
                               l1 = 50, l2 = 50, l10 = "No", 
                               l30a = "Major Obstacle", 
                               n2a = 90289, n2b = 90289, sector_MS = "Manufacturing",
                               countryname = "Afghanistan", hcpi_m = 50, 
                               inc_conversioninc = lincome)

df_conf_income[, -lfactor] <- lapply(df_conf_income[, -lfactor], as_factor)

cate_rf_loan_income <- EstimateCate(xl_rf_loan, df_conf_income)

plot(df_conf_income$inc_conversioninc, cate_rf_loan_income, type = "l")

#30 save everything

Finalcolnames <- as.matrix(var_label(Final))
write.table(Finalcolnames, file = "Final Column Names.csv")

