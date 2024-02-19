library(dplyr)
library(haven)
library(tidyverse)
library(ggplot2)
library("car")

#turkey2002
turkey2005 <- read_dta("Turkey-2005--full data-5.dta")
turkey2008 <- read_dta("Turkey-2008--full-data-.dta")
turkey2013 <- read_dta("Turkey-2013-full-data.dta")
turkey2015 <- read_dta("Turkey-2015-full-data.dta")
turkey2019 <- read_dta("Turkey-2019-full-data.dta")

#turkey2002
turkey2005 <- turkey2005[c("idstd", "country", "s3", "s2a", "s5a", "s5b", "s5c",
                           "s1a", "q65a1", "q65b1", "q23a1", "q23b1", "q23a2", 
                           "q23b2", "q19", "q55a4", "q55a3", "q66a", "q66b")]
turkey2008 <- turkey2008[c("idstd", "a1", "a4b", "a14m" ,"a14y", "b1", "b2a", 
                           "b2b", "b2c", "b5", "f1", "f2", "c7", "c8", "c16", 
                           "c17", "c26", "c27", "ECAo1", "ECAo11", "ECAo12",
                           "l11a", "ECAo15", "d2", "e4", "e16", "e17", "k4", 
                           "n5a", "l1", "l2", "c9b")]
turkey2013 <- turkey2013[c("idstd", "a1", "a4b", "a14m" ,"a14y", "b1", "b2a", 
                           "b2b", "b2c", "b5", "f1", "f2", "c7", "c8", "c16", 
                           "c17", "ECAo14f", "l11a", "d2", "k4", "n5a",
                           "l1", "l2", "c9b")]
turkey2015 <- turkey2015[c("idstd", "a1", "a4b", "a14m", "a14y", "tub1", "b2a", 
                           "b2b", "b2c", "b5", "f1", "f2", "c7", "c8a", "c8b", 
                           "c16", "c17", "tuc12", "tuc13", "h1", "h4a", "h4a", 
                           "d2", "n2p", "k4", "n5a", "l1", "l2", "c9b")]
#turkey2019 <- turkey2019[c("idstd", "a1", "a4b", "a14m" ,"a14y", "b1", "b2a", 
#                         "b2b", "b2c", "b5", "f1", "f1", "c7", "c8", "c16", 
#                         "c17", "c26", "c27", "ECAo1", "ECAo11", "ECAo12", 
#                         "l11a", "ECAo15", "d2", "e4", "e16", "e17", "k4", 
#                         "n5a", "l1", "l2", "c9b")]

china2012 <- read_dta("China-2012-full-ES-N2700-data.dta")
#china2005

china2012 <- china2012[c("idstd", "a1", "a4b", "a14", "a14", "b1", "b2a", 
                       "b2b", "b2c", "b5", "f1", "f1", "c7", "c8", "c16", 
                       "c17", "CNo1", "l11a", "d2", "k4", "n5a",
                       "l1", "l2", "c9b")]
#china2005

#india2002
india2005 <- read_dta("India-2005--full-data-.dta")
india2014 <- read_dta("India-2014-full-data-.dta")

#india2002
india2005 <- india2005[c("idstd", "code2", "r1_1", "r1_2a1", "r1_2a2", "r1_2b", 
                         "r1_4", "r4_1a", "r6_1a1", "r6_1b1", "r6_1a2", "r6_1b2",
                         "r6_1c1", "r6_1c2", "r4_7a", "r4_7d", "r3_5a", "r3_5b")]
india2014 <- india2014[c("a4b", "a14m", "a14y", "b1", "b2a", "b2b", "b2c", "b5",
                         "f1", "f2", "c7", "c8", "c16", "c17", "h1", "h4a", "h4a",
                         "l11a", "d2", "k4", "n5a", "l1", "l2", "c9b")]

#nigeria2007 <- read_dta("")

brazil2009 <- read_dta("Brazil2009-full-data-.dta")

brazil2009 <- brazil2009[c("idstd", "a1", "a4b", "a14m", "b1", "b2a", "b2b", 
                           "b2c", "b5", "f1", "f2", "c7", "c8", "c16", "c17", 
                           "c26", "c27", "l11a", "d2", "e4", "e16", "e17", "k4",
                           "l1", "l2", "c9b")]
                           
#pakistan2007 <- 

indonesia2009 <- read_dta("Indonesia-2009-full-data-.dta")
#indonesia2015 <- read_dta("Indonesia-2015-full-data.dta") *ERROR*

inflation <- read_dta("monthly.dta")

rm(annual_inflation)
rm(df)
rm(ecpi)
rm(inflation_monthly)
rm(inflation_oc)
rm(inflation_SC)
rm(OC)
rm(poweroutage_SC)
rm(SC_turkey2008)
rm(SC_turkey2013)
rm(SC_turkey2015)
rm(SC_turkey2019)
rm(water_SC)
rm(year)
rm(operating_capacity)
rm(try)
rm(subset_turkey2019)
rm(total_1)
rm(total_2)
rm(total_3)
rm(total_4)
rm(trySC)
rm(turkeySC2008)
rm(turkeySC2013)
rm(turkeySC2015)
rm(turkeySC2019)
rm(turkey2002)

hello <- full_join(x=turkey2005, y=turkey2008, by=c("idstd"="idstd", 
                                                    "country"="a1", "s3"="a4b", 
                                                    "s2a"="b1", "s5a"="b2a", 
                                                    "s5b"="b2b", "s5c"="b2c", 
                                                    "s1a"="b5", "q65a1"="f1", 
                                                    "q65b1"="f2", "q23a1"="c7", 
                                                    "q23b1"="c8", "q23a2"="c16",
                                                    "q23b2"="c17", "q19"="e16", 
                                                    "q55a4"="e17", "q55a3"="k4",
                                                    "q66a"="l1", "q66b"="l2"))

hello2 <- full_join(x=turkey2013, y=hello, by=c("idstd"="idstd", "a1"="country", "a4b"="s3", "a14m"="a14m","a14y"="a14y","b1"="s2a","b2a"="s5a","b2b"="s5b","b2c"="s5c","b5"="s1a","f1"="q65a1","f2"="q65b1","c7"="q23a1","c8"="q23b1","c16"="q23a2","c17"="q23b2","ECAo14f"="ECAo11","l11a"="l11a","d2"="d2","k4"="q55a3","l1"="q66a","l2"="q66b"))





                         
                        







