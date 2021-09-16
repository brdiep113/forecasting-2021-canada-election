library(haven)
library(tidyverse)
library(plyr)
library(cesR)

# Load CES dataset
get_ces("ces2019_web")

'%ni%' <- Negate('%in%')

# province - ces_2019$province
# constituency name - ces_2019$constituencyname
# age
cleaned_data <- data.frame()

# Define CES encoding vectors
ces_province_id <- 14:26
ces_education_id <- 1:12
province_code <- c("AB", "BC", "MB", "NB", "NL", "NT", "NS", "NU", "ON", "PE",
               "QC", "SK", "YT")
education_code <- c("No schooling", "asdf")


ces_2019$cps19_province <- as.numeric(ces_2019$cps19_province)
province <- mapvalues(ces_2019$cps19_province, 
                               from=ces_province_id, 
                               to=province_code)
cleaned_data <- cbind(cleaned_data, t(province))

province_premier_party <- c("")
