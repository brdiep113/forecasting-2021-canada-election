library(haven)
library(tidyverse)

# Read federal riding census data
riding <- read_csv('data/riding_data/98-401-X2016045_English_CSV_data.csv')

# Filter off provincial/national aggregates
riding <- riding %>%
  subset(GEO_LEVEL == 2)

# Get human-readable names for columns
prompt <- riding[1:2247,9]

riding_male <- riding %>%
  select(
    GEO_NAME,
    colnames(riding)[10],
    colnames(riding)[13]
    )

riding_female <- riding %>%
  select(
    GEO_NAME,
    colnames(riding)[10],
    colnames(riding)[14]
  )

names(riding_male)[1] <- 'Riding'
names(riding_male)[2] <- 'Code'
names(riding_male)[3] <- 'Val'

names(riding_female)[1] <- 'Riding'
names(riding_female)[2] <- 'Code'
names(riding_female)[3] <- 'Val'

riding_male <- riding_male %>%
  pivot_wider(names_from = Code, values_from = Val)

riding_female <- riding_female %>%
  pivot_wider(names_from = Code, values_from = Val)

riding_male$sex <- c('male')
riding_female$sex <- c('female')

age_cols <- 9:33
income_cols <- 760:779
home_ownership_cols <- 1618:1620
language_cols <- 106:109
education_cols <- 1684:1697
ethnicity_cols <- 1325:1337
home_cols <- 1676
immigrants_cols <- 1141:1142
sex <- 2249
employment_cols <- 1867:1870

riding_male <- riding_male %>%
  select(age_cols,
         income_cols,
         home_ownership_cols,
         language_cols,
         education_cols,
         home_cols,
         immigrants_cols,
         sex,
         employment_cols)

riding_female <- riding_female %>%
  select(age_cols,
         income_cols,
         home_ownership_cols,
         language_cols,
         education_cols,
         home_cols,
         immigrants_cols,
         sex,
         employment_cols)

riding_data <- rbind(riding_male, riding_female)
'%ni%' <- Negate('%in%')
