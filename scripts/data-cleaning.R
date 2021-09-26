library(haven)
library(tidyverse)

# Read federal riding census data
riding <- read_csv('data/riding_data/98-401-X2016045_English_CSV_data.csv')

# Filter off provincial/national aggregates
riding <- riding %>%
  subset(GEO_LEVEL == 2)

# Get human-readable names for columns
prompt <- riding[1:2247,9]

# Get data by riding grouped by sex where data is available,
# otherwise aggregate by riding total
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

riding_total <- riding %>%
  select(
    GEO_NAME,
    colnames(riding)[10],
    colnames(riding)[12]
  )

rm(riding)

# Rename columns
names(riding_male)[1] <- 'Riding'
names(riding_male)[2] <- 'Code'
names(riding_male)[3] <- 'Val'

names(riding_female)[1] <- 'Riding'
names(riding_female)[2] <- 'Code'
names(riding_female)[3] <- 'Val'

names(riding_total)[1] <- 'Riding'
names(riding_total)[2] <- 'Code'
names(riding_total)[3] <- 'Val'

# Pivot column
riding_total <- riding_total %>%
  pivot_wider(names_from= Code, values_from = Val)

riding_male <- riding_male %>%
  pivot_wider(names_from = Code, values_from = Val)

riding_female <- riding_female %>%
  pivot_wider(names_from = Code, values_from = Val)

# Add columns including sex
riding_male$sex <- c('male')
riding_female$sex <- c('female')

# Combine ridings by sex into one df
riding_data <- rbind(riding_male, riding_female)
rm(riding_male, riding_female)

age_cols <- 9:33
income_cols <- 760:780
home_ownership_cols <- 1618:1619
language_cols <- 106:109
education_cols <- 1685:1698
ethnicity_cols <- 1325:1337
home_cols <- 1676
immigrants_cols <- 1141:1142
sex <- 2249
employment_cols <- 1867:1869

prompt_subset <- prompt[c(age_cols,
         income_cols,
         home_ownership_cols,
         language_cols,
         education_cols,
         home_cols,
         immigrants_cols,
         sex,
         employment_cols),]

riding_full_data <- riding_data %>%
  select(1,
         age_cols,
         income_cols,
         home_ownership_cols,
         language_cols,
         education_cols,
         home_cols,
         immigrants_cols,
         sex,
         employment_cols)

# Clean riding income data
riding_income_data <- riding_total %>%
  select(1, income_cols) %>%
  select(-c('759', '775'))

names(riding_income_data) <- c("Riding", deframe(prompt[760:774,]), 
                               deframe(prompt[776:779,]))
riding_income_data <- riding_income_data %>%
  mutate(
    "0-30k" = rowSums(.[2:7]),
    "30-60k" = rowSums(.[8:12]),
    "60-90k" = rowSums(.[13:15]),
    "90-150k" = rowSums(.[16:18]),
    "150-200k" = rowSums(.[19]),
    "200k+" = rowSums(.[20])
  ) %>% 
  select(-c(2:20))

income_prop <- riding_income_data %>%
  pivot_longer(cols=2:7, names_to="income_bracket", values_to="n") %>%
  group_by(Riding) %>%
  mutate(freq = n / sum(n)) %>%
  select(-c(3))

# Impute sex (assume 50:50 distribution)
income_prop$freq <- income_prop$freq / 2
income_prop_2 <- data.frame(income_prop)
income_prop$Sex <- c('male')
income_prop_2$Sex <- c('female')
income_prop <- rbind(income_prop, income_prop_2)

rm(income_prop_2)

# Clean education data

riding_education_data <- riding_data %>%
  select(1, education_cols, sex)

names(riding_education_data) <- c("Riding", deframe(prompt[education_cols,]), "Sex")
riding_education_data[, c(2:15)] <- sapply(riding_education_data[, c(2:15)], as.numeric)
riding_education_data <- riding_education_data %>%
  mutate(
    "No degree/diploma" = rowSums(.[2]),
    "High school" = rowSums(.[3]),
    "College/Trades" = rowSums(.[c(4:9)]),
    "Bachelors" = rowSums(.[10:11]),
    "Postgrad" = rowSums(.[12:15])
  ) %>%
  select(-c(2:15))

education_prop <- riding_education_data %>%
  pivot_longer(cols=3:7, names_to="education", values_to="n") %>%
  group_by(Riding) %>%
  mutate(freq = n / sum(n)) %>%
  select(-c(4))

# Clean employment data

riding_employment_data <- riding_data %>%
  select(1, employment_cols, sex)

names(riding_employment_data) <- c("Riding", deframe(prompt[employment_cols,]), "Sex")
riding_employment_data[, c(2:4)] <- sapply(riding_employment_data[, c(2:4)], as.numeric)

employment_prop <- riding_employment_data %>%
  pivot_longer(cols=2:4, names_to="employment_status", values_to="n") %>%
  group_by(Riding) %>%
  mutate(freq = n / sum(n)) %>%
  select(-c(4))

# Clean immigration data

riding_immigration_data <- riding_data %>%
  select(1, 1142, 1143, sex)

names(riding_immigration_data) <- c("Riding", "Non-immigrant", "Immigrant", "Sex")
riding_immigration_data[, c(2:3)] <- sapply(riding_immigration_data[, c(2:3)], as.numeric)

immigration_prop <- riding_immigration_data %>% 
  pivot_longer(cols=2:3, names_to="is_immigrant", values_to="n") %>%
  group_by(Riding) %>%
  mutate(freq = n / sum(n)) %>%
  select(-c(4))

# Clean age data
riding_age_data <- riding_data %>%
  select(1, 15:24, 26:30, sex)
names(riding_age_data) <- c("Riding", deframe(prompt[c(14:23, 25:29),]), "Sex")
riding_age_data[, c(2:16)] <- sapply(riding_age_data[, c(2:16)], as.numeric)

riding_age_data  <- riding_age_data %>%
  mutate(
  "15-24" = rowSums(.[2:3]),
  "25-34" = rowSums(.[4:5]),
  "35-44" = rowSums(.[6:7]),
  "45-54" = rowSums(.[8:9]),
  "55-64" = rowSums(.[10:11]),
  "65-74" = rowSums(.[12:13]),
  "75-84" = rowSums(.[14:15]),
  "85+" = rowSums(.[16])
  ) %>%
  select(-c(2:16))

age_prop <- riding_age_data %>%
  pivot_longer(cols=3:10, names_to="age_bracket", values_to="n") %>%
  group_by(Riding) %>%
  mutate(freq = n / sum(n)) %>%
  select(-c(4))

# Clean home ownership
riding_home_ownership <- riding_total %>%
  select(1, 1618, 1619)

names(riding_home_ownership) <- c("Riding", "Owner", "Renter")

home_ownership_prop <- riding_home_ownership %>%
  pivot_longer(cols=2:3, names_to="home_ownership", values_to="n") %>%
  group_by(Riding) %>%
  mutate(freq = n / sum(n)) %>%
  select(-c(3))

# Impute sex (assume 50:50 distribution)
home_ownership_prop$freq <- home_ownership_prop$freq / 2
home_ownership_prop_2 <- data.frame(home_ownership_prop)
home_ownership_prop$Sex <- c('male')
home_ownership_prop_2$Sex <- c('female')
home_ownership_prop <- rbind(home_ownership_prop, home_ownership_prop_2)

rm(home_ownership_prop_2)


# Clean language
riding_language_data <- riding_data %>%
  select(1, 107:110, sex)

names(riding_language_data) <- c("Riding", "English", "French", "Both", 
                                 "Neither", "Sex")
riding_language_data[, c(2:5)] <- sapply(riding_language_data[, c(2:5)], as.numeric)

language_prop <- riding_language_data %>%
  pivot_longer(cols=2:5, names_to="language", values_to="n") %>%
  group_by(Riding) %>%
  mutate(freq = n / sum(n)) %>%
  select(-c(4))

# Clean ethnicity
riding_ethnicity_data <- riding_data %>%
  select(1, ethnicity_cols)

# Combine individual distributions

x <- merge(income_prop, education_prop, by=c("Riding", "Sex")) %>%
  mutate(prop = freq.x * freq.y) %>%
  select(-c(4, 6))

y <- merge(x, employment_prop, by=c("Riding", "Sex")) %>%
  mutate(prop = prop * freq * 2) %>%
  select(-c(7))

z <- merge(y, immigration_prop, by=c("Riding", "Sex")) %>%
  mutate(prop = prop * freq * 2) %>%
  select(-c(8))

x <- merge(z, age_prop, by=c("Riding", "Sex")) %>%
  mutate(prop = prop * freq * 2) %>%
  select(-c(9))

y <- merge(x, home_ownership_prop, by=c("Riding", "Sex")) %>%
  mutate(prop = prop * freq * 2) %>%
  select(-c(10))

z <- merge(y, language_prop, by=c("Riding", "Sex")) %>%
  mutate(prop = prop * freq * 2) %>%
  select(-c(11))

write.csv(z, 'poststrat.csv')