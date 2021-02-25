# import libraries
lapply(
  c("readr", "dplyr","stringr","stargazer"),
  require,
  character.only = TRUE)

setwd("~/Downloads/PROJECT")

# import mortality dataset
mort_data <- read_csv("mortality_data.csv", col_names = TRUE)

# keep only the years that are in between 1992 and 2016 exclusive
mort_data <- mort_data[mort_data$year > 1992 & mort_data$year < 2016, ]

# change the names of the columns
colnames(mort_data)[4:11] <- c("mort_rate", "prob_death", "ave_length_surv",
                               'num_of_surv', "num_of_deaths", "num_years_lived",
                               "num_years_left", "life_expec")

# find the location of dash, store the index before the '-' in a separate column name tmp
mort_data$tmp <- str_locate(mort_data$age, "-")[ ,2] - 1

# if a null value in tmp, then populate that cell with the length of the age string
mort_data$tmp[is.na(mort_data$tmp)] <- str_length(mort_data$age[is.na(mort_data$tmp)])

# create new column, age2, that stores the string value of age (age[1] to age[tmp])
mort_data$age2 <- str_sub(mort_data$age, 1, mort_data$tmp)

# change age2 from character to numeric
mort_data$age2 <- as.numeric(substr(mort_data$age2, 1, 3))

# group the ages based on age groups
mort_data$age_group <- cut(mort_data$age2,
                           c(0, 18, 64, 110),
                           labels = c("<18", "18-64", ">64"),
                           include.lowest = TRUE)

# because we already have the age group, and we are grouping the data based on age group
# we can delete the columns age, tmp, and age2, all we need is age_group from here on out
mort_data <- cbind(mort_data$state,
                   mort_data$year,
                   mort_data$age_group,
                   mort_data[ , 4:11])

# change the column names in the first 3 columns
names(mort_data)[1:3] = c("state", "year", "age_group")

# create aggregation
# we are aggregating all of the data based on the columns on the right hand side of the formula
mort_data <- aggregate(.~ state + year + age_group,
                       data = mort_data,
                       FUN = function(x) sum(x, na.rm = TRUE))

mort_data = mort_data[order(mort_data$state, mort_data$year, mort_data$age_group), ]

### This is the end of motality_data

# ===================================================================================

### We will now move on to the per capita income data

# import the income data .csv
inc_data <- read.csv("income_data.csv", header = TRUE)

# reshape inc_data to long form instead of wide form
inc_data <- reshape(inc_data,
                    varying = names(inc_data)[2:ncol(inc_data)],
                    times = c(1993, 2015),
                    timevar = "year",
                    direction = "long",
                    sep = "."
                    )

# drop id var and sort by state and year
inc_data <- inc_data[ ,-ncol(inc_data)]
inc_data <- inc_data[order(inc_data$state, inc_data$year), ]

stargazer(inc_data, type = "text")

### This is the end of the income data

# ===================================================================================

### Time to handle the education dataset

# set the working directory to Downloads/PROJECT/eduction
setwd("~/Downloads/PROJECT/education")

# create list that contains all the files that we will traverse in a later step
file_list <- list.files(path = "~/Downloads/PROJECT/education")

# create empty dataframe
educ_data <- data.frame();

# iterate thru file_list and populate educ_data with each file
for (i in 1:length(file_list)) {
  temp_data <- data.frame(read_csv(file_list[i], col_names = TRUE))
  colnames(temp_data) <- c("state", "year", "percent_highschool", "percent college")
  educ_data <- rbind(educ_data, temp_data)
}

# remove unnecessary variables (data and values)
rm(file_list, temp_data, i)

# change working directory back to Downloads/PROJECT
setwd("~/Downloads/PROJECT")

# import education_0715 dataset
temp_data <- data.frame(read_csv("education_0715.csv", col_names = TRUE))

# name of the columns were different for educ_data and temp_data
colnames(educ_data) <- c("state", "year", "phs", "pcoll")
colnames(temp_data) <- c("state", "year", "pcoll", "phs")

# merge educ_data and temp_data
educ_data <- rbind(educ_data, temp_data)

# delete temp_data
rm(temp_data)

# sort educ_data
educ_data <- educ_data[order(educ_data$state, educ_data$year), ]

# drop the footnote entries
educ_data <- educ_data[-which(educ_data$state == "Footnotes:"), ]

# this is the end of education data

# ===================================================================================

### Handle expenditure data

# change working directory from PROJECT to ~/expenditure
setwd("~/Downloads/PROJECT/expenditure")

# create list that contains all the datasets from expenditure directory
file_list <- list.files(path = "~/Downloads/PROJECT/expenditure")

# create empty dataframe
expnd_data <- data.frame()

# populate expnd_data with the datasets within file_list
for (i in 1:length(file_list)) {
  temp_data <- data.frame(read_csv(file_list[i], col_names = TRUE))
  colnames(temp_data) <- c("state", "year", "tot_revenue", "taxes", "tot_expnd", 
                           "education", "public_welfare", "hospital", "health")
  expnd_data <- rbind(expnd_data, temp_data)
}

# sort expnd_data by the state and year
expnd_data <- expnd_data[order(expnd_data$state, expnd_data$year), ]

# California 2000 is duplicated
expnd_data <- expnd_data[-which(duplicated(expnd_data[,c(1,2)]) == TRUE), ]

# remove temp variables
rm(file_list, temp_data, i)

# We are done with expenditure data

# ===================================================================================

# time to merge the datasets

# one to one merge
data <- merge(inc_data, educ_data, by = c("state", "year"))
data <- merge(data, expnd_data, by = c("state", "year"))

# many to one merge
data <- merge(mort_data, data, by = c("state", "year"))

# remove the rest
rm(mort_data, educ_data, expnd_data, inc_data)

# change the measurements in 10 thousand dollars
data$pinc <- data$pinc/1e4
data$tot_revenue <- data$tot_revenue/1e4
data$taxes <- data$taxes/1e4
data$tot_expnd <- data$tot_expnd/1e4
data$education <- data$education/1e4
data$public_welfare <- data$public_welfare/1e4
data$hospital <- data$hospital/1e4
data$health <- data$health/1e4

# change to ratio
data$phs <- data$phs/100
data$pcoll <- data$pcoll/100

# create a table of descriptive statistics for data
stargazer::stargazer(data,
                     type = "html",
                     out = "~/Downloads/PROJECT/descriptives.doc")

# regression time
reg1 <- lm(mort_rate ~ health + hospital + log(pinc) + phs + pcoll,
           data = data,
           subset = (age_group == ">64"))

reg2 <- lm(mort_rate ~ health + hospital + log(pinc) + phs + pcoll + factor(state),
           data = data,
           subset = (age_group == ">64"))

reg3 <- lm(mort_rate ~ health + hospital + log(pinc) + phs + pcoll + factor(state) + factor(year),
           data = data,
           subset = (age_group == ">64"))

stargazer(reg1, reg2, reg3,
          title = "Estimation Results",
          type = "html",
          #keep.stat = c("n", "rsq", "adj.rsq"),
          out = "~/Downloads/PROJECT/results.doc"
          #se = list(
          #  coef(summary(reg1, cluster = c('state')))[, 2],
          #  coef(summary(reg2, cluster = c('state')))[, 2],
          #  coef(summary(reg3, cluster = c('state')))[, 2]
          #),
          #keep = c('health', 'hospital', 'pinc', 'phs', 'pcoll')
)

