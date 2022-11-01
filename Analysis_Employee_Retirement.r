# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
# PROGRAMMING FOR DATA ANALYSIS - INDIVIDUAL ASSIGNMENT
# Module Code : (062022-ASU)
# Module Name : PROGRAMMING FOR DATA ANALYSIS
# Title       : Employee Attrition Analysis
# Intake      : APD2F2206SE
# Student Name: ABIR HASAN
# Student ID  : TP047388
# Lecturer    : AMARDEEP SINGH A/L UTTAM SINGH
# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-

#-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-Load Libraries (Code)-_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
library(tidyverse)
library(lubridate)
library(scales)

#-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-Import Data  (Code)-_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
#Set the working Directory From Device
setwd("C:/Users/Abir (01829740716)/Music/2nd year 1st assignment/PFDA(R)")
#Clean Dataset
data<- read.csv("employee_attrition.csv")

# _-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Data Pre-processing _-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
str(data)
summary(data)

#  _-_-_-_-_-_--_-_- Convert char type data into factor, for date data convert from char to date. _-_-_-_-_-_-
emp <- data %>%
  mutate(
    orighiredate_key = as.Date(orighiredate_key, format = "%m/%d/%Y"),
    # 1/1/1900 means still working so set those as NA
    terminationdate_key = ifelse(terminationdate_key == "1/1/1900", NA, terminationdate_key),
    terminationdate_key = as.Date(terminationdate_key, format = "%m/%d/%Y"),
    city_name = as.factor(city_name),
    department_name = as.factor(department_name),
    job_title = as.factor(job_title),
    store_name = as.factor(store_name),
    gender_short = as.factor(gender_short),
    # typo. "Resignaton" -> "Resignation"
    termreason_desc = ifelse(termreason_desc == "Resignaton", "Resignation", termreason_desc),
    termreason_desc = as.factor(termreason_desc),
    termtype_desc = as.factor(termtype_desc),
    STATUS_YEAR = as.factor(STATUS_YEAR),
    STATUS_YEAR = ordered(STATUS_YEAR, c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)),
    STATUS = as.factor(STATUS),
    BUSINESS_UNIT = as.factor(BUSINESS_UNIT)
  ) %>% 
  # Remove useless attributes
  select(everything(), -c(gender_full, recorddate_key, birthdate_key))

# check the data and its structure
#-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-Structure of data-_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_
str(data)

#-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-Summary of data-_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-_-_-
summary(data)


#-_-_-_-_--_-_-_-_-_-Treasure Hunt 1- Get idea from the trend-_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-_-_

emp %>% 
  filter(STATUS == "TERMINATED") %>% 
  group_by(STATUS_YEAR) %>% 
  ggplot(aes(STATUS_YEAR, fill = termreason_desc)) +
  geom_bar(position = "fill") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~gender_short) +
  coord_flip() +
  labs(title = "Over the past ten years, the overall trend in employee termination", fill = "Reason") +
  xlab("Year") +
  ylab("Percentage (%)") +
  scale_fill_manual(values = c("Layoff" = "#214946",
                               "Resignation" = "#946f6a",
                               "Retirement" = "#b97f18"))

# Some findings got from Treasure Hunt 1:
# Layoff seen only in 2014 and 2015
# No male worker leave work anymore, starting from 2011.
# many workers retire in 2014 and most common cause was layoff regardless the gender
# Mostly female worker go through retirement


# Questions that I am tring to figure out
# 1. What age did employee retire at?
# 2. What are the casues of layoffs?

#-_-_-_-_--_-_-_-_-_-Treasure Hunt 2- Get idea from the trend-_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-_-_
emp %>% 
  filter(
    STATUS == "TERMINATED",
  ) %>% 
  group_by(gender_short) %>% 
  ggplot(aes(gender_short, fill = gender_short)) +
  geom_bar() +
  facet_grid(termreason_desc~STATUS_YEAR) +
  labs(title = "An analysis of the number of terminated employees by gender and reason for termination each year", fill = "Gender") +
  xlab("Gender") +
  ylab("Count") +
  scale_fill_manual(values = c("F" = "#902625",
                               "M" = "#946f6a"))


# Demonstrate that layoff occur in 2014 and 2015
# uneven age distribution due to women workers mainly go through retirement.
# It indicate that organization got more old woman workers compared to man employee in the past 10 years.
# the number of resignation got higher in 2011 to 2012 and decrease again


#_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-_-_ Question 1 _-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-_-_
#What age did people retire at?

# In Canada, The standard age for getting Social Security is 65, but it can be obtained as early as 60 or as late as 70.
# Very soon older employee going to retire. It can be a good news for young employee
# This can be a big loss for company because they are loosing experience employees.
# Organization must be prepare for retirement wave so that the transation could be smooth.

#_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-_-_ Analysis: 1 _-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-_-_
# Find the relationship between termination reason and age
age_terminated <- emp %>% 
  drop_na(termreason_desc) %>% 
  filter(STATUS == "TERMINATED") %>% 
  group_by(age)

## boxplot with jitter
ggplot(data = age_terminated, mapping = aes(x = termreason_desc, y = age)) +
  geom_boxplot(mapping = aes(fill = termreason_desc)) +
  labs(title = "The reason of termination based on age (With Jitter)", fill = "Termination Reason") +
  geom_jitter(color="black", size=0.6, alpha=0.8) +
  xlab("Termination Reason") +
  ylab("Age") +
  scale_fill_manual(values = c("Layoff" = "#950b99",
                               "Resignation" = "#472348",
                               "Retirement" = "#aa5b3d"))

# There is  bimodal distribution on "Retirement". Seems like employees will retire at the age of 60 or 65.
_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-_-_ Analysis: 2 _-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-_-_
#Find the total number of active employees that will be going to leave the workplace due to retirement. 
retire_soon <- emp %>% 
  filter(
    STATUS_YEAR == 2015,
    STATUS == "ACTIVE"
  ) %>% 
  group_by(job_title) %>% 
  mutate(
    might_retired_soon = ifelse((age >= 55 & age <= 65), TRUE, FALSE)
  )

ggplot(retire_soon, mapping = aes(x = fct_rev(fct_infreq(job_title)), fill = might_retired_soon)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage of employees that soon leave the workplace due to retirement based on business 
       unit (55 to 65 years old in 2015)", fill = "soon leave the work place due to retirement") +
  xlab("Job Title") +
  ylab("Percentage(%)") +
  coord_flip() +
  facet_wrap(~BUSINESS_UNIT, scales = "free")

ggplot(retire_soon, mapping = aes(x = fct_rev(fct_infreq(job_title)), fill = might_retired_soon)) +
  geom_bar() +
  scale_y_continuous(breaks = pretty_breaks()) +
  labs(title = "Number of employees that soon soon leave the workplace due to retirement based on business 
       unit (55 to 65 years old in 2015)", fill = "Soon to be retired") +
  xlab("Job Title") +
  ylab("Percentage(%)") +
  coord_flip() +
  facet_wrap(~BUSINESS_UNIT, scales = "free")

_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-_-_Analysis: 3_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-_-_
## Find which store and city will loose most meat cutter's due to the retirement.
stores_retire_soon %>% 
  filter(
    job_title == "Meat Cutter"
  ) %>%
  ggplot(aes(fct_rev(fct_infreq(city_name)))) +
  geom_bar(aes(fill = might_retired)) +
  labs(
    title = "Total number of meat cutters retire in each city (2015 data)",
    fill = "Might Retire (in 5 to 10 years)") +
  xlab("City name") +
  ylab("Count") +
  coord_flip()
# Finding are:
## total number of meat cutter will reduce significantly and reach an extent
## some city will not have any meat cutter
## the graph also demonstrate between new meat cutter and experienced meat cutter
## in order to avoid knowledge gap company should let the old meat cutters to trains new meat cutter.
## they shoud welcome more new meat cutter.

# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Question 2 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
# What are the Basic reasons for resignations?
# Resignation: It is like giving up the job or leave the workplace formally or oficially
# Some reasons that are given below that makes resignation

### A. Having a bad relationship with the boss make people resignation.
### B. Workplace is  boring.
### C. Relationship with other staff are very poor.
### D. Very less opportunities to utilize their skills.
### E. Contributions they make to the organization's objectives
### F. Most of the time they work independently in the office.
### G. Job is meaningless
### H. Company don't appreciate the employees effort
### I. The company's financial condition
### J. The culture of the organization dosen't match with the employee
#   When a company knows why employees resign, it can determine how to retain them

# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 1 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-

age_resign <- emp %>% 
  filter(
    termreason_desc == "Resignation"
  ) %>% 
  group_by(as.factor(age))

ggplot(age_resign, aes(age)) +
  geom_histogram(aes(y = ..density.., fill = gender_short), bins = 30) +
  geom_density(fill = "#cc0052", color = "#330033", alpha = 0.6) +
  facet_wrap(~gender_short) +
  labs(title = "Density plot and Histogram of the relationship between total number of resignation and age", fill = "Gender") +
  xlab("Age") +
  ylab("Density") +
  scale_fill_manual(values = c("F" = "#ff1aff",
                               "M" = "#4d4d00"))

# Findings for the analysis: 
### Problem found from analysis:
 
## A. Their career path is not certain.
## B. They take the job that are not ment for them.
## C. New jobs knock the door with massive opportunity.
## D. They feel like their are growth has been stops.
# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 3 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
emp %>% 
  filter(
    termreason_desc == "Resignation"
  ) %>% 
  group_by(BUSINESS_UNIT) %>% 
  summarise(
    total_number_of_resignation = n()
  ) %>% 
  View()
# headoffice got only one ,so we can ignore it

store_resignation <- emp %>% 
  filter(
    termreason_desc == "Resignation",
    BUSINESS_UNIT == "STORES"
  ) %>% 
  group_by(department_name)

ggplot(store_resignation, aes(department_name)) +
  geom_bar(fill = "#0000cc") +
  labs(title = "Resignations in STORES over the years in each department") +
  xlab("Department Name") +
  ylab("Count")

### highest resignation has been situated in the customer service department
### what kinds of job customer service department contain? 

emp %>% 
  filter(
    department_name == "Customer Service",
    termreason_desc != "Not Applicable"
  ) %>% 
  group_by(STATUS_YEAR) %>% 
  ggplot(aes(job_title, fill = termreason_desc)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "The Termination Reasons and jobs in Customer Service Department", fill = "Termination Reasons") +
  xlab("Job Title") +
  ylab("Percentage (%)")

#### Some findings from the analysis 3
#### Most resignation rate has been seen in the customer service department.
#### Only has the highest layoff among other
#### their jobs under risk
#### afraid of beings replaces, so they choose to leave the company
#### leave the workplace to find another better opportunity

# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 4 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-

emp %>% 
  filter(
    job_title == "Cashier"
  ) %>% 
  group_by(STATUS_YEAR) %>% 
  ggplot(aes(STATUS_YEAR, fill = gender_short)) +
  geom_bar(position = "dodge") +
  labs(title = "Total numbers of cashiers by years", fill = "Gender") +
  xlab("Year") +
  ylab("Count")


### Finding for this analysis:
### total number of cashiers gradually increasing every year
### 2014 dramatically reduces the number of cashiers
### high competition force cashier to leave the workplace
# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Question 3 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-

# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 1 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
# Find which job has the highest layoff
emp %>% 
  group_by(STATUS_YEAR) %>% 
  filter(
    termreason_desc == "Layoff",
  ) %>% 
  ggplot(aes(fct_rev(fct_infreq(job_title)), fill = gender_short))+
  geom_bar(position = "dodge") +
  labs(title = "Totall number of layoffs derived from job title", fill = "Gender") +
  xlab("Job Title") +
  ylab("Count") +
  scale_fill_manual(values = c("F" = "#4d0000",
                               "M" = "#330066"))
  coord_flip()

## Findings are looking for this analysis:
## cashier got the first position once again, but this time is layoff
## dairy person got the second highest position
## meat cutter got the third highest position

## robotics and automation are gradually taken over human’s job.
## According to the Craft cashiers are at 97% risk of being unemployed due to automation
## According to the economists’ cashiers will vanished in the next 12 years because of automation

## dairy person and meat cutter both positions are in danger,
## because technology is also taking their places as well.

 ## solid reasons: 
## employee’s redundancies
## Automation and technology

# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 2 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
# Find the relationship between age and layoffs

# Find the relationship between age and layoffs
age_layoff <- emp %>% 
  filter(
    termreason_desc == "Layoff"
  ) %>% 
  group_by(as.factor(age))

ggplot(age_layoff, aes(age)) +
  geom_histogram(aes(y = ..density.., fill = gender_short), bins = 30) +
  geom_density(fill = "#1a0000", color = "#00804d", alpha = 0.6) +
  facet_wrap(~gender_short) +
  labs(
    title = "Density plot and Histogram of the relationship between number of layoffs and age",
    fill = "Gender") +
  xlab("Age") +
  ylab("Density") +
  scale_fill_manual(values = c("F" = "#004d99",
                               "M" = "#990000"))

## Findings are looking for this analysis:
## young employees around 30± years old got high layoff rate regardless of gender
## both man and woman workers that are around 65 years old got the highest number of layoffs in both sides.
## This could be a strategy that the organization may apply to hide the older workers larger-scale layoffs. 
## https://www.hg.org/legal-articles/how-companies-may-use-layoffs-to-hide-age-discrimination-50105

# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 3 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
# Has the company experienced any pregnancy discrimination layoffs?

# Utilized the graph from analysis 2.0

## Findings are looking for this analysis
## Females in Canada are on average 30.8 years old when they get married for the first time
## Employees around that age have a high layoff rate, according to the graph
## organization may get pregnancy discrimination
## Normally, companies lay off pregnant or postpartum women for a period of time to allow them to recover
## It is possible for them to return to work once they have recovered fully
## But sometimes some organization will lay off those workers permanent as they think
## Women who are pregnant or postpartum will be less productive and their focus will not
## be on the work as it was earlier, so employers want to layoff them. This is obviously pregnancy discrimination

## This dataset does not contain any information indicating that those layoffs took place
## are temporary or permanent, 
## As a result, we can only suspect that the company discriminates against pregnant women


# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 4 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
# Are there any gender discrimination layoffs that happened in the company?
emp %>% 
  filter(
    termreason_desc == "Layoff"
  ) %>% 
  group_by(gender_short) %>% 
  ggplot(aes(gender_short, fill = gender_short)) +
  geom_bar() +
  facet_wrap(~STATUS_YEAR) +
  labs(title = "Totall Number of Layoff in 2014 and 2015 based on gender", fill = "Gender") +
  xlab("Year") +
  ylab("Count") +
  scale_fill_manual(values = c("F" = "#00e68a",
                               "M" = "#006622"))

## Findings are looking for this analysis:
## women look like have higher layoff rate compared to man
## but they only have little difference so this normal
## In this case, no gender discrimination in layoff

# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Conclusion (Q3) -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
# Automation look like be the major issue that caused jobs like Cashier, Dairy Person and Meat cutter being laid off
# There are some obvious results when attempting to discover discrimination in layoffs within a company
## 1. Age discrimination - Might have. In order to cover up the huge number of older employees 
##                                               being laid off, some younger employees are being laid off as well
## 2. Pregnancy discrimination – Due to the insufficient info about the types of layoffs so couldn't prove anything at all
## 3. Gender discrimination - Didn't exist in this company

## Before laying off Cashier, Dairy Person, or Meat cutter, HR should discuss with them
## Covering up age discrimination in layoffs is not a good idea

# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Question 4 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
# What is the growth of the company based on Business Unit
# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 1 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
Find the number of employees in 2006 and 2015
## below code for 2015

emp %>% 
  filter(
    STATUS_YEAR == "2015",
  ) %>% 
  ggplot(aes(fct_rev(fct_infreq(job_title)), fill = gender_short))+
  geom_bar(position = "dodge") +
  labs(title = "The number of employees based on job title in 2015", fill = "Gender") +
  xlab("Job Title") +
  ylab("Count") +
  scale_fill_manual(values = c("F" = "#F289AF",
                               "M" = "#68C1EB")) +
  coord_flip()

## below code for 2006

emp %>% 
  filter(
    STATUS_YEAR == "2006",
  ) %>% 
  ggplot(aes(fct_rev(fct_infreq(job_title)), fill = gender_short))+
  geom_bar(position = "dodge") +
  labs(title = "The number of employees based on job title in 2006", fill = "Gender") +
  xlab("Job Title") +
  ylab("Count") +
  scale_fill_manual(values = c("F" = "#F289AF",
                               "M" = "#68C1EB")) +
  coord_flip()

## Finding for this analysis
## In 2006 Meat Cutter become the most popular job in the company
## In 2015 largest number of employees worked as a Cashier. 
## Company selling huge product and making revenue out of the business
## Due to the automation employees loosing their jobs
# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 2 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-

## Find the number of store Manager in each year
emp %>% 
  filter(
    job_title == "Store Manager"
  ) %>% 
  group_by(STATUS_YEAR) %>% 
  ggplot(aes(STATUS_YEAR, fill = gender_short)) +
  geom_bar(position = "dodge") +
  labs(title = "Total numbers of Store Manager from 2006 to 2015", fill = "Gender") +
  xlab("Year") +
  ylab("Count")

## Findings:
## In 2006 there was around 33 store managers between
## Them 17 was male and 15 was female manager
## On the other hand in 2015 there was total 11 manager
## It but looks like company going through hard time.  

# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 3 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
## Find the number of young employees exist in the company
manager = c("Store Manager", "Produce Manager", "Processed Foods Manager", "Meats Manager", "Customer Service Manager", "Bakery Manager")

Young_Employee <- emp %>% 
  filter(
    STATUS == "ACTIVE",
    BUSINESS_UNIT == "STORES",
    STATUS_YEAR == 2015
  ) %>% 
  mutate(
    level = ifelse(job_title %in% manager, "Manager", "Employee"),
    might_retired = ifelse((age >=18 & age <= 35), TRUE, FALSE)
  ) %>% 
  group_by(job_title)

ggplot(Young_Employee, aes(job_title, fill = might_retired)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~level, scales = "free") +
  labs(
    title = "Percentage of employee that will going to retire in 35 to  42 years (Based on level)",
    fill = "Might Retire (in 35 to  42 years)") +
  xlab("Job Title") +
  ylab("Percentage (%)") +
  coord_flip()

Young_Employee %>% 
  group_by(level) %>% 
  summarise(
    count = n(),
    retire_rate = sum(might_retired == TRUE),
    percentage = paste(round((retire_rate/count) * 100, 0),"%")
  ) %>% 
  View()
# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Question 5 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
#What is the career future in company?
# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 1 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-

#--> Find which city has more job opportunity
emp %>% 
  filter(
    
    termreason_desc == "Retirement",
    STATUS_YEAR == "2015"
  ) %>% 
  group_by(city_name) %>% 
  summarise(
    total_Retirement = n()
  ) %>% 
  View()
## Vancouver cities has most jobs opportunity
## Next city is Terrace
## Rest of the cities got 1 or 2 vacation only.
# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 2 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
#--> Find which business unit has more job opportunity

emp %>% 
  filter(
    termreason_desc == "Retirement",
    STATUS_YEAR == "2015"
  ) %>% 
  group_by(BUSINESS_UNIT) %>% 
  summarise(
    total_Retirement = n()
  ) %>% 
  View()

## 51 vacancy exist in store 
## 12 vacancy available in head office
## Store job is easier to get hired compared to head office 

# -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_- Analysis 3 -_-_-_-_-_-_--_-_-_-_-_-_--_-_-_-_-_-_-
#--> Find which gender has more opportunity
emp %>% 
  filter(
    termreason_desc == "Retirement",
    STATUS_YEAR == "2015",
    gender_short == "M"
  ) %>% 
  group_by(BUSINESS_UNIT) %>% 
  summarise(
    total_Retiremen_based_on_Male = n()
  ) %>% 
  View()

## Male employee doesn’t retire in 2015
## 63 female employees retire from the company according to the analysis 2 question 5
## 12 employees retire from the head office 
## Perfect employee ratio in the workplace is 28:84
