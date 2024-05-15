library(tidyr)
library(tidyverse)
library(modelsummary)
library(knitr)
library(dplyr)
library(skimr)
library(visreg)
library(devtools)
library(see)
library(ggplot2)
library(psych)
library(pscl)
library(lmtest)
library(performance)

data<-Data
mod_level <- function(level) {
  if (level == "Bachelor's Degree") {
    return(0)
  } else if (level == "Master's Degree") {
    return(1)
  } else {
    return(2) 
  }
}

data <- data %>%
  mutate(level = replace(level, TRUE, sapply(level, mod_level)))

mod_gen <- function(gender) {
  if (gender == "Male") {
    return(0)
  } else if (gender == "Female") {
    return(1)
  } else {
    return(NA)  
  }
}
data <- data %>%
  mutate(gender = replace(gender, TRUE, sapply(gender, mod_gen)))


map_field_to_number <- function(field) {
  if (field %in% c("Medicine/Nursing/Health Sciences", "Natural Sciences (Biology, Physics, Chemistry, etc)", "Engineering")) {
    return(3)
  } else if (field %in% c("Business/Economics/Finance", "Law")) {
    return(2)
  } else if (field %in% c("Social Sciences (Psychology, Sociology, Political Science, etc.)", "Humanities")) {
    return(1)
  } else {
    return(NA)  
  }
}
data <- data %>%
  mutate(field = sapply(field, map_field_to_number))

view(data)


#### DESCRIPTIVES

# mean ideal job threat for each field category
mean_idealjobthreat <- aggregate(idealjobthreat ~ field, data, mean)

data <- merge(data, mean_idealjobthreat, by = "field", suffixes = c("", "_mean"))

data <- data %>%
  group_by(field) %>%
  mutate(normalized_idealjobthreat = idealjobthreat / idealjobthreat_mean) %>%
  ungroup()

view(data)

### IDEAL JOB THREAT

responsethreat <- table(data$idealjobthreat)

total_threat <- sum(responsethreat)

percentage <- (responsethreat / total_threat) * 100

print(percentage)

##         1         2         3         4         5 
## 34.117647 30.588235 24.705882  8.235294  2.352941

### Labour market concern

marketconcern <- table(data$labourmarketconcern)

market_concern <- sum(marketconcern)

marketpercentage <- (marketconcern / market_concern) * 100

print(marketpercentage)

##         1         2         3         4         5 
## 4.705882 17.647059 32.941176 36.470588  8.235294 

### DISPLACEMENT

displacement <- table(data$displacementconcern)

displacement_concern <- sum(displacement)

displacement_percentage <- (displacement / displacement_concern) * 100

print(displacement_percentage)

## 1         2         3         4         5 
## 38.823529 35.294118  9.411765 14.117647  2.352941 

## replace human

replace <- table(data$replacehuman)

replace_humans <- sum(replace)

replace_percentage <- (replace / replace_humans) * 100

print(replace_percentage)

##        1         2         3         4         5 
## 11.764706 40.000000 30.588235 14.117647  3.529412 


### confidence to compete

compete <- table(data$competeconfidence)

compete_confidence <- sum(compete)

compete_percentage <- (compete / compete_confidence) * 100

print(compete_percentage)


## 1         2         3         4         5 
## 5.882353 12.941176 24.705882 36.470588 20.000000 

### JOB opportunities

opportunities <- table(data$jobopportunities)

opportunities_field <- sum(opportunities)

opportunities_percentage <- (opportunities / opportunities_field) * 100

print(opportunities_percentage)

## 1         2         3         4         5 
## 9.411765 22.352941 25.882353 23.529412 18.823529 

##job opportunities

opportunities <- table(data$jobopportunities)

opportunities_field <- sum(opportunities)

opportunities_percentage <- (opportunities / opportunities_field) * 100

print(opportunities_percentage)


##benefits field of work

benefitfield <- table(data$optimismfieldwork)

fieldwork <- sum(benefitfield)

field_percentage <- (benefitfield / fieldwork) * 100

print(field_percentage)

#1         2         3         4         5 
#9.411765 14.117647 38.823529 17.647059 20.000000 

#society as a whole

benefitsociety <- table(data$benefitssociety)

benefit_society <- sum(benefitsociety)

society_percentage <- (benefitsociety / benefit_society) * 100

print(society_percentage)

## familiarity 

familiarity <- table(data$familiarity)

familiarity_ai <- sum(familiarity)

familiarity_percentage <- (familiarity / familiarity_ai) * 100

print(familiarity_percentage)

## TRAINING

training <- table(data$training)

training_ai <- sum(training)

training_percentage <- (training / training_ai) * 100

print(training_percentage)

print(count_per_gender)
count_per_level <- table(data$level)
print(count_per_level)

## responsability institutions

institutions <- table(data$insitureponsability)

institutions_res <- sum(institutions)

institutions_percent <- (institutions / institutions_res) * 100

print(institutions_percent)

#trust

trustinsti <- table(data$trustinstitutions)
trust <- sum(trustinsti)
trust_institutions <- (trustinsti / trust) * 100
print(trust_institutions)



## GENDER, FIELD OF STUDY AND AI THREat
model <- lm(cbind(idealjobthreat, normalized_idealjobthreat) ~ gender + field, data = data)
summary(model)

## field of study, gender, training, information, frequency = ideal job threat

model2 <- lm(cbind(idealjobthreat, normalized_idealjobthreat) ~ gender + field + training + frequency + information , data = data)
 summary (model2)
 
 #  relationship between training and gender
 model_training_gender <- lm(training ~ gender, data = data)
 summary(model_training_gender)
 
 #  relationship between frequency and gender
 model_frequency_gender <- lm(frequency ~ gender, data = data)
 summary(model_frequency_gender)
 
 #  relationship between information and gender
 model_information_gender <- lm(information ~ gender, data = data)
 summary(model_information_gender)
 
 #  regression analysis field and gender
 model_gender_field <- lm(field ~ gender, data = data)
 
 summary(model_gender_field)
 
 
 #  regression analysis ideal job thread and gender
 model_gender_threat <- lm(idealjobthreat ~ gender, data = data)
 
 summary(model_gender_threat)

 
 
 #### REGRESSION
 #### NORMALIZED THREAT PER FIELD OF STUDY
 
field_labels <- c("Humanities/Social Sciences", "Business/Economics/Law", "Technical/Natural Sciences/Health")
threat_labels <- c("Not at all threatening", "Slightly threatening", "Moderately threatening", "Very threatening", "Extremely threatening")
color_palette <- c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c")
 
counts <- table(data$field, data$idealjobthreat)
proportions <- prop.table(counts, margin = 1)
proportions_df <- as.data.frame.matrix(proportions)
 
 proportions_df$field <- factor(rownames(proportions_df), levels = 1:length(field_labels))
 proportions_df <- tidyr::pivot_longer(proportions_df, -field, names_to = "Ideal_Job_Threat", values_to = "Proportion")
 
 field_lookup <- data.frame(field = factor(1:length(field_labels), labels = field_labels))

 p <- ggplot(proportions_df, aes(x = factor(Ideal_Job_Threat, labels = threat_labels), y = Proportion, fill = Ideal_Job_Threat)) +
   geom_bar(stat = "identity") +
   labs(x = NULL, y = "Normalized Percentage") +
   facet_wrap(~ field, scales = "free", labeller = labeller(field = setNames(field_labels, 1:length(field_labels))), 
              strip.position = "top") +
   scale_fill_manual(values = color_palette, name = "Perceived Threat per Field of Study", 
                     labels = c("Not at all threatening", "Slightly threatening", "Moderately threatening", "Very threatening", "Extremely threatening")) +
   theme(legend.position = "right", legend.title = element_blank(), legend.box = "vertical", 
         plot.title = element_text(hjust = 0.5)) +  
   ggtitle("Perceived Threat per Field of Study")  
 

 print(p)
 

 ## MODEL 1
 model_idealjobthreat <- lm(idealjobthreat ~ field, data = data)
  summary(model_idealjobthreat)
 
 anova_table1 <- anova(model_idealjobthreat)
 print(anova_table1)
 
 
 ## TOTAL MODEL 
totalmodel <- lm(idealjobthreat ~ field + information + training + level + frequency + gender, data = data)
summary(totalmodel)

 
 
 ## GENDER AND TRAINING
 gender_colors <- c("steelblue3", "hotpink")
  model_gender <- lm(normalized_idealjobthreat ~ gender, data = data)
  data_filtered <- data[data$gender %in% c("Male", "Female"), ]
 
   ggplot(data_filtered, aes(x = factor(gender), y = idealjobthreat, fill = as.factor(gender))) +
   geom_boxplot() +
   labs(title = "Effect of Gender on Ideal Job Threat",
        x = "Gender",
        y = "Ideal Job Threat",
        fill = "Normalized Gender") +
   scale_fill_manual(values = gender_colors, labels = c("Male", "Female"), drop = FALSE) +
   theme_minimal()
 
  
## Perceived Job Threat by Training Leve
 
 likert_labels <- c("Not at all threatening", "Slightly threatening", 
                    "Moderately threatening", "Very threatening", 
                    "Extremely threatening")
 
 # Define custom color palette for Likert scale
 likert_colors <- c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c")
 
 # Create a bar plot
 ggplot(data, aes(x = factor(training), fill = factor(idealjobthreat))) +
   geom_bar() +
   scale_fill_manual(values = likert_colors, guide = guide_legend(reverse = TRUE), 
                     labels = likert_labels, 
                     name = "Perceived Job Threat") +
   labs(title = "Perceived Job Threat by Training Level",
        x = "Training Level",
        y = "Frequency") +
   theme_minimal()
 
 ## DISPLACEMENT CONCERN
 
 modeldisplace <- lm(displacementconcern ~ field + information + familiarity + training + level + frequency + gender, data = data)
 summary(modeldisplace)
 plot(modeldisplace, which = 1:4)
check_model(modeldisplace)

view(data)

## Confidence to compete 
confidencemodel <- lm(competeconfidence ~ field + information + familiarity + training + level + frequency + gender, data = data)
summary(confidencemodel) 

## labour market concern
labourmarket <- lm(labourmarketconcern ~ field + information + familiarity + training + level + frequency + gender, data = data)
summary(labourmarket)

##replace humans
replacehumansmodel <- lm(replacehuman ~ field + information + familiarity + training + level + frequency + gender, data = data)
summary(replacehumansmodel)

##Opportunities field of study
opportuntiesmodel <- lm(jobopportunities ~ field + information + familiarity + training + level + frequency + gender, data = data)
summary(opportuntiesmodel)

## optimism
optimismmodel <- lm(optimismfieldwork ~ field + information + familiarity + training + level + frequency + gender, data = data)
summary(optimismmodel)


#second part

## benefitsociety
benefitssocietym <- lm(benefitssociety ~ idealjobthreat + labourmarketconcern + displacementconcern + competeconfidence + replacehuman + jobopportunities + optimismfieldwork, data = data)
summary(benefitssocietym)

## insititute responsability
responmodel <- lm(insitureponsability ~ idealjobthreat + labourmarketconcern + displacementconcern + competeconfidence + replacehuman + jobopportunities + optimismfieldwork, data = data)
summary(responmodel)

## trust institutions
trustmodel <- lm(trustinstitutions ~ idealjobthreat + labourmarketconcern + displacementconcern + competeconfidence + replacehuman + jobopportunities + optimismfieldwork, data = data)
summary(trustmodel)

## tripple model

tripl <- lm(trustinstitutions ~  insitureponsability + benefitssociety, data=data )
summary(tripl)



## pie chart 

map_affectedsector <- function(affectedsector) {
  if (affectedsector %in% c("Healthcare/Medical")) {
    return("Healthcare/Medical")
  } else if (affectedsector %in% c("Scientific Research")) {
    return("Scientific Research")
  } else if (affectedsector %in% c("Social Research")) {
    return("Social Research")
  } else if (affectedsector %in% c("Arts and Humanities")) {
    return("Arts and Humanities")
  } else if (affectedsector %in% c("Business and Finance")) {
    return("Business and Finance")
  } else if (affectedsector %in% c("Legal Services")) {
    return("Legal Services")
  } else if (affectedsector %in% c("Engineering and Technology")) {
    return("Engineering and Technology")
  } else {
    return("Other")  
  }
}

data$affectedsector_merged <- sapply(data$affectedsector, map_affectedsector)
unique(data$affectedsector_merged)

custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")

affectedsector_merged_counts <- table(data$affectedsector_merged)

percentage <- prop.table(affectedsector_merged_counts) * 100

pie(percentage, labels = paste(names(percentage), " (", round(percentage, 1), "%)", sep = ""), 
    main = "Distribution of Affected Sectors", col = custom_colors)


#Relationship between Training on AI and Frequency of Use
ggplot(data, aes(x = training, y = frequency)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relationship between Training on AI and Frequency of Use",
       x = "Training on AI",
       y = "Frequency of Use") +
  theme_minimal()

ggplot(data, aes(x = training, y = familiarity)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Relationship between Training on AI and Familiarity",
       x = "Training on AI",
       y = "Frequency of Use") +
  theme_minimal()

#Gender Distribution by Field of Study
filtered_data <- data[complete.cases(data[c("gender", "field")]), ]

total_counts <- table(filtered_data$gender, filtered_data$field)

ggplot(filtered_data, aes(x = factor(field, labels = c("Humanities/Social Sciences", "Business/Economics/Law", "Technical/Natural Sciences/Health")), fill = factor(gender))) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Gender Distribution by Field of Study",
       x = "Field of Study",
       y = "Count",
       fill = "Gender") +
  scale_fill_manual(values = c("steelblue1", "hotpink1"), labels = c("Male", "Female")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

#  mean ideal job threat
mean_threat_by_field <- aggregate(idealjobthreat ~ field, data = data, FUN = mean)

print(mean_threat_by_field)


##MEANS

#  mean familiarity and mean frequency of use by training level
mean_familiarity <- data %>%
  group_by(training) %>%
  summarise(mean_familiarity = mean(familiarity, na.rm = TRUE))

mean_frequency <- data %>%
  group_by(training) %>%
  summarise(mean_frequency = mean(frequency, na.rm = TRUE))

mean_training_data <- merge(mean_familiarity, mean_frequency, by = "training")

#  mean familiarity by training level
ggplot(mean_training_data, aes(x = factor(training), y = mean_familiarity)) +
  geom_bar(stat = "identity", fill = "steelblue2", color = "steelblue") +
  labs(title = "Mean Familiarity with AI by Training Level",
       x = "Training on AI",
       y = "Mean Familiarity") +
  theme_minimal()

#  mean frequency of use by training level
ggplot(mean_training_data, aes(x = factor(training), y = mean_frequency)) +
  geom_bar(stat = "identity", fill = "maroon", color = "maroon4") +
  labs(title = "Mean Frequency of Use by Training Level",
       x = "Training on AI",
       y = "Mean Frequency of Use") +
  theme_minimal()

##Relationship between Gender and Field of Study (Normalized)

data_filtered <- data[complete.cases(data[c("gender", "field")]), ]

counts <- data_filtered %>%
  count(gender, field)

counts <- counts %>%
  group_by(gender) %>%
  mutate(prop = n / sum(n))

field_labels <- c("Humanities/Social Sciences", "Business/Economics/Law", "Technical/Natural Sciences/Health")

ggplot(counts, aes(x = factor(field, labels = field_labels), y = prop, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relationship between Gender and Field of Study (Normalized)",
       x = "Field of Study",
       y = "Proportion",
       fill = "Gender") +
  scale_fill_manual(values = c("steelblue1", "hotpink1"), labels = c("Male", "Female")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  


data_filtered <- data[complete.cases(data[c("training", "idealjobthreat")]), ]
counts <- data_filtered %>%
  count(training, idealjobthreat)

counts <- counts %>%
  group_by(training) %>%
  mutate(prop = n / sum(n))

ggplot(counts, aes(x = factor(idealjobthreat), y = prop, fill = factor(training))) +
  geom_bar(stat = "identity") +
  labs(title = "Relationship between Training on AI and Labour Market Concern (Normalized)",
       x = "Labour Market Concern",
       y = "Proportion",
       fill = "Training on AI") +
  scale_fill_manual(values = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"),
                    labels = c("No, none at all", 
                               "Yes, but very minimal",
                               "Yes, to a moderate extent",
                               "Yes, quite a bit",
                               "Yes, extensive education or training")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


mean_ideal_job_threat <- data_filtered %>%
  group_by(training) %>%
  summarize(mean_idealjobthreat = mean(idealjobthreat, na.rm = TRUE))

print(mean_ideal_job_threat)
