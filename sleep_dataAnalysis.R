## ----setup, include=FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
knitr::opts_chunk$set(fig.align="center")
library(png)
library(grid)
img <- readPNG("IMG_3053.png")


## ----Pomi Image, echo=F-------------------------------------------------------------------------
grid.raster(img)


## ----Load Packages, echo=T, results='hide'------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(GGally)
library(caret)


## ----Read Data----------------------------------------------------------------------------------
data <- read.csv("sleepdata.csv")
head(data)


## -----------------------------------------------------------------------------------------------
summary(data)


## ----null values--------------------------------------------------------------------------------
#Number of null values per column
colSums(is.na(data))


## ----Unique IDs---------------------------------------------------------------------------------
length(unique(data$Person.ID)) == nrow(data)


## ----average age and BMI per gender-------------------------------------------------------------
gen_avg <- data %>% 
  group_by(Gender) %>%
  summarise(mean_age = mean(Age), n = n())

gen_avg


## ----Exploratory graphs-------------------------------------------------------------------------
data$Sleep.Disorder = factor(data$Sleep.Disorder, levels = c('None','Insomnia','Sleep Apnea'))

df <- data %>% 
  group_by(Sleep.Disorder) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(df, aes(x = "", y = perc, fill = Sleep.Disorder)) +
  geom_col() +
  geom_label(aes(label = labels), color = "black",
            position = position_stack(vjust = 0.5),
            show.legend = FALSE) +
  scale_fill_brewer(palette = "Reds") +  
  coord_polar("y", start = 0) +  
  theme_void() +
  ggtitle("Percentage of Sleep Disorders") +
  theme(plot.title = element_text(hjust=0.5))




## ----Quality of Sleep---------------------------------------------------------------------------
my_comparisons <- list( c("None", "Insomnia"), 
                        c("Insomnia", "Sleep Apnea"), 
                        c("None", "Sleep Apnea") )

ggplot(data, aes(x=Sleep.Disorder, y=Quality.of.Sleep, fill=Sleep.Disorder)) +
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_brewer(palette="Reds") +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 2, color = "white")+ 
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 12) + # Add global p-value
  ggtitle("Quality fo Sleep score distribution \n per Sleep Disorder") +
  theme(plot.title = element_text(hjust=0.5))     


## ----Gender-------------------------------------------------------------------------------------

gend_group <- data %>%
  group_by(Gender,Sleep.Disorder,Age)

gender_bar <- ggplot(data, aes(x=Gender, y=Age, fill=Sleep.Disorder)) +
  scale_fill_brewer(palette="Reds") +
  geom_bar(stat="summary", fun.y = "mean", position="dodge") +
  ggtitle("Sleep Disorders per Gender")+
  theme(plot.title = element_text(hjust=0.5))     

gender_bar 


## ----BMI----------------------------------------------------------------------------------------
data$BMI.Category <- gsub("Normal Weight", "Normal", data$BMI.Category)
data$BMI.Category <- factor(data$BMI.Category, labels=c('Normal','Overweight','Obese'))

bmi_bar <- ggplot(data, aes(x=BMI.Category, fill=Sleep.Disorder)) +
  scale_fill_brewer(palette="Reds") +
  geom_bar() +
  ggtitle("BMI effect on Sleep") +
  theme(plot.title = element_text(hjust=0.5))
bmi_bar


## ----Occupation and Stress, fig.width=12, fig.height=6------------------------------------------
data_stress <- data %>%
  group_by(Occupation) %>%
  summarize(avg_stress = mean(Stress.Level),
            avg_age = mean(Age),
            count = n())
  
occup_point <- ggplot(data, aes(x=Occupation, y=Stress.Level)) +
  geom_point(aes(fill=Sleep.Disorder,size=Age), color='black', shape=21, stroke=0.4) +
  scale_fill_brewer(palette="Reds") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        plot.margin = margin(t = 5, r = 10, b = 5, l = 10),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.title = element_text(hjust=0.5)) +
  ggtitle("Occupation, Age and Stress \n effect on Sleep") 

occup_bar <- ggplot(data, aes(x=Occupation, fill=Sleep.Disorder)) +
  geom_bar() +
  scale_fill_brewer(palette="Reds")+
  ggtitle("Occupation \n effect on Sleep") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.7),
        plot.margin = margin(t = 5, r = 10, b = 5, l = 10),
        axis.title.x = element_text(margin = margin(t = 20)),
        plot.title = element_text(hjust=0.5)) 

print(data_stress)

occup_bar + occup_point + plot_layout(widths = c(1, 1))


## ----Blood ranges-------------------------------------------------------------------------------
BP_ranges = c('Normal', 'Elevated', 'Hypertension_1', 'Hypertension_2', 'Hypertensive_Crisis')
BP_systolic_limits = list(c(0,120),c(120,130),c(130,140),c(140,180),c(180,200))
BP_diastolic_limits = list(c(0,80),c(0,80),c(80,90),c(90,120),c(120,140))

# Categorize into BP ranges depending on the systolic and diastolic values 
data <- data %>%
  separate(Blood.Pressure, into=c('Systolic','Diastolic'), sep="/") %>%
  mutate(Systolic = as.numeric(Systolic),
         Diastolic = as.numeric(Diastolic)) %>%
  mutate(BP_range = case_when(
    between(Systolic, BP_systolic_limits[[1]][1], BP_systolic_limits[[1]][2]) & 
      between(Diastolic, BP_diastolic_limits[[1]][1], BP_diastolic_limits[[1]][2]) ~ BP_ranges[1],
    
    between(Systolic, BP_systolic_limits[[2]][1], BP_systolic_limits[[2]][2]) & 
      between(Diastolic, BP_diastolic_limits[[2]][1], BP_diastolic_limits[[2]][2]) ~ BP_ranges[2],
    
    between(Systolic, BP_systolic_limits[[3]][1], BP_systolic_limits[[3]][2]) & 
      between(Diastolic, BP_diastolic_limits[[3]][1], BP_diastolic_limits[[3]][2]) ~ BP_ranges[3],
    
    between(Systolic, BP_systolic_limits[[4]][1], BP_systolic_limits[[4]][2]) & 
      between(Diastolic, BP_diastolic_limits[[4]][1], BP_diastolic_limits[[4]][2]) ~ BP_ranges[4],
    
    between(Systolic, BP_systolic_limits[[5]][1], BP_systolic_limits[[5]][2]) & 
      between(Diastolic, BP_diastolic_limits[[5]][1], BP_diastolic_limits[[5]][2]) ~ BP_ranges[5],
    
    TRUE ~ "Unknown" # Default case
  ))


# New values to replace "Unknown"
replacement_values <- c(rep("Elevated", 12), rep("Hypertension_1", 2), "Elevated")

data <- data %>%
  mutate(BP_range = replace(BP_range, BP_range == "Unknown", replacement_values)) %>%
  mutate(Heart.Rate = as.factor(Heart.Rate))

data$Heart.Rate <- as.numeric(as.character(data$Heart.Rate))
# Group by BP_range and Heart.Rate, then count occurrences
data_grouped <- data %>%
  group_by(BP_range, Heart.Rate, Sleep.Disorder, Age) %>%
  summarise(Count = n(), .groups = "drop")  # Count occurrences of each Heart Rate

# Convert BP_range into a factor with the specified order
data_grouped$BP_range <- factor(data_grouped$BP_range, levels = BP_ranges)

blood_bar <- ggplot(data_grouped, aes(x = BP_range, y = Count, fill = Heart.Rate)) +
  geom_col() +  
  scale_fill_viridis_c(option = "magma", direction = -1) +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  ggtitle("Heart rate of \n of blood pressure ranges") +
  theme(plot.title = element_text(hjust=0.5))

blood_bar


## ----BP and Heart Rate--------------------------------------------------------------------------
data_grouped$Sleep.Disorder = factor(data_grouped$Sleep.Disorder, levels = 
                                       c('None','Insomnia','Sleep Apnea'))

blood_sleep <- ggplot(data_grouped, aes(x=BP_range, y=Heart.Rate)) + 
  geom_jitter(aes(fill=Sleep.Disorder,size=Age), color='black', 
              shape=21, stroke=0.4, width=0.3) + 
  scale_fill_brewer(palette="Reds") +
  ggtitle("Effect of BP range, Heart Rate \n and Age on Sleep") +
  theme(plot.title = element_text(hjust=0.5))

blood_sleep


## ----Sport and Stress, fig.width=12, fig.height=6-----------------------------------------------
data <- data %>% 
  mutate(Stress.Level = as.factor(Stress.Level))

act_bar <- ggplot(data, aes(x=Physical.Activity.Level,y=Daily.Steps)) +
  geom_jitter(aes(size=Age, fill=Stress.Level), color='black', shape=21, stroke=0.4) +
  scale_fill_brewer(palette="Blues") +
  geom_smooth(method=lm,  linetype="dashed",
              color="darkblue") +
  ggtitle("Effect of Physical Activity, \n Daily Steps and Age on Stress") +
  theme(plot.title = element_text(hjust=0.5))

act_stress_bar <- ggplot(data, aes(x=Physical.Activity.Level,y=Daily.Steps)) +
  geom_jitter(aes(fill=Sleep.Disorder, size=Age), color='black', shape=21, stroke=0.4) +
  scale_fill_brewer(palette="Reds") +
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred")+
  ggtitle("Effect of Physical Activity, \n Daily Steps and Age on Sleep") +
  theme(plot.title = element_text(hjust=0.5))

act_bar + act_stress_bar + plot_layout(widths = c(1, 1))


## ----Stress, Activity and Steps-----------------------------------------------------------------
data %>%
  group_by(Occupation, Stress.Level) %>%
  summarise(avg_steps = mean(Daily.Steps),
            avg_activity = mean(Physical.Activity.Level))


## ----Sleep duration and quality-----------------------------------------------------------------

sleep_q <- ggplot(data, aes(x=Sleep.Duration,y=Quality.of.Sleep)) +
  geom_jitter(aes(fill=Sleep.Disorder), color='black', 
              shape=21, stroke=0.4, size=3, width=0.5) +  
  scale_fill_brewer(palette="Reds") +
  ggtitle("Effect of Sleep Duration and \n Quality of Sleep on Sleep Disorders") +
  theme(plot.title = element_text(hjust=0.5))

sleep_q


## ----Correlation--------------------------------------------------------------------------------
library(corrplot)

# Compute correlation matrix (only for numeric variables)
numeric_data <- data[sapply(data, is.numeric)]
numeric_data <- numeric_data[2:ncol(numeric_data)]
cor_matrix <- cor(numeric_data, use="complete.obs")

# Visualize using corrplot
corrplot(cor_matrix, method="color", type="upper", 
         col= colorRampPalette(c("blue", "white", "red"))(200),
         tl.col="black", tl.srt=45)  # Adjust label angle


## ----ML preprocessing---------------------------------------------------------------------------
df <- data %>%
  select(!c(Person.ID)) %>%
  mutate(
    Gender = if_else(data$Gender == "Female", 0, 1), #Female=0, Male=1,
    Occupation = as.integer(factor(data$Occupation)),
    BMI.Category = as.integer(factor(data$BMI.Category)),
    BP_range = as.integer(factor(BP_range)),
    Stress.Level = as.integer(Stress.Level)
  )



## ----Check distributions 1, fig.width=12, fig.height=6------------------------------------------

age <- ggplot(data, aes(x=Age)) + geom_histogram() 
sleepdur <- ggplot(data, aes(x=Sleep.Duration)) + geom_histogram() 
sleepqual <- ggplot(data, aes(x=Quality.of.Sleep)) + geom_histogram() 

age+sleepdur+sleepqual+ plot_layout(widths = c(1, 1,1))



## ----Check distributions 2, fig.width=12, fig.height=6------------------------------------------

phy <- ggplot(data, aes(x=Physical.Activity.Level)) + geom_histogram() 
stress <- ggplot(data, aes(x=Stress.Level)) + geom_histogram(stat="count") 
sys <- ggplot(data, aes(x=Systolic)) + geom_histogram() 

phy+stress+sys+plot_layout(widths = c(1, 1,1))


## ----Check distributions 3, fig.width=12, fig.height=6------------------------------------------

dia <- ggplot(data, aes(x=Diastolic)) + geom_histogram() 
hr <- ggplot(data, aes(x=Heart.Rate)) + geom_histogram() 
ds <- ggplot(data, aes(x=Daily.Steps)) + geom_histogram() 

dia+hr+ds+ plot_layout(widths = c(1, 1,1))


## ----Train/Test split---------------------------------------------------------------------------
set.seed(123)  # For reproducibility
split <- createDataPartition(df$Sleep.Disorder, p = 0.8, list = FALSE)

train_data <- df[split, ]
test_data  <- df[-split, ]


## ----Random Forrest Model-----------------------------------------------------------------------
# Convert target variable to factor (for classification)
train_data$Sleep.Disorder <- as.factor(train_data$Sleep.Disorder)
test_data$Sleep.Disorder  <- as.factor(test_data$Sleep.Disorder)

# Set up training control with cross-validation
train_control <- trainControl(method = "cv", number = 5)  

# Train Random Forest model
model <- train(Sleep.Disorder ~ ., data = train_data,
               method = "rf",  
               trControl = train_control,
               tuneLength = 5)  

# View model details
print(model)

# Make predictions
predictions <- predict(model, newdata = test_data)

# Evaluate accuracy
conf_matrix <- confusionMatrix(predictions, test_data$Sleep.Disorder)
print(conf_matrix)


## ----Pomi Image 2, echo=F-----------------------------------------------------------------------
img <- readPNG("pom_reading.png")
grid.raster(img)

