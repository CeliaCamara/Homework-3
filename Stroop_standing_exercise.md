# HOMEWORK 3
 Distinction Material
2. Stroop, standing up or sitting down 
In one study, participants who completed the Stroop task showed a smaller
Stroop effect (difference between congruent and incongruent trials) when they
performed the task standing than when sitting. Another group of researchers decided to try to replicate this effect.

- Fit an appropriate model to this data
- Using this model and any plots you have made, qualitatively assess whether the replication study showed a smaller Stroop eect when participants performed the task standing v.s. sitting (i.e. whether it was able to replicate the original study). 

Import the data file
#load the data 
library(tidyverse)
data <- read.csv("stroop_standing_data.csv",
                 header=TRUE, stringsAsFactors = FALSE)
Data pre-processing
#select only correct responses
correct_data <- filter(data, 
                       correct == TRUE) # Correct responses are coded as 1 (1=TRUE)
#remove extreme values to leave the final data for analysis
final_data <- correct_data %>%
  filter(rt > 0.200 & rt < 2.00) %>% #select rts that fall within 200ms and 2000ms as the set of boundaries
   select(-X) #tidy the table up

#remove 'practice' rows in the 'phase' columns for analysis purposes
final_data <- subset(final_data, phase!="practice")

Fit model  
- The study looks at the effect of standing up vs. sitting down during a word-colour stroop task on the stroop effect. 
- It includes a between-subject condition in which participants either start the experiment standing or sitting, and two within-subjects variables (block type:standing v. sitting, and stimulus congruency:congruent, incongruent, baseline) selected to test the hypothesis that performing the task standing reduces the stroop effect (i.e., the difference in response time between congruent and incongruent trials). 
- As the study includes both between-subjects and within-subjects measurements, a mixed model may be appropriate.
```{r}
library(lme4)
x <- final_data$phase #I have considered the variable 'phase' as the predicting variable for the model since the hypothesis asks specifically whether participants' response time varies according to the block type (i.e., phase variable)
y <- final_data$rt #participants' response time, hence, is the outcome variable
mixed_model <- lmer(y ~ x + (x|subject), data=final_data) #the model looks at the effect of standing v. sitting (measured by x) influences rt (measured by y), and how this varies within subjects. Final_data values are taken for this analysis
summary(mixed_model) #summarise the model for interpretation
```
- Total variance = sum of variance estimates 
```{r}
2416.9 + 714.4 + 18706 
```
- Proportion of variance accounted for = nested effect variance/total variance
```{r}
2416.9/21837.3
```
- Only 11% of the total variance of the random effects is attributed to the nested effect.
```{r}
714.4/21837.3
```
- Additionally, the effect of standing is minimal, only accounting for 3.2% of the total variance. This suggests that standing has no meaningful effect.

Data visualization
•	Phase and congruency are the two within-subjects IVs, with two and three levels respectively. This means that we need the average response time for six different conditions.
final_data$rt <- as.numeric(final_data$rt) #the rt is in the form of character in the final_data table, and we need it as numeric for the analysis 

#Define conditions and calculate the mean response time for each condition
analysis_data <- final_data %>%
  mutate(condition = paste0(phase, congruency)) %>% #combine the IVs into separate conditions
  group_by(condition)%>% #group the analysis to get separate means for each condition (i.e., groups)
   summarise(mean_rt=mean(rt)) #get the mean rt for each condition
 
analysis_data #this data frame includes the mean response times in the stroop task as a function of the stimulus-response congruency within the sitting and standing blocks. 
## # A tibble: 6 x 2
##   condition           mean_rt
## * <chr>                 <dbl>
## 1 sittingbaseline       1163.
## 2 sittingcongruent      1167.
## 3 sittingincongruent    1187.
## 4 standingbaseline      1140.
## 5 standingcongruent     1150.
## 6 standingincongruent   1183.
•	From here, we can plot the data to visualize it.
library(ggplot2)

#plot the data to show the sample distribution
plot <- ggplot(analysis_data, aes(x=condition, y=mean_rt, col=condition)) + 
  geom_point() +
  ggtitle("Fig.: Stroop Standing Effect") + 
  ylab("Response Time(ms)") + xlab("Posture")

print(plot)
![image](https://user-images.githubusercontent.com/79974568/110116432-660fea80-7daf-11eb-8d55-04e6e85e0cf1.png)


Interpretation and conclusion
•	The plot shows the three sitting conditions (baseline, congruent, incongruent) on the left hand, and the three standing conditions (baseline, congruent, incongruent) on the right hand.
•	Observe that in both blocks, response time is notably higher in the incongruent conditions than in the rest.
•	Additionally response time seems slightly lower in the standingincongruent condition than in the sittingincongruent one.
•	Nevertheless, the difference between incongruent and congruent trials actually seems larger in the standing block, meaning that the stroop effect was not smaller when participants were standing.
•	In conclusion, the present analysis rejects the hypothesis that the stroop effect is reduced by standing up, and hence indicates that this study was not able to replicate the original experiment.

