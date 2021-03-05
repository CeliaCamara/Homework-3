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

3.The Dimensions of Popular Music
Alasdair thinks that modern pop music is rubbish, and all sounds the same. To support his view, he found an interesting article on the internet that walks through applying PCA to the Top 200 songs from Spotify (from 2018). The writer claims that when PCA is applied to this dataset, 99% of the variance can be explained using only two dimensions! (Alasdair feels justified in his biases.)
•	Download the dataset used for the article. Can you replicate their PCA results? [10 marks]
•	How would you interpret their finndings? [10 marks]
•	Improve on their analysis in any way you see fit, visualise your results and discuss your conclusions. [15 marks]
We want to build a model for [the Global Top 200 songs on Spotify in 2018] (Retrieved from: github.com/PrathamNawal/All-About-the-Music/blob/master/Spotifier/Data/spotify_cleaned.csv)
Import the data file
library(tidyverse)
#load the data
data <- read.csv('spotify_cleaned.csv')
Principal Component Analysis (PCA)
•	Before proceeding to the PCA, we will need to discard the variables that are not relevant for our analysis (i.e., we remove the number of the song, the track name, the artist and the number of streams).
•	Then, we can apply the command prcomp() to the data in order to generate the PCA - I labelled the new data as pca_data to indicate the PCA.
•	Finally, we summarise the pca_data to observe the resulting components.
data <- select(data, -Track.Name, -Artist, -Streams) #remove the variables that are not needed for analysis
pca_data <- prcomp(data) #Principal Component Analysis with updated data

summary(pca_data) #summarise pca_data to observe principal components
## Importance of components:
##                            PC1     PC2     PC3     PC4     PC5     PC6     PC7
## Standard deviation     58.0716 38.3781 28.8100 3.73023 1.92576 0.46402 0.20982
## Proportion of Variance  0.5923  0.2587  0.1458 0.00244 0.00065 0.00004 0.00001
## Cumulative Proportion   0.5923  0.8510  0.9968 0.99929 0.99994 0.99998 0.99998
##                            PC8    PC9   PC10   PC11    PC12    PC13    PC14
## Standard deviation     0.18297 0.1615 0.1173 0.1026 0.08942 0.07783 0.01857
## Proportion of Variance 0.00001 0.0000 0.0000 0.0000 0.00000 0.00000 0.00000
## Cumulative Proportion  0.99999 1.0000 1.0000 1.0000 1.00000 1.00000 1.00000
Scree plot
•	Enter the command screeplot () to generate a scree plot and visualize the pca_data in order to further observe the dimensions that need to be considered in our model.
screeplot(pca_data, type="barplot", main="Scree Plot")
 ![image](https://user-images.githubusercontent.com/79974568/110117585-f7cc2780-7db0-11eb-8fac-1b2c8e2fea7c.png)

Interpretation of the results
•	The summary table indicates that almost 100% of the variance is explained by the first two components of our model, with the first component (PC1) accounting for approximately 64% of the variance (Proportion of Variance=.6375), and the second component (PC2) accounting for about 35% of the variance (Proportion of Variance=.3548).
•	This shows that the vast majority of the information included in the dataset seems to be explained by these two principal components.
•	Additionally, the scree plot nicely illustrates how the other principal components account for very little of the variance. Therefore, we can say that the components from PC3 onward only add noise.
•	These results coincide with Nawal’s(2018), meaning that their PCA has been successfully replicated.
Improve the analysis
•	We can improve this analysis by re-running it with scaling.
pca_data <- prcomp(data, scale = TRUE) #introduce scaling to pca_data

summary(pca_data) #summarise updated pca_data to observe new principal components
## Importance of components:
##                           PC1    PC2     PC3     PC4     PC5     PC6     PC7
## Standard deviation     1.5893 1.2644 1.17187 1.14573 1.05091 0.99113 0.94839
## Proportion of Variance 0.1804 0.1142 0.09809 0.09376 0.07889 0.07017 0.06425
## Cumulative Proportion  0.1804 0.2946 0.39271 0.48648 0.56536 0.63553 0.69977
##                            PC8     PC9    PC10    PC11    PC12    PC13    PC14
## Standard deviation     0.91503 0.88676 0.86356 0.79923 0.77089 0.61075 0.47723
## Proportion of Variance 0.05981 0.05617 0.05327 0.04563 0.04245 0.02664 0.01627
## Cumulative Proportion  0.75958 0.81575 0.86901 0.91464 0.95709 0.98373 1.00000
Scree Plot After Scaling
screeplot(pca_data, main="Scree Plot After Scaling") #second screeplot to visualize new principal components
 ![image](https://user-images.githubusercontent.com/79974568/110117556-ee42bf80-7db0-11eb-9195-9abc7a234b4c.png)

•	Observe how, after scaling the data, the proportion of variance accounted for the model is much more distributed across the different principal components, which makes it harder to establish a clear cut for our model.
•	This doesn’t fit well with the original interpretation as it seems that the first two components only account for about 30% of the variance.
Conclusion
•	Altogether, our additional analysis seems to suggest that there is some variation in pop music after all.
