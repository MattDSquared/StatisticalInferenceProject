# Does the Type of Vitamin C Suppliment Affect Tooth Growth?
MattDSquared  
Saturday, June 20, 2015  

## Question
By looking at the R datasets package for ToothGrowth, does the type of suppliment used for vitamin C delivery have an effect on tooth growth in guinea pigs?

_This project is a part of the Coursera Statistical Inference Class_

## Project Setup

The original data set (as found by calling `?ToothGrowth` from Rstudio) comes from a study by C. I. Bliss focusing on length of odontoblasts (teeth) in each of 10 guinea pigs at each of three dose levels of Vitamin C (0.5, 1, and 2 mg) with each of two delivery methods (orange juice or ascorbic acid).


```r
library(datasets)
library(ggplot2)
library(plyr); library(dplyr)
library(knitr)
data(ToothGrowth) # loads ToothGrowth data frame

# rename variables for plotting clarity
names(ToothGrowth) <- c("length","suppliment","dose")

# assign more clear suppliment labels
ToothGrowth$suppliment <- factor(ToothGrowth$suppliment, levels=c("OJ","VC"), labels=c("Orange Juice", "Ascorbic Acid"))

# number of guinea pigs per sample
n <- 10
```

## Exploratory Analysis
What are the general trends in tooth length vs suppliment delivery method and dosage? Let's assume the underlying distribution is iid normally distributed and the confidence bounds of any 10-guinea-pig group can be explained by a t distribution with degrees of freedom, n-1=9. 


```r
df <- n-1
ToothStats <- ToothGrowth %>% 
    group_by(suppliment,dose) %>%
    summarize(mean_length = mean(length), sd_length=sd(length), 
              lbound=mean_length-qt(.975,df)*sd_length,
              ubound=mean_length+qt(.975,df)*sd_length)
```
The below plot shows how tooth length varies by suppliment type and dosage. The line plot connects the group means, with the 95% confidence interval shown in grey.

```r
gg <- ggplot() +
    facet_grid(. ~ suppliment) +
    geom_point(data=ToothGrowth, 
               aes(x=dose,y=length, color=suppliment)) +
    geom_line(data = ToothStats, 
              aes(x=dose, y=mean_length, color=suppliment)) +
#    geom_errorbar(data = ToothStats, 
#                  aes(x=dose, ymin=lbound, ymax=ubound)) +
    geom_ribbon(data = ToothStats, 
                  aes(x=dose, ymin=lbound, ymax=ubound), 
                alpha=.3) +
    labs(title="Effect of vitamin C suppliments on Tooth Growth") +
    labs(x="Dosage Size [mg]") +
    labs(y="Tooth Length")
print(gg)
```

![](ToothGrowthSignificance_files/figure-html/exploratory.display-1.png) 

It certainly appears that orange juice does improve tooth growth on average, however lets use t-tests to understand if these differences are significant. 

## Signficance of suppliment type by dose on length of tooth
The objective of this study is to determine whether or not one suppliment significantly increases tooth growth over the other at a given dosage. Or more formally:

H~0~: suppliment type has no effect on tooth length at any dosage level.  
H~a~: suppliment type has a non-zero effect on tooth length at any dosage level.  

As stated earlier, assume the underlying distribution is iid normal, with the t distribution able to explain the distribution of this small sample size. 


```r
# t-test for suppliment groups between each dosage
dose.t.test <- function(dosage) {
    dose.t <- t.test(data = filter(ToothGrowth, dose == dosage), 
                     length ~ suppliment, 
                     paired = FALSE, var.equal = TRUE)
    dose.t <- with(dose.t, data.frame(statistic, parameter, p.value, 
                                      conf.int[1], conf.int[2],
                                      row.names=dosage))
    names(dose.t) <- c("t","df","p.value","conf.int.l","conf.int.u")
    dose.t
}
t.stats <- rbind(dose.t.test(.5), dose.t.test(1), dose.t.test(2))
```
The table below shows the t test for significance between Orange Juice and Ascorbic Acid suppliment (in that order) for each dosage level. 

```r
kable(t.stats, format="markdown", digits = 3,
      col.names=c("Dosage (mg)","t statistic","p-value",
                  "95% t-interval, lower","95% t-interval, upper"))
```



|    | Dosage (mg)| t statistic| p-value| 95% t-interval, lower| 95% t-interval, upper|
|:---|-----------:|-----------:|-------:|---------------------:|---------------------:|
|0.5 |       3.170|          18|   0.005|                 1.770|                 8.730|
|1   |       4.033|          18|   0.001|                 2.841|                 9.019|
|2   |      -0.046|          18|   0.964|                -3.723|                 3.563|

## Conclusion
The t-tests show that at a dosage level of 0.5 and 1.0 mg, Orange Juice has a significantly greater effect on increasing tooth growth than Ascorbic Acid, however at a dosage level of 2.0 mg, there is no significant difference between the two suppliments. With this information we can certainly decide H~a~, suppliment type has an effect on tooth growth, at least at certain dosage levels.