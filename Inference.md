# Part 2: Inferential Data Analysis
Keh-Harng Feng  
March 7, 2017  



## Synopsis
Teeth length data are extracted from the ToothGrowth dataset. Utilizing one-sided T-tests, multiple hypothesis are tested. The result indicates that the highest dosage, 2 mg/day gives the most significant increase to teeth length in guinea pigs. Also in general, orange juice is more effective than asorbic acid.  However, both supplements are about equally effective at the highest dosage tested.

## 1. Data Summary

According to the [R documentation](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html), ToothGrowth is a data frame containing the following variables:

1. len (numeric): Tooth length of guinea pigs.
2. supp (factor): Supplement type (VC for asorbic acid or OJ for orange juice).
3. dose (numeric): Dose in milligrams/day (0.5, 1, or 2).

each containing 60 observations. The first couple of data entries can be seen below:


```r
head(ToothGrowth)
```

```
##    len supp dose
## 1  4.2   VC  0.5
## 2 11.5   VC  0.5
## 3  7.3   VC  0.5
## 4  5.8   VC  0.5
## 5  6.4   VC  0.5
## 6 10.0   VC  0.5
```

Without additional information, it is reasonable to assume that the guinea pigs are independent of each other and the supplement type/dosage given are randomly decided with no bias. Since there are only a total of 60 observations, each group with the same supplement type and dosage can potentially have a sample size < 30. T tests are therefore more appropriate for samples of such small sizes.

## 2. Exploratory Analysis

The data is factored into six groups for each supplement type and each dosage. The mean tooth length under each supplement types and each dosage is shown in Figure 1 below.


```r
group_extract <- function(supp_str, dose) {
    return(ToothGrowth$len[(ToothGrowth$supp == supp_str) & (ToothGrowth$dose == dose)])
}
    
# Dosage: 0.5
OJ_0.5 <- group_extract('OJ', 0.5)
mean_OJ_0.5 <- mean(OJ_0.5)
VC_0.5 <- group_extract('VC', 0.5)
mean_VC_0.5 <- mean(VC_0.5)

# Dosage: 1
OJ_1 <- group_extract('OJ', 1)
mean_OJ_1 <- mean(OJ_1)
VC_1 <- group_extract('VC', 1)
mean_VC_1 <- mean(VC_1)
                  
# Dosage: 2
OJ_2 <- group_extract('OJ', 2)
mean_OJ_2 <- mean(OJ_2)
VC_2 <- group_extract('VC', 2)
mean_VC_2 <- mean(VC_2)

means <- c(mean_OJ_0.5, mean_VC_0.5, mean_OJ_1, mean_VC_1, mean_OJ_2, mean_VC_2)
names(means) <- c('OJ 0.5', 'VC 0.5', 'OJ 1', 'VC 1', 'OJ 2', 'VC 2')

# Plot

barplot(means)
```

![**Figure 1**: Mean tooth length from each supplement type and dosage.](Inference_files/figure-html/panel_plot-1.png)

Each observation group is referred to by supplement type followed by dosage. For example, asorbic acid delivered at 2 mg/day is referred to as VC 2. From the plot it seems that VC 2 produces the longest tooth length, followed by OJ 2, OJ 1, VC 1, OJ 0.5 and finally VC 0.5. An assertion is formulated first:

$$H_0: \bar{X}_{VC 2} - \bar{X}_{OJ 2} = \bar{X}_{VC 2} - \bar{X}_{VC 1} = \bar{X}_{VC 2} - \bar{X}_{OJ 1} = \bar{X}_{VC 2} - \bar{X}_{VC 0.5} = \bar{X}_{VC 2} - \bar{X}_{OJ 0.5} = 0$$
$$H_A: \bar{X}_{VC 2} > \bar{X}_{OJ 2}, \bar{X}_{VC 1}, \bar{X}_{OJ 1}, \bar{X}_{OJ 0.5}, \bar{X}_{VC 0.5}$$
In other words, the alternative hypothesis is having 2 mg/day of vitamin C from asorbic acid produces the longest teeth on average compared to the other dosages. The null hypothesis is simply that there is no significant difference between VC 2 and any of the other averages.

## 3. Hypothesis Test

Variances and sample sizes of each group is determined as follows:

```r
var_VC_2 <- var(VC_2)
var_OJ_2 <- var(OJ_2)
var_VC_1 <- var(VC_1)
var_OJ_1 <- var(OJ_1)
var_VC_0.5 <- var(VC_0.5)
var_OJ_0.5 <- var(OJ_0.5)

n_VC_2 <- length(VC_2)
n_OJ_2 <- length(OJ_2)
n_VC_1 <- length(VC_1)
n_OJ_1 <- length(OJ_1)
n_VC_0.5 <- length(VC_0.5)
n_OJ_0.5 <- length(OJ_0.5)
```

P values of one sided T-tests between the average of VC 2 and averages of the other groups are presented below with a confidence level of 0.95:


```r
pvals <- c(t.test(VC_2, OJ_2, alternative = 'greater', conf.level = 0.95)$p.value,
           t.test(VC_2, VC_1, alternative = 'greater', conf.level = 0.95)$p.value,
           t.test(VC_2, OJ_1, alternative = 'greater', conf.level = 0.95)$p.value,
           t.test(VC_2, VC_0.5, alternative = 'greater', conf.level = 0.95)$p.value,
           t.test(VC_2, OJ_0.5, alternative = 'greater', conf.level = 0.95)$p.value)

names(pvals) <- c('vs OJ 2', 'vs VC 1', 'vs OJ 1', 'vs VC 0.5', 'vs OJ 0.5')

barplot(log(pvals), main = 'Log P values of One-Sided T-Test (VC 2 vs Others)')
abline(h = log(0.05), col = 'red')
legend('bottomleft', c('Log P Values', 'Log Cutoff (0.05)'), col = c('black', 'red'), lty = c(1, 1))
```

![**Figure 2**: Log of P-values from one-sided T-test (x > y) between VC 2 and other groups. Cutoff at P-value = log(0.05) is drawn as the red line.](Inference_files/figure-html/limited_t_test-1.png)

It can be seen that besides OJ 2, comparison to all other groups results in a p-value < 0.05. This means the null hypothesis can be rejected for all groups except OJ 2. This is somewhat expected since the mean for OJ 2 is the closest to VJ 2 in Figure 1. It can be concluded that VC 2 results in significantly longer teeth in all cases except when compared to OJ 2.

### 3.1 Effect of Dosage
Since OJ 2 and VC 2 do not differ with each other significantly, it is maybe interesting to limit the deciding factor to just the dosage. Another set of t.test is carried out for a modification of the original assertion, that is a dosage of 2 mg/day of vitamin C of both delivery methods produce significantly longer tooth compared to smaller dosages. Or in formal statistical language:

$$H_0: \bar{X}_{2} - \bar{X}_{1} = \bar{X}_{2} - \bar{X}_{0.5} = 0$$
$$H_A: \bar{X}_{2} > \bar{X}_{1}, \bar{X}_{0.5}$$
Once again one-sided T-tests are carried out to check if 2 mg/day average is greater than other groups. The p-values are listed below:

```r
group_0.5 <- c(OJ_0.5, VC_0.5)

group_1 <- c(OJ_1, VC_1)

group_2 <- c(OJ_2, VC_2)

pvals_dose <- c(t.test(group_2, group_1, alternative = 'greater', conf.level = 0.95)$p.value,
                t.test(group_2, group_0.5, alternative = 'greater', conf.level = 0.95)$p.value)

names(pvals_dose) <- c('> 1 mg/day', '> 0.5 mg/day')

print(pvals_dose)
```

```
##   > 1 mg/day > 0.5 mg/day 
## 9.532148e-06 2.198762e-14
```

With such small p-values it is safe to reject the null hypothesis. Therefore 2 mg/day of vitamin C, regardless of supplement, results in the longest teeth length by a significant margin.

### 3.2 Effect of Supplement Type
While OC 2 does not differ significantly from VC 2, perhaps the same cannot be said when comparing the effect of supplement type at other dosage levels. The following hypothesis is tested:

$$H_0: \bar{X}_{OJ} - \bar{X}_{VC} = 0$$
$$H_A: \bar{X}_{OJ} > \bar{X}_{VC}$$

In other words, a hypothesis test is performed to see if giving orange juice is more effective at increasing teeth length compared to giving asorbic acid.

```r
group_OJ <- c(OJ_2, OJ_1, OJ_0.5)
group_VC <- c(VC_2, VC_1, VC_0.5)

t.test(group_OJ, group_VC, alternative = 'greater', conf.level = 0.95)$p.value
```

```
## [1] 0.03031725
```

Since p-value is once again smaller than the traditional cutoff value at 0.05, the null hypothesis is rejected. Orange juice is found to increase teeth length more effectively compared to asorbic acid. Notice that in Section 3 a comparison has already been made between VC 2 and OJ 2 and the null hypothesis is not rejected in that case. Therefore it is also safe to conclude that the higher effectiveness of orange juice is only significant at dosages lower than 2 mg/day.
