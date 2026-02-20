# geo411-Analysis-of-Variance
Analysis of variance

# Title: 
An analysis of variance in annual precipitation between 3 different time periods for Buffalo and San Diego between 1940 and 2025.
# Overview: 
This is analysis of the variances of annual precipitation between 3 time periods (around 27-28 years each) between 1940 and 2025 for the city of Buffalo and San Diego in order to make an conclusion if the annual precipitation of the 3 time periods for the 2 city varies from each other. 
# Methods: 
This is calculated using R Studio, we use mainly use ANOVA, Kruskal-Wallis and Median Test to draw an conclusion. However, we also used Kolmogorov–Smirnov test and Levene Test.
# Keyfindings: 
There are evidence of variations in annual precipitation between the 3 time period. The results for San Diego show p-scores of 0.7401(Chi-Square/median test), 0.7362 (Kruskal-Wallis test) and 0.2436 (ANOVA test). While, Buffalo has the results of 0.2604 (Chi-Square), 0.03095 (Kruskal-Wallis) and 0.5242 (ANOVA test). These 6 p-values are all greater than the acceptible critical value of 0.05, in which we have to reject the null hypothesis and declare there is an variation in annual precipitations for the 3 different time periods for both of the cities.
# Tools:
R Studio, Github and Microsoft Words
# Reflection: 
I had assumed San Diego to be an drier place than Buffalo, I didn't expect that San Diego has an greater variation in precipitation than Buffalo, in fact, it shows a greater variation than Buffalo, judging from the p-scores. I assume that San Diego's precipitation are more suspectible to changes in the El Niño Southern Oscillation than Buffalo, seeing how the precipitation increases for 1 time period, while it decrease for the next one.
