setwd("/home/pratima/Insync/pgautam1@umbc.edu/Google Drive/Cusick Lab/Metagenomic_analysis/Megan7_analysis/statistics/binomial_cuhmmer/")

n<-9 #total cu protein studied in hmm test
x<-9 #total cu enriched in biofilm (success)

p <- 0.5 #H0 ... no enrichment
p_hat <- 9/9 # oberved probability


Binom_coeff<- factorial(n)/(factorial(x) * (factorial(n-x)))
Binom_coeff
#1


prob_x <- Binom_coeff * p^x * p^(n-x)
prob_x #result = 0.001953125
#1/512
#two sided 

#build in bionomial function
binom.test(x=9, n=9, p=0.5, alternative= "greater")

#result for line 21
# Exact binomial test
# 
# data:  9 and 9
# number of successes = 9, number of trials = 9, p-value = 0.001953
# alternative hypothesis: true probability of success is greater than 0.5
# 95 percent confidence interval:
#   0.7168712 1.0000000
# sample estimates:
#   probability of success 
# 1 