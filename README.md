<h1> statistics_metagenomics </h1>

This repository provides R code for performing few non parametric statistical test on metagenomics data. It is applicable to when sample replictes are not available, particulary useful for environmental metagenomic studies, done using long read sequencing protccol.

Statistical Test
We use these statistical test to compare the gene abundance data observed from metagenomic functional profiling of long read metagenomic sequence data. Under SEED functional classification system generated with MEGAN functional annotation, functional data at various subsystems level is acquired with count represting number of aligned bases associated with each functional category. In order to validate the statistical ssignificane of the observed differences between any two sample types, following non paarametric statistical measures are applied.

<h3> Binomial test </h3>
Binomial test can be used to compare the proportion of a functional data or gene data count between two  metagenomic samples, determining if the observed difference in proportions is statistically significant. In this case, the binomial test calculates a p-value, which represents the probability of observing a count as extreme as, or more extreme than, the one obtained from the sample, assuming the null hypothesis is true.
It can be used to identify functional gene that shows significant differences between the two sample types.

 <h3> Wilcoxon rank sum test </h3>
Wilcoxon rank sum test is a non-parametric statistical test used to compare two independent groups when the data is not normally distributed. In metagenomics data, it can be applied to test whether the abundance of a particular group of gene involved in a function significantly differs between two samples/conditions. The HMMER homology search of proteins belonging to a certain functional category such as copper resistance can be compared between the two samples and the observed differences is statistically validated with this test.
The p-value from a Wilcoxon rank-sum test tells you whether the distributions of two independent groups are significantly different i.e if the observed difference in count for a certain group of gene belonging to a functional category is significantly different between sample types.

 <h3> Permutation test </h3>
Permutation test provide a non-parametric alternative for detecting functional groups (genes, pathways, or taxa) that show significant differences across multiple groups. 
It is used to identify one or many functional groups that are significantly different in comparison to all other functional profile. This will characterize gene functions that are important for a sample type in comparison to others.
In this repo, we will be comparing two samples functional profile to parse out functional groups among all the categories that are significantly abundant in one sample of interest. This includes looking at the sum ratios for a particular category, in comparison to the randomly generated pseduoreplicate data from the overall distribution. The p-value is calculated by computing null distribution for each group, and comparing its difference to the permuted distribution.

 <h3> Benjamini-Hochberg (BH) error correction </h3> 
BH procedure is a statistical method used to control the False Discovery Rate (FDR) when performing multiple hypothesis tests. The calculation of adjusted p-values involves comparing each individual p-value to a critical value or threshold.  
