setwd("/home/pratima/Insync/pgautam1@umbc.edu/Google Drive/Cusick Lab/Metagenomic_analysis/Megan7_analysis/statistics/permutation_22group_seed/")

library(ggplot2)
library(readxl)
library(openxlsx)
library(writexl)
### PERMUTATION HYPOTHESIS TEST ###

# load CSV file for data permutation
alldata<- read.csv(file="terminalnode.csv", header=T, sep="\t")

#load observed data file
excel_path <-("seed_group.xlsx")
#list of sheets in observed data file
sheet_names <- excel_sheets(excel_path)



results <- data.frame(Category = character(), PValuefrom = character(), Biofilm = numeric(), Water= numeric(), Ratio = numeric(), PValue= numeric(), stringsAsFactors = FALSE)

# check the names, etc
names(alldata)
#creating dataframe from CSV file for comparision
compdata <- data.frame(
  Category <- alldata[1]
  ,Water <- alldata[5]
  ,Biofilm <- alldata[6]
)
#View(compdata)

#read individual sheet from observed data file
for (sheet in sheet_names) {
  sheet_data <- #read_excel(excel_path, sheet = sheet)
    read.xlsx(excel_path, sheet = sheet, colNames = TRUE)
  # create Dataframe from sheet in observed data 
  data <- data.frame(
    Category <- sheet_data[1]
    ,Ratio <- sheet_data[6]
  )
#number of rows in sheet  
counts <- nrow(data)-1

  # get ratio Need to update
  test.stat <- sheet_data[[6]][nrow(data)] 
  #print(test.stat)
  #print(sheet)
  
  
  # the number of observations to sample
  n <- counts
  # the number of permutation samples to take
  P <- 1000
  sum_A=numeric()
  sum_B=numeric()
  variableBio=numeric()
  variableWater=numeric()
  workbook <- createWorkbook() #this with code in line number 56,57,79-82, 88-91, 104 and 105 are used for create excel file with water and biofilm random records.
  permutedresults <- data.frame( Biofilm = numeric(), Water= numeric(), Ratio = numeric(), stringsAsFactors = FALSE)
  
  
  
  #Create a function to calculate permuted ratios
  calculate_permuted_ratios <- function(compdata, n_permutations = P, sample_size = n) {
    permuted_ratios <- as.numeric(n_permutations)
    
    
    for (i in 1:n_permutations) {
      
      
      # Randomly select a row index
      #set.seed(357)  # For reproducibility
      random_row <- sample(nrow(compdata), sample_size,replace = TRUE)
      
      # Extract var a and var b from the selected row
      variableBio <- (compdata[random_row, "Biofilm"])
      variableWater <- (compdata[random_row, "Water"])
      
      #print(sample_size)
      #print("random")
      #print(random_row)
      #print("bio")
      #print(variableBio)
      #print("water")
      #print(variableWater)
      # Calculate sums
      sum_A <- sum(variableBio)
      sum_B <- sum(variableWater)
      
      # Calculate ratio
      if (sum_B != 0) {
        permuted_ratios[i] <- sum_A / sum_B
      } else {
        permuted_ratios[i] <- NA  # Avoid division by zero
      }
      ratiodata <- data.frame(
        Biofilm = sum_A,
        Water = sum_B,  # Random data
        Ratio= sum_A/sum_B
      )
      # Sheet name
      permutedresults <- rbind(permutedresults,ratiodata)
      
    }
    sheet_name <-paste0(sheet)
    # Add data to a new sheet
    addWorksheet(workbook, sheetName = sheet_name)
    writeData(workbook, sheet = sheet_name, permutedresults)
    #View(sum_A)
    #View(sum_B)
    #print(variableBio)
    #print(variableWater)
    
    # Return the resulting ratios, excluding any NA values
    na.omit(permuted_ratios)
   # write.csv(permuted_ratios, "permutedratio.csv" )
 
  }
  
  #Generate 1000 permuted ratios
  #set.seed(123)  # Set seed for reproducibility
  
  permuted_ratios <- calculate_permuted_ratios(compdata, n_permutations = P, sample_size = n)
  
  outputfile <- file.path(folder= "/home/pratima/Insync/pgautam1@umbc.edu/Google Drive/Cusick Lab/Metagenomic_analysis/Megan7_analysis/statistics/permutation_22group_seed/results/", file=paste0(sheet,".xlsx"))
  saveWorkbook(workbook, outputfile, overwrite = TRUE)
  write.csv(permuted_ratios, "permuted_ratios.csv" )
  #View(permuted_ratios)

  # initialize a matrix to store the permutation data
  PermSamples <- matrix(0, nrow=n, ncol=P)
  # each column is a permutation sample of data
  
  # now, get those permutation samples, using a loop
  # let's take a moment to discuss what that code is doing...
  for(i in 1:P){
    PermSamples[,i] <- sample(permuted_ratios, size= n, replace=FALSE)
  }
  
  # we can take a quick look at the first 5 columns of PermSamples
  PermSamples[, 1:2]
  PermSamplesm <- colMeans(PermSamples)
  
  # initialize vectors to store all of the Test-stats:
  Perm.test.stat <- rep(0, P)
  
  # loop thru, and calculate the test-stats
  for (i in 1:P){
    # calculate the perm-test-stat1 and save it
    Perm.test.stat[i] <- abs(permuted_ratios[i])
  }
  #View(Perm.test.stat)
  #open(permutedratio.csv, "w")
  
  write.csv(Perm.test.stat, "permutedratio.csv" )
  # before going too far with this, let's remind ourselves of 

  
  
  # and, take a look at the first 15 permutation-TEST STATS for 1 and 2
  #round(Perm.test.stat[1:15], 1)
  #round(Perm.test.stat2[1:15], 1)
  
  # and, let's calculate the permutation p-value...
  # notice how we can ask R a true/false question...(for the first 15)
  (Perm.test.stat >= test.stat)[1:15]
  # and if we ask for the mean of all of those, it treats 0=FALSE, 1=TRUE
  #mean((Perm.test.stat >= test.stat)[1:15])
  
  #...calculate the p-value, for all P=100,000
  Pvalue<-mean(Perm.test.stat >= test.stat)
  print(Pvalue)
  
  
  
  # Write information to the output file
  pvalue_data <- data.frame(
    #Category= sheet_data[1],
    SeedCategory = sheet,
    Seed = sheet_data[[1]][nrow(data)] ,
    #Biofilm= Biofilm,
    #Water= Water,
    Ratio = test.stat,
    PValue=  Pvalue
  
  )
  results <- rbind(results,pvalue_data)
  #define data
  data <- data.frame(x=Perm.test.stat)#(x=rnorm(1000))
  
  
  #View(data)
  #create histogram and overlay normal curve
  his<- ggplot(data, aes(x)) +
    geom_histogram(aes(y =..density..), fill='lightgray', col='black') +
    stat_function(fun = dnorm, args = list(mean=mean(data$x), sd=sd(data$x)))
  
  # Define the folder path and filename
  folder <- "/home/pratima/Insync/pgautam1@umbc.edu/Google Drive/Cusick Lab/Metagenomic_analysis/Megan7_analysis/statistics/permutation_22group_seed/figures/"
  filename <- paste0(sheet,".pdf")
  
  # Combine the folder and filename
  output_path <- file.path(folder, filename)
  
  # Save the plot
  ggsave(filename = output_path, plot = his, width = 8, height = 6) #saves  figure
}

#define data
#data <- data.frame(x=Perm.test.stat)#(x=rnorm(1000))
#View(data)
#create histogram and overlay normal curve

# Directory to save the output file
output_file <- "seedgroup_result_adjusted.xlsx"

# Open a connection to the output file
file_connection <- file(output_file, "w")
#Write the results to an Excel file

p_values <-results$PValue
adjusted_p_values <- p.adjust(p_values, method = "BH")
results$adjustedp_value <- adjusted_p_values

write_xlsx(results, output_file)
# Close the file connection
close(file_connection)

cat("Summary saved to:", output_file, "\n")