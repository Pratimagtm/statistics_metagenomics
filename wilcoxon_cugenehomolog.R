setwd("/home/pratima/Insync/pgautam1@umbc.edu/Google Drive/Cusick Lab/Metagenomic_analysis/Megan7_analysis/statistics/wilcoxon/")

library(ggplot2)
library(readxl)
library(openxlsx)
library(writexl)

excel_path <-("cugenehomologcount10E-30.xlsx")
sheet_names <- excel_sheets(excel_path)


# Directory to save the output file
output_file <- "wilcoxon_cugene_result_greater.xlsx"
workbook <- createWorkbook() 

for (sheet in sheet_names) {
  data  <- read.xlsx(excel_path, sheet = sheet, colNames = TRUE)
  # Perform Wilcoxon test (adjust 'paired' based on your study design)
  result <- wilcox.test(data$Biofilm, data$Water, alternative = "greater")

  # Prepare the result as a data frame for saving
  result_df <- data.frame(
    Statistic = result$statistic,
    P_Value = result$p.value,
    Method = result$method,
    Alternative_Hypothesis = result$alternative
  )
sheet_name <-paste0(sheet)# Add data to a new sheet
addWorksheet(workbook, sheetName = sheet_name)
writeData(workbook, sheet = sheet_name, result_df)
}

saveWorkbook(workbook, output_file, overwrite = TRUE)
cat("Summary saved to:", output_file, "\n")