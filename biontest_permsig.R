setwd("../binomial/")

library(ggplot2)
library(readxl)
library(openxlsx)
library(writexl)

excel_path <-("files2.xlsx")
sheet_names <- excel_sheets(excel_path)

results <- data.frame(Category = character(),Sample1 = numeric(), Sample2= numeric(), PValue= numeric(), stringsAsFactors = FALSE)


# Directory to save the output file
output_file <- "pvalue_permsig_result_greater_adjusted.xlsx"
workbook <- createWorkbook() 

for (sheet in sheet_names) {
  data  <- read.xlsx(excel_path, sheet = sheet, colNames = TRUE)
  #View(data)
  rows = nrow(data)
  
  # Step 3: Initialize a vector to store p-values
  p_values <- numeric(nrow(data))
  
  for (i in 1:rows) {
    # Extract the current row as a data frame or vector
    Category <- data[i,1]
    Sample1 <- data[i,4]
    Sample2 <- data[i,5]
   
    total <- Sample1 + Sample2
    # Perform binomial test if total > 0
    if (total > 0) {
      p_values[i] <- binom.test(as.integer(Sample1), as.integer(total), p = 0.5, alternative = "greater")$p.value
      adjusted_p_values <- p.adjust(p_values, method = "BH")
    } else {
      p_values[i] <- NA  # Assign NA if total is zero (no observations)
    }
  }
  # Step 5: Add p-values to the dataset
  data$p_value <- p_values
  data$adjustedp_value <- adjusted_p_values

sheet_name <-paste0(sheet)# Add data to a new sheet
addWorksheet(workbook, sheetName = sheet_name)
writeData(workbook, sheet = sheet_name, data)
}

saveWorkbook(workbook, output_file, overwrite = TRUE)
cat("Summary saved to:", output_file, "\n")