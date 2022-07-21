library(readxl)
library(tidyverse)

merged <- read.csv("merged.csv")

merged <- merged[rowSums(is.na(merged)) >= 10, ]       # Apply rowSums & is.na number >10
merged                                            # Printing updated data