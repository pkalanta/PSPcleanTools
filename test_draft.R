
Require::Require("devtools")
devtools::install_github("pkalanta/PSPcleanTools")
library(PSPcleanTools)
# Load your PSP data
psp_data <- read.csv("your_PSP_file.csv")

# 1. Check DBH issues
clean_dbh <- process_dbh_issues(psp_data)

# 2. Fix tree number linked to multiple PSPs
corrected_tree <- treenum_to_multiplePSP(clean_dbh)

# 3. Classify tree status
classified_data <- classify_tree_status(corrected_tree)

# Save the final cleaned dataset
#write.csv(classified_data, "PSP_cleaned_output.csv", row.names = FALSE)


class(classified_data)

Browse[1]> ls(classified_data)

classified_data$Alive       # Shows the data for Alive trees
classified_data$Mortality   # Shows the data for dead trees
classified_data$Regen       # Shows the data for regenerating trees
classified_data$Trees

classified_data[["Alive"]]

head(classified_data$Alive)
head(classified_data$Mortality)
head(classified_data$Regen)

classified_data$Alive
# or
View(classified_data$Alive)

names(classified_data)       # Shows all parts
str(classified_data$Regen)   # Structure of regenerated trees
head(classified_data$Mortality)  # First few rows of dead trees
