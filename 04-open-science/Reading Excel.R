
# Remove all data
objects <- ls() 
rm(list =  objects[ !objects == 'Results'] )


#  Reading the Excel for the Parameters  ----
# Identify the onglets
onglets <- excel_sheets("data/Attrakdiff.xlsx")

# Reading all table
Parameters_AttrakDiff <- 
  read_excel(path = "data/Attrakdiff.xlsx", sheet = "Parameters" )

# Reading all table but considering as title the second row
data <- read_excel(path = "data/Data.xlsx" , skip = 1)

# Obtaining the names of the columns
names(data)





