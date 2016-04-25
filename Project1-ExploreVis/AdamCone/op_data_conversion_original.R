# set working directory first!
setwd("/Users/adamcone/Desktop/Visualization Project/data/original data")
library(dplyr)
library(chron)

# get sales data and convert to tbl
sales_tbl = tbl_df(read.csv("sales.txt",
                            header = FALSE,
                            sep = "",
                            stringsAsFactors = FALSE))
# name columns (roughly...)
names(sales_tbl) = c("Sale_ID",
                     "Clerk_ID",
                     "Customer_ID",
                     "Payment_Code",
                     "Is_Void",
                     "Sale_Total",
                     "Date_Started",
                     "Time_Started",
                     "Date_Ended",
                     "Time_Ended",
                     "CC_Trans",
                     "CC_Last4",
                     "CC_Name",
                     "CC_PNref",
                     "CC_Auth",
                     "CC_Brand",
                     "extra_column"
)

# next, I'll get the data for the sales items
# get sale_items data and convert to tbl
sale_items_tbl = tbl_df(read.csv("sale_items.txt", header = FALSE, sep = ""))
# name columns
names(sale_items_tbl) = c("ID",
                          "Sale_ID",
                          "Item_ID",
                          "Quantity",
                          "Unit_Cost",
                          "Cost",
                          "Tax",
                          "Item_Total",
                          "Is_Refund"
                          )

# upload another table to get the item names:
item_names_tbl = tbl_df(read.csv("items.txt",
                                 sep = "\t",
                                 header = FALSE,
                                 stringsAsFactors = FALSE))
# name columns
names(item_names_tbl) = c("Item_ID", #int(11)
                          "Item_Name", #varchar(255)
                          "Price_ID", #int(11)
                          "Tax_Category_ID", #int(11)
                          "PLU", #varchar(5)
                          "Size", #decimal(8,4)
                          "Size_Unit_ID", #int(11)
                          "Count", #int(11)
                          "Count_Timestamp", #datetime
                          "Last_Manual_Count", #int(11)
                          "Last_Manual_Count_Timestamp", #datetime
                          "Is_Discontinued", #tinyint(1)
                          "Notes" #text
                          )

# upload another table to get the mapping Category_ID to Categories
# get category information
categories_tbl = tbl_df(read.csv("categories.txt",
                                 sep = "\t",
                                 header = FALSE,
                                 stringsAsFactors = FALSE))
# name columns
names(categories_tbl) = c("Category_ID",
                          "Category_Name")

# upload another table to get mapping of Items to CategoryID
category_items_tbl = tbl_df(read.csv("category_items.txt",
                                     sep = "\t",
                                     header = FALSE,
                                     stringsAsFactors = FALSE))
#name columns
names(category_items_tbl) = c("ID",
                              "Item_ID",
                              "Category_ID")