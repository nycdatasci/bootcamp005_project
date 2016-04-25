# set working directory first!
setwd("/Users/adamcone/Desktop/Visualization Project/data/filtered data (processed)")
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

# keep only non-empty rows: Payment_Code = 0 is a voided transaction, empty
# fields and VISA indicate conversion artifacts
sales_tbl = filter(sales_tbl,
                   !Payment_Code %in% c("0", "", "VISA")
                   )
# get rid of Is_Void == 1, which represent voided transactions
sales_tbl = filter(sales_tbl,
                   Is_Void == 0)

# get rid of the single transaction with Sale_Totals >= $2,000. Correspond to
# Steven Lucy accounting transactions independent of standard commerce.
sales_tbl = filter(sales_tbl, Sale_Total < 2e4)

# get rid of all rows with Sale_Total < $0. There are 477 of them after I
# applied the other filters, and they total -2,204.66. According to Andy, these
# are abberant transactions and should be disregarded for my analysis. In fact,
# Andy recommended that I let Steven know about these transactions, as they may
# indicate theft. I'll keep that in mind.
sales_tbl = filter(sales_tbl, Sale_Total >= 0)

# get rid of all rows with Sale_Total < 0.02 based on my conversations with
# Andy. It seems that these transactions are mostly to re-run credit or debit
# cards, since the companies may not allow the same card to be run twice for
# the same transaction.
sales_tbl = filter(sales_tbl, Sale_Total > 0.01)

# keep only relevant columns
sales_tbl = select(sales_tbl,
                   Sale_ID,
                   Payment_Code,
                   Sale_Total,
                   Date_Started,
                   Time_Started
                   )


# convert column formats for data hygiene
# Sale_ID from factor to character
sales_tbl = mutate(sales_tbl, Sale_ID = as.character(Sale_ID))
# Payment_Code should be Payment_Type according to this table:
Payment_Type_tbl = tbl_df(data.frame(Payment_Code = as.character(1:5),
                                     Payment_Type = as.factor(c("Cash",
                                                                "Check",
                                                                "Credit/Debit",
                                                                "Tab",
                                                                "Food Stamps"
                                                                )
                                                              ),
                                     stringsAsFactors = FALSE
                                     )
                          )
# to join these tables by Payment_Code, convert Payment_Code in sales_tbl go
# from factor to character:
sales_tbl = mutate(sales_tbl, Payment_Code = as.character(Payment_Code))
# Next, left-join these tables
sales_tbl = left_join(sales_tbl, Payment_Type_tbl, by = "Payment_Code")
# get rid of Payment_Type_tbl
rm(Payment_Type_tbl)
# delete Payment_Code
sales_tbl = select(sales_tbl, -Payment_Code)

# next, I'll add a column with the combined date and time using chron
sales_tbl = mutate(sales_tbl,
                   DateTime = chron(Date_Started,
                                    Time_Started,
                                    format = c(dates = "y-m-d", times = "h:m:s"),
                                    out.format = c(dates = "year-m-d", times = "h:m:s")
                   )
)
# now, get rid of Date_Started and Time_Started columns
sales_tbl = select(sales_tbl, -Date_Started, -Time_Started)

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
# keep only relevant columns
sale_items_tbl = select(sale_items_tbl,
                        Sale_ID,
                        Item_ID,
                        Quantity,
                        Unit_Cost,
                        Item_Total)
# get rid of Sale_ID == 0 (corresponds to no documented sales)
sale_items_tbl = filter(sale_items_tbl, Sale_ID != 0)
# convert Sale_ID and Item_ID from integer to character for data hygiene
sale_items_tbl = mutate(sale_items_tbl,
                        Sale_ID = as.character(Sale_ID),
                        Item_ID = as.character(Item_ID)
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
# keep only relevant columns
item_names_tbl = select(item_names_tbl,
                        Item_ID,
                        Item_Name)
# filter irrelevant rows
item_names_tbl = filter(item_names_tbl,
                        Item_Name != "")

# create a new table items_tbl that joins sale_items_tbl and item_names_tbl:
items_tbl = left_join(sale_items_tbl, item_names_tbl, by = "Item_ID")
# now, it appears that Item_ID = 807 is "tab payment". While that's
# interesting, it's not relevent to this data set, so I'll purge it now:
items_tbl = filter(items_tbl, Item_ID != "807")
rm(sale_items_tbl, item_names_tbl)

# Next, left_join sales_tbl and items_tbl for total sales data table
items_tbl = left_join(sales_tbl, items_tbl, by = "Sale_ID")
items_tbl = select(items_tbl,
                   DateTime,
                   Sale_ID,
                   Sale_Total,
                   Payment_Type,
                   Item_Name,
                   Quantity,
                   Unit_Cost,
                   Item_Total)

# get rid of Item_Name = "bulk order". There are 947 of these sales. Open
# Produce has seperate business arrangements with nearby co-ops, which is an
# artifact of being in the UC catchment. These sales are not broken down by
# item, follow a different pricing scheme than standard retail sales, and
# constitute a functionally sepearate business for Open Produce.
items_tbl = filter(items_tbl, Item_Name != "bulk order")

# finally, I want to build a data frame for just the independent sales:
sales_tbl = select(items_tbl,
                   DateTime,
                   Sale_ID,
                   Sale_Total,
                   Payment_Type
                   ) %>%
            distinct(.)