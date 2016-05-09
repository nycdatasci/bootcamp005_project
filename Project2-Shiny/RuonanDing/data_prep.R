rawdata <- read.csv("~/Downloads/world-food-facts/FoodFacts.csv", header = T, stringsAsFactors = F)
dim(rawdata)
summary(rawdata)
ls(rawdata)

#fields that I focus on for this analysis because most of the other nutrition fields are not filled in 
library(dplyr)
FoodFacts <- select(rawdata, c(product_name, brands, main_category_en,countries_en,
                                nutrition_score_uk_100g, energy_100g, fat_100g, carbohydrates_100g,
                               proteins_100g, sodium_100g, salt_100g, fiber_100g ,sugars_100g))
FoodFacts <- filter(FoodFacts, nutrition_score_uk_100g != "NA")

#top focus countries data
Countries <- as.data.frame(table(FoodFacts$countries_en))[-1,]
TopCountries <- head(Countries[order(-Countries$Freq),],10)
countrylist <- c("France", "Germany", "Spain", "United Kingdom", "United States",
                 "Australia", "Belgium", "Switzerland", "Portugal", "Italy", "Austria")

#there are multiple coutries coded in the country field column. Here are the 
newtable <- data.frame()
for (i in (1:length(countrylist))){
  for (j in (1: length(FoodFacts$countries_en))){
    if (countrylist[i] %in% FoodFacts$countries_en[j]==TRUE){
      newrow <- cbind(countrylist[i], FoodFacts[j,c(2:3,5:13)])
      newtable <- rbind(newtable, newrow)
    }
    j = j+1
  }
  i = i+1
}
topcountry.food.facts <- newtable
colnames(topcountry.food.facts)[1] <- "country"

country.table<-group_by(topcountry.food.facts, country) %>%
  summarise("ave.score" = mean(nutrition_score_uk_100g),
            "ave.energy" = mean(energy_100g),
            "ave.fat" =  mean(fat_100g),
            "ave.carb" =  mean(carbohydrates_100g),
            "ave.protein" = mean(proteins_100g),
            "ave.sodium" = mean(sodium_100g),
            "ave.fiber" =  mean(fiber_100g),
            "ave.sugar" = mean(sugars_100g))

write.csv(country.table, "~/Desktop/shiny_project/country.table.csv")

#top 10 category only 
main_category <- as.data.frame(table(FoodFacts$main_category_en))
main_category <- main_category[-1,]
TopCategory <- as.data.frame(main_category[order(-main_category$Freq),][1:10, 1])
topcategory.food.facts <- filter(FoodFacts,main_category_en %in% TopCategory[,1])

category.table<-group_by(topcategory.food.facts, main_category_en) %>%
  summarise("ave.score" = mean(nutrition_score_uk_100g),
            "ave.energy" = mean(energy_100g),
            "ave.fat" =  mean(fat_100g),
            "ave.carb" =  mean(carbohydrates_100g),
            "ave.protein" = mean(proteins_100g),
            "ave.sodium" = mean(sodium_100g),
            "ave.fiber" =  mean(fiber_100g),
            "ave.sugar" = mean(sugars_100g))

write.csv(category.table, "~/Desktop/shiny_project/category.table.csv")

category <- as.character(category.table[,1])

