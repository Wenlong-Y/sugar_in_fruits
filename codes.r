library(rvest)
library(tidyverse)
url <- "https://thepaleodiet.com/fruits-and-sugars/amp/"
htmlsource <- read_html(url)
nodes <- html_nodes(htmlsource, "table")
fruit_sugar <- html_table(nodes[[1]])
names(fruit_sugar)[1] <- "Names"
fruit_sugar[is.na(fruit_sugar)]=0
write.csv(fruit_sugar,"Fruit_sugar_raw.csv")
row_freshfruit <- which(str_detect(fruit_sugar$Names,"FRESH FRUIT"))
row_dryfruit <- which(str_detect(fruit_sugar$Names,"DRIED FRUIT"))
row_pursugars <- which(str_detect(fruit_sugar$Names,"PURE SUGARS"))
row_candies <- which(str_detect(fruit_sugar$Names,"CANDY"))
fruit_sugar <- fruit_sugar %>% mutate(category="")
fruit_sugar$category[(row_freshfruit+1):(row_dryfruit-1)] <- "Fresh Fruit"
fruit_sugar$category[(row_dryfruit+1):(row_pursugars-1)] <- "Dried Fruit"
fruit_sugar$category[(row_pursugars+1):(row_candies-1)] <- "Pure Sugar"
fruit_sugar %>% filter(category=="Candies" & `TOT. MET. FRUCTOSE`==0) %>% mutate(`TOT. MET. FRUCTOSE`= `Total Sugars`/2)
temp <- fruit_sugar%>% filter(category=="Candies") %>% mutate(`TOT. MET. FRUCTOSE`= if_else(`TOT. MET. FRUCTOSE`>`Total Sugars`/2, `TOT. MET. FRUCTOSE`,`Total Sugars`/2))
fruit_sugar[(row_candies+1):nrow(fruit_sugar),] <-temp



final_table <- fruit_sugar %>% mutate(Sugar_Percentage = `Total Sugars`/100, Fructose_Percentage = `TOT. MET. FRUCTOSE`/100, Fructose_per_Sugar=`TOT. MET. FRUCTOSE`/`Total Sugars`)
final_table <- na.omit(final_table)
write.csv(final_table,"fruit_candy_sugar.csv")
report_table <- final_table %>% select(Names,Sugar_Percentage,Fructose_per_Sugar,Fructose_Percentage,category)
write.csv(report_table,"report.csv")
report_table %>% ggplot(aes(Fructose_Percentage,reorder(Names,Fructose_Percentage),color=category)) + geom_point(size=1)+theme(axis.text.y=element_text(size=5), axis.title.y = element_blank())