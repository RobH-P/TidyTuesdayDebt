# TidyTuesdayDebt
Creates a very basic couple of plots to illustrate the total amount of debt owed to loan companies.

# read in the loans data
loans_raw <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv"
loans <- read.csv(loans_raw)


# display name of the loans data 
names(loans) 
str(loans)

# loans by year

loans %>% 
  count(year)
loans

# merge year and quarter

loans <-  loans %>% 
  mutate(new_date = paste0(year,"/",quarter )) 
str(loans)
 loans <- loans[-c(2,3)]
 
# clean the company names
loans$agency_name <- gsub("[*]","", loans$agency_name, ignore.case = T)
loans$agency_name <- gsub(", Inc.","", loans$agency_name, ignore.case = T)
loans$agency_name <- gsub("FMS","FMS Investment Corp", loans$agency_name, ignore.case = T)
loans$agency_name <- gsub("FMS Investment Corp Investment Corp","FMS Investment Corp", loans$agency_name, ignore.case = T)
loans$agency_name <- gsub(" LP","", loans$agency_name, ignore.case = T)
loans$agency_name <- gsub(" Inc","", loans$agency_name, ignore.case = T)
loans$agency_name <- gsub(", LLC","", loans$agency_name, ignore.case = T)
loans$agency_name <- gsub("Pioneer Credit Recovery,","Pioneer", loans$agency_name, ignore.case = T)
loans$agency_name <- gsub("Windham Professionals","Windham", loans$agency_name, ignore.case = T)
loans$new_date <- gsub(" ", "-", loans$new_date, ignore.case = T) 
 
 theme_set(theme_bw())

 agencies <- loans %>% 
   count(agency_name)
 print(agencies) 
 
 ### group by company
 
 agency_total <- loans %>% 
   group_by(agency_name) %>%
   summarise(total=mean(total,na.rm=T)) 
   
   
### bar chart of all firms

ggplot(agency_total) +
 aes(x=reorder(agency_name,total), fill = agency_name, size = -total, weight = total) +
 geom_bar() +
  scale_y_continuous(labels = dollar_format()) +
 labs(x = "finance company", y = "amount owed", title = "Student debt by company", caption = "HT/Dignity and Debt") +
 theme(legend.position = "none", axis.title=element_text(face="bold",size="10",color="black"),axis.text=element_text(size=10,face="bold")) +
  coord_flip()
  
  ### filtered to show top ten
 agency_total <- agency_total %>%
 filter(total >= 19200000L & total <= 26400000L)
ggplot(agency_total) +
 aes(x=reorder(agency_name,total), fill = agency_name, size = -total, weight = total) +
 geom_bar() +
  scale_y_continuous(labels = dollar_format()) +
 labs(x = "finance company", y = "amount owed", title = "Student debt by company", caption = "HT/Dignity and Debt") +
 theme(legend.position = "none", axis.title=element_text(face="bold",size="10",color="black"),axis.text=element_text(size=10,face="bold")) +
  coord_flip()
  
 ### different colour scheme 

ggplot(agency_total, aes(x=reorder(agency_name, total), y=total, fill= ..y..)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Student debt by company", x = "Firm", y = "amount owed $", caption = "HT/Dignity and Debt", fill = "total") +
  theme(axis.title=element_text(face="bold",size=
