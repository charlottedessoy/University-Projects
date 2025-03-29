
#packages: 
library(dplyr) 
library(lubridate) 
library(ggplot2) 
library(patchwork) 
library(tidyr) 
library(tibble) 
#reading in the data 
Cases <- read.csv("Cases.csv") 
Fires<- read.csv("Fires.csv") 
HousingReg <- read.csv("Housing Register.csv") 
Maltese <- read.csv("Maltese.csv") 
Pets <- read.csv("Pets.csv") 


#question1  
#a) How has the number of dogs and cats changed over time? 
#How has the popularity of the different dog breeds evolved since 1990? 

glimpse(Pets) 

Pets$Date <- paste(Pets$Year, Pets$Month, sep = "-")# date column with year and month together 
Pets$Date <- as_date(Pets$Date , format = "%Y-%m") # changing from a string to a date.  

Pets$TotalDogs <- Pets$Beagles+ Pets$Dachshund + Pets$Maltese 

Pets%>% 
  select("Date","TotalDogs", "Cats" )%>% 
  ggplot(aes(x = Date)) + geom_line( aes(y =TotalDogs, colour = "Dogs"))+ 
  geom_line(aes(y =Cats, colour = "Cats")) + labs(title = "Change in number of Cats and Dogs over Time",  
                                                  y = "Number")+  
  scale_colour_manual("", breaks = c("Dogs", "Cats"), values = c("red", "blue")) 

#Analysis: We see that the population of dogs has been increasing at a high rate. While numbers of cats increased until around 2004 where it peaked, and we've been seeing a downwards trend since, actaully steeping below where the population was the data was first collected.  

# Now looking at the popularity of dog breeds: 
Pets%>% 
  select("Date", "Beagles", "Dachshund", "Maltese")%>% 
  ggplot(aes(x = Date)) + geom_line( aes(y= Beagles, colour = "Beagles")) +  
  geom_line( aes(y = Dachshund, colour = "Dachshund")) + 
  geom_line( aes(y = Maltese, colour = "Maltese")) +  
  labs(title = "Popularity of Dog Breeds", y = "Number")+  
  scale_colour_manual("", breaks = c("Beagles", "Dachshund", "Maltese"), values = c("orange", "brown", "grey")) 

#Analysis: We see that Dachshunds are, by quite a large margin, the most popular dog breed. Maltese had a bit of a surge in popularity in the 2000s, but never quite took over Beagles, who are the second most popular.  

#b) Maltese are known to experience respiratory issues, such as wheezing or asthma. 
#How do environmental and physiological factors affect the risk of a Maltese experiencing these issues? 

#First lets look at Cases, this is the environmental impact on a Maltese's issues 

glimpse(Cases) 
#the Date column in Cases is not in the right format.  
Cases$Date <- ymd(Cases$Date)  

Cases%>% 
  ggplot(aes(x =Date, y = Temperature )) + geom_line()



#We can also look at Maltese which shows us the physiological impact on a Maltese's health. 

#we can calculate bmi, being careful to use the imperial formula:

Maltese <- mutate(Maltese, BMI = ((Weight)/(Height)))

Maltese%>%
  filter(RespiratoryIssues==1)%>%
  ggplot(aes(x = BMI)) + geom_boxplot()


BMI_Good <- Maltese%>%
  filter(0.80<BMI, BMI<0.90)%>%
  distinct(RespiratoryIssues)%>%
  summarise(BMI)

BMI_Good
BMI_Low<- Maltese%>%
  filter(BMI>0.80)
BMI_High<- Maltese%>%
  filter(BMI>0.90)
#Question 2a
#How does the frequency of the different causes for fires vary over time? 


Fires<- read.csv("Fires.csv") 

glimpse(Fires) # date isnt the right type
Fires$Date <- as_datetime(Fires$Date, format = "%Y-%m-%d %H:%M") # changing from a string to a date.  

Fires%>% 
  group_by(MonthYear =floor_date(Date, 'month'))%>%
  select("MonthYear","Cause" )%>% 
  count(Cause)%>%
  ggplot(aes(x = MonthYear, y = n , colour = factor(Cause))) + facet_wrap(~Cause)+geom_line() + 
    labs( title = "How the frequency of the different cases of fires change over time", 
          y = "Number of Fires", x = "Month", colour ="Causes") + theme(axis.text.x = element_text(angle = 45))



#How many casualties were attributed to each cause and 
#are there differences in the frequency with which casualties occur across causes?


Fires%>%
  group_by(Cause)%>%
  summarise(Casualties = n())


Fires%>% 
  select("Casualties" , "Cause")%>% 
  filter(Casualties|0)%>%
  ggplot(aes( y= Casualties, fill = Cause))  + geom_boxplot()

#Ok so when we excluded fires where there are no casulties, we see that fires categorised as "Other" have a higher average mortatilty rate. We also see this with Heating fires. 



#Question 2 b

#Are there any differences in the risk of fire for the different types of property? 

#Joining the data sets:
Fires <- rename(Fires, "ID" = RegisterNumber)
Fires_joined <- inner_join(Fires, HousingReg, by = "ID")

Fires_joined$SafeScore = Fires_joined$Smoke + Fires_joined$CO

Fires_joined%>%
  select("Date", "SafeScore", "Type")%>%
  group_by(Type)%>%
  summarise(AvgSafeScore = mean(SafeScore))%>%
  arrange(AvgSafeScore)

#I've defined a measure of risk of fire as each properties possesion of smoke alarms and carbon monoixide alarms, as without these a fire could go undetected by home-owners and hence become a problem where the fire brigade has to be called. So the higher the "Average Safety Score" the less risk of a fire. Here we see that Flats have the lowest risk of fire, while Detached houses have the highest risk of fire.
#Here I displaythe differences in graphical format:

Fires_joined%>%
  group_by(Type, SafeScore)%>%
  summarise(Number = n())%>%
  ggplot(aes(fill = as.character(SafeScore), x = Type, y = Number)) + 
    geom_bar(position = "dodge", stat = "identity") + labs(title = "The risks of fires across different types of properties", fill = "Safe Score")

#This graph empahsises the really low risk that flats have, while the bars for Detached and Terraced can be quite decieving. It appears that Terraed has a larger amount of properties with a low "Safe Score" but thats just because there's more Terraced houses. Therefore its better to look at the averages.


#What is the relation between the year a property was built and the risk of fire?

Fires_joined%>%
  select("Year", "SafeScore")%>%
  group_by(Year)%>%
  summarise(AvgSafeScore = mean(SafeScore))%>%
  ggplot(aes(x = Year, y  = AvgSafeScore)) + geom_point(size = 0.75) + geom_smooth() + 
    labs( title = "Change in safety of properties over time", y = "Average Safety Score")

#We see that over time, safety has increased, so risk of fire has decreased. This is probably due greater public awareness of fore safety. 

#Question 2 c:

#The Fire Department wishes to run a campaign which encourages home owners to install smoke and carbon monoxide detectors. 
#What does the data reveal regarding the benefits of installing smoke and carbon monoxide detectors?
  

#Again we'll use our safescore variable. But compare it to damages:

Fires_joined%>%
  select("SafeScore", "Damage")%>%
  group_by(SafeScore)%>%
  summarise(AvgDam = mean(Damage))


