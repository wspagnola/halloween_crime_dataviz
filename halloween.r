'
Bill Spagnola

NYU Statistics Club: Halloween Visualization Contest

'

# LF will beplaced by CRLF in data/*.csv ???


#setwd("Desktop/halloween")

source('source.R')


#### Load and Clean Data ####


drunk <- read.csv("data/drunk.csv")
drunk <- drunk %>% mutate_cols %>% clean_data %>% add_halloween()

murder <-  read.csv("data/murder.csv")
murder <- murder %>% mutate_cols %>% clean_data %>% add_halloween()

assault <-  read.csv("data/assault.csv")
assault <- assault %>% mutate_cols %>% clean_data %>% add_halloween()


kidnapping <-  read.csv("data/kidnapping.csv")
kidnapping <- kidnapping %>% mutate_cols %>% clean_data %>% add_halloween()

#### Plots ####

#Drunk Driving
drunk_sum <- halloween_compare(drunk) #Compute Summary Stats
drunk_sum_long <- convert_long(drunk_sum) #Convert to Long Format

#Plot
drunk_sum_long %>% 
  ggplot(aes(x=Year, y=Number_of_Incidents, color=Halloween))+
  geom_point()+
  geom_line() + 
  ggtitle("Drunk Driving") +
  ylab("Number of Incidents")+
  scale_color_manual(values=c( "black", "orange")) +
  scale_x_continuous(breaks = 2006:2017)


#Murder
murder_sum <- halloween_compare(murder) #Compute Summary Stats
murder_sum_long <- convert_long(murder_sum) #Convert to Long Format

murder_sum
drunk_sum
#Plot
murder_sum_long %>% 
  ggplot(aes(x=Year, y=Number_of_Incidents, color=Halloween))+
  geom_point()+
  geom_line() + 
  ggtitle("Murders") +
  ylab("Number of Incidents")+
  scale_color_manual(values=c( "black", "orange")) +
  scale_x_continuous(breaks = 2006:2017)+
  scale_y_continuous(breaks = 1:9)

  


#assault
assault_sum <- halloween_compare(assault) #Compute Summary Stats
assault_sum_long <- convert_long(assault_sum) #Convert to Long Format


#Plot
assault_sum_long %>% 
  ggplot(aes(x=Year, y=Number_of_Incidents, color=Halloween))+
  geom_point()+
  geom_line() + 
  ggtitle("Felony Assaults") +
  ylab("Number of Incidents")+
  scale_color_manual(values=c( "black", "orange")) +
  scale_x_continuous(breaks = 2006:2017)+
  scale_y_continuous()

#kidnapping
kidnapping_sum <- halloween_compare(kidnapping) #Compute Summary Stats
kidnapping_sum_long <- convert_long(kidnapping_sum) #Convert to Long Format


#Plot
kidnapping_sum_long %>% 
  ggplot(aes(x=Year, y=Number_of_Incidents, color=Halloween))+
  geom_point()+
  geom_line() + 
  ggtitle("Kidnappings") +
  ylab("Number of Incidents")+
  scale_color_manual(values=c( "black", "orange")) +
  scale_x_continuous(breaks = 2006:2017)+
  scale_y_continuous()






                            
                        