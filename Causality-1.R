library(tidyverse)
library(lubridate)
library(ISLR)
library(dagitty)
library(lavaan)
library(ggridges)
library(cowplot)
library(ggExtra)

setwd("C:/Users/ellen/Documents/UH/Fall 2020/Rethinking")

Advertising = read_csv("Advertising.csv")
mAd  = data.matrix(Advertising[,2:5])
corAd = cor(mAd)
covAd = cov(mAd)

# Beginning with previous regression exercises

g1 = dagitty('dag {

  TV [pos = "1,1"]
  Radio [pos = "2,1"]
  Newspaper [pos = "2,2"]
  Sales [pos = "1,2"]

TV -> Sales 
Radio -> Sales
Newspaper -> Sales

             }')
plot(g1)

# based on correlation, condition on Radio

Advertising = Advertising %>% mutate(
  RGrp = case_when(
    Radio == 4.1 ~ "G1", 
    Radio == 5.7 ~ "G2", 
    TRUE ~ "G0"
        )) 

plot_center = ggplot(Advertising, aes(x=Radio,y=Sales, colour = RGrp)) + 
  geom_point(alpha = .4) +
  geom_smooth(method="lm", se = F, color = "gray") +
  theme(panel.background = element_rect(fill = "white")) +  
  xlim(0, 50) + ylim(0, 30) + 
  ylab("Sales") + xlab("Radio") + 
  theme(legend.position="none")
p3 = ggMarginal(plot_center, type="density", groupColour = FALSE, groupFill = TRUE)
p3


# P(Sales | Radio = 4.1, 5.7 )

p4 = ggplot(Advertising, aes(x = Sales, fill = RGrp)) + 
  geom_density(bw = 5, alpha = .2) +
  xlim(-10, 40)  +
  theme(panel.background = element_rect(fill = "white")) 
p4

# NHST 
z <- (mean(Advertising$Sales) - mean(filter(Advertising, RGrp == "G2")$Sales))/((sd(filter(Advertising, RGrp == "G2")$Sales))/sqrt(nrow(filter(Advertising, RGrp == "G2"))))
z
p <- 2*pnorm(-abs(z))
p

# so reject the null (remember, null is no difference in populations, 
# so there is an effect, or Radio Advertising causes Sales)
# Assume TV works out the same


# Now, for Newspaper

Advertising %>% group_by(Newspaper) %>%
  summarize(Count=n()) %>%
  arrange(desc(Count))  

Advertising = Advertising %>% mutate(
  NGrp = case_when(
    Newspaper == 8.7 ~ "G1", 
    Newspaper == 9.3 ~ "G2", 
    TRUE ~ "G0"
  )) 

plot_center = ggplot(Advertising, aes(x=Newspaper,y=Sales, colour = NGrp)) + 
  geom_point(alpha = .4) +
  geom_smooth(method="lm", se = F, color = "gray") +
  theme(panel.background = element_rect(fill = "white")) +  
  xlim(0, 50) + ylim(0, 30) + 
  ylab("Sales") + xlab("Radio") + 
  theme(legend.position="none")
p3 = ggMarginal(plot_center, type="density", groupColour = FALSE, groupFill = TRUE)
p3

# you can see that the coefficient is small, which is a CLUE 
# (this really gets into dimension managemnet and generalization
# or Under-management and Over-generalization! We'll get into all this later, 
# but it's a BIG issue with senior management)

# P(Sales | Newspaper = 8.7, Newspaper = 9.3)

p4 = ggplot(Advertising, aes(x = Sales, fill = NGrp)) + 
  geom_density(bw = 5, alpha = .2) +
  xlim(-10, 40)  +
  theme(panel.background = element_rect(fill = "white")) 
p4

# NHST

z <- (mean(Advertising$Sales) - mean(filter(Advertising, NGrp == "G2")$Sales))/((sd(filter(Advertising, NGrp == "G2")$Sales))/sqrt(nrow(filter(Advertising, NGrp == "G2"))))
z
p <- 2*pnorm(-abs(z))
p

# so canNOT reject the null
# this means that the Newspaper -> Sales relationship is most likely independent
# and Newspaper does not cause Sales
# check correlations again

corAd

# there appears to be some despendency with Radio -> Newspaper 
# let's look at that later - let's update the graph
# to reflect our new understanding

g1 = dagitty('dag {

  TV [pos = "1,1"]
  Radio [pos = "2,1"]
  Newspaper [pos = "2,2"]
  Sales [pos = "1,2"]

TV -> Sales <- Radio -> Newspaper

             }')
plot(g1)

# Now let's dig into the structure, the collider first:

# ------------------ COLLIDER ----------------------- #

# COLLIDER TV -> Sales <- Radio (keep in mind, 
# that colliders are similar structure to the regression models we've been depending on for 2+ semesters)
# So, TV and Radio are independent
# Sales and TV are dependent
# Sales and Radio are dependent
# TV and Radio are dependent, conditional on Sales (this is a new perspective on correlation):
# Let's condition on Sales to check

Advertising %>% group_by(Sales) %>%
  summarize(Count=n()) %>%
  arrange(desc(Count))  

Advertising = Advertising %>% mutate(
  SGrp = case_when(
    Sales == 9.7 ~ "G1", 
    Sales == 11.7 ~ "G2", 
    TRUE ~ "G0"
  )) 

# First, Showing Independence in collider (this is a regression assumption, remember)

plot_center = ggplot(Advertising, aes(x=Radio,y=TV)) + 
  geom_point() +
  geom_smooth(method="lm", se = F) +
  theme(panel.background = element_rect(fill = "white")) +  
  xlim(0, 20) + ylim(0, 300) + 
  ylab("TV") + xlab("Radio")
p5 = ggMarginal(plot_center, type="density", fill = "cyan4", color = "white")
p5

# showing Dependence in collider (regression assumption is dependence, but this is a conditioned relationship)

plot_center = ggplot(filter(Advertising, SGrp %in% c("G1", "G2")), aes(x=Radio, y=TV, colour = SGrp)) + 
  geom_point() +
  geom_smooth(method="lm", se = F) +
  theme(panel.background = element_rect(fill = "white")) +  
  xlim(0, 20) + ylim(0, 300) + 
  ylab("TV") + xlab("Radio")
p6 = ggMarginal(plot_center, type="density", groupColour = FALSE, groupFill = TRUE)
p6


## ---------- now the Fork (opposite of collider)

# Radio and Sales are dependent
# Radio and Newspaper are dependent
# Sales and Newspaper are likely dependent
# Sales and Newspaper are independent conditional on Radio


# Showing Radio and Newspaper dependence

plot_center = ggplot(Advertising, aes(x=Newspaper,y=Radio)) + 
  geom_point() +
  geom_smooth(method="lm", se = F) +
  theme(panel.background = element_rect(fill = "white")) +  
  xlim(0, 50) + ylim(0, 50) + 
  ylab("Radio") + xlab("Newspaper")
p5 = ggMarginal(plot_center, type="density", fill = "cyan4", color = "white")
p5

plot_center = ggplot(Advertising, aes(x=Newspaper,y=Sales)) + 
  geom_point() +
  geom_smooth(method="lm", se = F) +
  theme(panel.background = element_rect(fill = "white")) +  
  xlim(0, 50) + ylim(0, 30) + 
  ylab("Sales") + xlab("Newspaper")
p5 = ggMarginal(plot_center, type="density", fill = "cyan4", color = "white")
p5

corAd

# so, reviewing, the overall correlation favors  the Radio -> Newspaper relatiionship
# This is a surrogate (Newspaper could sub for Radio - although not the best model)
# but it's still a fork, and:

# Sales and Newspaper are independent conditional on Radio

plot_center = ggplot(Advertising, aes(x=Newspaper,y=Sales, colour = RGrp)) + 
  geom_point(alpha = .4) +
  geom_smooth(method="lm", se = F, color = "gray") +
  theme(panel.background = element_rect(fill = "white")) +  
  xlim(0, 50) + ylim(0, 30) + 
  ylab("Sales") + xlab("Newspaper") + 
  theme(legend.position="none")
p6 = ggMarginal(plot_center, type="density", groupColour = FALSE, groupFill = TRUE)
p6



# NHST

z <- (mean(Advertising$Sales) - mean(filter(Advertising, RGrp == "G1")$Sales))/((sd(filter(Advertising, RGrp == "G1")$Sales))/sqrt(nrow(filter(Advertising, RGrp == "G1"))))
z
p <- 2*pnorm(-abs(z))
p

# So the null is rejected (barely, which is what we would expect - surrogate)


