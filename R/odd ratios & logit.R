library(epiR)

#### OR of unemployment for post-9/11 Veterans

## 2017
vetdata %>% filter(Year == 2017 & ServicePeriod %in% c("Gulf War-era II","Gulf War-era I","Nonveterans")) %>%
  select(ServicePeriod,`Labor Force`,Unemployed, Employed) 

## umeployed veterans - imputation 
100100 - 96300
59700 -  58300


OR17 <- matrix(c(3800,1400, 419600, 96300, 59700,8825400), nrow = 3)
dimnames(OR17) <- list("Group" = c("P911 2017","GW 2017", "NonVeteran 2017"), "Unemployed" = c("Yes","No"))
OR17
tab17 <- epitab(OR17, method = "oddsratio", rev = "b")$tab


## 2016
vetdata %>% filter(Year == 2016 & ServicePeriod %in% c("Gulf War-era II","Gulf War-era I","Nonveterans")) %>%
  select(ServicePeriod,`Labor Force`,Unemployed, Employed) 

OR16 <- matrix(c(9100,5900, 423800, 83800,58900,8690200), nrow = 3)
dimnames(OR16) <- list("Group" = c("P911 2016","GW 2016", "NonVeteran 2016"), "Unemployed" = c("Yes","No"))
OR16
tab16 <- epitab(OR16, method = "oddsratio", rev = "b")$tab


## 2015 
vetdata %>% filter(Year == 2015 & ServicePeriod %in% c("Gulf War-era II","Gulf War-era I","Nonveterans")) %>%
  select(ServicePeriod,`Labor Force`,Unemployed, Employed) 

## umeployed veterans - imputation 
86200  - 81800
76700 - 75000

OR15 <- matrix(c( 4400,1700, 473600, 81800, 75000,8657000), nrow = 3)
dimnames(OR15) <- list("Group" = c("P911 2015","GW 2015", "NonVeteran 2015"), "Unemployed" = c("Yes","No"))
OR15
tab15 <- epitab(OR15, method = "oddsratio", rev = "b")$tab


## 2014
vetdata %>% filter(Year == 2014 & ServicePeriod %in% c("Gulf War-era II","Gulf War-era I","Nonveterans")) %>%
  select(ServicePeriod,`Labor Force`,Unemployed, Employed) 

OR14 <- matrix(c(7100,6300, 563900, 78700, 73600,8473100), nrow = 3)
dimnames(OR14) <- list("Group" = c("P911 2014","GW 2014", "NonVeteran 2014"), "Unemployed" = c("Yes","No"))
OR14
tab14 <- epitab(OR14, method = "oddsratio", rev = "b")$tab


## 2013
vetdata %>% filter(Year == 2013 & ServicePeriod %in% c("Gulf War-era II","Gulf War-era I","Nonveterans")) %>%
  select(ServicePeriod,`Labor Force`,Unemployed, Employed) 

OR13 <- matrix(c(6500,6700, 677000, 74300, 73500,8457200), nrow = 3)
dimnames(OR13) <- list("Group" = c("P911 2013","GW 2013", "NonVeteran 2013"), "Unemployed" = c("Yes","No"))
OR13
tab13 <- epitab(OR13, method = "oddsratio", rev = "b")$tab


## 2012
vetdata %>% filter(Year == 2012 & ServicePeriod %in% c("Gulf War-era II","Gulf War-era I","Nonveterans")) %>%
  select(ServicePeriod,`Labor Force`,Unemployed, Employed) 

OR12 <- matrix(c(7700,5500, 762600, 64600, 80200,8219800), nrow = 3)
dimnames(OR12) <- list("Group" = c("P911 2012","GW 2012", "NonVeteran 2012"), "Unemployed" = c("Yes","No"))
OR12
tab12 <- epitab(OR12, method = "oddsratio", rev = "b")$tab

## 2011
vetdata %>% filter(Year == 2011 & ServicePeriod %in% c("Gulf War-era II","Gulf War-era I","Nonveterans")) %>%
  select(ServicePeriod,`Labor Force`,Unemployed, Employed) 

## imputation - GW
81600 - 80200

OR11 <- matrix(c(11300,1400,707500,54900,80200,8211700), nrow = 3)
dimnames(OR11) <- list("Group" = c("P911 2011","GW 2011", "NonVeteran 2011"), "Unemployed" = c("Yes","No"))
OR11
tab11 <- epitab(OR11, method = "oddsratio", rev = "b")$tab

## 2010
vetdata %>% filter(Year == 2010 & ServicePeriod %in% c("Gulf War-era II","Gulf War-era I","Nonveterans")) %>%
  select(ServicePeriod,`Labor Force`,Unemployed, Employed) 

## imputation - GW
71400 - 68200

OR10 <- matrix(c(7800,3200,753400,43300,68200,8320400), nrow = 3)
dimnames(OR10) <- list("Group" = c("P911 2010","GW 2010", "NonVeteran 2010"), "Unemployed" = c("Yes","No"))
OR10
tab10 <- epitab(OR10, method = "oddsratio", rev = "b")$tab


#### merged tables

merged_all <-  rbind(tab10,tab11,tab12,tab13,tab14,tab15,tab16,tab17) %>% broom::tidy() %>%
  separate(.rownames, into= c("Group", "Year")) 

merged <- merged_all %>% 
  filter(Group != "NonVeteran")


merged %>% filter(Group == "P911") %>% 
  ggplot(aes(as.factor(Year), oddsratio)) + 
  geom_linerange(aes(ymin = lower, ymax = upper),size = 1, 
                 position = position_dodge(width = .25)) + 
  geom_point(position = position_dodge(width = .25), size = 2) +
  geom_hline(yintercept =  1, lty = 2) + theme_bw() +
  labs(y = "Odds ratio compared to non-Veterans", x = " ", color = " ") +
  ggtitle("Odds of unemployment among NYS Post-9/11 Veterans in the labor force") 

ggplot(merged, aes(as.factor(Year), oddsratio,color = Group)) + 
  geom_linerange(aes(ymin = lower, ymax = upper),size = 1, 
                 position = position_dodge(width = .25)) + 
  geom_point(position = position_dodge(width = .25), size = 2) +
  geom_hline(yintercept =  1, lty = 2) + theme_bw()  +
  theme(axis.text = element_text(color = "black")) + 
  labs(y = "Odds ratio compared to non-Veterans", x = " ", color = " ") +
  ggtitle("Odds of unemployment among NYS Veterans in the labor force") +
  scale_color_brewer(palette= "Set1", labels = c("Gulf War", "Post-9/11"))


########## logit regression ##########

merge2 <- merged_all %>% filter(Group != "GW") %>%
  select(Group,Year, No, Yes) %>% 
  gather("No","Yes", key = "Unempl", value = "count") %>%
  mutate(outcome = ifelse(Unempl == "Yes",1,0),
         Veteran = ifelse(Group == "P911", 1, 0),
         time = as.numeric(Year))

merge2$Year <- as.factor(merge2$Year)

xx <- glm(outcome~Veteran*Year, family = binomial, data=merge2,weights = count)
xx <- glm(outcome~Veteran*time, family = binomial, data=merge2,weights = count)

library(jtools)
interact_plot(xx,pred = time, modx = Veteran, interval = TRUE)
