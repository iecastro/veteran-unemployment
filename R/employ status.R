library(tidyverse)
library(readxl)
library(scales)
library(epitools)


vetdata <- read_excel("~/Desktop/DataProjects/veteran-unemployment/data and codebook/cpsdata_APR2018.xlsx")

ggplot(vetdata, aes(as.factor(Year),Unempl_pct, fill = ServicePeriod)) + 
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Unemployed") + theme_minimal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) 

### Lab Force participation rate
vetdata %>% filter(Year == 2017 & ServicePeriod != c("Nonveterans","All Veterans")) %>%
  ggplot(aes(ServicePeriod, `Labor Force`)) + geom_col(aes(fill = LabForce_pct)) +
  scale_fill_viridis_c() + guides(fill = FALSE) +
  scale_y_continuous(labels = scales::comma) + theme_minimal() +
  theme(axis.text = element_text(color = "black")) +
  scale_x_discrete(labels = c("Gulf War-era", 
                              "Post-9/11", "Other","Vietnam, Korea, \n World War II")) +
  labs(y = " ", x = " " , caption = "Source: data.ny.gov") +
  ggtitle("Veteran Population in Labor Force", subtitle = "New York State - 2017 CPS estimates")


vetdata %>% filter(ServicePeriod == "Gulf War-era II") %>%
  ggplot(aes(as.factor(Year), Unempl_pct, fill = Unemployed)) +
  geom_col() + scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_c( option = "cividis", labels = scales::comma) + 
  labs(fill = "Count", x = " ", y = " ", caption = "Source: data.ny.gov") + 
  theme_minimal() +
  ggtitle("NYS Post-9/11 Veterans: In Labor Force - Unemployed")


vetdata %>% filter(Year == 2016 & 
                     ServicePeriod %in% c("All Veterans", "Gulf War-era I","Gulf War-era II", "Nonveterans")) %>%
  ggplot(aes(ServicePeriod, Unempl_pct)) + geom_col(aes(fill = Unempl_pct)) +
  scale_fill_viridis_c() + guides(fill = FALSE) +
  scale_y_continuous(labels = scales::percent) + theme_minimal() +
  theme(axis.text = element_text(color = "black")) +
  labs(y = " ", x = " " , caption = "Source: data.ny.gov") +
  ggtitle("% Unemployed by Service Period", subtitle = "New York State - 2016 CPS estimates")


### 
vetdata %>% filter(ServicePeriod %in% c("Gulf War-era II","Nonveterans","All Veterans")) %>%
  ggplot(aes(as.factor(Year),LabForce_pct)) + geom_col(aes(fill=ServicePeriod), position = "dodge2")+
  theme_minimal() + scale_fill_viridis_d(option = "cividis") +
  labs(fill = " ", y = " ", x = " ",caption = "Source: data.ny.gov") + 
  scale_y_continuous(labels=scales::percent)+
  ggtitle("Labor Force Participation", subtitle = "New York State - CPS estimates")  


###
vetdata %>% filter(ServicePeriod %in% c("Gulf War-era II","Nonveterans","All Veterans")) %>%
  ggplot(aes(as.factor(Year),LabForce_pct)) + geom_col(aes(fill=ServicePeriod), position = "dodge2")+
  theme_minimal() + scale_fill_viridis_d(option = "cividis") +
  labs(fill = " ", y = " ", x = " ",caption = "Source: data.ny.gov") + 
  scale_y_continuous(labels=scales::percent)+ theme(axis.text = element_text(color = "black")) +
  ggtitle("Labor Force Participation", subtitle = "New York State - CPS estimates")  



### compare post-9/11 to nonvets 
## 2016
vetdata %>% filter(Year == 2016 & ServicePeriod %in% c("Gulf War-era II","Nonveterans")) %>%
  select(ServicePeriod,`Labor Force`,Unemployed, Employed) 

x <- c(423800, 9100)
n <- c(9114000,92900)

prop.test(x,n,alternative = "two.sided",correct = TRUE)

vet.tab <- matrix(c(9100, 423800,83800,8690200), nrow = 2)
dimnames(vet.tab) <- list("Group" = c("Veteran","Non-Veteran"), "Unemployed" = c("Yes","No"))

vet.tab
epitab(vet.tab, method = "oddsratio", rev = "b")

### compare all vets 
## 2016
vetdata %>% filter(Year == 2016 & ServicePeriod %in% c("All Veterans","Nonveterans")) %>%
  select(ServicePeriod,`Labor Force`,Unemployed, Employed) 

x2 <- c(423800, 19900)
n2 <- c(9114000,354700)

prop.test(x2,n2,alternative = "two.sided",correct = TRUE)

vet.tab2 <- matrix(c(19900, 423800,334700,8690200), nrow = 2)
dimnames(vet.tab2) <- list("Group" = c("Veteran","Non-Veteran"), "Unemployed 2016" = c("Yes","No"))
vet.tab2

epitab(vet.tab2, method = "oddsratio", rev = "b")


vetdata %>% filter(Year == 2016 & ServicePeriod != "Nonveterans") %>%
  select(ServicePeriod,Unemployed, Employed)

vet.tab3 <- matrix(c(423800,9100,5900,8690200 ,83800,58900), nrow = 3)
dimnames(vet.tab3) <- list("Group" = c("Non-Veteran","Post-9/11 Veteran","Gulf War Veteran"), "Unemployed" = c("Yes","No"))

epitab(vet.tab3, method = "oddsratio", rev = "c")

## 2017 all vets
vetdata %>% filter(Year == 2017 & ServicePeriod %in% c("All Veterans","Nonveterans")) %>%
  select(ServicePeriod,`Labor Force`,Unemployed, Employed) 


vet.tab5 <- matrix(c(12700, 419600,309900,8825400), nrow = 2)
dimnames(vet.tab5) <- list("Group" = c("Veteran","Non-Veteran"), "Unemployed 2017" = c("Yes","No"))
vet.tab5

epitab(vet.tab5, method = "oddsratio", rev = "b")

## 2017 post-9/11 vets

vetdata %>% filter(Year == 2017 & ServicePeriod %in% c("Gulf War-era II","Nonveterans")) %>%
  select(ServicePeriod,`Labor Force`,Unemployed, Employed) 




