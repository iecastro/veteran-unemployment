
vetdata %>% filter(ServicePeriod == "Gulf War-era II") %>%
  ggplot(aes(as.factor(Year), LabForce_pct, fill = CivNonInstPop)) +
  geom_col() + scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_c(option = "cividis",direction = -1,labels = scales::comma) + 
  labs(fill = "Population", x = " ", y = " ", caption = "Source: data.ny.gov") + 
  theme_minimal() + theme(axis.text = element_text(color = "black")) +
  ggtitle("Labor force participation rate among Post-9/11 Veterans is steady, \nin spite of an increasing population",
          subtitle = "New Yorsk State CPS estimates 2017")



vetdata %>% filter(ServicePeriod %in% c("Gulf War-era II","Nonveterans","All Veterans")) %>%
  ggplot(aes(as.factor(Year),Unempl_pct)) + geom_col(aes(fill=ServicePeriod), position = "dodge2")+
  theme_minimal() + scale_fill_viridis_d(option = "cividis") +
  labs(fill = " ", y = " ", x = " ",caption = "Source: data.ny.gov   
       *Data suppresed for estimates of less than 5,000 individuals") + 
  scale_y_continuous(labels=scales::percent) + theme(axis.text = element_text(color = "black")) +
  annotate("text", x = "2015", y = .05, label = "*", size = 6.3) +
  annotate("text", x = "2017", y = .05, label = "*", size = 6.3) +
  ggtitle("Labor Force - Unemployed", subtitle = "New York State - CPS estimates")  
  


merged %>% filter(Group == "P911") %>% 
  ggplot(aes(as.factor(Year), oddsratio)) + 
  geom_linerange(aes(ymin = lower, ymax = upper),size = 1, 
                 position = position_dodge(width = .25)) + 
  geom_point(position = position_dodge(width = .25), size = 2) +
  geom_hline(yintercept =  1, lty = 2) + theme_minimal() + 
  theme(axis.text = element_text(color = "black")) +
  labs(y = "Odds Ratio", x = " ", color = " ") +
  ggtitle("Post-9/11 Veterans have higher odds of being unemployed",
          subtitle="compared to non-Veterans in the labor force") 


ggplot(merged, aes(as.factor(Year), oddsratio,color = Group)) + 
  geom_linerange(aes(ymin = lower, ymax = upper),size = 1, 
                 position = position_dodge(width = .25)) + 
  geom_point(position = position_dodge(width = .25), size = 2) +
  geom_hline(yintercept =  1, lty = 2) + theme_minimal()  +
  theme(axis.text = element_text(color = "black")) + 
  labs(y = "Odds Ratio", x = " ", color = " ") +
  ggtitle("Post-9/11 Veterans have higher odds of being unemployed",
          subtitle="compared to non-Veterans in the labor force") +
  scale_color_brewer(palette= "Set1", labels = c("Gulf War", "Post-9/11"))



interact_plot(xx,pred = time, modx = Veteran, interval = TRUE, int.width = 0.95,
              int.type = "confidence", modx.labels = c("No","Yes"),
              y.label = " ", x.label = " ") +
              scale_y_continuous(labels = scales::percent) +
              theme_minimal() +
              theme(axis.text = element_text(color = "black")) +
              ggtitle("Probability of unemployment steadily \ndecreasing for Post-9/11 Veterans")
  

          
bls_pct %>% filter(State != "Total, 18 years and over") %>%
  ggplot(aes(pct,reorder(State,-pct))) + geom_point() +
  geom_segment(aes(x = 0, y = State, xend = pct, yend = State)) +
  theme_minimal() + 
  theme(axis.text.y = element_text(color = "black", size = 7.3),
        axis.text.x = element_text(color = "black"),
        panel.grid.major.y = element_blank()) +
  scale_x_continuous(breaks = c(0,2,4,6), 
                     labels = c("0", "2%", "4%", "6%")) +
  labs(y = "", x = "Labor Force Unemployed", caption = "Source: Bureau of Labor Statistics")  +
  ggtitle("Veteran unemployment is highest in DC and Rhode Island", 
          subtitle = "2017 Averages")


