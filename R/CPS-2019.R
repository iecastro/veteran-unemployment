library(tidyverse)
library(patchwork)
library(epitools)
library(ggrepel)

#--- ingest 
vets <- read_csv("https://data.ny.gov/resource/xnam-chv6.csv")

glimpse(vets)


#--- eda
p1 <- vets %>% 
  filter(veteran_service_period %in%
           c("Gulf War-era I veterans",
             "Gulf War-era II veterans",
             "Nonveteran")) %>% 
  ggplot(aes(as.factor(year), 
             (civilian_labor_force_as_a_percent_of_population / 100),
             color = veteran_service_period)) +
  geom_line(aes(group = veteran_service_period)) +
  geom_point() +
  labs(x = NULL, color = NULL,
       y = NULL, subtitle = "Labor force participation rate") +
  gameofthrones::scale_color_got_d() +
  #ylim(.3,.9) +
  scale_y_continuous(limits = c(.2,1),
                     labels = scales::percent) +
  theme_minimal(13, base_family = "serif") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "top")


p2 <- vets %>% 
  filter(veteran_service_period %in%
           c("Gulf War-era I veterans",
             "Gulf War-era II veterans",
             "Nonveteran")) %>% 
  ggplot(aes(as.factor(year), 
             (unemployed_as_a_percent_of_civilian_labor_force/100),
             color = veteran_service_period)) +
  geom_line(aes(group = veteran_service_period)) +
  geom_point() +
  labs(y = NULL, subtitle = "Unemployment rate", 
       x = NULL, color = NULL,
       caption = "*Data suppresed for estimates of less than 5,000 individuals, \nSource: data.ny.gov") +
  gameofthrones::scale_color_got_d() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(13, base_family = "serif") +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none")


p1 / p2 



#--- 

# get odd ratios
vets %>% 
  filter(veteran_service_period %in% 
           c("Gulf War-era II veterans", "Nonveteran", "Veteran") & 
           year == 2019) %>%
  select(cohort = veteran_service_period,
         employed, unemployed,
         civilian_labor_force)

# compare post-9/11 vets  
tab <- matrix(c(8702900,108800, 350600,9200), nrow = 2)
dimnames(tab) <- list("Group" = 
                             c("Non-Veteran","Post-9/11 Veteran"),
                           "Unemployed" = c("No","Yes"))

tab
  
or <- epitab(tab, method = "oddsratio", oddsratio = "fisher")

or

# compare all vets
tab2 <- matrix(c(8702900,317900, 350600,13100), nrow = 2)
dimnames(tab2) <- list("Group" = 
                        c("Non-Veteran","All_veterans"),
                      "Unemployed" = c("No","Yes"))

tab2

or2 <- epitab(tab2, method = "oddsratio", oddsratio = "fisher")

or2


#--- final plots 
unemploy_plot <- vets %>% 
  filter(veteran_service_period != "Veteran") %>% 
  mutate(label = case_when(
    veteran_service_period == "Gulf War-era I veterans" ~ "Gulf War Veterans",
    veteran_service_period == "Gulf War-era II veterans" ~ "Post-9/11 Veterans",
    veteran_service_period == "Nonveteran" ~ "Non-Veterans",
    veteran_service_period == "Veterans of other service periods" ~ "Veterans of other \nservice periods",
    veteran_service_period == "Vietnam, Korean War, and  World War II veterans" ~
      "Vietnam, Korean War, and  \nWorld War II veterans"
    
  )) %>% 
  ggplot(aes(as.factor(year), 
             (unemployed_as_a_percent_of_civilian_labor_force/100),
             color = fct_relevel(veteran_service_period,
                                 "Nonveteran", after = Inf))) +
  geom_line(aes(group = veteran_service_period)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  labs(y = NULL, subtitle = "New York State unemployment rate: \nCurrent Population Survey (CPS) estimates", 
       x = NULL, color = NULL,
       caption = "*Data suppresed for estimates of \nless than 5,000 individuals | Source: data.ny.gov") +
  #gameofthrones::scale_color_got_d() +
  scale_color_manual(values = 
                       c("Gulf War-era I veterans" = "darkblue",
                         "Gulf War-era II veterans" = "firebrick",
                         "Veterans of other service periods" = "gray",
                         "Vietnam, Korean War, and  World War II veterans" = "gray",
                         "Nonveteran" = "gray30"),
                     guide = FALSE) +  
  theme_minimal(12, base_family = "serif") +
  theme(axis.text = element_text(color = "black"))


unemploy_plot <- unemploy_plot +  
  geom_text_repel(data = . %>% 
                    filter(year == 2019 &
                             veteran_service_period == 
                             "Gulf War-era II veterans"),
                  aes(label = label), nudge_y = .005) +
  geom_text_repel(data = . %>% 
                    filter(year == 2014 &
                             veteran_service_period %in% 
                             c("Gulf War-era I veterans")),
                  aes(label = label), direction = "x", nudge_x = .05) +
  geom_text_repel(data = . %>% 
                    filter(year == 2013 &
                             veteran_service_period == 
                             "Vietnam, Korean War, and  World War II veterans"),
                  aes(label = label), direction = "y", 
                  nudge_x = .05, nudge_y = .015) +
  geom_text_repel(data = . %>% 
                    filter(year == 2013 &
                             veteran_service_period == 
                             "Veterans of other service periods"),
                  aes(label = label), direction = "both", 
                  nudge_x = .05, nudge_y = -.005) +
  geom_text_repel(data = . %>% 
                    filter(year == 2019 &
                             veteran_service_period == 
                             "Nonveteran"),
                  aes(label = label), direction = "both", 
                  nudge_x = .05, nudge_y = .0075)

unemploy_plot


vetdf <- or$tab %>% as_tibble() %>% 
  slice(2) %>% add_column(cohort = "Post-9/11 Veterans") %>% 
  bind_rows(or2$tab %>% as_tibble() %>% 
              slice(2) %>% add_column(cohort = "All Veterans"))

oddplot <- ggplot(vetdf, 
                  aes(oddsratio, cohort)) + 
  geom_linerange(aes(xmin = lower, xmax = upper), size = 1) +
  geom_point(size = 3) +
  scale_x_continuous(limits = c(0, 3),
                     labels = c("0" = "Less \nlikely",
                                "1" = "Just as \n likely",
                                "2" = "2x as \nlikely",
                                "3" = "3x as \nlikely")) +
  geom_vline(xintercept = 1, lty = "dashed") +
  theme_classic(12, base_family = "serif") +
  theme(axis.text.x = element_text(color = "black", face = "bold"),
        axis.text.y = element_blank(),
        panel.grid.major = element_line(),
        panel.grid.major.y = element_line(),
        axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL,
       subtitle = "Post-9/11 Veterans in the labor force are \n twice as likely than their peers to be unemployed",
       caption = "OR = 2.10, 95% CI(2.05-2.14) \nbased on 2019 CPS estimates") +
  geom_text_repel(aes(label = cohort),
                  direction = "both",
                  nudge_x = .5,
                  nudge_y = .25) +
  annotate("text", x = .5, y = "Post-9/11 Veterans", 
           label = "Comparison: \nNon-Veterans") +
  geom_segment(aes(x = .75, xend = .95,
                   y = "Post-9/11 Veterans", 
                   yend = "Post-9/11 Veterans"),
               color = "Gray40",
               arrow = arrow(length = unit(0.03, "npc")))



unemploy_plot + oddplot

