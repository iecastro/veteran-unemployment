library(tidyverse)
library(patchwork)
library(epitools)
library(ggrepel)

#--- ingest 
vets <- read_csv("https://data.ny.gov/resource/xnam-chv6.csv")

glimpse(vets)

#--- 

# get odd ratios
vets %>% 
  filter(veteran_service_period %in% 
           c("Gulf War-era II veterans", "Nonveteran", "Veteran") & 
           year == 2021) %>%
  select(cohort = veteran_service_period,
         employed, unemployed,
         civilian_labor_force)

# compare post-9/11 vets  
tab <- matrix(c(8172100,96700, 613300,5500), nrow = 2)
dimnames(tab) <- list("Group" = 
                             c("Non-Veteran","Post-9/11 Veteran"),
                           "Unemployed" = c("No","Yes"))

tab
  
or <- epitab(tab, method = "oddsratio", oddsratio = "fisher")

or

# compare all vets
tab2 <- matrix(c( 8172100,240700, 613300,13800), nrow = 2)
dimnames(tab2) <- list("Group" = 
                        c("Non-Veteran","All_veterans"),
                      "Unemployed" = c("No","Yes"))

tab2

or2 <- epitab(tab2, method = "oddsratio", oddsratio = "fisher")

or2


#--- final plots 
unemploy_plot <- vets %>% 
  filter(veteran_service_period %in% 
           c("Veteran", "Gulf War-era II veterans", "Nonveteran") & year > 2012) %>% 
  mutate(label = case_when(
    veteran_service_period == "Veteran" ~ "All Veterans",
    veteran_service_period == "Gulf War-era II veterans" ~ "Post-9/11 Veterans",
    veteran_service_period == "Nonveteran" ~ "Non-Veterans"
  )) %>% 
  ggplot(aes(as.factor(year), 
             (unemployed_as_a_percent_of_civilian_labor_force/100),
             color = fct_relevel(veteran_service_period,
                                 "Nonveteran", after = Inf))) +
  geom_line(aes(group = veteran_service_period)) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0.01, .1)) +
  labs(y = NULL, 
       subtitle = "New York State unemployment rate: \nCurrent Population Survey (CPS) estimates", 
       x = NULL, 
       color = NULL,
       caption = "*Data suppresed for estimates of less than 5,000 individuals | Source: data.ny.gov") +
  scale_color_manual(values = 
                       c("Veteran" = "gray70",
                         "Gulf War-era II veterans" = "firebrick",
                         "Nonveteran" = "gray30"),
                     guide = "none") +  
  theme_minimal(12, base_family = "serif") +
  theme(axis.text = element_text(color = "black"))


unemploy_plot <- unemploy_plot +  
  geom_text_repel(data = . %>% 
                    filter(year == 2021 &
                             veteran_service_period == 
                             "Gulf War-era II veterans"),
                  aes(label = label), 
                  nudge_y = - .025, 
                  segment.linetype = 2) +
  geom_text_repel(data = . %>% 
                    filter(year == 2021 &
                             veteran_service_period %in% 
                             c("Veteran")),
                  aes(label = label), 
                  nudge_y = .030, nudge_x = - .75,
                  segment.linetype = 2) +
  geom_text_repel(data = . %>% 
                    filter(year == 2021 &
                             veteran_service_period == 
                             "Nonveteran"),
                  aes(label = label), 
                  nudge_y = .035, 
                  segment.linetype = 2)

unemploy_plot


vetdf <- or$tab %>% as_tibble() %>% 
   slice(2) %>% add_column(cohort = "Post-9/11 Veterans") #%>% 
 # bind_rows(or2$tab %>% as_tibble() %>%
         #     slice(2) %>% add_column(cohort = "All Veterans"))

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
       subtitle = "Post-9/11 Veterans in the labor force: \n less likely than their peers to be unemployed based on 2021 CPS estimates",
       caption = "OR = .758, 95% CI(0.737-0.779)") +
  geom_text_repel(aes(label = cohort),
                  direction = "both",
                  nudge_x = - .25,
                  nudge_y = .25) +
  annotate("text", x = 1.6, y = "Post-9/11 Veterans", 
           label = "Comparison: \nNon-Veterans") +
  geom_segment(aes(x = 1.4, xend = 1.02,
                   y = "Post-9/11 Veterans", 
                   yend = "Post-9/11 Veterans"),
               color = "Gray40",
               arrow = arrow(length = unit(0.03, "npc")))



#-- compare participation rates

change_comp <- vets %>% 
  filter(year %in% c(2019, 2021)) %>% 
  select(year, veteran_service_period,
         starts_with("civilian"))

d <- change_comp %>% 
  select(-civilian_labor_force) %>% 
  mutate(label = case_when(
    veteran_service_period == "Veteran" ~ "All Veterans",
    veteran_service_period == "Gulf War-era II veterans" ~ "Post-9/11 Veterans",
    veteran_service_period == "Nonveteran" ~ "Non-Veterans"
  )) %>% 
  filter(veteran_service_period %in%
           c("Nonveteran", #"Veteran",
             "Gulf War-era II veterans")) %>% 
  ggplot(aes(as.factor(year), 
             (civilian_labor_force_as_a_percent_of_population/100),
             color = veteran_service_period,
             group = veteran_service_period)) + 
  geom_point(size = 3) +
  geom_text(aes(label = 
                  paste0(
                    civilian_labor_force_as_a_percent_of_population,
                    "%")),
            nudge_y = .025) +
  geom_line() +
  scale_y_continuous(labels = scales::percent,
                     name = NULL,
                     limits = c(.5, .9)) +
  geom_text_repel(data = . %>% 
                    filter(year == 2021),
                  aes(label = label), 
                  nudge_y =- .045, 
                  nudge_x = .3,
                  segment.linetype = 2) +
  scale_color_manual(values = 
                       c("Gulf War-era II veterans" = "firebrick",
                         "Nonveteran" = "gray30"),
                     guide = "none") +
  theme_classic(12, base_family = "serif") +
  theme(axis.text.x = element_text(color = "black", face = "bold"),
        axis.text.y = element_blank(),
        panel.grid.major = element_line(),
        panel.grid.major.y = element_line(),
        axis.ticks = element_blank()) +
  labs(x = NULL,
       subtitle = "NYS Participation Rate (CPS, 2021)")

unemploy_plot + (oddplot / d)

ggsave("figures/cps2022.png", plot = last_plot())


(unemploy_plot + oddplot) / d  

unemploy_plot / (oddplot + d)

unemploy_plot + oddplot / d

oddplot + (unemploy_plot / d)



