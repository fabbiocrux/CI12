# ================ Script for data analysis ================
# Create a reproductible Script that enables the analysis of the AttrakDiff / UEQ Methodology

# Content:
#   1. Reading the Excel/CSV Data
#   2. Treating Excel/CSV Data
#   3. Making the Tables and the Graphics
#   4. Introduce the results in Rmarkdown table.

# Load the packages ----
library(tidyverse) # Data Science Tools
library(readxl)  # Read a Excel File


#  Reading the Data ----
UEQ <- read_csv("data/Data Experimentaion - UEQ.csv")

### Version UEQ
UEQ_donnes_final <- 
  UEQ %>% 
  filter(Group == "Emeric, Ayoub, Nada et Aníbal") %>% 
  #filter(Experimentation == "GSI") %>% 
  select(Group:Status, EFF1 : ATT6)

titre <- "UEQ for the ENSGSI"

# Data Analysis of the Methodologies ----
# Step 1 : Transformer les donnes dans une version longe ----
## function `gather()`: Fusionne des colonnes en lignes. 

## EUQ Step 1 ----
UEQ.step.1 <- UEQ_donnes_final %>% gather(EFF1 : ATT6, key = "Variable", value = "Value")


# Step 2: Changer l'echelle des réponsesentre le range de (-3, +3)  ----
## function `mutate()`: Calcule et ajoute une ou plusieurs nouvelles variables
## function `case_when()`: Faire quelque chose en function d'une premise logique.


## UEQ Step 2 ----
UEQ.step.2 <- 
  UEQ.step.1 %>% 
  mutate(Valeur_ajuste= 
           case_when(
             Value == 7 ~ 3,
             Value == 6 ~ 2,
             Value == 5 ~ 1,
             Value == 4 ~ 0,
             Value == 3 ~ -1,
             Value == 2 ~ -2,
             Value == 1 ~ -3,
             TRUE ~ Value
           )
  )


# Step 3: Inverser certain valeurs en fonction de la Méthodplogie ----
## UEQ Step 3 ----
### Approach 1 : en faisant pour chaque Variable ----

UEQ.step.3 <- 
  UEQ.step.2 %>% 
  mutate(Valeur_inverse= case_when(
    Variable == "EFF1"   ~  Valeur_ajuste*(-1),
    Variable == "EFF4"   ~  Valeur_ajuste*(-1),
    Variable == "PERS2"   ~  Valeur_ajuste*(-1),
    Variable == "PERS4"   ~  Valeur_ajuste*(-1),
    Variable == "DEP3"  ~  Valeur_ajuste*(-1),
    Variable == "DEP4"  ~  Valeur_ajuste*(-1),    
    Variable == "STIM1"  ~  Valeur_ajuste*(-1),
    Variable == "STIM4"  ~  Valeur_ajuste*(-1),
    Variable == "NOV1"  ~  Valeur_ajuste*(-1),
    Variable == "NOV2"  ~  Valeur_ajuste*(-1),
    Variable == "ATT2"  ~  Valeur_ajuste*(-1),
    Variable == "ATT5"  ~  Valeur_ajuste*(-1),
    Variable == "ATT6"  ~  Valeur_ajuste*(-1),
    TRUE ~ Valeur_ajuste
  )
  )



# Step 4: Grouper les dimmensions du Modèle correspondant ----
## UEQ Step 4 ----
UEQ.step.4 <-
  UEQ.step.3 %>% 
  mutate(
    Facteurs =
      case_when(
        str_detect(Variable, "ATT") ~ "Attraction",
        str_detect(Variable, "PERS") ~ "Compréhensibilité",
        str_detect(Variable, "DEP") ~ "Contrôlabilité",
        str_detect(Variable, "EFF") ~ "Efficacité",
        str_detect(Variable, "NOV") ~ "Originalité",
        str_detect(Variable, "STIM") ~ "Stimulation",
        TRUE ~ "ATTENTION, Erreur dans la basses de donnes"
      ))




# Step 5: Calculer les valeur moyennes et leur deviation standards ----
## function `group_by()`: Regroupe les observations par rapport à une ou plusieurs variables
# More Info: # https://www.r-graph-gallery.com/4-barplot-with-error-bar.html



## UEQ Step 5 ----
UEQ.step.5 <-
  UEQ.step.4 %>%
  group_by(Experimentation, Facteurs) %>%
  summarise(Mean = mean(Valeur_inverse),
            Stand_dev = sd(Valeur_inverse),
            Se = Stand_dev / sqrt(length(Valeur_inverse))
  ) 


# Global Results ----
UEQ.Results <- list()



### UEQ Graphique 1 ----
UEQ.Results$Graphique.1 <-
  UEQ.step.5 %>%
  ggplot() +
  aes( x=Facteurs, y=Mean, fill=Experimentation) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar( aes(x=Facteurs, 
                     ymin = Mean - Se,
                     ymax = Mean + Se ),
                 position=position_dodge(.9),
                 width=0.1, colour="orange", alpha=0.9, size=0.5) +
  scale_y_continuous(limits = c(-3,3), breaks = c(-3:3)) +
  labs(x = "",
       y = "Level ",
       title = titre,
       subtitle = paste("Quantité de Participants:" , UEQ_donnes_final %>% nrow()),
       caption =  paste("Denière mise à jour: ", Sys.time() %>% format( '%d/%m/%Y'))
  ) +
 theme_minimal(base_size = 12, base_family = "Palatino") +
  theme(
    legend.position = "right",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 18, family = "Palatino")
  )

# Saving the File
#ggsave("figures/UEQ-1.jpg", width = 10, height = 5, dpi="print" )




## UEQ Graphique 2 ----
## Charger les donnes des parametres d'Attrakdiff du Google Docs
UEQ_parameters <- read_csv("data/Data Experimentaion - Parameters UEQ.csv")

UEQ.step.6 <- 
  UEQ.step.4 %>% 
  group_by( Experimentation, Variable ) %>%
  summarise( Mean = mean(Valeur_inverse)) %>% 
  left_join(UEQ_parameters, by="Variable")


# Faire que certain variables deviennent des variables catégoriques. 
UEQ.step.7 <- 
  UEQ.step.6 %>% 
  mutate(
    Variable = factor(Variable),
    Scale = factor(Scale)
  )

UEQ.Results$Graphique.2 <-
  UEQ.step.7 %>%
  ggplot() + 
  aes(x = Variable, y=Mean, group=Experimentation, color=Experimentation) + 
  geom_line() +
  geom_point() +
  coord_flip() +
  annotate("text", x = 1:26, y = -4, label = UEQ_parameters$Left) +
  annotate("text", x = 1:26, y = 4, label = UEQ_parameters$Right) +
  scale_y_continuous(name="Moyenne", breaks=seq(-3,3,1), limits=c(-5, 5)) +
  annotate("rect", xmin=c(1,7,11,15, 19, 23), xmax=c(6,10,14,18, 22, 26),
           ymin=rep(-3,6), ymax=rep(3, 6),
           alpha = .1 , fill = c("blue", "red", "grey","green", "orange", "yellow")) +
  annotate("text",
           y = c(2, 2, 2, 2, 2, 2),
           x = c(4, 8, 12, 16, 20, 23),
           label = c("Attraction",
                     "Contrôlabilité",
                     "Efficacité",
                     "Originalité",
                     "Compréhensibilité",
                     "Stimulation"
           ),
           family = "Palatino", fontface = 3, size=3) +
  theme_minimal(base_size = 12, base_family = "Palatino") +
  labs(x = "",
       y = "Level ",
       title = titre,
       subtitle = paste("Total of answers:" , UEQ_donnes_final %>% nrow()),
       caption = "Group X") +
  theme(
    legend.position = "right",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 18, family = "Palatino")
  )

# Saving the File
#ggsave("figures/UEQ-2.jpg", width = 8, height = 8, dpi="print" )


# Graphique 3 ----


## UEQ Graphique 3 ----
UEQ.step.8 <- 
  UEQ.step.4 %>% 
  mutate(
    Global_scale =
      case_when(
        str_detect(Facteurs, "Compréhensibilité") ~ "Qualité Pragmatique (QP)",
        str_detect(Facteurs, "Efficacité") ~ "Qualité Pragmatique (QP)",
        str_detect(Facteurs, "Contrôlabilité") ~ "Qualité Pragmatique (QP)",
        str_detect(Facteurs, "Originalité") ~ "Qualité Hédonique",
        str_detect(Facteurs, "Stimulation") ~ "Qualité Hédonique",
        str_detect(Facteurs, "Attraction") ~ "Attraction",
        TRUE ~ "ATTENTION"
      )
  )


## Identification des Valeurs pour chaque composant du modèle UEQ
UEQ.step.9 <- 
  UEQ.step.8 %>% 
  group_by(Experimentation, Global_scale) %>%
  summarise(Moyenne = mean(Valeur_inverse),
            Std = sd(Valeur_inverse),
            Se = Std / sqrt(length(Valeur_inverse)))



UEQ.Results$Graphique.3 <-
  UEQ.step.9 %>%
  ggplot() +
  aes(x= Global_scale, y=Moyenne, fill=Experimentation) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar( aes(ymin = Moyenne - Se,
                     ymax = Moyenne + Se ),
                 position=position_dodge(.9),
                 width=0.1, colour="orange", alpha=0.9, size=0.5
  ) +
  scale_y_continuous(limits = c(-3,3), breaks = c(-3:3)) +
  labs(x = "UEQ Results",
       y = "Level ",
       title = titre,
       subtitle = paste("Total of answers:" , UEQ_donnes_final %>% nrow()),
       caption =  paste("Denière mise à jour: ", Sys.time() %>% format( '%d/%m/%Y'))
  ) +
  theme_minimal(base_size = 12, base_family = "Palatino") +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 18, family = "Palatino")
  )


# Saving the File
#ggsave("Figures/UEQ-3.jpg", width = 10, height = 5, dpi="print" )

UEQ.Results$Graphique.1
ggsave("figures/UEQ-1.jpg", width = 10, height = 5, dpi="print" )

UEQ.Results$Graphique.2
ggsave("figures/UEQ-2.jpg", width = 8, height = 8, dpi="print" )

UEQ.Results$Graphique.3
ggsave("Figures/UEQ-3.jpg", width = 10, height = 5, dpi="print" )