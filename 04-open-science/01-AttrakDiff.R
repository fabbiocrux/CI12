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
## Méthodologie Attrakdiff ----
Attrakdiff <- read_csv("data/Data Experimentaion - Attrakdiff.csv")

### Version 2 Attrakdiff
Attrakdiff_donnes_final <- 
  Attrakdiff %>% 
  filter(Group == "Linda-Yasmine-Rodrigo-Syrine") %>% 
  select(Group:Status, QP1:ATT7)

titre <- "AttrakDiff for the ENSGSI vs Polytech Nancy"


# Data Analysis of the Methodologies ----
# Step 1 : Transformer les donnes dans une version longe ----
## function `gather()`: Fusionne des colonnes en lignes. 

## AttrakDiff Step 1 ----
Attrakdiff.step.1 <- Attrakdiff_donnes_final %>% gather(QP1:ATT7, key = "Variable", value = "Value")



# Step 2: Changer l'echelle des réponsesentre le range de (-3, +3)  ----
## function `mutate()`: Calcule et ajoute une ou plusieurs nouvelles variables
## function `case_when()`: Faire quelque chose en function d'une premise logique.


## AttrakDiff Step 2 ----

## Changing the scale of the answers (Page 16 du PDF) 
## See: https://carinelallemand.files.wordpress.com/2015/09/version-franc3a7aise-attrakdiff_lallemand_2015.pdf

Attrakdiff.step.2 <- 
  Attrakdiff.step.1 %>% 
  mutate(Valeur_ajuste = 
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
## AttrakDiff Step 3 ----
### Approach 1 : en faisant pour chaque Variable ----
Attrakdiff.step.3 <- 
  Attrakdiff.step.2 %>% 
  mutate(Valeur_inverse= case_when(
    Variable == "QP1"   ~  Valeur_ajuste*(-1),
    Variable == "QP2"   ~  Valeur_ajuste*(-1),
    Variable == "QP3"   ~  Valeur_ajuste*(-1),
    Variable == "QP5"   ~  Valeur_ajuste*(-1),
    Variable == "ATT1"  ~  Valeur_ajuste*(-1),
    Variable == "ATT3"  ~  Valeur_ajuste*(-1),
    Variable == "ATT5"  ~  Valeur_ajuste*(-1),
    Variable == "ATT7"  ~  Valeur_ajuste*(-1),
    Variable == "QHS1"  ~  Valeur_ajuste*(-1),
    Variable == "QHS3"  ~  Valeur_ajuste*(-1),
    Variable == "QHS4"  ~  Valeur_ajuste*(-1),
    Variable == "QHS7"  ~  Valeur_ajuste*(-1),
    Variable == "QHS1"  ~  Valeur_ajuste*(-1),
    Variable == "QHS3"  ~  Valeur_ajuste*(-1),
    Variable == "QHS4"  ~  Valeur_ajuste*(-1),
    Variable == "QHS7"  ~  Valeur_ajuste*(-1),
    Variable == "QHS7"  ~  Valeur_ajuste*(-1),
    TRUE ~ Valeur_ajuste
  )
  )



# Step 4: Grouper les dimmensions du Modèle correspondant ----
## AttrakDiff Step 4 ----
Attrakdiff.step.4 <-
  Attrakdiff.step.3 %>% 
  mutate(
    Facteurs =
      case_when(
        str_detect(Variable, "QP") ~ "Qualité Pragmatique (QP)",
        str_detect(Variable, "QHS") ~ "Qualité Hédonique - Stimulation (QH-S)",
        str_detect(Variable, "QHI") ~ "Qualité Hédonique - Identité (QH-I)",
        str_detect(Variable, "ATT") ~ "Attractivité Globale (ATT)",
        TRUE ~ "ATTENTION, Erreur dans la basses de donnes"
      ))




# Step 5: Calculer les valeur moyennes et leur deviation standards ----
## function `group_by()`: Regroupe les observations par rapport à une ou plusieurs variables

# More Info: # https://www.r-graph-gallery.com/4-barplot-with-error-bar.html

## AttrakDiff Step 5 ----
Attrakdiff.step.5 <-
  Attrakdiff.step.4 %>%
  group_by(Experimentation,Facteurs) %>%
  summarise(Mean = mean(Valeur_inverse),
            Stand_dev = sd(Valeur_inverse),
            Se = Stand_dev / sqrt(length(Valeur_inverse))
  ) 



# Global Results ----
Attrakdiff.Results <- list()


### AttrakDiff Graphique 1 ----
Attrakdiff.Results$Graphique.1 <-
  Attrakdiff.step.5 %>% 
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
       subtitle = paste("Quantité de Participants:" , Attrakdiff_donnes_final %>% nrow()),
       caption =  paste("Denière mise à jour: ", Sys.time() %>% format( '%d/%m/%Y'))
  ) +
  theme_minimal(base_size = 12, base_family = "Palatino") +
  theme(
    legend.position = "right",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 18, family = "Palatino")
  )


### Saving the global Results
Attrakdiff.Results$Graphique.1
Attrakdiff.Results$Tableau_1 <- Attrakdiff.step.5




# Graphique 2:   ----
## function `left_join()`: Fusionne des tableaux

## AttrakDiff Graphique 2 ----
### Charger les donnes des parametres d'Attrakdiff du Google Docs
Attrakdiff.parameters <- read_csv("data/Data Experimentaion - Parameters Attrakdiff.csv")
Attrakdiff.parameters <- 
  Attrakdiff.parameters %>% arrange(Variable)



Attrakdiff.step.6 <- 
  Attrakdiff.step.4 %>% 
  group_by(Experimentation,Facteurs,Variable) %>% 
  summarise(Mean = mean(Valeur_inverse)) %>% 
  left_join(Attrakdiff.parameters, by="Variable")

# Faire que certain variables deviennent des variables catégoriques. 
Attrakdiff.step.7 <- 
  Attrakdiff.step.6 %>% 
  mutate(
    Variable = factor(Variable),
    Scale = factor(Scale),
    Left = factor(Left),
    Right = factor(Right)
  )





Attrakdiff.Results$Graphique.2 <-
  Attrakdiff.step.7 %>%
  ggplot() + 
  aes(x = Variable, y=Mean, group=Experimentation, color=Experimentation) + 
  geom_line() +
  geom_point() +
  coord_flip() +
  annotate("text", x = 1:28, y = -4, label = Attrakdiff.parameters$Right ) +
  annotate("text", x = 1:28, y = 4, label = Attrakdiff.parameters$Left ) +
  scale_y_continuous(name="Moyenne", breaks=seq(-3,3,1), limits=c(-5, 5)) +
  theme_minimal(base_size = 12, base_family = "Palatino") +
  annotate("rect", xmin=c(1,8,15,22), xmax=c(7,14,21,28),
           ymin=rep(-3,4), ymax=rep(3, 4),
           alpha = .1 , fill = c("blue", "red", "grey","green")) +
  annotate("text",
           y = c(2, 2, 2, 2),
           x = c(4, 11, 19, 26),
           label = c("Attractivité \n globale",
                     "Qualité \n hédonique - identification",
                     "Qualité \n hédonique - stimulation",
                     "Qualité \n pragmatique"),
           family = "Palatino", fontface = 3, size=3) +
  labs(x = "",
       y = "Level ",
       title = "AtrakDiff Profile",
       subtitle = paste("Total of answers:" , Attrakdiff_donnes_final %>% nrow() ) 
  ) +
  theme(
    legend.position = "right",
    panel.border = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 18, family = "Palatino")
  )

# Saving the File
#ggsave("figures/AttrakDiff-2.jpg", width = 7, height = 7, dpi="print" )




# Graphique 3 ----
## Attrakdiff Graphique 3 ----
Attrakdiff.step.8 <-
  Attrakdiff.step.4 %>% 
  filter(Facteurs == "Qualité Hédonique - Stimulation (QH-S)" | 
           Facteurs == "Qualité Hédonique - Identité (QH-I)") %>%
  group_by(Experimentation) %>% 
  summarise(QH = mean(Valeur_inverse),
            QH_sd = sd(Valeur_inverse),
            QH_IC_min = t.test(Valeur_inverse)$conf.int[1], # see https://larmarange.github.io/analyse-R/intervalles-de-confiance.html
            QH_IC_max = t.test(Valeur_inverse)$conf.int[2]
  )

Attrakdiff.step.9 <-
  Attrakdiff.step.4 %>%
  filter(Facteurs == "Qualité Pragmatique (QP)") %>%
  drop_na() %>% 
  group_by(Experimentation) %>% 
  summarise(QP = mean(Valeur_inverse),
            QP_sd = sd(Valeur_inverse),
            QP_IC_min = t.test(Valeur_inverse)$conf.int[1], # see https://larmarange.github.io/analyse-R/intervalles-de-confiance.html
            QP_IC_max = t.test(Valeur_inverse)$conf.int[2]
  )


Attrakdiff.Table <- Attrakdiff.step.8 %>% left_join(Attrakdiff.step.9, by="Experimentation")
names(Attrakdiff.Table)


Attrakdiff.Results$Graphique.3 <-
  Attrakdiff.Table %>%
  ggplot() +
  aes(x=QP, y=QH, color=Experimentation) +
  geom_point()+
  ylim(-3,3)+ xlim(-3,3) +
  geom_hline(yintercept=c(-1,1))+
  geom_vline(xintercept=c(-1,1)) +
  annotate("rect", 
           xmin = Attrakdiff.Table$QP_IC_min, xmax = Attrakdiff.Table$QP_IC_max,
           ymin = Attrakdiff.Table$QH_IC_min, ymax = Attrakdiff.Table$QH_IC_max,
           alpha = .2 , fill = c("blue", "orange")) +
  annotate("rect", xmin=c(-1), xmax=c(1),
           ymin=c(-1), ymax=c(1),
           alpha = .1 , fill = c("#009999")) +
  annotate("text",
           y = c(0.5),
           x = c(0),
           label = c("Neutre"),
           family = "Palatino", fontface = 3, size=4) +
  labs(title = "Global AttrakDiff ",
       #subtitle = paste("Total of answers:" , total),
       x = "Qualité Pragmatique",
       y = "Qualité Hedonique ") +
  theme_minimal(base_size = 10, base_family = "Palatino")
#ggsave("figures/AttrakDiff-3.jpg", width = 5, height = 7, dpi="print" )

rm(Attrakdiff.Table )



# Saving the File
#ggsave("Figures/UEQ-3.jpg", width = 10, height = 5, dpi="print" )

Attrakdiff.Results$Graphique.1
ggsave("figures/Attrakdiff-1.jpg", width = 10, height = 5, dpi="print" )

Attrakdiff.Results$Graphique.2
ggsave("figures/Attrakdiff-2.jpg", width = 8, height = 8, dpi="print" )

Attrakdiff.Results$Graphique.3
ggsave("Figures/Attrakdiff-3.jpg", width = 10, height = 5, dpi="print" )