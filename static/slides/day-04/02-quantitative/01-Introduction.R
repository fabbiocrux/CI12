# ================ Introduction à la Recherche Reproductible ================
#  But: Faire une script reproductible d'analyse de donnes

# CONTENTS
#   0. Introduction
#   1. Reading the Excel/CSV Data
#   2. Treating Excel Data
#   3. Making the Tables and the Graphics
#   4. Introduce the results in Rmarkdown table.


# 1. Introduction ------

## 1.1 R en tant q'une calculatrice -----

1 + 2
5 - 2
2 * 3
3 * 2
2 / 2

## 1.2 Considering the Parenthesis

3 + 5 * 2
(3 + 5) * 2


## Assignation et Type de variables

### Numeriques
a <- 2+2 # Numeric
b <- 5-7 # Numeric
c <- 4*12
d <- 10/3
e <- 5^2
Resultat <- a + b + c + d +e

### Text
prenom <- "Ram"
nom <- "Sess"

message <- paste(prenom, nom)   

### Tabeau
salary <- c(20, 30, 40, 50, -10 )
name <- c("Ram", "Rani", "Ali", "Preeti", "John")
age <- c(34, 54, 23, 65, 2 )
place <- c("ny", "ber", "dhl", "tko", "lon")
books <- c(4, 0, 3, 24, 5)



## Fonctions

sin(20)
cos(20)
log(30)
sqrt(40)

rnorm(100)
?rnorm
args(rnorm)
rnorm(100, mu = 100, sd = 50)




# Dataframe
social <- data.frame(salary, name, age, place, books)

# Exporté des donneés
write_csv(social, file = "social.csv")

# Donnes world
state <- c("Germany", "France", "India", "Russia", "USA", "New Zealand")
pop <- c(20, 19, 50, 25, 30, 5)
capital <- c("Berlin", "Paris", "Delhi", "Moscow", 
             "Washington", "Wellington")
foundation <- c("1870-12-10", "1789-07-14", "1947-08-15", 
                "1990-06-12", "1776-07-04", "1840-02-06")
world <- data.frame(state, pop, capital, foundation)
world

# Subsetting [ ] :
state[1]

state <- c( p1 = "Germany", p2="France", p3="India" )
state["p1"]

state[1:2]

# Acces au colonnes '$'
world[1]
world$state

# What is the mean population?
mean( world$pop )

?mean
?length( world$pop )

?
   
   hist( rnorm(n =500, mean = 39, sd=24) )

rnorm(n =100)



glimpse(penguins)

summary(penguins)

# Grafics in ggplot

ggplot(data = penguins) 

ggplot(data = penguins, mapping = aes(x = species)) +
   geom_bar()

# Exporté
ggsave("peng-species.pdf")

penguins <- penguins

ggplot(data = penguins, mapping = aes(x = species)) +
   geom_bar( fill = c("blue", "orange", "green" ) )

ggplot(data = penguins ) +
   aes(x = species, fill = species) +
   geom_bar() +
   scale_fill_brewer(palette = "Dark2")



ggplot(data = penguins,
       mapping = aes(x = species,
                     fill = species)) +
   geom_bar() +
   scale_fill_brewer(palette = "Dark2") +
   theme(legend.position = "none",
         text = element_text(size = 20)) +
   labs(
      title = "Species of palmer penguins",
      subtitle = "This data is about penguins",
      x = "Species",
      y = "Frequency"
   )


# Variables
ggplot(data = penguins) +
   aes(x = bill_length_mm,
       y = bill_depth_mm,
       color = species) +
   geom_point() +
   scale_fill_brewer(palette = "Dark2") +
   theme(legend.position = "none",
         text = element_text(size = 10)) +
   labs(
      title = "Relationship between bill length \n& depth of palmer penguins",
      subtitle = "This data is about penguins",
      x = "Bill length (mm)",
      y = "Bith depth (mm)"
   ) +
   theme_minimal() +
   geom_smooth()


# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

# create a dataset
data <- data.frame(
   name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
   value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)


# Plot
data %>%
   ggplot( aes(x=name, y=value, fill=name)) +
   geom_boxplot() +
   scale_fill_viridis(discrete = TRUE, alpha=0.6) +
   geom_jitter(color="black", size=0.4, alpha=0.9) +
   #theme_ipsum() +
   theme(
      legend.position="none",
      plot.title = element_text(size=11)
   ) +
   ggtitle("A boxplot with jitter") +
   xlab("")



# DPLRY tutorial ------

# Load packages
library(tidyverse)
library(readxl) # Read Excel data


# Load the data
prenoms <- read_csv2("data/prenoms_france.csv")


# select()

select(prenoms, name, year)

select(prenoms, year : name)


select(prenoms, -sex ) # Exclusion de colones

select(prenoms, -c(year : name) ) # Exclusion of group of columns

select(prenoms, starts_with("n"))  #Select column based on the character

select(prenoms, ends_with("x")) # select columns


# filter()


## Logical operators
#   == (Equal to)
#   != (Not equal to)
#   < (Less than)
#   <= (Less than or equal to)
#   > (Greater than)
#   >= (Greater than or equal to)


fabio <- filter(prenoms, name == "Fabio")


fabio2 <-  filter(prenoms, name < "Fabio")


## Combination of logical tests

# - & (logical AND) --> Est-ce que les conditionsA et B sont toutes les deux vraies ?
# - | (logical OR) --> Est-ce que l’ une ou les deux conditions  A et B sont vraies ?
# - ! (logical NOT) --> Est-ce que A n’est pas vraie ?
# - %in%  --> Est-ce que x est dans l’ensemble a, b, et c ?

filter(prenoms, name == "Fabio" & year == 2000 )

filter(prenoms, name == "Fabio"| name == "Nicolas"| name == "Jean" ) 
filter(prenoms, name %in%  c("Fabio", "Nicolas") )


# arrange

test <- arrange(prenoms, desc(year))   

# slice()

slice(prenoms, 1:10)

# Pipe '%>%'
prenoms %>% 
   filter (name == "Fabio" ) %>%
   ggplot() +
   aes(x = year, y= n ) +
   geom_point() +
   geom_line() +
   labs(
      title = "Popularité du prenom 'Fabio"
   ) +
   theme_minimal()

ggsave(filename = "Fabio.jpg", width=8, height = 9)







# Graphic of the profile of your name

# summarise()
prenoms %>% 
   filter(name == "Fabio" ) %>% 
   summarise( 
      total =  sum(n),
      moyenne = mean(n),
      max = max(n)
   )

# group_by() --> 

prenoms %>% 
   group_by(sex) %>% 
   summarise(
      total = sum(n),
      moyenne = mean(n),
      max = max(n)
   )

# mutate()

prenoms %>% 
   mutate( nouvelle_colonne = n*10)







































