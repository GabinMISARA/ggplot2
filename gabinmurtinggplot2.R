#06/02/2024
#MURTIN GABIN

library(ggplot2)
library(dplyr)

# Charger le jeu de données starwars
data(starwars)


###################################################################################################################
#oids en fonction du sexe pour les homeworlds sélectionnés#
################################################################################################################
# Homeworlds sélectionnés
selected_homeworld <- c("Naboo", "Tatooine")

# Filtrer les données pour les homeworld sélectionnés
filtered_starwars <- starwars[starwars$homeworld %in% selected_homeworld, ]

# Créer un histogramme
ggplot(data = filtered_starwars, aes(x = sex, y = mass, fill = homeworld)) +
  geom_point(size = 4, shape = 16, position = position_jitter(width = 0.2, height = 0.1), alpha = 0.8, color = "yellow") +
  labs(title = "Poids en fonction du sexe pour les homeworlds sélectionnés",
       x = "Sexe",
       y = "Masse") +
  facet_wrap(~ homeworld, scales = "free_y") +
  scale_fill_manual(values = c("#FF9999", "#66B2FF"), guide = FALSE) +  # Couleurs de remplissage personnalisées
  theme_minimal() +
  theme(panel.background = element_rect(fill = "black"),  # Fond du panel noir
        axis.text = element_text(color = "red",face = "bold", size = 20),  # Couleur du texte des axes 
        strip.text = element_text(color = "green",face = "bold", size = 30),  # Couleur du texte 
        plot.title = element_text(color = "purple",face = "bold", size = 30),
        text = element_text(color ="pink", face = "bold", size = 30)) 


###################################################################################################################
#Poids en fonction du sexe pour les homeworlds sélectionnés#
################################################################################################################

# Filtrer les données pour exclure la couleur "none" pour les cheveux et les yeux
filtered_starwars2 <- starwars[!(starwars$hair_color %in% c("none", NA)) & !(starwars$eye_color %in% c("none", NA)), ]

# Créer un graphique en coordonnées polaires
ggplot(filtered_starwars2, aes(x = hair_color, fill = eye_color)) +
  geom_bar(stat = "count", position = "stack") +
  coord_polar() +
  theme_minimal() +
  labs(title = "Répartition des couleurs de cheveux en fonction des couleurs des yeux",
       subtitle = "Jeu de données Star Wars (Excluant 'none'et NA)",
       fill = "Couleur des Yeux",
       x = "Couleur des Cheveux",
       y = "Nombre d'Individus")     
