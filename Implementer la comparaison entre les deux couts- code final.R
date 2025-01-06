# Simulation des coûts pour différents niveaux de stock
n_tilde <- 34  # Nombre de nouveaux composants
C_s <- 5  # Coût de stockage par unité
C_a <- 20  # Coût d'achat par unité
M <- 10000  # Nombre de simulations

alpha.post.1 <- 2  # Exemple de valeur
beta.post.1 <- 5   # Exemple de valeur



# Simuler les composants défaillants prévus
sim_failures <- rbinom(M, size = n_tilde, prob = rbeta(M, alpha.post.1, beta.post.1))




# Calcul des coûts totaux pour différents niveaux de stock
stock_levels <- 0:n_tilde
total_costs <- sapply(stock_levels, function(S) {
  expected_failures <- mean(pmax(0, sim_failures - S))
  C_s * S + expected_failures * C_a
})




# Ajout d'une ligne pour le coût minimal 
min_cost <- min(total_costs)
optimal_stock <- stock_levels[which.min(total_costs)]


# Créer le DataFrame avec les niveaux de stock et les coûts totaux
df <- data.frame(Stock = stock_levels, Total_Cost = total_costs)

library(ggplot2)

# Visualisation des coûts
ggplot(df, aes(x = Stock, y = Total_Cost)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_vline(xintercept = optimal_stock, linetype = "dashed", color = "green") +
  geom_hline(yintercept = min_cost, linetype = "dotted", color = "purple") +
  labs(
    title = "Coût total en fonction du niveau de stock",
    subtitle = paste("Niveau de stock optimal (Beta-Binomial) :", optimal_stock),
    x = "Niveau de stock",
    y = "Coût total"
  ) +
  theme_minimal()

















###############################################################################
###############################################################################
### Methode 2 : imulation Monte Carlo avec des distributions alternatives #####
# Cette méthode utilise une distribution de Poisson pour modéliser 
#le nombre de défaillances. Contrairement à la première méthode, 
#où l'incertitude est modélisée par une distribution bêta-binomiale, 
#ici on suppose que les défaillances suivent une fréquence constante, 
#définie par un paramètre de taux moyen


lambda <- 10  # Taux moyen de défaillances
sim_failures_poisson <- rpois(M, lambda)


optimize_cost <- function(S) {
  expected_failures <- mean(pmax(0, sim_failures - S))
  return(C_s * S + expected_failures * C_a)
}

result <- optimize(optimize_cost, interval = c(0, n_tilde))
optimal_stock <- result$minimum
min_cost <- result$objective


grid <- expand.grid(C_s = seq(1, 10, 1), C_a = seq(10, 30, 5))
results <- apply(grid, 1, function(params) {
  C_s <- params[1]
  C_a <- params[2]
  mean(sapply(stock_levels, function(S) {
    expected_failures <- mean(pmax(0, sim_failures - S))
    C_s * S + expected_failures * C_a
  }))
})








# Fonction objectif pour calculer les coûts
optimize_cost <- function(S) {
  expected_failures <- mean(pmax(0, sim_failures - S))
  return(C_s * S + expected_failures * C_a)
}

# Trouver le niveau de stock optimal avec 'optimize'
result <- optimize(optimize_cost, interval = c(0, n_tilde))
optimal_stock <- result$minimum
min_cost <- result$objective

# Ajouter au graphique initial
ggplot(df, aes(x = Stock, y = Total_Cost)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_vline(xintercept = optimal_stock, linetype = "dashed", color = "green") +
  geom_hline(yintercept = min_cost, linetype = "dotted", color = "purple") +
  labs(
    title = "Coût total en fonction du niveau de stock",
    subtitle = paste("Niveau de stock optimal (Poisson):", round(optimal_stock)),
    x = "Niveau de stock",
    y = "Coût total"
  ) +
  theme_minimal()
















###################################################################
########################################################################
### Visualison le Cout Totale pour differents valeurs de couts de stockage
# Scénarios de coûts de stockage et d'achat
C_s_values <- seq(1, 10, 1)  # Différents coûts de stockage
C_a_values <- seq(10, 30, 5)  # Différents coûts d'achat

# Calcul des coûts pour chaque combinaison de C_s et C_a
cost_scenarios <- expand.grid(Stock = stock_levels, C_s = C_s_values, C_a = C_a_values)
cost_scenarios$Total_Cost <- mapply(
  function(S, C_s, C_a) {
    expected_failures <- mean(pmax(0, sim_failures - S))
    C_s * S + expected_failures * C_a
  },
  cost_scenarios$Stock, cost_scenarios$C_s, cost_scenarios$C_a
)

# Visualisation
library(ggplot2)
ggplot(cost_scenarios, aes(x = Stock, y = Total_Cost, color = factor(C_s))) +
  geom_line() +
  labs(
    title = "Coût total en fonction du niveau de stock pour différents C_s",
    x = "Niveau de stock",
    y = "Coût total",
    color = "C_s (Coût de stockage)"
  ) +
  theme_minimal()











###############################################################################
###############################################################################


# Charger les bibliothèques nécessaires
library(shiny)
library(ggplot2)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Optimisation des niveaux de stock"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("n_tilde", "Nombre de nouveaux composants (n_tilde):", 34, min = 1),
      numericInput("C_s", "Coût de stockage par unité (C_s):", 5, min = 1),
      numericInput("C_a", "Coût d'achat par unité (C_a):", 20, min = 1),
      numericInput("lambda", "Taux moyen de défaillances (lambda):", 10, min = 1),
      numericInput("M", "Nombre de simulations (M):", 10000, min = 100)
    ),
    
    mainPanel(
      plotOutput("costPlot"),
      verbatimTextOutput("optimalOutput")
    )
  )
)

# Serveur
server <- function(input, output) {
  # Calcul des coûts et des résultats optimaux
  reactive_values <- reactive({
    # Simulation bêta-binomiale
    sim_failures <- rbinom(input$M, size = input$n_tilde, 
                           prob = rbeta(input$M, 2, 5))
    stock_levels <- 0:input$n_tilde
    total_costs <- sapply(stock_levels, function(S) {
      expected_failures <- mean(pmax(0, sim_failures - S))
      input$C_s * S + expected_failures * input$C_a
    })
    
    # Simulation de Poisson et optimisation
    sim_failures_poisson <- rpois(input$M, input$lambda)
    optimize_cost <- function(S) {
      expected_failures <- mean(pmax(0, sim_failures_poisson - S))
      return(input$C_s * S + expected_failures * input$C_a)
    }
    result <- optimize(optimize_cost, interval = c(0, input$n_tilde))
    
    list(
      df = data.frame(Stock = stock_levels, Total_Cost = total_costs),
      optimal_stock = result$minimum,
      min_cost = result$objective
    )
  })
  
  # Rendu du graphique
  output$costPlot <- renderPlot({
    values <- reactive_values()
    ggplot(values$df, aes(x = Stock, y = Total_Cost)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      geom_vline(xintercept = values$optimal_stock, linetype = "dashed", color = "green") +
      geom_hline(yintercept = values$min_cost, linetype = "dotted", color = "purple") +
      labs(
        title = "Coût total en fonction du niveau de stock",
        subtitle = paste("Niveau de stock optimal :", round(values$optimal_stock)),
        x = "Niveau de stock",
        y = "Coût total"
      ) +
      theme_minimal()
  })
  
  # Rendu du texte
  output$optimalOutput <- renderText({
    values <- reactive_values()
    paste(
      "Niveau de stock optimal trouvé :", round(values$optimal_stock), 
      "\nCoût total minimal :", round(values$min_cost, 2)
    )
  })
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)










####### Comment connecter sur shinny :

# Etape 1: Installer les bibliothèques nécessaires
install.packages("rsconnect")


# Etape 2: Se connecter à ShinyApps.io
rsconnect::setAccountInfo(name='elio123',
                          token='FC99DCCDB4F874ED38C56C959D287A07',
                          secret='NZ0K3Dsq00Dn869+2Tx8Uo1r5RmTR5xhK/HjxqlZ')

# Etape 3: Déployer votre application
library(rsconnect)
rsconnect::deployApp("C:/Users/User/Desktop/Elio's File/Aix Marseille Uni- AMU/SMSCU66- JEAN MARC/Projet Final")

