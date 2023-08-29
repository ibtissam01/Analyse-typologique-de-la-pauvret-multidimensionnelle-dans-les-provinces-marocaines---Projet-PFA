
# Charger le package
library(FactoInvestigate)
library(shiny)
library(shinydashboard)
library(FactoMineR)
library(ggplot2)
library(readxl)
library(factoextra)
library(pander)
library(rsconnect)
# Définition de l'interface utilisateur pour la page des jeux de données
ui_dataset <- dashboardPage(
  dashboardHeader(title = "Poverty_2014"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Télécharger et Afficher", tabName = "download_display", icon = icon("database")),
      menuItem("ACP", tabName = "acp", icon = icon("chart-bar")),
      menuItem("Classification", tabName = "classification", icon = icon("chart-pie")),
      menuItem("Télécharger les Résultats", tabName = "download_results", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "download_display",
              h2("Télécharger et Afficher les Données"),
              fluidRow(
                column(width = 6,
                       fileInput("datafile", "Télécharger les données",
                                 accept = c(".csv", ".xlsx")
                       ),
                       selectInput("filetype", "Veuillez sélectionner le type de fichier",
                                   choices = c("csv", "xlsx")
                       ),
                       actionButton("loadData", "Télécharger")
                )
              ),
              fluidRow(
                column(width = 12,
                       tabsetPanel(
                         id = "download_display_tabs",
                         tabPanel("Afficher les Données", dataTableOutput("data_table")),
                         tabPanel("Statistique descriptive", verbatimTextOutput("summary_output"))
                       )
                )
              )
      ),
      
      tabItem(tabName = "acp",
              h2("Analyse en Composantes Principales (ACP)"),
              tabsetPanel(
                id = "acp_tabs",
                tabPanel("Correlation", dataTableOutput("correlation_table")),
                tabPanel("Inertie",
                         plotOutput("scree_plot"),
                         tableOutput("eigenvalues_table"),
                         tableOutput("variances_table")
                ),
                tabPanel("Graphes",
                         plotOutput("var_graph"),
                         plotOutput("var_graph1"),
                         plotOutput("indiv_graph"),
                         plotOutput("indiv_graph1")
                ),
                tabPanel("Résultats des variables",
                         dataTableOutput("variable_results_table")
                ),
                tabPanel("Résultats sur les individus",
                         dataTableOutput("individual_results_table")
                ),
                tabPanel("Résultats des variables qualitatives",
                         dataTableOutput("categorical_variable_results_table")
                )
              )
      ),
      
      tabItem(tabName = "classification",
              h2("Classification hiérarchique CHA"),
              fluidRow(
                column(width = 6,
                       selectInput("num_clusters", "Nombre de clusters", choices = seq(10), selected = 3)
                )
              ),
              tabsetPanel(
                id = "classification_tabs",
                # Onglet "Graphes"
                tabPanel("Graphes",
                         plotOutput("hcpc_tree_plot"),
                         plotOutput("hcpc_map_plot"),
                         plotOutput("hcpc_3d_map_plot")
                ),
                # Onglet "Caractérisation des classes"
                tabPanel("Caractérisation des classes",
                         tableOutput("class_characterization_table")
                ),
                # Onglet "Liens avec la partition"
                tabPanel("Liens avec la partition",
                         tableOutput("partition_links_table")
                )
              )
      ),
      tabItem(tabName = "download_results",
              h2("Télécharger les Résultats"),
              fluidRow(
                column(width = 6,
                       actionButton("download_acp", "Télécharger les Résultats de l'ACP"),
                       actionButton("download_classification", "Télécharger les Résultats de Classification")
                )
              ),
              fluidRow(
                column(width = 12,
                       h4("Lien de téléchargement pour l'ACP :"),
                       downloadLink("download_link_acp", "Cliquez ici pour télécharger les résultats de l'ACP")
                )
              ),
              fluidRow(
                column(width = 12,
                       h4("Lien de téléchargement pour la Classification :"),
                       downloadLink("download_link_classification", "Cliquez ici pour télécharger les résultats de Classification")
                )
              )
      )
      
    )
  )
)


# Lancer l'application Shiny
shinyApp(
  ui = ui_dataset,
  server = function(input, output, session) {
    
    # Charger les données sélectionnées
    data <- reactive({
      req(input$loadData)
      if (is.null(input$datafile))
        return(NULL)
      
      file <- input$datafile$datapath
      if (input$filetype == "csv") {
        read.csv(file)
      } else if (input$filetype == "xlsx") {
        read_excel(file)
      }
    })
    
    
    
    # Afficher les données sur le tableau de bord
    output$data_table <- renderDataTable({
      data()
    }) 
    
    output$summary_output <- renderPrint({
      req(data())  # Assurez-vous d'avoir les données nécessaires pour les statistiques descriptives
      
      # Génération du résumé des statistiques descriptives
      summary(data())
    })
    # Matrice de corrélation
    output$correlation_table <- renderDataTable({
      
      data <- data()  # Supposons que vos données réactives sont stockées dans la variable 'data'
      
      # Exclure la première colonne (catégorique) des données
      data <- data[, -1]
      matrice_corr <- cor(data)
      
      # Arrondir chaque élément de la matrice de corrélation à deux décimales
      matrice_corr_arrondie <- round(matrice_corr, 2)
      
    })
    # Effectuer l'ACP et stocker les résultats dans res.PCA
    res.PCA <- reactive({
      req(data())
      PCA(data(), quali.sup = c(1), graph = FALSE)
    })
    
    # Graphique des variables de l'ACP
    output$var_graph <- renderPlot({
      plot.PCA(res.PCA(), choix = "var", title = "Graphe des variables de l'ACP")
    })
    
    # Graphique des individus de l'ACP
    output$indiv_graph <- renderPlot({
      plot.PCA(res.PCA(), title = "Graphe des individus de l'ACP")
    })
    output$var_graph1 <- renderPlot({
      req(res.PCA())
      fviz_pca_var(res.PCA(), col.var = "contrib")
    })
    
    output$indiv_graph1 <- renderPlot({
      req(res.PCA())
      fviz_pca_ind(res.PCA(), col.ind = "cos2", repel = TRUE)
    })
    
    # Graphique de l'inertie
    output$scree_plot <- renderPlot({
      data <- data()  # Assuming your data is reactive and stored in the variable 'data'
      
      # Exclude the first column (categorical) from the data
      data <- data[, -1]
      
      res <- prcomp(data, center = TRUE, scale = TRUE)
      fviz_screeplot(res, addlabels = TRUE)
    })
    
    
    # Tableau des valeurs propres
    output$eigenvalues_table <- renderTable({
      res.PCA()$eig
    })
    
    # Tableau des variances
    output$variances_table <- renderTable({
      res.PCA()$var
    })
    
    # Tableau des résultats des variables
    output$variable_results_table <- renderDataTable({
      res.PCA()$var$coord
    })
    
    # Tableau des résultats sur les individus
    output$individual_results_table <- renderDataTable({
      res.PCA()$ind$coord
    })
    
    # Tableau des résultats des variables qualitatives
    output$categorical_variable_results_table <- renderDataTable({
      res.PCA()$quali$coord
    })
    
    # Observer les changements de la valeur de nb_clusters
    observe({
      req(res.PCA(), input$num_clusters)
      
      nb_clusters <- as.integer(input$num_clusters)
      
      # Vérifier si nb_clusters est défini et est un nombre valide
      if (!is.na(nb_clusters) && nb_clusters >= 2) {
        res <- HCPC(res.PCA(), nb.clust = nb_clusters, consol = TRUE, graph = FALSE)
        
        # Mettre à jour les sorties avec les nouveaux résultats
        output$hcpc_tree_plot <- renderPlot({
          plot.HCPC(res, choice = 'tree', title = 'Arbre hiérarchique')
        })
        
        output$hcpc_map_plot <- renderPlot({
          plot.HCPC(res, choice = 'map', draw.tree = FALSE, title = 'Plan factoriel')
        })
        
        output$hcpc_3d_map_plot <- renderPlot({
          plot.HCPC(res, choice = '3D.map', ind.names = FALSE, centers.plot = FALSE, angle = 60, title = 'Arbre hiérarchique sur le plan factoriel')
        })
        
        output$class_characterization_table <- renderTable({
          res$desc.axes
        })
        
        #Description of the clusters by the variables
        output$partition_links_table <- renderTable({
          res$desc.var
        })
        
      }
    })
    
    # ... Ajoutez ici le code pour le téléchargement des résultats ..
    
    
    
  })




