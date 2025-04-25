##### Create Shiny Application
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

options(shiny.launch.browser = TRUE)
shinyApp(ui, server)


# Deploy on Shinyapps.io
#library(rsconnect)
#rsconnect::deployApp("C:/Users/JiwoonPark/OneDrive - Key Proteo/Desktop/RShiny/anxiety", appName = "anxiety")
