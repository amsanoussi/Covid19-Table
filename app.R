library(shiny)
library(readr)
library(shinydashboard)
library(ggplot2)
library(graphics)
library(leaflet)
library(DT) 
library(RColorBrewer)

b <- read_delim("infos1.csv", ";", escape_double = FALSE, 
                col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                trim_ws = TRUE)
df <- read.csv2("infos3.csv")
cas = df$Cas
region = df$region
today = 85

ui <- dashboardPage(
    dashboardHeader(
        title = "World-Covid19 By \n Abdoul-Madjid & Moussa",
        # Dropdown menu for messages
        dropdownMenu(type = "messages", badgeStatus = "success",
                     messageItem("Contact",
                                 "amsanousssi@gmail.com"
                     )
        )
    ),
    dashboardSidebar(
        sidebarMenu(
            # Setting id makes input$tabs give the tabName of currently-selected tab
            id = "tabs",
            menuItem("Recap.", tabName = "recap", icon = icon("dashboard"), badgeLabel = "2020-06-11",
                     badgeColor = "green"),
            menuItem("Mapping", icon = icon("line-chart"), tabName = "carto"),
            menuItem("Data", tabName = "donnee", icon = icon("bar-chart-o")
            )
        )
    ),
    dashboardBody(
        
        tabItems(
            tabItem("recap",
                    h1("World Covid19 Situation"),
                    fluidRow(
                        box(
                            title = "", status = "primary", solidHeader = T,
                            collapsible = T,
                            width = 0,
                            infoBox("Positive Cases", b$`Cas confirmes`[today], "Confirmed Cases", icon = icon("credit-card")),
                            # Dynamic infoBoxes
                            infoBoxOutput("actifs"),
                            infoBoxOutput("approvalBox"),
                            infoBoxOutput("progressBox"))),
                    fluidRow(
                        box(
                            title = "Repartition cumuls cases by country", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput("region"))),
                    fluidRow(
                        box(
                            title = "Evolution of Confirmed, Recovery, and Death case", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput("plot"))),
                    fluidRow(
                        box(
                            title = "Evolution of actifs and recovery cases", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput("plotActif"))),
                    fluidRow(
                        box(
                            title = "Daily evolution of new cases", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput("graph1")))
            ),
            tabItem("carto",
                    h1("Mapping"),
                    box(
                        title = "Map of confirmed cases", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        leafletOutput("mymap"))
            ),
            tabItem("donnee",
                    h1("Data Base"),
                    dataTableOutput("dataTable")
            )
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$dataTable = DT::renderDataTable(b)
    output$mymap <- renderLeaflet({
        leaflet(df) %>% addTiles() %>%
            addCircles(lng = ~Longitude, lat = ~Latitude, weight = ~sqrt(df$Cas)*3,
                       radius = ~sqrt(df$Cas), popup = ~paste(region, ":", cas, "positive(s) cases"),
                       color = "#cc1d1d", fillOpacity = 0.5) %>%
            setView(lng = 9.3238432, lat = 17.7356214, zoom = 5)
    })
    output$plot <- renderPlot({
        # 2. Tracer une premiere ligne
        plot(b$Date, b$`Cas confirmes`, type = "b", frame = FALSE, pch = 19,
             col = "blue", xlab = "Date", ylab = "Case Number", 
             lty = 1, lwd = 4)
        
        # 3. Ajouter une deuxieme ligne
        lines(b$Date, b$Deces, pch = 18, col = "red", type = "b", 
              lty = 2, lwd = 4)
        lines(b$Date, b$Gueris, pch = 17, col = "black", type = "b", 
              lty = 3, lwd = 4)
        # 4. Ajouter une legende au graphique et definir la legende de `lty`
        legend("topleft", legend = c("Confirmed Case","Recovery", "Death"),
               col = c("blue",'black',"red"), lty = 1:3, cex = 1.2)
    })
    output$plotActif <- renderPlot({
        # 2. Tracer une premiere ligne
        plot(b$Date, b$Gueris, type = "b", frame = FALSE, pch = 19,
             col = "green", xlab = "Date", ylab = "Case Number", 
             lty = 1, lwd = 4)
        
        # 3. Ajouter une deuxieme ligne
        lines(b$Date, b$Actifs, pch = 18, col = "blue", type = "b", 
              lty = 2, lwd = 4)
        # 4. Ajouter une legende au graphique et definir la legende de `lty`
        legend("topleft", legend = c("Recovery","Active"),
               col = c("green",'blue'), lty = 1:2, cex = 1.5)
    })
    output$graph1 <- renderPlot({
        plot(b$Date, b$`nouveau cas`, type = "l", frame = FALSE, pch = 19,
             col = "blue", xlab = "Date", ylab = "Daily confirmed case", 
             lty = 1, lwd = 5)
    })
    output$camem <- renderPlot({
        ggplot(df, aes(x = "", y = cas, fill = region)) +
            geom_bar(width = 1, stat = "identity", color = "white") +
            coord_polar("y", start = 0) + scale_fill_manual(values = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#009999", "#584841")) +
            theme_void()
    })
    output$progressBox <- renderInfoBox({
        infoBox(
            "Death", b$Deces[today], "Death case", icon = icon("info-circle", lib = "font-awesome"),
            color = "red" ## purple
        )
    })
    output$approvalBox <- renderInfoBox({
        infoBox(
            "Recovery", b$Gueris[today], "Recovery case", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "green"
        )
    })
    output$actifs <- renderInfoBox({
        infoBox(
            "Active", b$`Cas confirmes`[today] -b$Gueris[today]- b$Deces[today], "Active cases", icon = icon("refresh", lib = "glyphicon"),
            color = "yellow"
        )
    })
    output$region <- renderPlot({
        ggplot(df, aes(reorder(region, Cas), Cas, width = .9)) + 
            coord_flip() + 
            xlab("Country") + ylab("Nb of saved case")+ 
            geom_col(fill="#4877C4") +
            geom_text(aes(label=Cas), vjust=-0, hjust = -0.05, color = "black", size=5)+
            theme_classic()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)