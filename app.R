#Packages
library(shiny)
library(readr)
library(shinydashboard)
library(ggplot2)
library(graphics)
library(leaflet)
library(DT) 
library(RColorBrewer)
library(plotly)
library(leafpop)
library(dplyr)
library(purrr)
library(magick)

# Loading Data
df <- read.csv("coronavirus.csv", stringsAsFactors = FALSE) %>%
    dplyr::mutate(country = ifelse(country == "United Arab Emirates", "UAE", country),
                  country = ifelse(country == "Mainland China", "China", country),
                  country = ifelse(country == "North Macedonia", "N.Macedonia", country),
                  country = trimws(country),
                  country = factor(country, levels = unique(country)))


#Transforming data in daily data
df_daily <- df %>% 
    dplyr::group_by(date, type) %>%
    dplyr::summarise(total = sum(cases, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(names_from = type,
                       values_from = total) %>%
    dplyr::arrange(date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(active =  confirmed - death - recovered) %>%
    dplyr::mutate(confirmed_cum = cumsum(confirmed),
                  death_cum = cumsum(death),
                  recovered_cum = cumsum(recovered),
                  active_cum = cumsum(active))

#Transforming data by country and type of cases
df_tree <- df %>%
    dplyr::group_by(country, type) %>%
    dplyr::summarise(total = sum(cases), .groups = "drop") %>%
    dplyr::mutate(type = ifelse(type == "confirmed", "Confirmed", type),
                  type = ifelse(type == "recovered", "Recovered", type),
                  type = ifelse(type == "death", "Death", type)) %>%
    tidyr::pivot_wider(names_from = type, values_from = total) %>%
    dplyr::mutate(Active = Confirmed - Death - Recovered) %>%
    tidyr::pivot_longer(cols = -country, names_to = "type", values_to = "total")

#Construct a rate data
df_rates <- df_tree %>%
    tidyr::pivot_wider(names_from = "type", values_from = "total") %>%
    dplyr::mutate(recovery_rate = round((Recovered / Confirmed)*100,2),
                  death_rate = round((Death / Confirmed)* 100, 2)) 

#Construct a sumary data
df_world <- df_tree %>%
    dplyr::group_by(type) %>%
    dplyr::summarise(total = sum(total), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = type, values_from = total)

names(df_world) <- tolower(names(df_world))
datee=max(df$date)


data2 = df_tree %>% dplyr::filter(type == "Confirmed")
data3 = distinct(df,country,.keep_all = TRUE)
data4=df_daily %>% 
    dplyr::filter(recovered>=0)%>%
    dplyr::filter(active>=0)
cas = data2$total
country = data2$country
confirmed_color <- "cadetblue"
active_color <- "saddlebrown"
recovered_color <- "forestgreen"
death_color <- "red"

ui <- dashboardPage(skin="black",
    dashboardHeader(
        title = "World-Covid19 Situation By Abdoul-Madjid & Moussa",
        titleWidth = 600,
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
            menuItem("Summary", tabName = "recap", icon = icon("dashboard"), badgeLabel = datee,
                     badgeColor = "green"),
            menuItem("Tree of country", tabName = "arbre", icon = icon("bar-chart-o")),
            menuItem("Graphs Evolutions", tabName = "graphes", icon = icon("bar-chart-o")),
            menuItem("Mapping", icon = icon("line-chart"), tabName = "carto"),
            menuItem("Dayly World Data", tabName = "donnee", icon = icon("bar-chart-o")),
            menuItem("Data & Rates", tabName = "donnee2", icon = icon("bar-chart-o")),
            menuItem("Rates Graphs", tabName = "rates", icon = icon("bar-chart-o")),
            menuItem("About", tabName = "about", icon = icon("bar-chart-o"))
        )
        
    ),
    dashboardBody(
        tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 20px;
      }
    '))),
        tabItems(
            
            
            tabItem("recap",
                    h1("Summary of World Covid19 Situation"),
                    h1(".                                                ."),
                    h1(".                                                ."),
                    h1(".                                                ."),
                    fluidRow(
                        box(
                            title = "", status = "primary", solidHeader = T,
                            collapsible = T,
                            width = 0,
                            infoBox("Positive Cases", format(df_world$confirmed, big.mark = ","), "Confirmed Cases", icon = icon("credit-card")),
                            # Dynamic infoBoxes
                            infoBoxOutput("actifs"),
                            infoBoxOutput("approvalBox"),
                            infoBoxOutput("progressBox")))
            ),
            
            
            tabItem("arbre",
                    h1("Cases by Country"),
                    plotlyOutput("arbre")
            ),
            tabItem('graphes',
                    h1("Graphs Evolutions"),
                    h1("Number cumulative of cases"),
                    plotlyOutput("country"),
                    h1("Evolution of actifs and recovered cases"),
                    plotlyOutput("plotActif"),
                    h1("Evolution of death and recovered cases"),
                    plotlyOutput("plotActif1"),
                    h1("Daily evolution of new cases"),
                    plotlyOutput("graph1"),
                    h1("Cases Histogramme by Country"),
                    plotlyOutput("graph2")
            ),
            tabItem("carto",
                    h1("Maps of cases"),
                        leafletOutput("mymap")
            ),
            tabItem("donnee",
                    h1("Dayly World Data"),
                    dataTableOutput("dataTable")
            )
            ,
            tabItem("donnee2",
                    h1("Data & Rate By Country"),
                    dataTableOutput("dataTable2")
            ),
            tabItem("rates",
                    h1("Recovery Ratio/Death Ratio"),
                    plotlyOutput("rates")
            ),
            tabItem("about",
                    h1(" Our Covid Report:"),
               h1("Human Index Developpement:"),
               tags$img(src="IndiceDH.png",height=500, width=1000),
               h3("Spatial analysis on HDI and covid-19 illustrate that even
if the most developed countries are more affected with a
high propagation of covid but they resist again virus fatality
than sub-developed countries which is clearly observed on
death rate for example Niger with 3,27% than 2,37% in
France.
            "),
            
            
           h1("Helth care access:"),
           tags$img(src="Helth.png",height=500, width=1000),
                
               h3("Of course, there are other factors that influences on recover
rate but health care access play an important role also, one hand
there are a huge contamination rate but a large space to receive
more critical patient with breathing problem or diabetic,… That
avoid explosion of death rate as we can see in African countries
or some American poor countries with less cases than other but a
high death ratio as Egypt with 5,49%,Mexico with 8,57%."),
            
            
           h1("Vulnerability vs Covid19: "),
           tags$img(src="Vulnerability.png",height=500, width=1000),    
               h3("As precedent analysis plus the the country
is vulnerable to face the pandemic plus death
ratio is high cause of lack of means to invest
in hospital, to make a consistent measures as
lock-down, telecommuting,...
            "),
            
           h1("Mouvements vs Cov19 :"),
           tags$img(src="Mouvment.png",height=500, width=1000),
           
           h3("In this part, we analyze the impact of mobility in the
contamination. These travels can be business plan that support
economic or importation of subsistence, also inside the country
interdependence of cities that can cause a huge damage in the
spread of viruses. Fortunately, this impact was cushioned by
sanitary barriers. But it has caused problems in high mobility
countries like the United States, Canada, ..."),
           h1("Death and recovery Rates:"),
           tags$img(src='Rates.png',height=500, width=1000),
           h3("In this map, we can see the comparison between the two rates:
              recovery and death. The cerle radius refer to the number of confirmed cases. 
              Thanks to this image, we can have an idea on the areas where the covid cause
              more deaths than recovery and vice versa.")
        )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$dataTable = DT::renderDataTable(df_daily)
    output$dataTable2 = DT::renderDataTable(df_rates)
    output$mymap <- renderLeaflet({
        
        cv_data_for_plot <- df %>% 
            filter(cases > 0) %>% 
            group_by(country,province,lat,long,type) %>% 
            summarise(cases = sum(cases)) %>% 
            mutate(log_cases = 2 * log(cases)) %>% 
            ungroup()
        cv_data_for_plot.split <- cv_data_for_plot %>% split(cv_data_for_plot$type)
        pal <- colorFactor(c("orange", "red","green"), domain = c("confirmed", "death","recovered"))
        map_object <- leaflet() %>% addProviderTiles(providers$Stamen.Toner)
        names(cv_data_for_plot.split) %>%
            purrr::walk( function(df) {
                map_object <<- map_object %>%
                    addCircleMarkers(data=cv_data_for_plot.split[[df]],
                                     lng=~long, lat=~lat,
                                     #                 label=~as.character(cases),
                                     color = ~pal(type),
                                     stroke = FALSE,
                                     fillOpacity = 0.8,
                                     radius = ~log_cases,
                                     popup =  leafpop::popupTable(cv_data_for_plot.split[[df]],
                                                                  feature.id = FALSE,
                                                                  row.numbers = FALSE,
                                                                  zcol=c("type","cases","country","province")),
                                     group = df,
                                     #                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                                     labelOptions = labelOptions(noHide = F,
                                                                 direction = 'auto'))
            })
        map_object %>%
            addLayersControl(
                overlayGroups = names(cv_data_for_plot.split),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
    
    output$plotActif <- renderPlotly({
          plotly::plot_ly(data = data4,
                            x = ~ date,
                            y = ~ active, 
                            name = 'Active', 
                            fillcolor = active_color,
                            type = 'scatter',
                            mode = 'lines') %>%
                plotly::add_trace(y = ~ recovered,
                                  name = "Recovered",
                                  fillcolor = recovered_color) %>%
                plotly::layout(title = "",
                               yaxis = list(title = "Number of Cases"),
                               xaxis = list(title = "Date",
                                            type = "date"),
                               legend = list(x = 0.1, y = 0.9),
                               hovermode = "compare")
               
    })
    output$plotActif1 <- renderPlotly({
        plotly::plot_ly(data = data4,
                        x = ~ date,
                        y = ~ death, 
                        name = 'Death', 
                        fillcolor = death_color,
                        type = 'scatter',
                        mode = 'lines') %>%
            plotly::add_trace(y = ~ recovered,
                              name = "Recovered",
                              fillcolor = recovered_color) %>%
            plotly::layout(title = "",
                           yaxis = list(title = "Number of Cases"),
                           xaxis = list(title = "Date",
                                        type = "date"),
                           legend = list(x = 0.1, y = 0.9),
                           hovermode = "compare")
        
    })
    output$graph1 <- renderPlotly({
        plotly::plot_ly(data = df_daily,
                        x = ~ date,
                        y = ~ confirmed, 
                        name = 'Active', 
                        fillcolor = confirmed_color,
                        type = 'scatter',
                        mode = 'lines' )%>%
        plotly::layout(title = "",
                       yaxis = list(title = "Confirmed Cases by Day"),
                       xaxis = list(title = "Date",
                                    type = "date"),
                       legend = list(x = 0.1, y = 0.9),
                       hovermode = "compare")
    })
    output$graph2 <- renderPlotly({
        
        plotly::plot_ly(data = df_rates,
                x = ~ country, 
                y=~Confirmed,
                type='bar',
                name = "Confirmed")%>%
            plotly::add_trace(
                y=~Recovered,
                name = "Recovered")%>%
            plotly::add_trace(
                y=~Active,
                name = "Active")%>%
            plotly::add_trace(
                y=~Death,
                name = "Death")%>%
            plotly::layout(title = "",
                           yaxis = list(title = "Confirmed Cases by country",type='linear'),
                           xaxis = list(title = "Country"),
                           legend = list(x = 0.1, y = 0.9),
                           barmode = "stack",
                           bargap=0.01)
    })
    output$progressBox <- renderInfoBox({
        infoBox(
            "Death", paste(format(df_world$death[1] , big.mark = ","), " (",
                           round(100 * df_world$death[1] / df_world$confirmed[1], 1), 
                           "%)", sep = ""),"Death case", icon = icon("info-circle", lib = "font-awesome"),
            color = "red" ## purple
        )
    })
    output$approvalBox <- renderInfoBox({
        infoBox(
            "recovered", paste(format(df_world$recovered[1] , big.mark = ","), " (",
                              round(100 * df_world$recovered[1] / df_world$confirmed[1], 1), 
                              "%)", sep = ""), "recovered case", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "green"
        )
    })
    output$actifs <- renderInfoBox({
        infoBox(
            "Active", paste(format(df_world$active[1], big.mark = ","), " (",
                            round(100 * df_world$active[1] / df_world$confirmed[1], 1), 
                            "%)", sep = ""), "Active cases", icon = icon("refresh", lib = "glyphicon"),
            color = "yellow"
        )
    })
    output$country <- renderPlotly({
        plotly::plot_ly(data = df_daily,
                             x = ~ date,
                             y = ~ active_cum, 
                             name = 'Active', 
                             fillcolor = active_color,
                             type = 'scatter',
                             mode = 'none', 
                             stackgroup = 'one') %>%
            plotly::add_trace(y = ~ confirmed_cum,
                              name = "Confirmed",
                              fillcolor = confirmed_color) %>%
            plotly::add_trace(y = ~ recovered_cum,
                              name = "recovered",
                              fillcolor = recovered_color) %>%
            plotly::add_trace(y = ~ death_cum,
                              name = "Death",
                              fillcolor = death_color) %>%
            plotly::layout(title = "",
                           yaxis = list(title = "Cumulative Number of Cases"),
                           xaxis = list(title = "Date",
                                        type = "date"),
                           legend = list(x = 0.1, y = 0.9),
                           hovermode = "compare")
        
    })
    output$arbre <-renderPlotly({
        plotly::plot_ly(
            data = df_tree %>% dplyr::filter(type == "Confirmed"),
            type= "treemap",
            values = ~total,
            labels= ~ country,
            parents=  ~type,
            domain = list(column=0),
            name = "Confirmed",
            textinfo="label+value+percent parent"
        ) %>%
        plotly::add_trace(
            data = df_tree %>% dplyr::filter(type == "Active"),
            type= "treemap",
            values = ~total,
            labels= ~ country,
            parents=  ~type,
            domain = list(column=1),
            name = "Active",
            textinfo="label+value+percent parent"
        ) %>%
        plotly::add_trace(
            data = df_tree %>% dplyr::filter(type == "Recovered"),
            type= "treemap",
            values = ~total,
            labels= ~ country,
            parents=  ~type,
            domain = list(column=2),
            name = "Recovered",
            textinfo="label+value+percent parent"
        ) %>%
        plotly::add_trace(
            data = df_tree %>% dplyr::filter(type == "Death"),
            type= "treemap",
            values = ~total,
            labels= ~ country,
            parents=  ~type,
            domain = list(column=3),
            name = "Death",
            textinfo="label+value+percent parent"
        ) %>%
        plotly::layout(grid=list(columns=4, rows=1))
    })
    output$rates=renderPlotly({
        df %>% 
            
            dplyr::group_by(country, type) %>%
            dplyr::summarise(total_cases = sum(cases)) %>%
            tidyr::pivot_wider(names_from = type, values_from = total_cases) %>%
            dplyr::arrange(- confirmed) %>%
            dplyr::filter(confirmed >= 20000) %>%
            dplyr::mutate(recover_rate = recovered / confirmed,
                          death_rate = death / confirmed) %>% 
            dplyr::mutate(recover_rate = dplyr::if_else(is.na(recover_rate), 0, recover_rate),
                          death_rate = dplyr::if_else(is.na(death_rate), 0, death_rate)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(confirmed_normal = as.numeric(confirmed) / max(as.numeric(confirmed))) %>%
            plotly::plot_ly(y = ~ round(100 * recover_rate, 1),
                            x = ~ round(100 * death_rate, 1),
                            size = ~  log(confirmed),
                            sizes = c(5, 70),
                            type = 'scatter', mode = 'markers',
                            color = ~ country,
                            marker = list(sizemode = 'diameter' , opacity = 0.5),
                            hoverinfo = 'text',
                            text = ~paste("", country, 
                                          "Confirmed cases: ", confirmed,
                                          " Recovery rates: ", paste(round(100 * recover_rate, 1), "%", sep = ""),
                                          " Death rates: ",  paste(round(100 * death_rate, 1), "%", sep = ""))
            ) %>%
            plotly::layout(title = "Racio Recovery/Ratio Death (Country with more than 20000 Cases)",
                           yaxis = list(title = "Recovery rates", ticksuffix = "%"),
                           xaxis = list(title = "Death rate", ticksuffix = "%", 
                                        dtick = 1, 
                                        tick0 = 0),
                           hovermode = "compare")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)