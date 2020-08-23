library(shinydashboard)

shinyUI(
    dashboardPage( 
        dashboardHeader(title = "NYCDSA"), 
        dashboardSidebar(
            sidebarUserPanel("Shiny Project", image = 'https://online.nycdatascience.com/assets/icons/nycdsa-logo-horizonal.png'), 
            sidebarMenu(
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Citywide Central Tendency", tabName = "cbyBorough", icon = icon("database")),
                menuItem("Boroughwide Central Tendency", tabName = "cbyDistrict", icon = icon("database")),
                menuItem("Citywide Trend", tabName = "tbyBorough", icon = icon("calendar")),
                menuItem("Boroughwide Trend", tabName = "tbyDistrict", icon = icon("calendar")),
                menuItem("Population Vs Waste", tabName = "corr", icon = icon("balance-scale")),
                menuItem("Corelation", tabName = "allcorr", icon = icon("balance-scale"))
            ),
            sliderInput("range", "Years:",
                        min = min(all.years),
                        max = max(all.years), 
                        value = c(1990,2009), step = 1
            )
        ), 
        dashboardBody(
            tabItems(
                tabItem( tabName = "home",
                         h3('About This Project'),
                         h4(tags$ul(tags$li('This Shiny app was developed by ',span(style='font-style: italic;','Harvey Imama'),' as a submission for the Shiny app project 1 for the ',span(style='font-style: italic;','New York data science academy'),'. The app seeks to validate the common concept that with higher population comes higher waste production and vice versa. Estimated yearly population figures for New York city boroughs as well as waste collection data for the newyork city area has been selected as the dataset of choice to investigate the earlier mention ascertion.'),
                                 tags$li('The expected target audience for this application are people who are interested in getting insights into how waste affects population in cities around the world.')),
                             p(strong('Note: '),tags$ol(tags$li(style='font-style: italic;','Population figures used in thsi project are estimated and are not obtained form actual census.') , tags$li(style='font-style: italic;','Waste collection does not nessesarily mean waste produced and an increase in waste collection does not categorically mean that more waste was produced for the period in question. ')))),
                         h3('About The Data Set'),
                        h4(div(p('The Waste collection data was obtained from the NYC Open data website. It gives monthly information about how much trash in tonnes was evacuated from each district in each borough in New York city from 1990 to 2020. The Open NY website was used for sourcing estimated population data which provided estimated population figures per borough from 1970 to 2019. '))),
                         h1('Demo'),
                         h4('Demonstration will be done by developer'),
                         h3('Future Consideration'),
                         h4(tags$ul(tags$li('Introduction of demographics to the data set which will allow for better insights as to how population changes affect waste movement'),tags$li('Obtaining discrict level population estimates to enable district level analysis of population to waste collection correlation')))
                         ),
                tabItem(
                    tabName = "cbyBorough",
                    fluidRow(
                        box(selectizeInput(inputId = "total.input",
                                           label = "Display Type",
                                           choices = c('All' = 'all','By Borough' = 'by.borough'))
                        )
                    ),
                    fluidRow(
                        box(plotOutput('boxTonnage')),
                        box(plotOutput('boxPopulation')),
                        box(plotOutput('denTonnage')),
                        box(plotOutput('denPopulation')))
                ),
                tabItem(
                    tabName = "cbyDistrict",
                    fluidRow(
                        box(selectizeInput(inputId = "borough",
                                           label = "Borough",
                                           choices = all.boros))
                    ),
                    fluidRow(
                        box(plotOutput('boxTonnageDist'),width=12) ,
                        box(plotOutput('boxPopulationDist'),width=12) 
                    )
                ),
                tabItem(
                    tabName = "tbyBorough",
                    fluidRow(
                        box(selectizeInput(inputId = "total.input2",
                                           label = "Display Type",
                                           choices = c('All' = 'all','By Borough' = 'by.borough'))
                        )
                    ),
                    fluidRow(
                        box(plotOutput('trendTonnage'),width=12)  
                    ),
                    fluidRow(
                        box(plotOutput('trendPopulation'),width=12) 
                    )
                ),
                tabItem(
                    tabName = "tbyDistrict",
                    fluidRow(
                        box(selectizeInput(inputId = "borough2",
                                           label = "Borough",
                                           choices = all.boros))
                    ),
                    fluidRow(
                        box(plotOutput('trendTonnageDistrict'),width=12)  
                    )
                ),
                tabItem(
                    tabName = "corr",
                    fluidRow(
                        box(selectizeInput(inputId = "borough3",
                                           label = "Borough",
                                           choices = c('All' = 'All', all.boros)))
                    ),
                    fluidRow(
                        box(plotOutput('corChart'),width=12)  
                    )
                ),
                tabItem(
                    tabName = "allcorr",
                    fluidRow(
                        box(('Y-axis'),selectizeInput(inputId = "boro1",
                                           label = "Borough",
                                           choices = all.boros),
                            selectizeInput(inputId = "numeric2",
                                           label = "Numeric field",
                                           choices = c('Population' = 'P', 'Waste Collected (Tons)'='R'))
                        ),
                        box(('X-axis'), selectizeInput(inputId = "boro2",
                                            label = "Borough",
                                            choices = all.boros),
                             selectizeInput(inputId = "numeric1",
                                            label = "Numeric field",
                                            choices = c('Population' = 'P', 'Waste Collected (Tons)'='R'))
                        )
                    ),
                    fluidRow(
                        box(plotOutput('AllcorChart'),width=12)  
                    )
                )
            ))
    ))