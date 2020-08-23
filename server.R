
library(shiny)

shinyServer(function(input, output) {
    
    get.total.range.data <- reactive(
        filter(tonnage.borough.clean.by.year,MON > (input$range[1]-1) & MON < input$range[2] )
    )
    
    get.range.data <- reactive(
        filter(tonnage.borough.clean,MON > (input$range[1]-1) & MON < input$range[2] )
    )
    
    get.range.data.district <- reactive(
        filter(tonnage.NY.clean,MON > (input$range[1]-1) & MON < input$range[2] )
    )
    
    
    output$boxTonnage <- renderPlot({
        if(input$total.input == 'by.borough')
        ggplot(get.range.data(),aes(REFUSETONSCOLLECTED,BOROUGH)) +
            geom_boxplot() + labs(x="Refuse collected (Tons)",y="Borough",title='Refuse Collection Box Plot by Borough')
        else
            ggplot(get.total.range.data(),aes(REFUSETONSCOLLECTED)) +
            geom_boxplot() + labs(x="Refuse collected (Tons)",y="Total",title='Total Refuse Collection Box Plot')
        
    })
    
    output$boxPopulation <- renderPlot({
        if(input$total.input == 'by.borough')
        ggplot(get.range.data(),aes(Population,BOROUGH)) +
            geom_boxplot() + labs(x="Population",y="Borough",title='Population Box Plot by Borough')
        else
            ggplot(get.total.range.data(),aes(Population)) +
            geom_boxplot() + labs(x="Population",y="Total",title='Total Population Box Plot')
            
    })
    
    output$denTonnage <- renderPlot({
        if(input$total.input == 'by.borough')
        ggplot(get.range.data(),aes(REFUSETONSCOLLECTED)) +
            geom_density() + facet_wrap(~BOROUGH) + labs(x="Refuse collected (Tons)",title='Refuse Collection Density plot by Borough')
        else
        ggplot(get.total.range.data(),aes(REFUSETONSCOLLECTED)) +
            geom_density() + labs(x="Refuse collected (Tons)",title='Total Refuse Collection Density plot')
        
    })
    
    output$denPopulation <- renderPlot({
        if(input$total.input == 'by.borough')
        ggplot(get.range.data(),aes(Population)) +
            geom_density() + facet_wrap(~BOROUGH) + labs(x="Population",title='Refuse Collection Density plot by Borough') 
          else 
        ggplot(get.total.range.data(),aes(Population)) +
            geom_density() + labs(x="Population",title='Total Refuse Collection Density plot') 
        
    })
    
    output$boxTonnageDist <- renderPlot({
        get.range.data.district() %>% filter(.,BOROUGH == input$borough) %>% mutate(., district = as.character(COMMUNITYDISTRICT),REFUSETONSCOLLECTED ) %>%
            ggplot(.,aes(REFUSETONSCOLLECTED,district)) +
            geom_boxplot(aes(group=district)) +
            labs(x="Refuse Collection (Tons)",y = "Community District" ,title='Refuse Collection Box plot by Community District') 
    })
    
    output$boxPopulationDist <- renderPlot({
        get.range.data.district() %>%  filter(.,BOROUGH == input$borough) %>% mutate(., district = as.character(COMMUNITYDISTRICT),REFUSETONSCOLLECTED ) %>%
            ggplot(.,aes(REFUSETONSCOLLECTED)) +
            geom_density() + facet_wrap(~district) +
            labs(x="Refuse Collection (Tons)",y = "Community District" ,title='Refuse Collection Density plot by Community District') 
    })
    
    output$trendTonnage <- renderPlot({
        if(input$total.input2 == 'by.borough')
        ggplot(get.range.data(),aes(MON,REFUSETONSCOLLECTED)) +
            geom_line(aes(color=BOROUGH)) +
            labs(x="Year",y = "Refuse Collection (Tons)" ,title='Refuse Collection Trend by Borough') 
        else
            ggplot(get.total.range.data(),aes(MON,REFUSETONSCOLLECTED)) +
            geom_line() +
            labs(x="Year",y = "Refuse Collection (Tons)" ,title='Total Refuse Collection Trend') 
        
    })
    
    output$trendPopulation <- renderPlot({
        if(input$total.input2 == 'by.borough')
        ggplot(get.range.data(),aes(MON,Population)) +
            geom_line(aes(color=BOROUGH))+
            labs(x="Year",y = "Population" ,title='Population Trend by Borough') 
        else
            ggplot(get.total.range.data(),aes(MON,Population)) +
            geom_line()+
            labs(x="Year",y = "Population" ,title='Total Population Trend by Borough') 
        
    })
    
    output$trendTonnageDistrict <- renderPlot({
        get.range.data.district() %>%   filter(.,BOROUGH == input$borough2) %>% mutate(., DISTRICT = as.character(COMMUNITYDISTRICT),REFUSETONSCOLLECTED ,MON) %>%
            ggplot(.,aes(MON,REFUSETONSCOLLECTED)) +
            geom_line(aes(color=DISTRICT))  +
            labs(x="Year",y = "Refuse Collection (Tons)" ,title='Refuse Collection Trend by Community District') 
        
    })
    
    output$corChart <- renderPlot({
        if(input$borough3 == 'All')
        ggplot(get.range.data(),aes(Population,REFUSETONSCOLLECTED)) +
            geom_point(aes(color=BOROUGH)) + geom_smooth(method = "lm") +
            labs(x="Population",y = "Refuse Collection (Tons)" ,title='Population vs Refuse Collection(Tons) - All') 
        else
        filter(get.range.data(),BOROUGH == input$borough3) %>% 
            ggplot(.,aes(Population,REFUSETONSCOLLECTED)) +
            geom_point() + geom_smooth(method = "lm") +
            labs(x="Population",y = "Refuse Collection (Tons)" ,title=paste('Population vs Refuse Collection(Tons) - ',input$borough3 , sep = " ")) 
    })
    
    output$AllcorChart <- renderPlot({
     
           if(input$numeric1 == 'P' & input$numeric2 == 'P')
            filter(get.range.data(),BOROUGH == input$boro1 | BOROUGH == input$boro2 ) %>% 
            ggplot(.,aes(Population,Population)) +
            geom_point() + geom_smooth(method = "lm") +
            labs(x='Population',y = 'Population' ,title=paste('Population Vs Population for ',input$boro1,' and ',input$boro2, sep=" ")) 
    else if (input$numeric1 == 'R' & input$numeric2 == 'R')
            filter(get.range.data(),BOROUGH == input$boro1 | BOROUGH == input$boro2 ) %>% 
            ggplot(.,aes(REFUSETONSCOLLECTED,REFUSETONSCOLLECTED)) +
            geom_point() + geom_smooth(method = "lm") +
            labs(x='Refuse Collection(Tons)',y = 'Refuse Collection(Tons)',title=paste('Refuse Collection(Tons)  Vs Refuse Collection(Tons) for ',input$boro1,' and ',input$boro2, sep=" ")) 
    else if (input$numeric1 == 'R' & input$numeric2 == 'P')
            filter(get.range.data(),BOROUGH == input$boro1 | BOROUGH == input$boro2 ) %>% 
            ggplot(.,aes(REFUSETONSCOLLECTED,Population)) +
            geom_point() + geom_smooth(method = "lm") +
            labs(x='Refuse Collection(Tons) ',y = 'Population' ,title=paste('Refuse Collection(Tons)  Vs  Population for ',input$boro1,' and ',input$boro2, sep=" ")) 
       else 
           filter(get.range.data(),BOROUGH == input$boro1 | BOROUGH == input$boro2 ) %>% 
            ggplot(.,aes(Population,REFUSETONSCOLLECTED)) +
            geom_point() + geom_smooth(method = "lm") +
            labs(x='Population',y = 'Refuse Collection(Tons)',title=paste('Population Vs Refuse Collection(Tons) for ',input$boro1,' and ',input$boro2, sep=" ")) 
        
         })
    
   

})
