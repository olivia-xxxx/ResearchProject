library(shiny)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
#=================================Load Data======================================



dataALStateRent<- read.csv("dataALStateRent.csv",stringsAsFactors=FALSE)
dataFinance<- read.csv("dataFinance.csv",stringsAsFactors=FALSE)
dataHomePriceIndex <- read.csv("dataHomePriceIndex.csv",stringsAsFactors=FALSE)
dataCrime <- read.csv("dataCrime.csv",stringsAsFactors=FALSE)
dataEarning <- read.csv("dataEarning.csv",stringsAsFactors=FALSE)


dataRJCityRentAll <- read.csv("dataRJCityRentAll.csv",stringsAsFactors=FALSE)
dataALCityRentAll <- read.csv("dataALCityRentAll.csv",stringsAsFactors=FALSE)

visualOption <- data.frame(
  dataSet = c(rep("Rental Prices of Apartment List by State",5), rep("Finance",2), "Home Price Index", 
              rep("Crime",3), rep("Earning",2)), 
  dataVariable = c("One Bed","Two Beds","Three Beds","Four Beds","Studio","S&P500","Interest Rate",
                   "Home Price Index","Violent Crime","Murder","Larceny-Theft", "Average Earnings", "Payroll"),
  stringsAsFactors = FALSE
)

cityOptionRJ <- dataRJCityRentAll %>% select(City, StateAbbr) %>% mutate(Comb =  paste(City,StateAbbr,sep = ",")) %>% 
  distinct(City, StateAbbr,Comb) %>% arrange(StateAbbr,City)
cityOptionAL <- dataALCityRentAll %>% select(City, StateAbbr) %>% mutate(Comb =  paste(City,StateAbbr,sep = ",")) %>% 
  distinct(City, StateAbbr,Comb) %>% arrange(StateAbbr,City)

resultOption <- data.frame(
  dataSet = c(rep("Rent Jungle",nrow(cityOptionRJ)), rep("Apartment List",nrow(cityOptionAL))),
  dataVariable = c(cityOptionRJ$Comb,cityOptionAL$Comb),
  stringsAsFactors = FALSE
)


#=====================================UI=========================================
ui <- 
  fluidPage(
    titlePanel("Data Wrangling  Project: Wen Chen"), 
    navbarPage("Options",
               tabPanel("Data Visualization",
                        sidebarLayout(
                          sidebarPanel(
                            
                                      #sliderInput("yearInput", "Year", min = as.Date("2010-01-01"), 
                                      #            max = as.Date("2020-01-01"), 
                                      #            value = c(as.Date("2011-01-01"), as.Date("2019-01-01"))),
                                      
                                      uiOutput("Box1"),
                                      uiOutput("Box2")
                                       
                          ), 
                          mainPanel(
                            plotOutput("mainPlotVisual")
                            
                          )
                        )
               ),
               
               tabPanel("Result Visualization",
                        sidebarLayout(
                          sidebarPanel(
                            
                            uiOutput("Box3"),
                            uiOutput("Box4")
                
                            
                          ), 
                          mainPanel(
                            plotOutput("mainPlotResult"),
                            tableOutput("summaryTableResult"),
                            tableOutput("glanceTableResult")
                          )
                        )
               )
               
               
    )
  )




#===================================Server========================================

server <- function(input, output, session) {
  
  output$Box1 = renderUI(selectInput("Dataset","Select a Dataset",c(unique(visualOption$dataSet),"Select One"),"Select One"))
  
  output$Box2 = renderUI(
    if (is.null(input$Dataset) || input$Dataset == "Select One"){return()
    }else selectInput("Variable", 
                      "Select a Variable", 
                      c(unique(visualOption$dataVariable[which(visualOption$dataSet == input$Dataset)]),"Select One"),
                      "Select One")
  )
  
  output$Box3 = renderUI(selectInput("DataSource","Select a DataSource",c(unique(resultOption$dataSet),"Select One"),"Select One"))
  
  output$Box4 = renderUI(
    if (is.null(input$DataSource) || input$DataSource == "Select One"){return()
    }else selectInput("City", 
                      "Select a City", 
                      c(unique(resultOption$dataVariable[which(resultOption$dataSet == input$DataSource)]),"Select One"),
                      "Select One")
  )
  
  
  dataVisual <- reactive({
     
    fileSelect <- switch(input$Dataset,
                      `Rental Prices of Apartment List by State` = "dataALStateRent.csv",
                      `Finance` = "dataFinance.csv",
                      `Home Price Index` = "dataHomePriceIndex.csv",
                      `Crime` = "dataCrime.csv",
                      `Earning` = "dataEarning.csv")
          
        read.csv(fileSelect,stringsAsFactors=FALSE)
                              
  })
  
  
  dataResult <- reactive({
    if(input$DataSource == "Rent Jungle"){
      # Rent Jungle
      dataRJ <- dataRJCityRentAll %>% filter(paste(City,StateAbbr,sep = ",") == input$City) 
      dataRJ_lm <- lm(AllBeds ~ SP500+InterestRate+HomePriceIndex+AdjPopulation+AdjViolentCrime+AdjMurder +
                        AdjLarcenyTheft+ AdjEarnS + AdjPayroll, data = dataRJ)
      list(data= dataRJ, lm_fit = dataRJ_lm )
    } else  {
      dataAL <- dataALCityRentAll %>% filter(paste(City,StateAbbr,sep = ",") == input$City) %>% 
        mutate(AllBeds = rowMeans(cbind(OneBed,TwoBeds,ThreeBeds,FourBeds,Studio)))
      dataAL_lm <- lm(AllBeds ~ SP500+InterestRate+HomePriceIndex+AdjPopulation+AdjViolentCrime+AdjMurder + 
                        AdjLarcenyTheft+ AdjEarnS + AdjPayroll, data = dataAL)
      list(data= dataAL, lm_fit = dataAL_lm )
    }
    
  })
  
  
  
  output$mainPlotVisual = renderPlot({
    if(is.null(input$Dataset) || is.null(input$Variable)){return()
    } else if (input$Dataset == "Select One" || input$Variable == "Select One"){return()
      
    } else return({
      
      data<- dataVisual() 
      data[is.na(data)]<-0
      variableSelect <- switch(input$Variable,
                           `One Bed` = "X1br",
                           `Two Beds` = "X2br",
                           `Three Beds` = "X3br",
                           `Four Beds` = "X4br",
                           `Studio` = "Studio",
                           `S&P500` = "SP500",
                           `Interest Rate` = "InterestRate",
                           `Home Price Index` = "HomePriceIndex",
                           `Violent Crime` = "ViolentCrime",
                           `Murder` = "Murder",
                           `Larceny-Theft` = "LarcenyTheft",
                           `Average Earnings` = "EarnS",
                           `Payroll` = "Payroll"
                           )
      
      if(variableSelect %in% c("ViolentCrime","Murder","LarcenyTheft")){
        dataReduced <- data[c("State","Year","Population",variableSelect)]
        colnames(dataReduced)<- c("State","Year","Population","Crime")
        dataReduced %>% select(region=State, Year, Population, Crime ) %>% group_by(region,Year) %>%
          summarise(Ratio = 100000 * sum(Crime)/sum(Population)) %>% group_by(region) %>%
          summarise(value = mean(Ratio)) %>%
          state_choropleth(title = paste("Graph of",variableSelect,sep = " "))
      }else if(variableSelect %in% c("EarnS","Payroll")){
        dataReduced <- data[c("state","county","year_time",variableSelect)]
        colnames(dataReduced)<- c("state","county","date","earn")
        dataReduced %>% drop_na() %>% mutate(region = as.numeric(paste(formatC(state, width=2, flag="0"), formatC(county, width=3, flag="0"), sep=""))) %>%
          group_by(region) %>% summarise(value = mean(earn)) %>% 
          county_choropleth(title = paste("Graph of",variableSelect,sep = " "))
      } else if (variableSelect %in% c("SP500","InterestRate","HomePriceIndex")){
        dataReduced <- data[c("Date",variableSelect)]
        colnames(dataReduced)<- c("Date","Index")
        dataReduced$Date <- as.Date(dataReduced$Date)
        dataReduced %>% drop_na() %>% ggplot(aes(x=Date, y = Index, group = 1)) + geom_line() + 
          ggtitle(paste("Graph of",variableSelect,sep = " "))
      } else{
        dataReduced <- data[c("Location",variableSelect)]
        colnames(dataReduced)<- c("State","Bed")
      dataReduced %>% drop_na() %>% mutate(region = tolower(State)) %>%
        group_by(region) %>% summarise(value = mean(Bed)) %>% 
        state_choropleth(title = paste("Graph of",variableSelect,sep = " "))
      
      }
      
    })
  })
  
  
  output$mainPlotResult = renderPlot({
    if(is.null(input$DataSource) || is.null(input$City)){return()
    } else if (input$DataSource == "Select One" || input$City == "Select One"){return()
      
    } else return({
      
      lm_fit <-dataResult()$lm_fit
      plotData <- dataResult()$data %>% mutate(FitValues = lm_fit$fitted.values) %>%
        select(Date,TrueValues = AllBeds,FitValues) 
      plotData$Date <- as.Date(plotData$Date)
      plotData %>% gather(ValueType, Price, -Date) %>% ggplot(aes(Date,Price, color= ValueType)) + 
        geom_line() + ggtitle(paste(input$DataSource,":",input$City,sep = ""))
    }
      
    )
  })
  
  output$summaryTableResult <- renderTable({ 
    if(is.null(input$DataSource) || is.null(input$City)){return()
    } else if (input$DataSource == "Select One" || input$City == "Select One"){return()
      
    } else return({
    lm_fit <-dataResult()$lm_fit
    lm_coef <- summary(lm_fit)$coefficients
    cbind(Variable = rownames(lm_coef),as.tibble(lm_coef))
    }
    
    )
  })
  
  output$glanceTableResult <- renderTable({ 
    if(is.null(input$DataSource) || is.null(input$City)){return()
    } else if (input$DataSource == "Select One" || input$City == "Select One"){return()
      
    } else return({
    lm_fit <-dataResult()$lm_fit
    glance(lm_fit)
    }
    
    )
  })
  
  
}




shinyApp(ui, server)



