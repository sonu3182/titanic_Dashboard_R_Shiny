library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)

titanic=read.csv('titanic.csv')
apply(titanic,2,function(x) sum(is.na(x)))
titanic$Age[is.na(titanic$Age)]=mean(titanic$Age,na.rm = TRUE)
titanic=select(titanic,-c('Name','Ticket','Cabin','PassengerId','Sex','Embarked'))
head(titanic)
max(titanic$Age)
min(titanic$Age)
table(titanic$Survived)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Shiny - Welcome to Our Titanic Data Visualization "),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId="color1",label="Choose Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                  selected = "Blue",multiple = F),
      
      radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","Yellow"="#FFFF00","Red"="#ff3300")),
      
      selectInput(inputId="channel1",label="Choose Feature",choices = c("Survived"="Survived",
                                                                        "Pclass"="Pclass",
                                                                        "Age"="Age",
                                                                        "SibSp"="SibSp",
                                                                        "Parch"="Parch",
                                                                        "Fare"="Fare"),
                  selected = "Fare",multiple = F),
      
      sliderInput(inputId = "bins1xz",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput(inputId = "range1",
                  label = "Age Range",
                  min = 0,
                  max = 80,
                  value = c(0,80))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      plotOutput(outputId = "distPlot1"),
      plotOutput(outputId = "distPlot2")
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output){
  
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$distPlot <- renderPlot({
    
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#00FF00"
    }
    
    p2 <- titanic %>%  filter(Survived>= input$range1[1] & Age <= input$range1[2]) %>% ggplot()
    p2
    if(input$channel1 == "Survived"){
      p2 <- p2 + geom_histogram(aes(x=Survived),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "Pclass"){
      p2 <- p2 + geom_histogram(aes(x=Pclass),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "Age"){
      p2 <- p2 + geom_histogram(aes(x=Age),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "SibSp"){
      p2 <- p2 + geom_histogram(aes(x=SibSp),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "Parch"){
      p2 <- p2 + geom_histogram(aes(x=Parch),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "Fare"){
      p2 <- p2 + geom_histogram(aes(x=Fare),bins = input$bins1xz,col=input$border1,fill=sColor)
    }
    p2 <- p2 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x=input$channel1,y='Count',title=paste(input$channel1,"v/s Count",sep = " "))
    
    p2

  })
  output$distPlot1 <- renderPlot({
    
    p1 <- titanic  %>%  filter(Age >= input$range1[1] & Age <= input$range1[2]) %>% ggplot(aes(x=Age))
    if(input$channel1 == "Survived"){
      p1 <- p1 + geom_line(aes(y=Survived,col="Survived"),size=0.5)
    }else if(input$channel1 == "Pclass"){
        p1 <- p1 + geom_line(aes(y=Pclass,col="Pclass"),size=0.5)
      }else if(input$channel1 == "Age"){
          p1 <- p1 + geom_line(aes(y=Age,col="Age"),size=0.5)
        }else if(input$channel1 == "SibSp"){
            p1 <- p1 + geom_line(aes(y=SibSp,col="SibSp"),size=0.5)
          }else if(input$channel1 == "Parch"){
              p1 <- p1 + geom_line(aes(y=Parch,col="Parch"),size=0.5)
            }else if(input$channel1 == "Fare"){
                p1 <- p1 + geom_line(aes(y=Fare,col="Fare"),size=0.5)
              }
    p1 <- p1 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Age",y=input$channel1,title=paste("Age v/s",input$channel1,sep = " "))
    
    p1
  })
  
  output$distPlot2 <- renderPlot({
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#00FF00"
    }
    
    p1 <- titanic  %>%ggplot(aes(x=Survived))
    if(input$channel1 == "Survived"){
      p1 <- p1 + geom_boxplot(aes(y=Survived,col="Survived"),fill=sColor)
    }else if(input$channel1 == "Pclass"){
      p1 <- p1 + geom_boxplot(aes(y=Pclass,col="Pclass"),fill=sColor)
    }else if(input$channel1 == "Age"){
      p1 <- p1 + geom_boxplot(aes(y=Age,col="Age"),fill=sColor)
    }else if(input$channel1 == "SibSp"){
      p1 <- p1 + geom_boxplot(aes(y=SibSp,col="SibSp"),fill=sColor)
    }else if(input$channel1 == "Parch"){
      p1 <- p1 + geom_boxplot(aes(y=Parch,col="Parch"),fill=sColor)
    }else if(input$channel1 == "Fare"){
      p1 <- p1 + geom_boxplot(aes(y=Fare,col="Fare"),fill=sColor)
    }
    p1 <- p1 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Survived",y=input$channel1,title=paste("Survived v/s",input$channel1,sep = " "))
    
    p1
  })
  
}

shinyApp(ui = ui, server = server)

