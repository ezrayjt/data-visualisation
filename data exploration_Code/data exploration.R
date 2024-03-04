library(tidyverse)
library(plotly)
library(readxl)
library(shiny)   #interaction webpage
library(shinythemes)




#get data for domesmitc
df <- read_excel("2022 GOS National Report Tables.xlsx",sheet=2)
df_dom <- df[2,c(2:7)]
colnames(df_dom) <-df[1,c(2:7)]
dom_long <- gather(df_dom,type_education,full_time_rate,c(1:6))
dom_long <- dom_long %>% mutate(year=if_else(str_detect(type_education,'2021'),2021,2022))
dom_long$type_education <- str_replace(dom_long$type_education, "202.*+", "")
dom_long$type_education <- trimws(dom_long$type_education)
dom_long$overall_employed <- as.numeric(df[3,c(2:7)])
dom_long$avg_salary <- as.numeric(df[5,c(2:7)])
dom_long$full_time_study <- as.numeric(df[6,c(2:7)])

#get data for international
df_int <- read_excel("2022 GOS International Report Tables.xlsx",sheet=2)

df_int_plt <- df_int[2,c(2:7)]
colnames(df_int_plt) <-df_int[1,c(2:7)]
int_long <- gather(df_int_plt,type_education,full_time_rate,c(1:6))

int_long <- int_long %>% mutate(year=if_else(str_detect(type_education,'2021'),2021,2022))
int_long$type_education <- str_replace(int_long$type_education, "202.*+", "")
int_long$type_education <- trimws(int_long$type_education)
int_long$overall_employed <- as.numeric(df_int[3,c(2:7)])
int_long$avg_salary <- as.numeric(df_int[5,c(2:7)])
int_long$full_time_study <- as.numeric(df_int[6,c(2:7)])

plt_final <- merge(dom_long,int_long,by=c("type_education","year"))

#conert all column type to numeric except the first column "education type"
plt_final[colnames(plt_final)[2:8]] <- sapply(plt_final[colnames(plt_final)[2:8]],as.numeric)
z <- plt_final %>% filter(year==2021) %>% filter(type_education=="Postgraduate coursework")

#read data for domestic undergraduate student

#zheet 23 undergrad:
df_20_local_dollar <- read_excel("2022 GOS National Report Tables.xlsx",sheet=23)
local_study_clear <- df_20_local_dollar[c(2:22),c(1:7)]
colnames(local_study_clear) <- c("study_area","male_21","male_22","femal_21","femal_22","total_21","total_22")
local_study_clear[colnames(local_study_clear)[2:7]] <- sapply(local_study_clear[colnames(local_study_clear)[2:7]],as.numeric)
local_study_clear <- local_study_clear %>% mutate(male_21=if_else(is.na(male_21),(2*total_21)-femal_21,male_21),
                                                  male_22=if_else(is.na(male_22),(2*total_22)-femal_22,male_22)                                  )
local_study_clear <- local_study_clear[-21,]

#read data for domestic postgraduate student
df_20_local_dollar_post <- read_excel("2022 GOS National Report Tables.xlsx",sheet=24)

local_study_clear_post <- df_20_local_dollar_post[c(2:22),c(1:7)]
colnames(local_study_clear_post) <- c("study_area","male_21","male_22","femal_21","femal_22","total_21","total_22")
local_study_clear_post[colnames(local_study_clear_post)[2:7]] <- sapply(local_study_clear_post[colnames(local_study_clear_post)[2:7]],as.numeric)
local_study_clear_post <- local_study_clear_post %>% mutate(male_21=if_else(is.na(male_21),(2*total_21)-femal_21,male_21),
                                                            male_22=if_else(is.na(male_22),(2*total_22)-femal_22,male_22)                                  ) %>% na.omit()
#UI and Server:

ui <- navbarPage(title="Higer Education Job Market in Australia",id="job",theme=shinytheme("flatly"),
                 tabPanel("Welcome Page",fluidRow(tags$br()),
                          fluidRow(tags$br()),
                          titlePanel(h1("Welcome to Higer Education Job Market in Australia",align="center")),
                          h3("This website will provide you an exploration about the job market in Austrlia.From this website you can know how much you could earn after you finishing your degree from Uni, and you can also find out what the key factors could influence your future salary",align="center"),fluidRow(tags$br()),fluidRow(tags$br()),fluidRow(column(5,""),column(4,actionButton("run", "Check your Salary!")))
                 ),
                 tabPanel("Insights",
                          titlePanel(h2(tags$b("Overall Job market review"),align="center")),
                          sidebarLayout(mainPanel(plotlyOutput("p1")),sidebarPanel(h5(tags$b("User Guide:"),align="center"),fluidRow(column(4,selectInput("year","Select Year:",
                                                                                                                                                          c("2021"=2021,
                                                                                                                                                            "2022"=2022))))
                                                                                   ,
                                                                                   
                                                                                   p("1. Please select the year you interested most to start the exploration."),
                                                                                   p("2. User can click the elements in the plot on the left to control the two plots in the bottom to get more information."),
                                                                                   p("3. The left plot shows the salary difference between different educational background by differernt type of students in the year chosen by user."),
                                                                                   p("4.The pie chart shows the employability of particular type of student chosen by user. "))),
                          fluidRow(tags$br()),
                          fluidRow(column(6,plotlyOutput("p2")),column(6,plotlyOutput("p3"))),
                          fluidRow(tags$br()),
                          titlePanel(h2(tags$b("Job Market in Different Study Area"),align="center")),
                          fluidRow(column(4,selectInput("year2","Select Year:",
                                                        c("2021"="2021",
                                                          "2022"="2022"))),column(4,selectInput("degree","Select Degree:",
                                                                                                c("Postgraduate coursework"="Postgraduate coursework",
                                                                                                  "Undergraduate"="Undergraduate")))),
                          fluidRow(column(8,plotlyOutput("p4")),column(4,plotlyOutput("p5")))
                          
                          
                          
                 ))


server <- function(input,output,session){
  
  
  observeEvent(input$run, {
    updateTabsetPanel(session, "job",selected = "Insights")
  })
  
  
  
  
  
  output$p1 <- renderPlotly({
    plt_final %>% filter(year==input$year) %>%  plot_ly(x=~type_education,y=~as.numeric(avg_salary.x),source="plt1",type="bar",name="Domestic") %>% 
      add_trace(y=~as.numeric(avg_salary.y),name="International") %>% 
      layout(title="Average Salary Earned by Domestic and International Student",
             legend = list(title = list(text = "Type of Student"))) %>% 
      layout(xaxis=list(title="Education background"),
             yaxis=list(title="Average Salary in $"))
  })
  
  output$p2 <- renderPlotly({
    plt_final %>% filter(year==input$year) %>% filter(type_education=="Postgraduate coursework")%>% plot_ly(type="pie",labels=c("Full time","Part time","Unemployed"),values=c(z$full_time_rate.x,z$overall_employed.x-z$full_time_rate.x,100-z$overall_employed.x)) %>% layout(title="Postgraduate coursework Employability of Domesitc Student",legend = list(title = list(text = "Employment Type"))) %>% layout(xaxis=list(title="Employment Type"))
    
  })
  output$p3 <- renderPlotly({
    plt_final %>% filter(year==input$year) %>% filter(type_education=="Postgraduate coursework") %>% plot_ly(y=~full_time_study.x,x=~type_education,type="bar",marker=list(color="red"),name="domesitc") %>%   add_trace(y=~full_time_study.y,x=~type_education,marker=list(color="blue"),name="international") %>% layout(title=paste("Postgraduate coursework ","Full time study rate of Domesitc Student"),legend = list(title = list(text = "Student Type"))) %>% 
      layout(xaxis=list(title="Course Type"),
             yaxis=list(title="Full Time Study Rate in %"))
  })
  
  output$p4 <- renderPlotly({
    if(input$year2=="2021"&input$degree=="Postgraduate coursework"){
      abc <- local_study_clear_post %>% arrange(desc(as.numeric(total_21)))
      
      xform <- list(categoryorder = "array",
                    categoryarray = abc$study_area)
      
      abc %>% 
        plot_ly(x=~study_area,y=~total_21,type="bar",name="Domestic",source="plt4") %>% layout(xaxis=xform) %>% layout(title=paste("Postgraduate Coursework ","Salary Earned by study areas in ",input$year2)) %>%layout(xaxis=list(title="Study Area"),
                                                                                                                                                                                                                         yaxis=list(title="Annual Salary in $"))}
    
    else if (input$year2=="2022"&input$degree=="Postgraduate coursework"){
      abc <- local_study_clear_post %>% arrange(desc(as.numeric(total_22)))
      
      xform <- list(categoryorder = "array",
                    categoryarray = abc$study_area)
      
      abc %>% 
        plot_ly(x=~study_area,y=~total_22,type="bar",name="Domestic",source="plt4") %>% layout(xaxis=xform) %>% layout(title=paste("Postgraduate Coursework ","Salary Earned by study areas in",input$year2)) %>%             layout(xaxis=list(title="Study Area"),
                                                                                                                                                                                                                                     yaxis=list(title="Annual Salary in $"))
    }
    else if(input$year2=="2021"&input$degree=="Undergraduate"){abc <- local_study_clear %>% arrange(desc(as.numeric(total_21)))
    
    xform <- list(categoryorder = "array",
                  categoryarray = abc$study_area)
    
    abc %>% 
      plot_ly(x=~study_area,y=~total_21,type="bar",name="Domestic",source="plt4") %>% layout(xaxis=xform) %>% layout(title=paste("Undergraduate ","Salary Earned by study areas in ",input$year2)) %>%             layout(xaxis=list(title="Study Area"),
                                                                                                                                                                                                                          yaxis=list(title="Annual Salary in $"))}
    
    else{abc <- local_study_clear %>% arrange(desc(as.numeric(total_22)))
    
    xform <- list(categoryorder = "array",
                  categoryarray = abc$study_area)
    
    abc %>% 
      plot_ly(x=~study_area,y=~total_22,type="bar",name="Domestic",source="plt4") %>% layout(xaxis=xform) %>% layout(title=paste("Undergraduate ","Salary Earned by study areas in ",input$year2)) %>%             layout(xaxis=list(title="Study Area"),
                                                                                                                                                                                                                          yaxis=list(title="Annual Salary in $"))}
    
  })
  
  
  output$p5 <- renderPlotly({
    if(input$year2=="2021"&input$degree=="Postgraduate coursework"){
      local_study_clear_post %>% filter(study_area=="Pharmacy") %>%   plot_ly(x=~study_area,y=~male_21,type="bar",name="Male") %>% 
        add_trace(x=~study_area,y=~femal_21,name="Female") %>% layout(title=paste("Salary difference by Gender in 2021<br>Postgraduate"),legend = list(title = list(text = "Gender Type"))) %>% 
        layout(xaxis=list(title="Study Area"),
               yaxis=list(title="Annual Salary in $"))
    }
    else if (input$year2=="2022"&input$degree=="Postgraduate coursework"){
      local_study_clear_post %>% filter(study_area=="Pharmacy") %>%   plot_ly(x=~study_area,y=~male_22,type="bar",name="Male") %>% 
        add_trace(x=~study_area,y=~femal_22,name="Female") %>% layout(title=paste("Salary difference by Gender in 2022<br>Postgraduate"),legend = list(title = list(text = "Gender Type"))) %>% 
        layout(xaxis=list(title="Study Area"),
               yaxis=list(title="Annual Salary in $"))
    }
    else if(input$year2=="2021"&input$degree=="Undergraduate"){
      local_study_clear %>% filter(study_area=="Pharmacy") %>%   plot_ly(x=~study_area,y=~male_21,type="bar",name="Male") %>% 
        add_trace(x=~study_area,y=~femal_21,name="Female") %>% layout(title=paste("Salary difference by Gender in 2021<br>Undergraduate"),legend = list(title = list(text = "Gender Type"))) %>% 
        layout(xaxis=list(title="Study Area"),
               yaxis=list(title="Annual Salary in $"))
    }
    else{
      local_study_clear %>% filter(study_area=="Pharmacy") %>%   plot_ly(x=~study_area,y=~male_22,type="bar",name="Male") %>% 
        add_trace(x=~study_area,y=~femal_22,name="Female") %>% layout(title=paste("Salary difference by Gender in 2022<br>Undergraduate"),legend = list(title = list(text = "Gender Type"))) %>% 
        layout(xaxis=list(title="Study Area"),
               yaxis=list(title="Annual Salary in $"))
    }
    
    
    
    
    
    
    
    
  })
  
  
  
  
  
  
  observeEvent(event_data("plotly_click", source = "plt4"),{
    click_data <- event_data("plotly_click", source = "plt4")
    #print(click_data)
    
    if(input$year2=="2021"&input$degree=="Postgraduate coursework"){
      output$p5 <- renderPlotly({
        local_study_clear_post %>% filter(study_area==click_data[1,3]) %>%   plot_ly(x=~study_area,y=~male_21,type="bar",name="Male") %>% 
          add_trace(x=~study_area,y=~femal_21,name="Female") %>% layout(title=paste("Salary difference by Gender in 2021<br>Postgraduate"),legend = list(title = list(text = "Gender Type"))) %>% 
          layout(xaxis=list(title="Study Area"),
                 yaxis=list(title="Annual Salary in $"))})
    }
    else if (input$year2=="2022"&input$degree=="Postgraduate coursework"){
      output$p5 <- renderPlotly({
        local_study_clear_post %>% filter(study_area==click_data[1,3]) %>%   plot_ly(x=~study_area,y=~male_22,type="bar",name="Male") %>% 
          add_trace(x=~study_area,y=~femal_22,name="Female") %>% layout(title=paste("Salary difference by Gender in 2022<br>Postgraduate"),legend = list(title = list(text = "Gender Type"))) %>% 
          layout(xaxis=list(title="Study Area"),
                 yaxis=list(title="Annual Salary in $"))})
    }
    else if(input$year2=="2021"&input$degree=="Undergraduate"){
      output$p5 <- renderPlotly({
        local_study_clear %>% filter(study_area==click_data[1,3]) %>%   plot_ly(x=~study_area,y=~male_21,type="bar",name="Male") %>% 
          add_trace(x=~study_area,y=~femal_21,name="Female") %>% layout(title=paste("Salary difference by Gender in 2021<br>Undergraduate"),legend = list(title = list(text = "Gender Type"))) %>% 
          layout(xaxis=list(title="Study Area"),
                 yaxis=list(title="Annual Salary in $"))})
    }
    else{
      output$p5 <- renderPlotly({
        local_study_clear %>% filter(study_area==click_data[1,3]) %>%   plot_ly(x=~study_area,y=~male_22,type="bar",name="Male") %>% 
          add_trace(x=~study_area,y=~femal_22,name="Female") %>% layout(title=paste("Salary difference by Gender in 2022<br>Undergraduate"),legend = list(title = list(text = "Gender Type"))) %>% 
          layout(xaxis=list(title="Study Area"),
                 yaxis=list(title="Annual Salary in $"))})
    }
    
    
  })
  
  
  
  
  observeEvent(event_data("plotly_click", source = "plt1"),{
    click_data <- event_data("plotly_click", source = "plt1")
    #0 domesitc, 1 international
    #print(click_data)
    z <- plt_final %>% filter(year==input$year) %>% filter(type_education==click_data[1,3])
    
    
    if(click_data[1,1]==0){
      output$p2 <- renderPlotly({
        z%>% plot_ly(type="pie",labels=c("Full time","Part time","Unemployed"),values=c(z$full_time_rate.x,z$overall_employed.x-z$full_time_rate.x,100-z$overall_employed.x)) %>% layout(title=paste(click_data[1,3],"Employability of Domesitc Student"),legend = list(title = list(text = "Employment Type"))) %>% layout(xaxis=list(title="Employment Type"))
        
        
      })
      output$p3 <- renderPlotly({
        z%>% plot_ly(y=~full_time_study.x,x=~type_education,type="bar",marker=list(color="red"),name="domesitc") %>%   add_trace(y=~full_time_study.y,x=~type_education,marker=list(color="blue"),name="international") %>% layout(title=paste(click_data[1,3],"Full time study rate of Domesitc Student"),legend = list(title = list(text = "Student Type"))) %>%                       layout(xaxis=list(title="Course Type"),
                                                                                                                                                                                                                                                                                                                                                                                                yaxis=list(title="Full Time Study Rate in %"))
        
        
        
        
        
      })
    }
    else{
      
      output$p2 <- renderPlotly({
        z%>% plot_ly(type="pie",labels=c("Full time","Part time","Unemployed"),values=c(z$full_time_rate.y,z$overall_employed.y-z$full_time_rate.y,100-z$overall_employed.y)) %>% layout(title=paste(click_data[1,3],"Employability of International Student"),legend = list(title = list(text = "Employment Type"))) %>% layout(xaxis=list(title="Employment Type"))
        
        
        
      })
      
      output$p3 <- renderPlotly({
        z%>% plot_ly(y=~full_time_study.x,x=~type_education,type="bar",marker=list(color="blue"),name="domesitc") %>%   add_trace(y=~full_time_study.y,x=~type_education,marker=list(color="red"),name="international") %>% layout(title=paste(click_data[1,3],"full time study rate of International Student"),legend = list(title = list(text = "Student Type"))) %>% 
          layout(xaxis=list(title="Course Type"),
                 yaxis=list(title="Full Time Study Rate in %"))
        
        
        
        
        
      })
      
      
      
    }
    
    
  })
  
  
  
  
  
  
  
}
shinyApp(ui,server)