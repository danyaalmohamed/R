## this is for illustration purposes with randomly chosen defaults

library(shiny)

ui<-fluidPage(
  titlePanel("Waiting List Volume"),
  hr(style="border-color: grey;"),
  sidebarLayout(
    # panel with all inputs
    sidebarPanel(selectInput("fixed","System type",choices=c("Fixed","Random")),
                 conditionalPanel(condition = "input.fixed == 'Random'",
                                  numericInput("seed","Set seed",0)),
                 numericInput("mean","Weekly arrivals",1000, step = 100),
                 conditionalPanel(condition = "input.fixed == 'Random'",
                                  numericInput("sda","Standard deviation",50, step = 10)),
                 numericInput("apps","Weekly appointments",900,step = 100),
                 conditionalPanel(condition = "input.fixed == 'Random'",
                                  numericInput("sdt","Standard deviation",50, step = 10)),
                 numericInput("dna","DNA rate %",20,min=0,max=100,step=5),
                 helpText("Patients are treated in turn, unless they DNA"),
                 hr(style="border-color: grey;"),
                 # start / reset buttons on same row
                 fluidRow(
                   column(7,uiOutput("resetbutton")),
                   column(5,uiOutput("startbutton"))
                 ),
                 hr(style="border-color: grey;"),
                 fluidRow(
                   column(7,numericInput("skipnum",NA,10)),
                   column(5,actionButton("skip","Skip"))
                 ),
                 fluidRow(
                   column(7,actionButton("stop","Stop")),
                   column(3,actionButton("play","Play"))
                 )
    ),
    # plot panel
    mainPanel(
      
      # tab layout
      tabsetPanel(
        
        tabPanel("Distribution",plotOutput('mygraph')),
        tabPanel("Summary",
                 
                 # plots on same row
                 fluidRow(
                   
                   column(6,
                          
                          plotOutput('voltrack')
                   ),
                   column(6,
                          plotOutput('pie')  
                   )
                   
                 )
                 
        )
      ),
      
      # visual data on same row
      fluidRow(
        column(3,textOutput("weekcount")),
        column(4,textOutput("volume")),
        column(4,textOutput("over"))
      )
      
    )
  )
  
)

server<-function(input,output){
  
  waits <- reactiveValues() # reactive to store all reactive variables
  waits$resetindicator<-0   # used to change button labels
  waits$data <- rep(0,52)   # initial waitlist profile
  waits$count<-0            # week number
  waits$vol<-0              # current week volume
  waits$trackvol<-rep(0,20) # volume for last 20 weeks
  waits$xdates<-rep(NA,20)  # x axis for volume graph
  waits$past52<-0           # number of expired
  waits$slices<-c(0,0,0)    # pie chart value
  
  
  
  # dynamic reset button label
  output$resetbutton<-renderUI({
    if(waits$resetindicator==0){
      lbl<-"Set Parameters"
    }else{
      lbl<-"Reset"
    }
    actionButton("reset",label=lbl)
  })
  
  # dynamic start button label
  output$startbutton<-renderUI({
    if(sum(waits$data)==0){
      lbl<-"Start"
    }else{
      lbl<-"Next Week"
    }
    actionButton("nextweek",label=lbl)
  })
  
  forward<-function(){
    
    req(input$seed)
    req(input$mean)
    req(input$sda)
    req(input$apps)
    req(input$sdt)
    req(input$dna)
    
    waits$resetindicator<-1 # change button label
    
    
    # set number of appointments
    if(input$fixed=="Random"){
      canremove<-max(round(rnorm(1,mean=input$apps,sd=input$sdt)),0)
    }else{
      canremove<-input$apps
    }
    
    coltarget<-length(waits$data) # column to remove patients from
    while(canremove>0){
      colremovesize<-ceiling((1-(0.01*input$dna))*waits$data[coltarget]) # how many can be removed from column
      removals<-min(canremove,colremovesize)
      waits$data[coltarget]<-waits$data[coltarget]-removals
      canremove<-canremove-removals # update number of appointments remaining
      coltarget<-coltarget-1 # target next column
    }
    
    waits$past52<-waits$past52+waits$data[52] # update expired vector
    
    # set number of new arrivals
    if(input$fixed=="Random"){
      newwait<-max(round(rnorm(1,mean=input$mean,sd=input$sda)),0)
    }else{
      newwait<-input$mean
    }
    
    waits$data <- head(c(newwait,waits$data),52) # append new arrivals to start
    waits$count<-waits$count+1 # update week number
    
    waits$slices<-c(sum(waits$data[1:25]),sum(waits$data[26:35]),sum(waits$data[36:52])) # values for pie chart
    waits$vol<-sum(waits$data) # update volume
    waits$trackvol<-tail(c(waits$trackvol,waits$vol),20) # vector of volumes for last 20 weeks
    waits$xdates<-tail(c(waits$xdates,waits$count),20)
  }
  
  
  ## when nextweek button is pressed
  observeEvent(input$nextweek,{
    forward()
  })
  
  observeEvent(input$skip,{
    for (fwd in 1:input$skipnum){
      forward()
    }
  })
  
  session<-reactiveValues()
  session$timer<-reactiveTimer(Inf)
  
  observeEvent(input$play,{
    session$timer<-reactiveTimer(300)
    observeEvent(session$timer(),{
      forward()
    })
  })
  
  
  observeEvent(input$stop,{
    session$timer<-reactiveTimer(Inf)
  })
  
  
  
  
  
  
  ## when reset button is pressed (set everything to original values, plus set seed)
  observeEvent(input$reset,{
    
    waits$resetindicator<-0
    
    set.seed(input$seed) # seed set here for now as it has to be a reactive environment
    waits$past52<-0
    waits$data<-rep(0,52)
    waits$count<-0
    waits$vol<-0
    waits$trackvol<-rep(0,20)
    waits$xdates<-rep(NA,20)
    waits$slices<-c(0,0,0)
  })
  
  ## main plot output
  output$mygraph<-renderPlot({
    if(sum(waits$data)==0){
      return() # no plot if reset
    }
    barcolours<-c(rep("green3",26),rep("yellow2",10),rep("red2",16))
    bp<-barplot(waits$data,names.arg=1:52,xlab="Weeks Wait",ylab="Volume",col=barcolours,space=0,width=1)
    bp
    abline(v=26)
    abline(v=36)
  })
  
  ## visual data outputs
  output$weekcount<-renderText({
    paste("Week number",waits$count)
  })
  output$over<-renderText({
    paste("Expired",waits$past52)
  })
  output$volume<-renderText({
    paste("Waitlist volume",waits$vol)
  })
  
  ## pie plot output
  output$pie<-renderPlot({
    if(sum(waits$slices)==0){
      return() # dont error if no data
    }
    par(xpd=TRUE) # allow legend outside plot margins
    pie(waits$slices,labels = waits$slices,col=c("green3","yellow2","red2"))
    legend(-1.3,-0.5,legend=c("<26","26 to 25","36 +"),fill=c("green3","yellow2","red2"),bty='n',cex=0.8)
  })
  
  output$voltrack<-renderPlot({
    ytop<-max(waits$trackvol)+100 # upper limit for y axis
    plot(waits$trackvol,type='l',ylab="Waitlist Volume",xaxt='n',xlab="Week Number",bty='n',lwd=1.5,ylim=c(0,ytop),main="20 Week Volume")
    points(20,tail(waits$trackvol,1),pch=21,bg="blue")
    axis(1,at=1:20,labels = waits$xdates)
  })
  
}

runApp(shinyApp(ui,server),launch.browser = TRUE)
