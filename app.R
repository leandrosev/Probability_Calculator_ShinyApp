library(shiny)
library(bslib)

# Probability Calcuator Shiny App
# Author: Evangelidakis Leandros
# Contact: leandrosevag@hotmail.com

ui <- navbarPage("Probability Calculator",
                 
    theme = bs_theme(version=3,bootswatch = "flatly"),
    navbarMenu("Discrete Distributions",
    tabPanel("Binomial",
      fluidRow(
          h2("Binomial Distribution"),
          withMathJax(),
          helpText('$$X \\sim {\\sf Binom}(n, p)$$'),
          helpText(' $$P[X=x]= \\binom{n}{x}p^k(1-p)^{n-x}$$ '),
          helpText('$$E[X]=np, Var[X]=npq$$'),
          ),
          sidebarLayout(
              sidebarPanel(
                  numericInput("binom_n",
                              "Number of trials (n)",
                              min = 1,
                              max = 1000,
                              value=20)
              ,
              numericInput("binom_x",
                           "Number of successfull outcomes (x)",
                           min = 0,
                           max = 1000,
                           value=5),
              
              numericInput("binom_p",
                           "Probability of success (p)",
                           min = 0,
                           max = 1,
                           value=0.5),
              radioButtons("binom_checkbox","Options",choices =list("Single Value, P[X=x]"="1",
                                                                    "Cumulative probability P[X<=x]"="2"),
                                                                    selected="1"),
              actionButton("binom_btn","Calculate",class = "btn-success")
          )
          ,
          mainPanel(
              verbatimTextOutput("binomial_prob",placeholder = T),
              plotOutput(outputId = "binom_plot")
            
          )
        )
    ),
    
#----------------------------------------------------------------------------------------------------------------------   
  tabPanel("Geometric",
    fluidRow(
      h2("Geometric Distribution"),
      withMathJax(),
      helpText('$$X \\sim {\\sf Geom}(p)$$'),
      helpText('$$P[X=x]=(1-p)^{x}p$$'),
      helpText('$$E[X]=\\frac{1}{p}, Var[X]=\\frac{1-p}{p}$$')
      ),
      sidebarLayout(
        sidebarPanel(
          numericInput("geom_x",
                       "Number of trials until success (x)",
                       min = 1,
                       max = 1000,
                       value=20)
          ,
          numericInput("geom_p",
                       "Probability of success (p)",
                       min = 0,
                       max = 1,
                       value=0.5),
          radioButtons("geom_checkbox","Options",choices = list("Single Value, P[X=x]"="1",
                                                                 "Cumulative probability P[X<=x]"="2"),
                                                                  selected="1"),
          actionButton("geom_btn","Calculate",class = "btn-success")
        ),
        mainPanel(
            verbatimTextOutput("geom_prob",placeholder = T),
            plotOutput(outputId = "geom_plot")
          )
        
      )
  )  ,
  
#----------------------------------------------------------------------------------------------------------------------
  tabPanel("Negative Binomial ",
           fluidRow(
             h2("Negative Binomial Distribution"),
             withMathJax(),
             helpText('$$X \\sim {\\sf NB}(r,p)$$'),
             helpText('$$P[X=k]=\\binom{k+r-1}{r-1}(1-p)^{k}p^r$$'),
             helpText('$$E[X]=\\frac{pr}{1-p}, Var[X]=\\frac{pr}{(1-p)^2}$$')
             ),
             sidebarLayout(
               sidebarPanel(
                 numericInput("neg_binom_k",
                              "Number of successes (k)",
                              min = 0,
                              max = 1000,
                              value=5)
                 ,
                 numericInput("neg_binom_r",
                              "Number of failures (r)",
                              min = 1,
                              max = 1000,
                              value=10)
                 ,
                 numericInput("neg_binom_p",
                              "Probability of success (p)",
                              min = 0,
                              max = 1,
                              value=0.6),
                 radioButtons("neg_binom_checkbox","Options",choices = list("Single Value, P[X=k]"="1",
                                                                       "Cumulative probability P[X<=k]"="2"),
                              selected="1"),
                 actionButton("neg_binom_btn","Calculate",class = "btn-success")
               ),
              mainPanel(
                 verbatimTextOutput("neg_binom_prob",placeholder = T),
                 plotOutput(outputId = "neg_binom_plot")
             )
           )
  ) ,
#----------------------------------------------------------------------------------------------------------------------
  
  tabPanel("Poisson ",
           fluidRow(
             h2("Poisson Distribution"),
             withMathJax(),
             helpText('$$X \\sim {\\sf Poisson}(\\lambda)$$'),
             helpText('$$P[X=x]=\\frac{\\lambda^xe^{-\\lambda}}{x!}$$'),
             helpText('$$E[X]=\\lambda, Var[X]=\\lambda$$')
             ),
             sidebarLayout(
               sidebarPanel(
                 numericInput("poisson_x",
                              "Number of occurrences  (x)",
                              min = 0,
                              max = 1000,
                              value=5)
                 ,
                 numericInput("poisson_l",
                              "Parameter (l)",
                              min = 0.0000001,
                              max = 1000,
                              value=10),
                 
                 radioButtons("poisson_checkbox","Options",choices = list("Single Value, P[X=x]"="1",
                                                                            "Cumulative probability P[X<=x]"="2"),
                                                                            selected="1"),
                 actionButton("poisson_btn","Calculate",class = "btn-success")
               ),
               mainPanel(
                 verbatimTextOutput("poisson_prob",placeholder = T),
                 plotOutput(outputId = "poisson_plot")
               )
             )
  ) , 
#----------------------------------------------------------------------------------------------------------------------  
  
  tabPanel("Hypergeometric  ",
           fluidRow(
             h2("Hypergeometric  Distribution"),
             withMathJax(),
             helpText('$$X \\sim {\\sf Hypergeometric}(N,K,n)$$'),
             helpText('$$P[X=k]=\\frac{\\binom{K}{k}\\binom{N-K}{n-k}}{\\binom{N}{n}}$$'),
             helpText('$$E[X]=n\\frac{K}{N}, Var[X]=n\\frac{K}{N}\\frac{N-K}{N}\\frac{N-n}{N-1}$$'),
           ),  
             sidebarLayout(
               sidebarPanel(
                 numericInput("hgeom_N",
                              "Population size  (N)",
                              min = 0,
                              max = 1000,
                              value=500)
                 ,
                 numericInput("hgeom_K",
                              "Number of success states in population (K)",
                              min = 0,
                              max = 1000,
                              value=50)
                 ,
                 numericInput("hgeom_n",
                              "Number of quantities drawn in each trial (n)",
                              min = 0,
                              max = 1000,
                              value=100)
                 ,
                 numericInput("hgeom_k",
                              "Number of observed successes (k)",
                              min = 0,
                              max = 1000,
                              value=20),
                 radioButtons("hgeom_checkbox","Options",choices = list("Single Value, P[X=x]"="1",
                                                                          "Cumulative probability P[X<=x]"="2"),
                              selected="1"),
                 actionButton("hgeom_btn","Calculate",class = "btn-success")
               ),
               mainPanel(
                 verbatimTextOutput("hgeom_prob",placeholder = T),
                 plotOutput(outputId = "hgeom_plot")
               )
             )
  )),   
#----------------------------------------------------------------------------------------------------------------------    
navbarMenu("Continuous Distributions",
tabPanel("Normal",
         fluidRow(
           h2("Normal  Distribution"),
           withMathJax(),
           helpText('$$X \\sim {\\sf N}(\\mu,\\sigma^2)$$'),
           helpText('$$f(x)=\\frac{1}{\\sqrt{2\\pi\\sigma^2}}e^{-{\\frac{(x-\\mu)^2}{2\\sigma^2}}}$$'),
           helpText('$$E[X]=\\mu, Var[X]=\\sigma^2$$'),
         ),  
         sidebarLayout(
           sidebarPanel(
             numericInput("normal_x",
                          "Input value  (x)",
                          min = 0,
                          max = 1000,
                          value=175)
             ,
             numericInput("normal_mu",
                          "Mean  (mu)",
                          min = 0,
                          max = 1000,
                          value=180)
             ,
             numericInput("normal_sigma",
                          "Standard Deviation (sigma)",
                          min = 0.0001,
                          max = 1000,
                          value=20)
             ,

             radioButtons("normal_checkbox","Options",choices = list("Density function, f(x)"="1",
                                                                     "Cumulative probability P[X<=x]"="2"),
                          selected="1"),
             actionButton("normal_btn","Calculate",class = "btn-success")
           ),
           mainPanel(
             verbatimTextOutput("normal_prob",placeholder = T),
             plotOutput(outputId = "normal_plot")
           )
         )
)   ,
#----------------------------------------------------------------------------------------------------------------------    

tabPanel("Exponential",
         fluidRow(
           h2("Exponential  Distribution"),
           withMathJax(),
           helpText('$$X \\sim {\\sf Exp}(\\lambda)$$'),
           helpText('$$f(x)=\\lambda e^{-\\lambda x}$$'),
           helpText('$$E[X]=\\frac{1}{\\lambda}, Var[X]=\\frac{1}{\\lambda^2}$$'),
         ),  
         sidebarLayout(
           sidebarPanel(
             numericInput("exp_x",
                          "Input value  (x)",
                          min = 0,
                          max = 100000,
                          value= 175)
             ,
             numericInput("exp_lambda",
                          "Parameter  (l)",
                          min = 0.0001,
                          max = 100000,
                          value= 0.2)
             ,

             radioButtons("exp_checkbox","Options",choices = list("Density function, f(x)"="1",
                                                                  "Cumulative probability P[X<=x]"="2"),
                          selected="1"),
             actionButton("exp_btn","Calculate",class = "btn-success")
           ),
           mainPanel(
             verbatimTextOutput("exp_prob",placeholder = T),
             plotOutput(outputId = "exp_plot")
           )
         )
),   

#----------------------------------------------------------------------------------------------------------------------    

tabPanel("Uniform",
         fluidRow(
           h2("Uniform  Distribution"),
           withMathJax(),
           helpText('$$X \\sim {\\sf U}(\\alpha,\\beta)$$'),
           helpText('$$f(x)=\\frac{1}{b-a}, a<x<b$$'),
           helpText('$$E[X]=\\frac{1}{2}, Var[X]=\\frac{1}{12}$$'),
         ),  
         sidebarLayout(
           sidebarPanel(
             numericInput("uniform_x",
                          "Input value  (x)",
                          min = 0,
                          max = 100000,
                          value= 50)
             ,
             numericInput("uniform_a",
                          "Parameter  (a)",
                          min = 0,
                          max = 100000,
                          value= 20)
             ,
             numericInput("uniform_b",
                          "Parameter  (b)",
                          min = 0.0001,
                          max = 100000,
                          value= 60)
             ,           
             radioButtons("uniform_checkbox","Options",choices = list("Density function, f(x)"="1",
                                                                      "Cumulative probability P[X<=x]"="2"),
                          selected="1"),
             actionButton("uniform_btn","Calculate",class = "btn-success")
           ),
           mainPanel(
             verbatimTextOutput("uniform_prob",placeholder = T),
             plotOutput(outputId = "uniform_plot")
           )
         )
)   ,

tabPanel("Gamma",
         fluidRow(
           h2("Gamma  Distribution"),
           withMathJax(),
           helpText('$$X \\sim {\\sf Gamma}(k,\\theta)$$'),
           helpText('$$f(x)=\\frac{x^{k-1}e^{-x/\\theta}}{\\theta^k \\Gamma(k)}$$'),
           helpText('$$E[X]=k\\theta, Var[X]=k\\theta^2$$'),
         ),  
         sidebarLayout(
           sidebarPanel(
             numericInput("gamma_x",
                          "Input value  (x)",
                          min = 0.000001,
                          max = 100000,
                          value= 4)
             ,
             numericInput("gamma_shape",
                          "Shape Parameter  (k)",
                          min = 0.000001,
                          max = 100000,
                          value= 9)
             ,
             numericInput("gamma_scale",
                          "Scale Parameter  (theta)",
                          min = 0.000001,
                          max = 100000,
                          value= 0.5)
             ,           
             radioButtons("gamma_checkbox","Options",choices = list("Density function, f(x)"="1",
                                                                    "Cumulative probability P[X<=x]"="2"),
                          selected="1"),
             actionButton("gamma_btn","Calculate",class = "btn-success")
           ),
           mainPanel(
             verbatimTextOutput("gamma_prob",placeholder = T),
             plotOutput(outputId = "gamma_plot")
           )
         )
 )   
)



#----------------------------------------------------------------------------------------------------------------------    
)

server <- function(input, output) {

    binomial_prob <- eventReactive(input$binom_btn,
                            switch( input$binom_checkbox,
                                "1" = paste("P[X=",input$binom_x,'] = ',dbinom(input$binom_x,size=input$binom_n,prob=input$binom_p),sep = ""),
                                "2" = paste("P[X<=",input$binom_x,'] = ',pbinom(input$binom_x,size=input$binom_n,prob=input$binom_p),sep = "")
                                )
    )
  
    output$binomial_prob <- renderText({
      binomial_prob()
    })
    
    binom_plot <- reactive({
      plot(seq(0,input$binom_n,by = 1),dbinom(seq(0,input$binom_n,by = 1),input$binom_n,input$binom_p),
                                              col='red',type='b',pch=20,main='Binomial Distribution',
                                              xlab='Number of successes', ylab='Probability'
                                              )
      grid()
    })
    

    output$binom_plot <- renderPlot({
      binom_plot()
        
    })
    
    
#----------------------------------------------------------------------------------------------------------------------
    
    
    geom_prob <- eventReactive(input$geom_btn,
                               switch (input$geom_checkbox,
                                 "1" = paste("P[X=" ,input$geom_x,'] = ',dgeom(input$geom_x,prob=input$geom_p),sep = ""),
                                 "2" = paste("P[X<=",input$geom_x,'] = ',pgeom(input$geom_x,prob=input$geom_p),sep = "")
                               )
    )
    output$geom_prob <- renderText({
      geom_prob()
    })
    
    geom_plot <- reactive({
      plot(seq(0,input$geom_x,by = 1),dgeom(seq(0,input$geom_x,by = 1),input$binom_p),
           col='red',type='b',pch=20,main='Geometric Distribution',
           xlab='Number of trials until success', ylab='Probability'
      )
      grid()
    })
    
    
    output$geom_plot <- renderPlot({
      geom_plot()
      
    })
    
#----------------------------------------------------------------------------------------------------------------------
    neg_binom_prob <- eventReactive(input$neg_binom_btn,
                               switch (input$neg_binom_checkbox,
                                       "1" = paste("P[X=" ,input$neg_binom_k,'] = ',dnbinom(input$neg_binom_k,size=input$neg_binom_r,prob=input$neg_binom_p),sep = ""),
                                       "2" = paste("P[X<=",input$neg_binom_k,'] = ',pnbinom(input$neg_binom_k,size=input$neg_binom_r,prob=input$neg_binom_p),sep = "")
                               )
    )
    output$neg_binom_prob <- renderText({
      neg_binom_prob()
    })
    
    
    neg_binom_plot <- reactive({
      plot(seq(0,input$neg_binom_k,by = 1),dnbinom(seq(0,input$neg_binom_k,by = 1),input$neg_binom_r,input$neg_binom_p),
           col='red',type='b',pch=20,main='Negative Binomial Distribution',
           xlab='Number of successes', ylab='Probability'
      )
      grid()
    })
    
    
    output$neg_binom_plot <- renderPlot({
      neg_binom_plot()
      
    })
    
#----------------------------------------------------------------------------------------------------------------------
    
    poisson_prob <- eventReactive(input$poisson_btn,
                                    switch (input$poisson_checkbox,
                                            "1" = paste("P[X=" ,input$poisson_x,'] = ',ppois(input$poisson_x,lambda=input$poisson_l),sep = ""),
                                            "2" = paste("P[X<=",input$poisson_x,'] = ',dpois(input$poisson_x,lambda=input$poisson_l),sep = "")
                                    )
    )
    output$poisson_prob <- renderText({
      poisson_prob()
    })
    
    poisson_plot <- reactive({
      plot(seq(0,input$poisson_x,by = 1),ppois(seq(0,input$poisson_x,by = 1),input$poisson_l),
           col='red',type='b',pch=20,main='Poisson Distribution',
           xlab='Number of occurrences', ylab='Probability'
      )
      grid()
    })
    
    
    output$poisson_plot <- renderPlot({
      poisson_plot()
      
    })
#----------------------------------------------------------------------------------------------------------------------

    
    hgeom_prob <- eventReactive(input$hgeom_btn,
                                  switch (input$hgeom_checkbox,
                                          "1" = paste("P[X=" ,input$hgeom_k,'] = ',dhyper(input$hgeom_k,m=input$hgeom_K,n=input$hgeom_N-input$hgeom_K,k=input$hgeom_n),sep = ""),
                                          "2" = paste("P[X<=",input$hgeom_k,'] = ',phyper(input$hgeom_k,m=input$hgeom_K,n=input$hgeom_N-input$hgeom_K,k=input$hgeom_n),sep = "")
                                  )
    )
    output$hgeom_prob <- renderText({
      hgeom_prob()
    })
    
    hgeom_plot <- reactive({
      plot(seq(0,input$hgeom_k,by = 1),dhyper(seq(0,input$hgeom_k,by = 1),m=input$hgeom_K,n=input$hgeom_N-input$hgeom_K,k=input$hgeom_n),
           col='red',type='b',pch=20,main='Hypergeometric Distribution',
           xlab='Number of occurrences', ylab='Probability'
      )
      grid()
    })
    
    
    output$hgeom_plot <- renderPlot({
      hgeom_plot()
      
    })    
    
#----------------------------------------------------------------------------------------------------------------------        
    
    normal_prob <- eventReactive(input$normal_btn,
                                switch (input$normal_checkbox,
                                        "1" = paste("f(" ,input$normal_x,') = ',dnorm(x=input$normal_x,mean=input$normal_mu,sd=input$normal_sigma),sep = ""),
                                        "2" = paste("P[X<=",input$normal_x,'] = ',pnorm(input$normal_x,mean=input$normal_mu,sd=input$normal_sigma),sep = "")
                                ))
    
    output$normal_prob <- renderText({
      normal_prob()
    })
    
    normal_plot <- reactive({
      plot(seq(input$normal_mu-4*input$normal_sigma,input$normal_mu+4*input$normal_sigma,by = .1),dnorm(seq(input$normal_mu-4*input$normal_sigma,input$normal_mu+4*input$normal_sigma,by = .1),mean=input$normal_mu,sd=input$normal_sigma),
           col='red',type='b',pch=20,main='Normal Distribution',
           xlab='x', ylab='f(x)'
      )
      grid()
    })
    
    
    output$normal_plot <- renderPlot({
      normal_plot()
      
    })        
    
#----------------------------------------------------------------------------------------------------------------------    
    
    exp_prob <- eventReactive(input$exp_btn,
                                 switch (input$exp_checkbox,
                                         "1" = paste("f(" ,input$exp_x,') = ',dexp(x=input$exp_x,rate=input$exp_lambda),sep = ""),
                                         "2" = paste("P[X<=",input$exp_x,'] = ',pexp(q=input$exp_x,rate=input$exp_lambda),sep = "")
    ))
    
    output$exp_prob <- renderText({
      exp_prob()
    })
    
    exp_plot <- reactive({
      plot(seq(0,input$exp_x,by = .1),dexp(seq(0,input$exp_x,by = .1),rate=input$exp_lambda),
           col='red',type='b',pch=20,main='Exponential Distribution',
           xlab='x', ylab='f(x)'
      )
      grid()
    })
    
    
    output$exp_plot <- renderPlot({
      exp_plot()
      
    })        
    
#----------------------------------------------------------------------------------------------------------------------   
    uniform_prob <- eventReactive(input$uniform_btn,
                              switch (input$uniform_checkbox,
                                      "1" = paste("f(" ,input$uniform_x,') = ',dunif(input$uniform_x,input$uniform_a,input$uniform_b),sep = ""),
                                      "2" = paste("P[X<=",input$uniform_x,'] = ',punif(input$uniform_x,input$uniform_a,input$uniform_b),sep = "")
                              ))
    
    output$uniform_prob <- renderText({
      uniform_prob()
    })
    
    uniform_plot <- reactive({
      plot(seq(0,2*input$uniform_b,by = .5),dunif(seq(0,2*input$uniform_b,by = .5),input$uniform_a,input$uniform_b),
           col='red',type='b',pch=20,main='Uniform Distribution',
           xlab='x', ylab='f(x)'
      )
      grid()
    })
    
    
    output$uniform_plot <- renderPlot({
      uniform_plot()
      
    })    
    

#----------------------------------------------------------------------------------------------------------------------   
    gamma_prob <- eventReactive(input$gamma_btn,
                                  switch (input$gamma_checkbox,
                                          "1" = paste("f(" ,input$gamma_x,') = ',dgamma(input$gamma_x,shape=input$gamma_shape,scale=input$gamma_scale),sep = ""),
                                          "2" = paste("P[X<=",input$gamma_x,'] = ',pgamma(input$gamma_x,shape=input$gamma_shape,scale=input$gamma_scale),sep = "")
                                  ))
    
    output$gamma_prob <- renderText({
      gamma_prob()
    })
    
    gamma_plot <- reactive({
      plot(seq(0,2*input$gamma_x,by = .5),dgamma(seq(0,2*input$gamma_x,by = .5),shape=input$gamma_shape,scale=input$gamma_scale),
           col='red',type='b',pch=20,main='Gamma Distribution',
           xlab='x', ylab='f(x)'
      )
      grid()
    })
    
    
    output$gamma_plot <- renderPlot({
      gamma_plot()
      
    }) 
    
#----------------------------------------------------------------------------------------------------------------------       
}



shinyApp(ui = ui, server = server)
