#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App" above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# install.packages("shinydashboard")
# Define UI for application that draws a histogram


library(shinydashboard)
library(shinyAce)
#library(shinyjs)
library(rgl)
library(plotly)


dashboardPage(
  ##############################################################################################  
  skin = "green",
  
  dashboardHeader(title = "Handbook of Regression Methods",
                  titleWidth = 250,
                  # ###############################################################
                  # ####################### Drop Down Menu ########################
                  # ###############################################################
                  # dropdownMenu(type = "tasks", badgeStatus = "success",
                  #              taskItem(value = 50, color = "green",
                  #                       "Study Progress"
                  #              )
                  # ),
                  # 
                  ###############################################################
                  ###############################################################
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Support",
                                 message = "Welcome to our Study Community",
                                 icon = icon("life-ring"),
                                 time = Sys.time(),
                                 href="mailto:kedai.cheng@uky.edu?Subject=HoRM App Question"
                               )
                  )
  ),
  ##############################################################################################  
  
  
  
  
  ###############################################################
  #################### Sider Bar ################################
  ###############################################################
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(id="sbmenu",
                ##############################################
                ############### Welcome Page #################
                ##############################################
                menuItem("Welcome!", icon = icon("list-alt"),href="https://horm.as.uky.edu/intro/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 2 #################
                ##############################################
                menuItem("Chapter 2", tabName = "chapter_2", icon = icon("list-alt"), startExpanded = TRUE,
                         
                         menuSubItem("Introduction", tabName = "02intro",selected=TRUE),
                         menuSubItem("Section 2.8", tabName = "0208")
                ),
                
                ##############################################
                ############# Tab: Chapter 3 #################
                ##############################################
                menuItem("Chapter 3", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch03/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 4 #################
                ##############################################
                menuItem("Chapter 4", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch04/" ,newtab = F),
                
                
                ##############################################
                ############# Tab: Chapter 5 #################
                ##############################################
                menuItem("Chapter 5", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch05/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 6 #################
                ##############################################
                menuItem("Chapter 6", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch06/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 7 #################
                ##############################################
                menuItem("Chapter 7", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch07/" ,newtab = F),
                ##############################################
                ############# Tab: Chapter 8 #################
                ##############################################
                menuItem("Chapter 8", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch08/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 9 #################
                ##############################################
                menuItem("Chapter 9", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch09/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 10 ################
                ##############################################
                menuItem("Chapter 10", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch10/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 11 ################
                ##############################################
                menuItem("Chapter 11", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch11/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 12 ################
                ##############################################
                menuItem("Chapter 12", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch12/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 13 ################
                ##############################################
                menuItem("Chapter 13", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch13/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 14 ################
                ##############################################
                menuItem("Chapter 14", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch14/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 15 ################
                ##############################################
                menuItem("Chapter 15", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch15/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 16 ################
                ##############################################
                menuItem("Chapter 16", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch16/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 17 ################
                ##############################################
                menuItem("Chapter 17", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch17/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 18 ################
                ##############################################
                menuItem("Chapter 18", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch18/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 19 ################
                ##############################################
                menuItem("Chapter 19", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch19/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 20 ################
                ##############################################
                menuItem("Chapter 20", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch20/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 21 ################
                ##############################################
                menuItem("Chapter 21", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch21/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 22 ################
                ##############################################
                menuItem("Chapter 22", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch22/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 23 ################
                ##############################################
                menuItem("Chapter 23", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch23/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 24 ################
                ##############################################
                menuItem("Chapter 24", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch24/" ,newtab = F),
                
                ##############################################
                ############# Tab: Chapter 25 ################
                ##############################################
                menuItem("Chapter 25", icon = icon("list-alt"),href="https://horm.as.uky.edu/Ch25/" ,newtab = F)
                
                ############## End of Sider Bar ###############      
    )
  ),
  
  #####################################################################################################  
  ########################## Details for Each Chapter Tab #############################################
  #####################################################################################################
  
  dashboardBody(
    
    ################################################################################################
    ################################### CSS Font ###################################################
    tags$head(tags$style(HTML("
                              .main-header .logo {
                              font-family: 'Georgia', Times, 'Times New Roman', serif;
                              font-weight: bold;
                              font-size: 14px;
                              }
                              "))),
    ##############################################################################################
    ###############################################################################################
    tabItems(
      ########################################################################
      ###################### Tab: Introduction ###############################
      ########################################################################
      tabItem(tabName = '02intro',
              fluidRow(
                tabBox(
                  title = (tagList(shiny :: icon('list-alt'), "Chapter 2: The Basics of Regression Models")),
                  id = "chapter_intro" , height = "500px" , width = "50px",
                  tabPanel("Introduction",
                           fluidRow(
                             box(status = "primary", solidHeader = TRUE,height = 500, width = 12,
                                 p("Chapter 2 of the handbook focuses on some standard statistical terminology. The simple linear regression model is introduced along with the notation used for the parameters of interest and the corresponding estimates.  A discussion of ordinary least squares is provided, including how the ordinary least squares estimates are derived.  Measures of overall variation from the simple linear regression line are presented, including sums of squares quantities and mean squared quantities.  These quantities are then used when defining the coefficient of determination.    The regression through the origin model is discussed, which is used when one assumes that the intercept term is set equal to 0.  An overview of correlation measures is provided, including how correlation is distinguished from regression.  Finally, this chapter concludes with a discussion of the regression effect and the regression fallacy.",br(),
                                   br(),
                                   "R packages used in this chapter’s examples:",br(),
                                   br(),
                                   a("aprean3",     href="https://cran.r-project.org/web/packages/aprean3/index.html",target="_blank"),br(),
                                   a("DescTools",     href="https://cran.r-project.org/web/packages/DescTools/index.html",target="_blank"),br(),
                                   a("ggplot2",     href="https://cran.r-project.org/web/packages/ggplot2/index.html",target="_blank"),br(),
                                   a("Hmisc",     href="https://cran.r-project.org/web/packages/Hmisc/index.html",target="_blank"),br(),
                                   a("HoRM",     href="https://cran.r-project.org/web/packages/HoRM/index.html",target="_blank")

                                   )
                             )
                           )
                  )
                ))),
      
      
      ###############################################
      ############### Section 02.08 #################
      ###############################################
      
      tabItem(tabName = "0208",
              fluidRow(
                tabBox(
                  title = (tagList(shiny :: icon("list-alt"), "Chapter 2 || Section 8")),
                  id = "chapter_2.8" , height = "500px" , width = "50px",
                  
                  ################################################
                  ########### Tabs for Chapter 2.8 ###############
                  ################################################
                  tabPanel("2.8.1 || Toy Data",
                           fluidRow(
                             column(width = 6,
                                    ######################### 
                                    box(status = "warning", solidHeader = T, title = a("Regression Results (Get R Code)", href="2.1.R",target="_blank"), 
                                        width = 12, height = 800 , collapsible = F,collapsed=F,
                                        #####################
                                        tabsetPanel(
                                          tabPanel("Summary", aceEditor("result020801",mode="r",theme="tomorrow",font=16,height=650,
                                                                      value="> out$fitted
1  2  3  4  5 
13 13 19 25 25 

##########################################################

> out$res
1             2             3             4            5
2.000000e+00 -2.000000e+00  4.440892e-16 -4.000000e+00 4.000000e+00"
))
                                          )
                                          )
                                    ),
                             ##########################
                             box(status = "warning", solidHeader = TRUE ,title = a("Data Plots (Get R Code)", href="2.1.R",target="_blank"),
                                 height=800, width = 6, collapsible = F,collapsed = F,
                                 ############################
                                 tabsetPanel(
                                   tabPanel(plotlyOutput("plot020801" , height = 650), title = "Toy Data")
                                 ))
                             
                             
#                              box(status = "warning", solidHeader = T, title = "R Code", width = 6, height = 820 , collapsible = F, collapsed=F,
#                                  aceEditor("code020801",mode="r",theme="tomorrow_night",font=16,height=745,
#                                            value="data(toy, package = 'HoRM')
# 
# out <- lm(y ~ x, data = toy)
# out$fitted
# out$res
# 
# ggplot(toy, aes(x = x, y = y)) + geom_point(size = 3) + geom_smooth(method = lm, 
#     se = FALSE) + ggtitle('Toy Data') + theme(text = element_text(size = 20))")
#                                  )
                             
                                 )
                                 ),
                  
                  ####################################################
                  
                  tabPanel("2.8.2 || Steam Output Data",
                           fluidRow(
                             column(width = 6,
                                    ######################### 
                                    box(status = "warning", solidHeader = T, title = a("Regression Results (Get R Code)", href="2.2.R",target="_blank"),
                                        width = 12, height = 800 , collapsible = F,collapsed=F,
                                        #######################
                                        tabsetPanel(
                                          tabPanel("Summary", aceEditor("result020802",mode="r",theme="tomorrow",font=16,height=650,
                                                                        value="> summary(out)
                                                                        
Call:
lm(formula = steam ~ temp)

Residuals:
  Min      1Q    Median    3Q     Max 
-1.6789 -0.5291 -0.1221  0.7988  1.3457 

Coefficients:
            Estimate  Std. Error  t value Pr(>|t|)    
(Intercept) 13.62299    0.58146  23.429  < 2e-16 ***
temp        -0.07983    0.01052  -7.586 1.05e-07 ***
---
Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1

Residual standard error: 0.8901 on 23 degrees of freedom
Multiple R-squared:  0.7144,	Adjusted R-squared:  0.702 
F-statistic: 57.54 on 1 and 23 DF,  p-value: 1.055e-07

#####################################################################

> sqrt(summary(out)$r.squared)
[1] 0.8452441"))
                                          )
                                          )
                                    ),
                             ###########################
                             box(status = "warning", solidHeader = TRUE ,title = a("Data Plots (Get R Code)", href="2.2.R",target="_blank"),
                                 height=800, width = 6, collapsible = F,collapsed = F,
                                 ########################
                                 tabsetPanel(
                                   tabPanel(plotlyOutput("plot020802" , height = 650), title = "Steam Output Data")
                                 ))
                             
                             
#                              box(status = "warning", solidHeader = T, title = "R Code", width = 6, height = 820 , collapsible = F, collapsed=F,
#                                  aceEditor("code020802",mode="r",theme="tomorrow_night",font=16,height=745,
#                                            value="data(dsa01a, package = 'aprean3')
# 
# ggplot(dsa01a, aes(x = x8, y = x1)) + geom_point(size = 3) + geom_smooth(method = lm, 
#     se = FALSE) + ggtitle('Steam Output Data') + theme(text = element_text(size = 20)) + 
#     xlab(expression(paste('Temperature (', degree, 'F)', sep = ''))) + ylab('Steam Usage (Monthly)')
# 
# temp <- dsa01a$x8
# steam <- dsa01a$x1
# out <- lm(steam ~ temp)
# summary(out)
# 
# sqrt(summary(out)$r.squared)")
#                                  )
                                 )
                             ),
                  
                  ######################################################################
                  
                  tabPanel("2.8.3 || Computer Repair Data",
                           fluidRow(
                             column(width = 6,
                                    ######################### 
                                    box(status = "warning", solidHeader = T, title = a("Regression Results (Get R Code)", href="2.3.R",target="_blank"),
                                        width = 12, height = 800 , collapsible = F, collapsed=F,
                                        #####################
                                        tabsetPanel(
                                          tabPanel("Summary", aceEditor("result020803.1",mode="r",theme="tomorrow",font=16,height=650,
                                                                        value="> summary(out)
                                                                        
Call:
lm(formula = minutes ~ units - 1)

Residuals:
   Min    1Q      Median  3Q      Max 
-9.5955 -2.4733  0.4417  5.0243  9.7023 

Coefficients:
       Estimate  Std. Error t value Pr(>|t|)    
units  16.0744     0.2213   72.62   <2e-16 ***
---
Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1

Residual standard error: 5.502 on 13 degrees of freedom
Multiple R-squared:  0.9975,	Adjusted R-squared:  0.9974 
F-statistic:  5274 on 1 and 13 DF,  p-value: < 2.2e-16")),
                                          
                                          #####################
                                          tabPanel("Correlations", aceEditor("result020803.2",mode="r",theme="tomorrow",font=16,height=210,
                                                                             value="> cor(minutes, units, method = 'pearson')
[1] 0.9936987

#################

> cor(minutes, units, method = 'spearman')
[1] 0.9955947

#################

> cor(minutes, units, method = 'kendall')
[1] 0.977775

#################

> hoeffd(cbind(minutes, units))$D
          minutes     units
minutes 1.0000000 0.8850524
units   0.8850524 1.0000000

#################

> KendallTauA(minutes, units)
[1] 0.956044

#################

> SomersDelta(minutes, units)
[1] 1

#################

> GoodmanKruskalGamma(minutes, units)
[1] 1"))    
                                          )
                                          )
                                    ),
                             ############################
                             box(status = "warning", solidHeader = TRUE ,title = a("Data Plots (Get R Code)", href="2.3.R",target="_blank"),
                                 height=800, width = 6, collapsible = F, collapsed = F,
                                 ###########################
                                 tabsetPanel(
                                   tabPanel(plotlyOutput("plot020803" , height = 650), title = "Computer Repair Data")
                                 ))
                             
                             
#                              box(status = "warning", solidHeader = T, title = "R Code", width = 6, height = 820 , collapsible = F, collapsed=F,
#                                  aceEditor("code020803",mode="r",theme="tomorrow_night",font=16,height=745,
#                                            value="data(repair, package = 'HoRM')
# attach(repair)
# 
# ggplot(repair, aes(x = units, y = minutes)) + geom_point(size = 3) + geom_smooth(method = lm,
#     se = FALSE) + ggtitle('Computer Repair Data') + theme(text = element_text(size = 20)) +
#     xlab('Units') + ylab('Minutes') + scale_x_continuous(breaks = c(2, 4, 6, 8, 10))
# 
# out <- lm(minutes ~ units - 1)
# summary(out)
# 
# cor(minutes, units, method = 'pearson')
# cor(minutes, units, method = 'spearman')
# cor(minutes, units, method = 'kendall')
# hoeffd(cbind(minutes, units))$D
# KendallTauA(minutes, units)
# SomersDelta(minutes, units)
# GoodmanKruskalGamma(minutes, units)")
#                                  )
                                 )
                                 )
               #########################################################   
                  )))
      
     
      ###################################################################
      ###################################################################
                
      #############################################
      #############################################
      #############################################
      
      
      
      
                                    )
                                    )
                             )




