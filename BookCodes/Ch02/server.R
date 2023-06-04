# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

###################################
######## Install Packages #########
###################################
# install.packages(c("shiny","ggplot2","aprean3","HoRM","DescTools","Hmisc","tolerance","psych","investr","MASS", 
#   "fueleconomy" ,"alr3","MPV","StatDA","rgl","car","perturb","conjoint","sandwich","fastR","MethComp","deming","sem",
#   "robustbase","RFreak","L1pack","quantmod","astsa","orcutt","car","forecast","nlme","HarmonicRegression",  
#   "leaps","fastR","DAAG","qpcR","lme4","car","rsm","daewr","mixexp","lars","pls", "dr","ISLR","lasso2","astrodatR","ash","np",
#   "MASS","deming" ,"boot" ,"bootstrap","rgl","AER", "survival","truncreg","SMPracticals","KMsurv","nlstools","pROC","lmtest", 
#   "mlogit","nnet","foreign","pscl","msme","VGAM","gamlss.tr","betareg","HSAUR2","gee"))

# install.packages(c("shinydashboard","shinyAce","tolerance"),lib=.libPaths()[3])


######################################################################################
######################################################################################
######################################################################################
options(rgl.useNULL = TRUE)
 ### Package for Chapter 2 ###
 library(ggplot2)
 library(HoRM)
 library(aprean3)
 library(DescTools)
 library(Hmisc)
 library(plotly)










shinyServer(function(input, output,session) {
  
  #########################################################################
  ############################ Chapter 2 ##################################
  #########################################################################
  #############################
  ######## 02.08.01 ###########
  #############################
  output$plot020801<-renderPlotly({
    data(toy)
    
    out <- lm(y ~ x, data = toy)
    out$fitted
    out$res
    ggplot(toy, aes(x = x, y = y)) + geom_point(size = 3) + geom_smooth(method = lm,
                                                                        se = FALSE) + ggtitle("Toy Data") + theme(text = element_text(size = 20))

  })
  
  
  #############################
  ######## 02.08.02 ###########
  #############################
  output$plot020802<-renderPlotly({
    data(dsa01a)
    
    ggplot(dsa01a, aes(x = x8, y = x1)) + geom_point(size = 3) + geom_smooth(method = lm,
                                                                             se = FALSE) + ggtitle("Steam Output Data") + theme(text = element_text(size = 20)) +
     xlab("Temperature (F)") + ylab("Steam Usage (Monthly)")
    
  })
  
  #############################
  ######## 02.08.03 ###########
  #############################
  output$plot020803<-renderPlotly({
    data(repair)
    attach(repair)
    
    ggplot(repair, aes(x = units, y = minutes)) + geom_point(size = 3) + geom_smooth(method = lm,
                                                                                     se = FALSE) + ggtitle("Computer Repair Data") + theme(text = element_text(size = 20)) +
      xlab("Units") + ylab("Minutes") + scale_x_continuous(breaks = c(2, 4, 6, 8, 10))
    
  })
  # ######################################################  
  # #################### The End #########################
  # ######################################################
})
