library(shiny)
# install.packages('ggplot2')
library(ggplot2)
library(dplyr)
library(graphics)

##-- Function to plot densities
figure1 <- function(y0,y1,y0_all,y1_all,measure_C, measure_T,tr,let,COLORS,ce=NA,esc=1,n, EXTERNAL.FILE, S=1){
  
  w = c(y0_all,y1_all)
  ymax <- quantile(w, 0.999) # max(w) # 12
  ymin <- quantile(w, 0.001) # min(w)
  plot(NA, xlim = c(0, 4), ylim = c(ymin, ymax), bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  abline(v=1:2,col="grey",lwd=2)
  #yaux <- seq(ymin,ymax,5)
  yaux <- pretty(w)
  segments(1,yaux,2,yaux,col="grey",lwd=1,lty=2)
  text(1.5,yaux,yaux,adj=0.5,cex=0.7*esc,pos=3,font=2,col="darkgrey")
  
  d0 <- density(y0_all/S)
  dx <- d0$x*S
  dy <- d0$y
  d0$x <- 1-3*dy
  d0$y <- dx
  polygon(d0,col=rgb(0,0,1,0.2),border=4,lwd=2)
  
  d1 <- density(y1_all/S, kernel='cosine', bw = "nrd0", adjust = 0.9)
  dx <- d1$x*S
  dy <- d1$y
  d1$x <- 2+3*dy
  d1$y <- dx
  polygon(d1,col=rgb(0,0,1,0.2),border=4,lwd=2)
  
  co1 <- character(8);co1[tr] <- COLORS[tr,1];co1[!(1:8 %in% tr)] <- COLORS[!(1:8 %in% tr),2]
  co2 <- character(8);co2[tr] <- COLORS[tr,2];co2[!(1:8 %in% tr)] <- COLORS[!(1:8 %in% tr),1]
  co3 <- COLORS[,3]
  
  points(rep(1,n),y0,pch=15,cex=ifelse(EXTERNAL.FILE,2.5*esc,2.5),col=co1)
  points(rep(2,n),y1,pch=15,cex=ifelse(EXTERNAL.FILE,2.5*esc,2.5),col=co2)
  segments(1,y0,2,y1,lwd=ifelse(EXTERNAL.FILE,2*esc,2),col=co3)
  par(xpd=NA)
  measure_C <- round(mean(measure_C),2)
  measure_T <- round(mean(measure_T),2)
  #if(is.na(ce[1])) ce <- c(1.5,3)
  ##a <- 1.95
  ##plot(5,5,main=bquote(sigma[C]^2==.(a)))
  
  text(1,ymax*1.03,expression(bold(C)),cex=ifelse(EXTERNAL.FILE,esc*ce[1],esc*ce[1]), pos=3)
  text(2,ymax*1.03,expression(bold(T)),cex=ifelse(EXTERNAL.FILE,esc*ce[1],esc*ce[1]), pos=3)
  text(1,mean(d0$y),bquote(sigma[C]^2==.(measure_C)),cex=ifelse(EXTERNAL.FILE,esc*ce[1],esc*ce[1]), pos=2)
  text(2,mean(d1$y),bquote(sigma[T]^2==.(measure_T)),cex=ifelse(EXTERNAL.FILE,esc*ce[1],esc*ce[1]), pos=4)
  #text(0.75,ymax*1.03,LETTERS[let],font=2,cex=ifelse(EXTERNAL.FILE,ce[2]*esc,esc*ce[2]), pos=3)
  #par(xpd=FALSE)
}



function(input, output) {
  # Código para generar la primera figura
  res_sim <- eventReactive(input$go,{
    
    # parameters ----------------------------------------------------   
    n_sim         <- input$n_sim
    n             <- input$n
    m             <- input$m
    mean_baseline <- input$mean_baseline
    delta_0       <- input$delta_0
    sd_effect_0   <- input$sd_effect
    sd_change_0   <- input$sd_change
    sd_baseline   <- input$sd_baseline
    ff            <- as.numeric(input$factor)
    variable      <- input$variable
    no_responders_0 <- input$no_responders
    
    ##-- Vector of parameters
    SD_CHANGE        <- round(unique(c(1/ff,
                                       ff^(-0.5),
                                       1,
                                       ff^0.5,
                                       ff)  * sd_change_0),3)
    SD_EFFECT        <- round(unique(c(1/ff,
                                       ff^(-0.5),
                                       1,
                                       ff^0.5,
                                       ff)  * sd_effect_0),3)
    TREATMENT_EFFECT <- round(unique(c(1/ff,
                                       ff^(-0.5),
                                       1,
                                       ff^0.5,
                                       ff)  * delta_0),3)
    PROP_NORES <- sort(unique(c(0,0.2,0.4,0.6,0.8,no_responders_0)))
    
    # store data ---------------------------------------------------- 
    # measure <- matrix(NA, nrow = n_sim, ncol = 5)
    
    # Scenarios
    SCENARIOS <- data.frame(variable= c(rep('SD_CHANGE',       length(SD_CHANGE)),
                                        rep('SD_EFFECT',       length(SD_EFFECT)),
                                        rep('TREATMENT_EFFECT',length(TREATMENT_EFFECT)),
                                        rep('PROP_NORES',      length(PROP_NORES))),
                            value   = c(SD_CHANGE,SD_EFFECT,TREATMENT_EFFECT,PROP_NORES))
    n_SCENARIOS <- nrow(SCENARIOS)
    
    # simulation ----------------------------------------------------
    # set.seed(12345) --> Discutim si posar-la
    treated   <- sample(c(FALSE, TRUE),2*n,replace = TRUE)
    
    for (j in 1:n_SCENARIOS){ 
      # Parameters
      sd_change     = ifelse(SCENARIOS$variable[j]=='SD_CHANGE',       SCENARIOS$value[j],sd_change_0)
      sd_effect     = ifelse(SCENARIOS$variable[j]=='SD_EFFECT',       SCENARIOS$value[j],sd_effect_0)
      delta_mean    = ifelse(SCENARIOS$variable[j]=='TREATMENT_EFFECT',SCENARIOS$value[j],delta_0)
      no_responders = ifelse(SCENARIOS$variable[j]=='PROP_NORES',      SCENARIOS$value[j],no_responders_0)
      
      
      # Responders
      responder <- sample(c(FALSE, TRUE),2*n, replace = TRUE, prob = c(no_responders,1-no_responders))
      
      # Save results
      measure_C <- measure_T <- measure <- numeric(n_sim)
      
      for(i in 1:n_sim){
        # Potential baseline values
        outcome_baseline    <- rnorm(2*n,mean_baseline,sd_baseline)
        
        # Potential control final values
        outcome_final_C_pot <- outcome_baseline + rnorm(n,0,sd_change)
        
        # Mean treatment effect
        #delta <- c(rep(0,round(2*n*no_responders)), rep(delta_mean,round(2*n*(1-no_responders))))
        
        # Additive effect ----------------
        if(!input$multiplicative){
          # outcome_final_T_pot <- outcome_final_C_pot + rnorm(n,delta,sd_effect)
          m <- sum(responder)
          outcome_final_T_pot <- numeric(n*2)
          outcome_final_T_pot[responder]  <- outcome_final_C_pot[responder] +  rnorm(m,delta_mean,sd_effect)
          outcome_final_T_pot[!responder] <- outcome_final_C_pot[!responder] + 0 # 0 or rnorm(0,sd_effect)?
          
          # Multiplicative effect ----------------
        }else{
          # outcome_final_T_pot <- outcome_final_C_pot * exp(rnorm(n,delta,sd_effect)) # això que?
          m <- sum(responder)
          outcome_final_T_pot <- numeric(n*2)
          outcome_final_T_pot[responder] <- outcome_final_C_pot[responder] * exp(rnorm(m,delta_mean,sd_effect))
          outcome_final_T_pot[!responder] <- outcome_final_C_pot[!responder] + 0
        }
        # Observed final values
        outcome_final_C     <- outcome_final_C_pot[!treated]
        outcome_final_T     <- outcome_final_T_pot[treated]
        
        # Measure of interest
        measure_C[i]  <- var(outcome_final_C)
        measure_T[i]  <- var(outcome_final_T)
        # measure[i]    <- var(outcome_final_T)/var(outcome_final_C)
        
        # Collect data for the last sim of the selected scenario --> It can be more efficient
        if(((SCENARIOS$variable[j]=='SD_CHANGE'        & SCENARIOS$value[j]==sd_baseline)    |
            (SCENARIOS$variable[j]=='SD_EFFECT'        & SCENARIOS$value[j]==sd_effect_0)    |
            (SCENARIOS$variable[j]=='TREATMENT_EFFECT' & SCENARIOS$value[j]==delta_0)        |
            (SCENARIOS$variable[j]=='PROP_NORES'       & SCENARIOS$value[j]==no_responders_0)) & 
           i==n_sim){
          # Responders and treateds (only last iteration)
          cat(SCENARIOS$variable[j],'-',SCENARIOS$value[j],"\n")
          dd_treat_res0 <- data.frame(variable  = rep(SCENARIOS$variable[j],n_sim),
                                      var_value = rep(SCENARIOS$value[j],   n_sim),
                                      treated             = treated,
                                      responder           = responder,
                                      outcome_final_C_pot = outcome_final_C_pot,
                                      outcome_final_T_pot = outcome_final_T_pot) 
         }
      }  
      
      # Calculate quantile for non-responders (only last iteration)
      dd_quantile0 <- data.frame(variable        = SCENARIOS$variable[j],
                                 value           = SCENARIOS$value[j],
                                 quantile_res    = quantile(outcome_final_C_pot[responder],0.4),
                                 quantile_nores  = quantile(outcome_final_C_pot[!responder],0.7))
      
      # Main results simulations
      dd0 <- data.frame(variable  = rep(SCENARIOS$variable[j],n_sim),
                        var_value = rep(SCENARIOS$value[j],   n_sim),
                        measure_C = measure_C,
                        measure_T = measure_T,
                        measure   = measure_T/measure_C)

      
        dd_simulation0 <- data.frame(variable     = SCENARIOS$variable[j],
                                   value           = SCENARIOS$value[j],
                                   mean_measure_C = mean(measure_C),
                                   mean_measure_T = mean(measure_T),
                                   mean_measure   = mean(measure_T/measure_C),
                                   sd_measure = sd(measure_T/measure_C))
        
        ##t_test_result <- t.test(measure_T, measure_C)
        
        # Afegir el resultat del test t al dataframe dd_simulation0
        ##dd_simulation0$t_value <- t_test_result$statistic
        ##dd_simulation0$p_value <- t_test_result$p.value
         
      # Rbind cumulated results
      if(j==1){
        dd <- dd0
        dd_quantile  <- dd_quantile0
        dd_simulation <- dd_simulation0

      }else{
        
        dd <- rbind(dd,dd0)
        dd_quantile  <- rbind(dd_quantile, dd_quantile0)
        dd_simulation  <- rbind(dd_simulation, dd_simulation0)
      }

      
      
      if(exists(x = "dd_treat_res")){
        dd_treat_res <- rbind(dd_treat_res,dd_treat_res0)
      }else if(exists(x = "dd_treat_res0")){
        dd_treat_res <- dd_treat_res0
      }
      
    
    }
    
    # Store the info for graphics in reactive values
    list(dd           = dd,
         dd_quantile  = dd_quantile,
         dd_treat_res = dd_treat_res,
         dd_simulation = dd_simulation ) 
    
    
  })
  
  
  
  output$figura1 <- renderPlot({
    
    # Run when press the button
    input$go
    
    # Recover values
    RS = isolate(res_sim())
    treated             <- (RS$dd_treat_res$treated)
    responder           <- (RS$dd_treat_res$responder)
    measure             <- (RS$dd$measure)
    outcome_final_C_pot <- (RS$dd_treat_res$outcome_final_C_pot)
    outcome_final_T_pot <- (RS$dd_treat_res$outcome_final_T_pot)
    #measure_C           <- (RS$dd$measure_C)
    #measure_T           <- (RS$dd$measure_T)
    measure_C           <- var(RS$dd_treat_res$outcome_final_C_pot)
    measure_T           <- var(RS$dd_treat_res$outcome_final_T_pot)
   
    
    # Código para generar la figura en función de los valores de entrada
    if(isolate(input$no_responders)!=0){
      # Select quantiles
      sel <- RS$dd_quantile$variable=="PROP_NORES" & RS$dd_quantile$value==isolate(input$no_responders)
      quantile_res    <- (RS$dd_quantile$quantile_res  [sel])
      quantile_nores  <- (RS$dd_quantile$quantile_nores[sel])
      
      pos_control_res    = sample(which(!treated &  responder & outcome_final_C_pot < quantile_res),  2)
      pos_control_nores  = sample(which(!treated & !responder & outcome_final_C_pot > quantile_nores),2)
      pos_tractats_res   = sample(which( treated &  responder & outcome_final_C_pot < quantile_res),  2)
      pos_tractats_nores = sample(which( treated & !responder & outcome_final_C_pot > quantile_nores),2)
      pos_control       <- c(pos_control_res, pos_control_nores)
      pos_tractats      <- c(pos_tractats_res, pos_tractats_nores)
    }else{
      pos_control        = sample(which(!treated),  4)
      pos_tractats       = sample(which( treated),  4)
    }
    
    y0 <- outcome_final_C_pot[c(pos_control,pos_tractats)]
    y1 <- outcome_final_T_pot[c(pos_control,pos_tractats)]
    
    ##-- Colors
    X1 = 0.01
    X2 = 0.75
    bin = sapply(0:7, function(x){ as.integer(intToBits(x))})[1:3,]
    bin[bin==1] = X1
    bin[bin==0] = X2
    COLORS <- matrix(ncol=3,nrow=8)
    for (i in 1:8) {
      COLORS[i,1] <- rgb(bin[1,i],bin[2,i],bin[3,i])
      COLORS[i,2] <- rgb(bin[1,i],bin[2,i],bin[3,i],alpha=0.25)
      COLORS[i,3] <- rgb(bin[1,i],bin[2,i],bin[3,i],alpha=0.6)
    }
    
    ##-- Generate figure
    ce  <- c(1,2)
    esc <- 1.5
    N   <- 8
    tr  <- c(5,6,7,8)
    let <- 3
    S = isolate(input$sd_baseline)
    figure1(y0,y1,outcome_final_C_pot, outcome_final_T_pot, measure_C, measure_T, tr,let,COLORS, ce, esc, N, EXTERNAL.FILE=FALSE, S)
    
    })
  
  output$figura2 <- renderPlot({
    # Ejecutar cuando se presione el botón
    input$go
    
    RS <- isolate(res_sim())
    dd <- RS$dd
    
    # Guardar el nombre de la variable seleccionada
    selected_variable_name <- switch(input$variable,
                                     "SD_CHANGE" = "SD of change over time",
                                     "SD_EFFECT" = "SD of Effect",
                                     "TREATMENT_EFFECT" = "Treatment Effect",
                                     "PROP_NORES" = "Proportion of Non-Responders")
  
    # Asignar nuevos nombres a los factores en dd$variable
    levels(dd$variable) <- selected_variable_name
    
    # Código para generar la figura en función de los valores de entrada
    ggplot(dd[dd$variable == input$variable,], aes(x = factor(var_value), y = measure)) +
      geom_boxplot() +
      xlab(selected_variable_name) + ylab("Variance ratio (Q)")
  })  
  

  output$taula1 <- renderTable({
    dd_simulation <- res_sim()$dd_simulation
    
    variable_elegida <- input$variable
    dd_simulation <- dd_simulation %>%
      filter(variable == variable_elegida) %>%
      select(variable, value, everything())
    
    colnames(dd_simulation) <- c("indicator", "value","variance mean in C", "variance mean in T", "mean of variance ratio", "SD of variance ratio")
    
   
    
    # Cambiar los nombres de los niveles de la variable "indicator"
    dd_simulation$indicator <- factor(dd_simulation$indicator,
                                      levels = c("SD_CHANGE", "SD_EFFECT", "TREATMENT_EFFECT", "PROP_NORES"),
                                      labels = c("SD of change over time",
                                                 "SD of Effect",
                                                 "Treatment Effect",
                                                 "Proportion of Non-Responders"))
    
    dd_simulation
  })
  
  
}
