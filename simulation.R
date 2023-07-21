
# parameters ----------------------------------------------------   
n_sim         <- 10000
n             <- 1000
mean_baseline <- 10
delta_0       <- -1
sd_effect_0   <- 1
sd_change_0   <- 1
sd_baseline   <- 1
ff            <- 1.2
# variable      <- input$variable
# no_responders_0 <- input$no_responders
    
##-- Vector of parameters
SD_CHANGE        <- round(unique(c(1/ff,
                                   ff^(-0.5),
                                   1,
                                   ff^0.5,
                                   ff)  * sd_change_0),3)
SD_EFFECT        <- c(0,round(unique(c(1/ff,
                                   ff^(-0.5),
                                   1,
                                   ff^0.5,
                                   ff)  * sd_effect_0),3))
TREATMENT_EFFECT <- c(0,round(unique(c(1/ff,
                                   ff^(-0.5),
                                   1,
                                   ff^0.5,
                                   ff)  * delta_0),3))
PROP_NORES <- c(0,0.2,0.4,0.6,0.8)
TYPE <- c('Add','Mult')
    
# store data ---------------------------------------------------- 
# Scenarios
SCENARIOS   <- expand.grid(SD_CHANGE,SD_EFFECT,TREATMENT_EFFECT,PROP_NORES,TYPE)
n_SCENARIOS <- nrow(SCENARIOS)
colnames(SCENARIOS) <- c('sd_change','sd_effect','treat_effect','prop_nores','type')
SCENARIOS$var_C <- NA
SCENARIOS$var_T <- NA
SCENARIOS$var_ratio <- NA

    
# simulation ----------------------------------------------------
set.seed(12345)
treated   <- sample(c(FALSE, TRUE),2*n,replace = TRUE)

t0 <- Sys.time()
for (j in 1:n_SCENARIOS){ 
  # Parameters
  sd_change     = SCENARIOS$sd_change[j]
  sd_effect     = SCENARIOS$sd_effect[j]
  delta_mean    = SCENARIOS$treat_effect[j]
  no_responders = SCENARIOS$prop_nores[j]
  type          = SCENARIOS$type[j]
  
  
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
    if(type=='Add'){
      m <- sum(responder)
      outcome_final_T_pot <- numeric(n*2)
      outcome_final_T_pot[responder]  <- outcome_final_C_pot[responder] +  rnorm(m,delta_mean,sd_effect)
      outcome_final_T_pot[!responder] <- outcome_final_C_pot[!responder] + 0 # 0 or rnorm(0,sd_effect)?
      
      # Multiplicative effect ----------------
    }else{
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
  }
  measure <- measure_T/measure_C
  SCENARIOS$var_C[j]    <- mean(measure_C,na.rm=TRUE)
  SCENARIOS$var_T[j]    <- mean(measure_T,na.rm=TRUE)
  SCENARIOS$var_ratio[j]<- mean(measure,na.rm=TRUE)
  cat('Scenario:',j,'of',n_SCENARIOS,'\n')
}
t1 <- Sys.time() - t0

load("scenarios.Rdata")
model_data <- data.frame(
  log_var_ratio = log(SCENARIOS$var_ratio),
  sd_change = SCENARIOS$sd_change,
  sd_effect = SCENARIOS$sd_effect,
  treat_effect = SCENARIOS$treat_effect,
  prop_nores = SCENARIOS$prop_nores,
  type = SCENARIOS$type
)


model <- lm(log_var_ratio ~ sd_change + sd_effect + treat_effect + prop_nores + type, data = model_data)
summary(model)



