spanGuess <- function(DF,InVar){
  temp <- DF%>%
    filter(!is.na(!!sym(InVar)))%>%
    summarise(n=n())
  span <- min(c(.1*178/temp$n,.6))#More can be done here
  return(span)
}


LoessSmoothMod <- function(DF,InVar, OutVar, span="guess"){
  if(span=="guess"){
    span <- spanGuess(DF,InVar)
  }
  
  DF2 <- DF%>%
    arrange(Date)
  
  DF2[[OutVar]] <- loessFit(y=(DF2[[InVar]]), 
                            x=DF2$Date, #create loess fit of the data
                            span=span, 
                            iterations=2)$fitted#2 iterations remove some bad patterns
  return(DF2)
}


alphaGuess <- function(DF,InVar){
  temp <- DF%>%
    filter(!is.na(!!sym(InVar)))%>%
    summarise(n=n())
  alpha <- min(c(.2*178/temp$n,.4))#More can be done here
  return(alpha)
}

betaGuess <- function(DF,InVar){
  temp <- DF%>%
    filter(!is.na(!!sym(InVar)))%>%
    summarise(n=n())
  beta <- min(c(.05*178/temp$n,.4))#More can be done here
  return(beta)
}


ExpSmoothMod <- function(DF,InVar, OutVar,alpha="guess",beta="guess" ){
  
  if(alpha=="guess"){
    alpha <- alphaGuess(DF,InVar)
  }
  if(beta=="guess"){
    beta <- betaGuess(DF,InVar)
  }
  
  DF2 <- DF%>%
    arrange(Date)
  
  DF2[[OutVar]] <- robets(y=DF2[[InVar]],
                          model = "AAN",
                          beta  = beta,
                          alpha=alpha  )$fitted#2 iterations remove some bad patterns
  return(DF2)
}