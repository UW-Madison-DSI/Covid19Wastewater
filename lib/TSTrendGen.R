spanGuess <- function(DF,InVar){
  temp <- DF%>%
    filter(!is.na((!!sym(InVar))))%>%
    summarise(n=n())
  span <- min(c(.1*178/temp$n,.6))#More can be done here
  return(span)
}


LoessSmoothMod <- function(DF,InVar, OutVar, span="guess", Filter = NULL){
  if(span=="guess"){
    span <- 2*spanGuess(DF,InVar)
  }
  if(!is.null(Filter)){
    OutDF <- DF%>%
      filter((!!sym(Filter)))%>%
      mutate(!!OutVar := NA)
    DF <- DF%>%
      filter(!(!!sym(Filter)))
  }
  
  DF <- DF%>%
    arrange(Date)
  
  DF[[OutVar]] <- loessFit(y=(DF[[InVar]]), 
                            x=DF$Date, #create loess fit of the data
                            span=span, 
                            iterations=2)$fitted#2 iterations remove some bad patterns
  
  if(!is.null(Filter)){
    DF <- DF%>%
      bind_rows(OutDF)
  }
  
  return(DF)
}


alphaGuess <- function(DF,InVar){
  temp <- DF%>%
    filter(!is.na((!!sym(InVar))))%>%
    summarise(n=n())
  alpha <- min(c(.2*178/temp$n,.4))#More can be done here
  return(alpha)
}

betaGuess <- function(DF,InVar){
  temp <- DF%>%
    filter(!is.na((!!sym(InVar))))%>%
    summarise(n=n())
  beta <- min(c(.05*178/temp$n,.4))#More can be done here
  return(beta)
}


ExpSmoothMod <- function(DF,InVar, OutVar,alpha="guess",beta="guess", Filter = NULL ){
  
  if(alpha=="guess"){
    alpha <- alphaGuess(DF,InVar)
  }
  if(beta=="guess"){
    beta <- betaGuess(DF,InVar)
  }
  if(!is.null(Filter)){
    OutDF <- DF%>%
      filter((!!sym(Filter)) | is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    DF <- DF%>%
      filter(!(!!sym(Filter))  & !is.na(!!sym(InVar)))
  }else{
    OutDF <- DF%>%
      filter(is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    DF <- DF%>%
      filter(!is.na(!!sym(InVar)))
  }
  
  DF <- DF%>%
    arrange(Date)
  
  DF[[OutVar]] <- robets::robets(y=DF[[InVar]],
                          model = "AAN",
                          beta  = beta,
                          alpha=alpha)$fitted#2 iterations remove some bad patterns
  
  DF <- DF%>%
    bind_rows(OutDF)
  
  return(DF)
}


NGuess <- function(DF,InVar){
  temp <- DF%>%
    filter(!is.na((!!sym(InVar))))%>%
    summarise(n=n())
  N <- max(c(floor(50*temp$n/178), 7))#More can be done here
  return(N + 1 - N%%2)
}

sgolaySmoothMod <- function(DF,InVar, OutVar,poly=5,n="guess", Filter = NULL){
  if(n=="guess"){
    n <- NGuess(DF,InVar)
  }
  if(!is.null(Filter)){
    OutDF <- DF%>%
      filter((!!sym(Filter)) | is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    SmthDF <- DF%>%
      filter(!(!!sym(Filter)) & !is.na(!!sym(InVar)))
  }else{
    OutDF <- DF%>%
      filter(is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    SmthDF <- DF%>%
      filter(!is.na(!!sym(InVar)))
  }
  
  SmthDF <- SmthDF%>%
    arrange(Date)

  SmthDF[[OutVar]] <- SmthDF%>%
    pull(!!InVar)%>%
    signal::sgolayfilt(p  = poly, n = n)
  
  RetDF <- SmthDF%>%
    bind_rows(OutDF)%>%
    arrange(Date)
  
  return(RetDF)
}