# fit a 5-parameter log-logistic model
# with random asymptotes, steepness and inflection point location
## starting values need to be provided:
# estimate a set of parameters for each Run

# Beta avec decroissance (Yin et al 2008) --------------------------------
BetaF <- function (x, ymax, tymax, tinflex) {
  ifelse(x<tymax,
         ymax*(1+((tymax-x)/(tymax-tinflex)))*((x/tymax)^(tymax/(tymax-tinflex))), 
         ymax)
}
BFSSdrm <- function() {                                                             
  BFdrm <- function (x, parm)                                                         
  {ifelse(x<parm[,2], parm[,1]*(1+((parm[,2]-x)/(parm[,2]-parm[,3])))*((x/parm[,2])^(parm[,2]/(parm[,2]-parm[,3]))),
          parm[,1])
  } 
  # Creation de la fonction Self starter
  ssBFdrm <- function(data) {                                                          
    Beta_grid <- expand.grid(list(ymax=seq(30, 100, by=10), 
                                  tymax=seq(225, 300, by=10), 
                                  tinflex=seq(125, 275, by=15)))    ## creation d une grille avec les valeurs a tester
    rec_BetaInit <- nls2(y~BetaF(x, ymax, tymax, tinflex), 
                         data=data,                                ## utilisation de nls2 pour tester la grille 
                         start=Beta_grid, algorithm="brute-force") ## et sortir des valeurs initiales  
    return(coef(rec_BetaInit)) 
  }     
  namesBFdrm <-c("ymax", "tymax", "tinflex")                       ## nom des parametres du modele
  return(list(fct=BFdrm, ssfct=ssBFdrm, names=namesBFdrm))
}


# Bell-shaped avec plateaux fixes ----------------------
# http://www.graphpad.com/guides/prism/7/curve-fitting/index.htm?reg_bellshaped_dose_response.htm
Bell <- function (x, Dip, tinflex_1, tinflex_2, nH1, nH2) {
  Plateau1=0; Plateau2=0; # plateaus at the left and right ends of the curve
  Span1=Plateau1-Dip 
  #  is the plateau level in the middle of the curve.
  Span2=Plateau2-Dip
  Section1=Span1/(1+10^((tinflex_1-x)*nH1)) 
  # tinflex are the time that give half-maximal stimulatory and 
  # inhibitory effects
  Section2=Span2/(1+10^((x-tinflex_2)*nH2)) 
  # nH1 and nH2 are the unitless  slope factors or Hill slopes. Consider 
  # constraining these to equal 1.0 (stimulation) and -1 (inhibition).
  Dip+Section1+Section2
}
BellSSdrm <- function() {                                                             
  Belldrm <- function (x, parm)                                                         
  {Plateau1=0; Plateau2=0; 
  Span1=Plateau1-parm[,1] 
  Span2=Plateau2-parm[,1]
  Section1=Span1/(1+10^((parm[,2]-x)*parm[,4]))
  Section2=Span2/(1+10^((x-parm[,3])*parm[,5]))
  parm[,1] +Section1+Section2     
  } 
  # Creation de la fonction Self starter
  ssBelldrm <- function(data) {                                                          
    Bell_grid <- expand.grid(list(Dip=seq(30, 100, by=10), 
                                  tinflex_1=seq(200, 275, by=15), 
                                  tinflex_2=seq(275, 375, by=20),
                                  nH1=seq(-.01, 0, by=.003),
                                  nH2=seq(-.01, 0, by=.003)))    
    rec_BellInit <- nls2(y~Bell(x, Dip, tinflex_1, tinflex_2, nH1, nH2), 
                         data=data,                                 
                         start=Bell_grid, algorithm="brute-force")  
    return(coef(rec_BellInit)) 
  }     
  namesBelldrm <-c("Dip", "tinflex_1", "tinflex_2", "nH1", "nH2")    
  return(list(fct=Belldrm, ssfct=ssBelldrm, names=namesBelldrm))
}


# Pseudo-Voigt (Archantoulis et Miguez 2015) -----------------------
PsVo <- function (x, a, b, c, X0) {
  a*(c/(1+((x-X0)/b)^2)+(1-c)*exp(-.5*(((x-X0)/b)^2)))
}
PsVoSSdrm <- function() {                                                             
  PsVodrm <- function (x, parm)                                                         
  {parm[,1]*(parm[,3]/(1+((x-parm[,4])/parm[,2])^2)+(1-parm[,3])*exp(-.5*(((x-parm[,4])/parm[,2])^2)))
  } 
  # 2.creation de la fonction Self starter
  ssPsVodrm <- function(data) {                                                          
    PsVo_grid <- expand.grid(list(a=seq(30, 100, by=10), 
                                  b=seq(100, 300, by=25), 
                                  c=seq(0, 1, by=.1), 
                                  X0=seq(175, 300, by=20)))   
    rec_PsVoInit <- nls2(y~PsVo(x, a, b, c, X0), 
                         data=data,                                 
                         start=PsVo_grid, algorithm="brute-force")  
    return(coef(rec_PsVoInit)) 
  }     
  namesPsVodrm <-c("a", "b", "c", "X0")    
  return(list(fct=PsVodrm, ssfct=ssPsVodrm, names=namesPsVodrm))
}


# Positive Pseudo-Voigt (Archantoulis et Miguez 2015) -------------------------
PsV2 <- function (x, a, b, c, X0) {
  ifelse(a*(c/(1+((x-X0)/b)^2)+(1-c)*exp(-.5*(((x-X0)/b)^2)))<0,0,
         a*(c/(1+((x-X0)/b)^2)+(1-c)*exp(-.5*(((x-X0)/b)^2))))
}
PsV2SSdrm <- function() {                                                             
  PsVodrm <- function (x, parm){                                                         
    ifelse(parm[,1]*(parm[,3]/(1+((x-parm[,4])/parm[,2])^2)+(1-parm[,3])*exp(-.5*(((x-parm[,4])/parm[,2])^2)))<0,0,
           parm[,1]*(parm[,3]/(1+((x-parm[,4])/parm[,2])^2)+(1-parm[,3])*exp(-.5*(((x-parm[,4])/parm[,2])^2))))
  } 
  # 2.creation de la fonction Self starter
  ssPsVodrm <- function(data) {                                                          
    PsVo_grid <- expand.grid(list(a=seq(30, 80, by=10), 
                                  b=seq(20, 150, by=10), 
                                  c=seq(0,1, by=.05), 
                                  X0=seq(250, 325, by=10)))   
    rec_PsVoInit <- nls2(y~PsVo(x, a, b, c, X0), 
                         data=data,                                 
                         start=PsVo_grid, algorithm="brute-force")  
    return(coef(rec_PsVoInit)) 
  }     
  namesPsVodrm <-c("a", "b", "c", "X0")    
  return(list(fct=PsVodrm, ssfct=ssPsVodrm, names=namesPsVodrm))
}


# Holling type IV (Bolker 2008) --------------------
Hol4 <- function (x, Asym, b, c) { (Asym*x^2)/(b+c*x+x^2) }
Hol4SSdrm <- function() {                                                             
  Hol4drm <- function (x, parm)                                                         
  {(parm[,1]*x^2)/(parm[,2]+parm[,3]*x+x^2)} 
  # 2.creation de la fonction Self starter
  ssHol4drm <- function(data) {                                                          
    Hol4_grid <- expand.grid(list(Asym=seq(0, 100, by=10), 
                                  b=seq(200, 300, by=15), 
                                  c=seq(0, 1, by=.1)))   
    rec_Hol4Init <- nls2(y~Hol4(x, Asym, b, c), 
                         data=data,                                 
                         start=Hol4_grid, algorithm="brute-force")  
    return(coef(rec_Hol4Init)) }     
  namesHol4drm <-c("Asym", "b", "c")    
  return(list(fct=Hol4drm, ssfct=ssHol4drm, names=namesHol4drm))}


# logN (Archantoulis et Miguez 2015) -------------------
LogN <- function (x, a, b, X0) { a*exp(-.5*(log(x/X0)/b)^2) }
LogNSSdrm <- function() {                                                             
  LogNdrm <- function (x, parm)                                                         
  {parm[,1]*exp(-.5*(log(x/parm[,3])/parm[,2])^2)} 
  # 2.creation de la fonction Self starter
  ssLogNdrm <- function(data) {                                                          
    LogN_grid <- expand.grid(list(a=seq(30, 100, by=10), 
                                  b=seq(0, 5, by=.1), 
                                  X0=seq(150, 300, by=10)))   
    rec_LogNInit <- nls2(y~LogN(x, a, b, X0), 
                         data=data,                                 
                         start=LogN_grid, algorithm="brute-force")  
    return(coef(rec_LogNInit)) }     
  namesLogNdrm <-c("a", "b", "X0")    
  return(list(fct=LogNdrm, ssfct=ssLogNdrm, names=namesLogNdrm))}


# Asymmetric lorentzian (Stancik et al 2008) -------------------------------
AsLo <- function (x, a, b, X0, A) {  
  g=2*b/(1+exp(a*(x-X0)))
  (2*A/(pi*g))/(1+4*((x-X0)/g)^2)
}
AsLoSSdrm <- function() {                                                             
  AsLodrm <- function (x, parm)                                                         
  { 
    g=2*parm[,2]/(1+exp(parm[,1]*(x-parm[,3])))
    (2*parm[,4]/(pi*g))/(1+4*((x-parm[,3])/g)^2)
  } 
  # 2.creation de la fonction Self starter
  ssAsLodrm <- function(data) {                                                          
    AsLo_grid <- expand.grid(list(a=seq(-.1, 0, by=.01), 
                                  b=seq(250, 375, by=20), 
                                  X0=seq(200, 300, by=10),
                                  A=seq(20000, 50000, by=5000)))   
    rec_AsLoInit <- nls2(y~AsLo(x, a, b, X0, A), 
                         data=data,                                 
                         start=AsLo_grid, algorithm="brute-force")  
    return(coef(rec_AsLoInit)) }     
  namesAsLodrm <-c("a", "b", "X0", "A")    
  return(list(fct=AsLodrm, ssfct=ssAsLodrm, names=namesAsLodrm))}


# GomppGrowth 3.1 ---------------------------
"gompGrowth.1" <-
  function(fixed = c(NA, NA, NA), names = c("c", "m", "plateau"))
  {
    ## Checking arguments
    numParm <- 3
    if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
    if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}  
    
    ## Fixing parameters (using argument 'fixed')
    notFixed <- is.na(fixed)
    parmVec <- rep(0, numParm)
    parmVec[!notFixed] <- fixed[!notFixed]
    ## Defining the non-linear function
    fct <- function(x, parm) 
    {
      parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
      parmMat[, notFixed] <- parm
      c <- parmMat[, 1]; m <- parmMat[, 2]; a <- parmMat[, 3] 
      a * exp( - (m/c) * exp (-c * x)) 
    }
    
    ## Defining self starter function        
    ssfct <- function(dataf) 
    {       
      x <- dataf[, 1]
      y <- dataf[, 2]
      plateau <- max(y) * 1.05
      ## Linear regression on pseudo y values
      pseudoY <- log( - log( y / plateau ) )
      coefs <- coef( lm(pseudoY ~ x) )
      k <- coefs[1]; c <- - coefs[2]
      b <- exp(k) 
      m <- b * c
      return(c(c, m, plateau)[notFixed])
    }
    
    ## Defining names
    pnames <- names[notFixed]
    ## Defining derivatives
    ## Defining the ED function
    ## Defining the inverse function
    ## Defining descriptive text
    text <- "Gompertz Growth Model"   
    ## Returning the function with self starter and names
    returnList <- list(fct = fct, ssfct = ssfct, names = pnames, text = text, noParm = sum(is.na(fixed))) 
    class(returnList) <- "drcMean"
    invisible(returnList)
  }


# GomppGrowth 3.2 ---------------------------
"gompGrowth.2" <-
  function(fixed = c(NA, NA, NA), names = c("c", "d", "plateau"))  {
    ## Checking arguments
    numParm <- 3
    if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
    if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}    
    ## Fixing parameters (using argument 'fixed')
    notFixed <- is.na(fixed)
    parmVec <- rep(0, numParm)
    parmVec[!notFixed] <- fixed[!notFixed]
    ## Defining the non-linear function
    fct <- function(x, parm)  {
      parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
      parmMat[, notFixed] <- parm
      a <- parmMat[, 3]; b <- parmMat[, 1]; c <- parmMat[, 2] 
      a * exp( - exp (b * (c - x)))  
    }
    
    ## Defining self starter function        
    ssfct <- function(dataf)  {       
      x <- dataf[, 1]
      y <- dataf[, 2]
      a <- max(y) * 1.05
      ## Linear regression on pseudo y values
      pseudoY <- log( log( a / (y +0.0001) ) )
      coefs <- coef( lm(pseudoY ~ x))
      k <- coefs[1]
      b <- - coefs[2]
      c <- k/b
      return(c(b, c, a)[notFixed])
    }
    
    ## Defining names
    pnames <- names[notFixed]
    ## Defining derivatives
    ## Defining the ED function
    ## Defining the inverse function
    ## Defining descriptive text
    text <- "Gompertz Growth Model 2"    
    ## Returning the function with self starter and names
    returnList <- list(fct = fct, ssfct = ssfct, names = pnames, text = text, noParm = sum(is.na(fixed))) 
    class(returnList) <- "drcMean"
    invisible(returnList)
  }

# GomppGrowth 3.3 ---------------------------
"gompGrowth.3" <-
  function(fixed = c(NA, NA, NA), names = c("b", "c", "plateau"))  {
    ## Checking arguments
    numParm <- 3
    if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
    if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}    
    
    ## Fixing parameters (using argument 'fixed')
    notFixed <- is.na(fixed)
    parmVec <- rep(0, numParm)
    parmVec[!notFixed] <- fixed[!notFixed]
    
    ## Defining the non-linear function
    fct <- function(x, parm) {
      parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
      parmMat[, notFixed] <- parm
      a <- parmMat[, 3]; b <- parmMat[, 1]; c <- parmMat[, 2] 
      a * exp( - b * exp (-c * x))  
    }
    
    ## Defining self starter function        
    ssfct <- function(dataf)  {       
      x <- dataf[, 1]
      y <- dataf[, 2]
      a <- max(y) * 1.05
      ## Linear regression on pseudo y values
      pseudoY <- log( - log( y / a ) )
      coefs <- coef( lm(pseudoY ~ x) )
      k <- coefs[1]
      c <- - coefs[2]
      b <- exp(k)
      return(c(b, c, a)[notFixed])
    }
    
    ## Defining names
    pnames <- names[notFixed]
    ## Defining derivatives
    ## Defining the ED function
    ## Defining the inverse function
    ## Defining descriptive text
    text <- "Gompertz Growth Model 3"    
    ## Returning the function with self starter and names
    returnList <- list(fct = fct, ssfct = ssfct, names = pnames, text = text, noParm = sum(is.na(fixed))) 
    class(returnList) <- "drcMean"
    invisible(returnList)
  }