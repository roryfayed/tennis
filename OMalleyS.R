
S <- function(a,b,Gp,Gq,TB){
  
  if(a==6 && b < 5 || a>6 && b <= 6){
    return(1)
  }
  
  if(b==6 && a < 5 || b>6 && a <= 6){
    return(0)
  }
  
  if(a==6 && b==6){
    return(TB)
  }
  if((b < 5 && a==5) || (a==6 && b == 5)){
    prob <- Gp + (1 - Gp)*(1 - S(b+1, a, Gq,Gp,TB))
    return(prob)
  }
  if((a < 5 && b==5) || (a == 5 && b == 6)){
    prob <- Gp*(1 - S(b, a+1, Gq,Gp,TB))
    return(prob)
  }
  prob <- Gp*(1 - S(b,a+1,Gq,Gp,TB)) + (1 - Gp)*(1 - S(b+1,a,Gq,Gp,TB))
  return(prob)
}

TB <- function(a,b,p,q){
  if(a == 0 && b == 0){
    prob <- p*(1 - TB(b,a+1,q,p)) + (1 - p)*(1 - TB(b+1,a,q,p))
    return(prob)
  }
  if(a>6 && b < 6){
    return(1)
  }
  if(a<6 && b>6){
    return(0)
  }
  if(a==7 && b==6){
    prob <-(p*(2*p^2*q^2-p*q^2-p^2*q + p*q - 1) + (1-q)*(p^2-p))/(p^2-2*p+4*p^2*q^2-4*p*q^2+q^2-4*p^2*q+6*p*q-2*q)
    return(prob)
  }
  if(b==6 && a==6){
    if(p==0.5 && q == 0.5){return(0.5)}
    prob <- (p^2 + 2*(1 - q)^2*(1-p)*p)/(1 - 4*p*q*(1-p)*(1-q))
    return(prob)
  }
  if(b==7 && a==6){
    prob <- (p*(p^2-p+2*p^2*q^2-p*q^2-3*p^2*q+2*p*q) + (1-q)*-p^2)/(p^2-2*p+4*p^2*q^2-4*p*q^2+q^2-4*p^2*q+6*p*q-2*q)
    return(prob)
  }
  if(a<6 && b==6){
    prob <- p*p*(1 - TB(b, a+2,q,p)) + p*(1-p)*(1 - TB(b+1,a+1,q,p))
    return(prob)
  }
  if(a==6 && b<6){
    prob <- p + (1-p)*p*(1 - TB(b+1, a+1,q,p)) + (1-p)*(1-p)*(1 - TB(b+2,a,q,p))
    return(prob)
  }
    prob <- p*p*(1 - TB(b, a+2,q,p)) + 2*p*(1-p)*(1 - TB(b+1,a+1,q,p)) + (1-p)*(1-p)*(1 - TB(b+2,a,q,p))
    return(prob)
}

G <- function(a,b,p){
  
  if(a>3 && a - b > 1){
    return(1)
  }
  if(b>3 && b - a > 1){
    return(0)
  }
  
  if(a==4 && b == 4){
    return(G(3,3,p))
  }
  if(a==5 && b == 4){
    return(G(4,3,p))
  }
  if(a==4 && b == 5){
    return(G(3,4,p))
  }
  
  if(a==4 && b==3){
    prob <- p + (1 - p)*(p^2/(p^2 + (1 - p)^2))
    return(prob)
  } 
  if(a==3 && b==4){
    prob <- (1 - p)*(p^2/(p^2 + (1 - p)^2))
    return(prob)
  } 
  if(a==3 && b < 3){
    prob <- p + (1-p)*G(a,b+1,p)
    return(prob)
  } 
  if(b==3 && a < 3){
    prob <- p*G(a+1,b,p)
    return(prob)
  }else {
    prob <- p*G(a+1,b,p) + (1-p)*G(a,b+1,p)
    return(prob)
  }
  
}