## Capítulo 9 RHKS para estimación de tendencia y ciclo

## Obtener el kernel de tercer orden
Get_kernel<-function(type){
  unname(list("Gaussian"=function(t){exp(-t^2/2)*(3-t^2)/(2*sqrt(2*pi))},
       "LOESS"=function(t){(70*(1-abs(t)^3)^3)/81*(539/293-3719*t^2/638)*(abs(t)<1)},
       "Henderson"=function(t){15*(1-abs(t)^2)^2/16*(7/4-21*t^2/4)*(abs(t)<1)},
       "Spline"=function(t){(5/(4*cosh(5*t/2)^2))*(21/16-2085*t^2/878)})[[type]])
}

Opt_bandwidth<-function(type){
  if(!(2*type+1) %in% c(9,13,23)){
    stop("Optimal bandwidth is only available for 
                                  9, 13 and 23 term Henderson filter")
  }
  list("4"=structure(c(8.00, 5.67, 4.87, 4.90),
                                names=as.character(0:3)),
                  "6"=structure(c(11.78, 9.24, 7.34, 6.85, 6.84, 6.95),
                                 names=as.character(0:5)),
                  "11"=structure(c(21.18, 18.40, 16.07, 13.89, 12.44,
                                   11.90, 11.72, 11.73, 11.83, 11.92, 11.98),
                                 names=as.character(0:10)))[[as.character(type)]]
}

## Obtener los pesos
Get_weights<-function(kernel,m,b=m+1){
  aux<-unlist(lapply(seq(-m,m),function(x,b){kernel(x/b)},b=b))
  aux/sum(aux)
}

## Obtener la serie ya pesada 
Get_trend<-function(y,type,m){
  #browser()
  ##Elegir tipo de kernel
  kn<-Get_kernel(type)
  
  ## Cargar pesos para la parte simétrica
  weights<-Get_weights(kn,m=m)
  
  ## Cargar pesos para la parte asimétrica
  if(type=="Henderson" & m%in%c(4,6,11)){
    opt<-Opt_bandwidth(m)
  } else {
    opt<-rep(m+1,m)
  }
  
  asym_weights<-lapply(opt,Get_weights,kernel=kn,m=m)
  
  ## Parte del filtro simetrico
  n<-length(y)
  sym_filter<-unlist(lapply((m+1):(n-m),
                            function(x,y,w,m){sum(y[(x-m):(x+m)]*w)},
                      y=y,w=weights,m=m))

  ## Parte del filtro asimetrico
  # a) El inicio
  start<-unlist(mapply(function(x,y,w,m){
    nw<-w[-(1:(m-x+1))]
    sum(y[1:(x+m)]*nw/sum(nw))
    }, x=as.list(1:m),w=asym_weights,
    MoreArgs = list(m=m,y=y), SIMPLIFY = F))

  # b) El final
  end<-unlist(mapply(function(x,y,w,m,n){
    nw<-rev(w[-(1:(x-n+m))])
    sum(y[(x-m):n]*nw/sum(nw))
  }, x=as.list((n-m+1):n),w=rev(asym_weights),
  MoreArgs = list(m=m,y=y,n=n), SIMPLIFY = F))
  
  ## Salida
  c(start,sym_filter,end)
}

# test<-5+(1:100)+arima.sim(list(ar=0.4,ma=-0.4),n=100,sd=20)
# plot(test,type="l")
# res<-Get_trend(test,type="Henderson",m=4)
# lines(res,col="red")
# res


## Que significa desplazar hacia adelante y hacia atrás?
# https://www.abs.gov.au/websitedbs/d3310114.nsf/4a256353001af3ed4b2562bb00121564/5fc845406def2c3dca256ce100188f8e!OpenDocument