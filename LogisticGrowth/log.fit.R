#https://stats.stackexchange.com/questions/47802/whats-the-most-pain-free-way-to-fit-logistic-growth-curves-in-r
library("nls2")
require(ggplot2)

##dep = name of your dependent variable, as it appears in data frame column name, in quotes eg "Size"

##ind = name of your dependent variable, as it appears in data frame column name, in quotes eg "Time.Point"

##yourdata = data.frame containing columns dep and ind

log.fit <- function(dep, ind, yourdata){
  ##Self-starting...
  
  y <- yourdata[, dep]
  x <- yourdata[, ind]
  
  ##observed population sizes
  N_obs = y
  ##time points
  times = x
  
  ##calculate initial parameters for the logistic growth model
  ##the final paramaters will be determined when the final model is created
  SS<-getInitial(N_obs~SSlogis(times,alpha,xmid,scale),
                 data=data.frame(N_obs=y,times=x))
  
  ##Adjust the paramaters so that they fit the values used in the classical ecological population growth equation
  K_start<-SS["alpha"]
  R_start<-1/SS["scale"]
  N0_start<-SS["alpha"]/(exp(SS["xmid"]/SS["scale"])+1)
  
  ##specify the ecological population growth formula for the model
  log_formula<-formula(N_obs~K*N0*exp(R*times)/(K+N0*(exp(R*times)-1)))
  
  ##fit the model
  m<-nls(log_formula,start=list(K=K_start,R=R_start,N0=N0_start))
  ##get some estimation of goodness of fit so that bad model fits can be later removed
  correlation_value = cor(N_obs,predict(m))
  
  ##determine a title for the given graph that is specific to condition, strain, and well
  title = paste("Logistic Function",yourdata$Condition[1],yourdata$Gene[1],yourdata$Well[1])
  
  ##graph the data as a scatterpot
  ##plot the model fit as a red line on the scatterplot
  print(ggplot(aes(x,y),data=yourdata) +
          geom_point(aes(x,y)) +
          ggtitle(title) +
          geom_line(aes(x,y=predict(m),color="red")) #plotting model fit
       )

  ##save the graph, user must have previously created "model_success_graphs" folder
  ggsave(paste(title,"png",sep="."),path="model_success_graphs")
  
  ##Pull out the final paramters of the model  
  K = coef(m)[1] #carrying capacity
  R = coef(m)[2] #rate of growth
  m0 = coef(m)[3] #initial population size
  
  
  ##compile the data into one output
  out <- data.frame(cbind(c(K=K,R=R,m0=m0,Corr = correlation_value)))
  
  ##throw a name onto the output column
  names(out)[1] <- "Logistic Curve"
  
  ##return the output
  return(out)
}
