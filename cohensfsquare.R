cohensfsquared <- function(testmod) {
  
  if(class(testmod) != "lm") {
    stop("testmod must be an lm object.")
  }
  
  tempdata = testmod$model #Get data from model
  
  for (camille in 1:ncol(tempdata)) { #Replace factors with numeric
    if(! is.numeric(tempdata[,camille])) {
      tempdata[,camille] = as.numeric(as.factor(tempdata[,camille]))
    }
  }
  
  names = attributes(testmod$terms)$term.labels #Get names of regressors
  
  fullrsquared = summary(testmod)$r.squared #Get full model r-squared
  usenames = names
  interactioncheck = grepl(":",names) #Check for interactions
  
  if (any(interactioncheck)) { #If there is an interaction, calculate the interaction term and specially name it in the temporary dataset
    
    interactionvars = str_split(names[interactioncheck],":")
    
    for (zeek in 1:length(interactionvars)) {
      for (adam in 1:length(interactionvars[[zeek]])) {
        if (adam == 1) {
          tempdata$tempname = tempdata[,(interactionvars[[zeek]][adam])] 
        } else {tempdata$tempname = tempdata$tempname * tempdata[,paste(interactionvars[[zeek]][adam])] }
      }
      names(tempdata)[ncol(tempdata)] = paste("interaction",zeek,sep="_")
      if (exists("tempnames")) {
        tempnames = c(tempnames, paste("interaction",zeek,sep="_"))
      } else {tempnames = paste("interaction",zeek,sep="_")}
    }
    usenames[interactioncheck] = tempnames
  }
  
  outtable = data.frame(regressors = names, f_squared = rep(NA,length(names))) #Generate an empty output table
  
  for (sarah in 1:length(usenames)) { #Calculate Cohen's f squared for each variable
    regressors = usenames[!1:length(usenames) %in% sarah]
    tempmod = lm(tempdata[,1]~as.matrix(tempdata[,c(regressors)]))
    tempr = summary(tempmod)$r.squared
    
    f = (fullrsquared - tempr)/(1 - fullrsquared)
    
    outtable[sarah,"f_squared"] = f
    
  }
  print(outtable)
}
