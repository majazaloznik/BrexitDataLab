require(tidyr)
require(dplyr)

## function to calculate the result based on count and 3 rates:

FunCalculateResult <- function(all.6age.groups, base="count") {
  if (base== "lexp") all.6age.groups$count <- all.6age.groups$years.left
  ## turount 
  all.6age.groups %>%
    mutate(registered.count = count*registered.prop,
           turnout.count =registered.count *turnout.prop,
           vote.remain = registered.prop*count*turnout.prop * remain.prop,
           vote.leave = registered.prop*count*turnout.prop * (1-remain.prop)) %>%
    summarise(count = sum(count), turnout.count = sum(turnout.count),
              registered.count = sum(registered.count),
              registered.prop = registered.count/count,
              turnout.prop = turnout.count/registered.count,
              remain.count = sum(vote.remain),
              leave.count = sum(vote.leave),
              remain.prop=remain.count/turnout.count,
              leave.prop = leave.count/turnout.count) ->    
    estimates.summary 
  return(as.data.frame(estimates.summary))
}



FunBestPlot <- function(all.6age.groups, base="count"){
  if (base == "lexp") all.6age.groups$count <- all.6age.groups$years.left
  all.6age.groups %>%
    mutate(turnout.abs = turnout.prop*registered.prop,
           turnout.count = count*turnout.abs,
           not.reg.count=(1-registered.prop)*count,
           unvoting.count = count-turnout.count,
           not.reg.prop.wasted = not.reg.count/unvoting.count) %>%
    select(age.group, unvoting.count, not.reg.prop.wasted,
           turnout.count, remain.prop) %>%
    gather( waste, prop, c(3,5))%>%
    mutate(prop.alt = 1-prop,
           denom = ifelse(waste=="remain.prop", turnout.count, unvoting.count),
           yellow=ifelse(waste=="remain.prop", prop, 0),
           blue = ifelse(waste=="remain.prop", prop.alt, 0),
           black = ifelse(waste=="not.reg.prop.wasted", prop, 0),
           gray = ifelse(waste=="not.reg.prop.wasted", prop.alt, 0))%>%
    select(age.group, waste, prop, prop.alt, denom, yellow, blue, black, gray) %>%
    arrange(age.group) -> all.6age.groups.best.plot2 
  
  par(xpd=TRUE)
  layout(matrix(1:2,2), heights=c(1,5))
  par(mar=c(1,3.6,0.1,0.1))
  barplot(t(as.matrix(FunCalculateResult(all.6age.groups)[8:9])), 
          beside = FALSE, horiz = TRUE,xlim=c(0,1), axes=FALSE,
          col=c("yellow", "blue"), names.arg = "Result", las=2)
  r <- 100*as.matrix(FunCalculateResult(all.6age.groups))[8]
  l <- 100*as.matrix(FunCalculateResult(all.6age.groups))[9]
  text(0.25, 0.6, round(r,2), col= ifelse(r>l, "red", "black"), cex=1.6)
  text(0.75, 0.6, round(l,2),col= ifelse(l>r, "red", "black"), cex=1.6)
  
  lines(c(0.5,0.5),c(0,1.3), col="red", lwd=2, lty=2)
  par(mar=c(2.1,3.6,0.1,0.1))
  mp <- barplot(t(as.matrix(all.6age.groups.best.plot2[,8:9])),
                width=all.6age.groups.best.plot2$denom, col=c("black", "gray"),
                horiz=TRUE, xlim=c(0,1), space=c(rep(c(1,0),6)), 
                density=20)
  data.frame( rep(1:6, each=2), mp, pair=rep(c(1,2),6)) %>% 
    spread(pair, mp) -> pos
  mp<- apply(pos[,2:3], 1, mean)
  
  barplot(t(as.matrix(all.6age.groups.best.plot2[,6:7])),
          width=all.6age.groups.best.plot2$denom, col=c("yellow", "blue"),
          horiz=TRUE, xlim=c(0,1), space=c(rep(c(1,0),6)), add=TRUE)
  text(-0.05, mp, unique(all.6age.groups.best.plot2$age.group))
  lines(c(0.5,0.5),c(0,  par("usr")[4]), col="red", lwd=2, lty=2)
}


FunResultPlot <- function(x){
  par(mar=c(2.1,4.1,0,0))
  par(xpd=TRUE)
  par(mfrow=c(2,1))
  barplot(t(as.matrix(x[8:9])), 
          beside = FALSE, horiz = TRUE,
          col=c("yellow", "blue"))
  lines(c(0.5,0.5),c(0,1.3), col="red", lwd=2, lty=2)
}


