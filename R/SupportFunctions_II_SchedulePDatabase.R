#' @title ins.line.data
#' @description It obtains the raw data from Schedule P Data
#' @param g.code Company code
#' @param a Schedule P Database
#' @return Raw data from the selected company code
#' @import keras
#' @import abind
#' @export
#'

ins.line.data=function(g.code,a){
  b=subset(a,a$GRCODE==g.code)
  name=b$GRNAME
  grpcode=b$GRCODE
  ay=b$AccidentYear
  dev=b$DevelopmentLag
  cum_incloss=b[,6]
  cum_pdloss=b[,7]
  bulk_loss=b[,8]
  dir_premium=b[,9]
  ced_premium=b[,10]
  net_premium=b[,11]
  single=b[,12]
  posted_reserve97=b[,13]
  # get incremental paid losses - assume data is sorted by ay and dev
  inc_pdloss=numeric(0)
  for (i in unique(ay)){
    s=(ay==i)
    pl=c(0,cum_pdloss[s])
    ndev=length(pl)-1
    il=rep(0,ndev)
    for (j in 1:ndev){
      il[j]=pl[j+1]-pl[j]
    }
    inc_pdloss=c(inc_pdloss,il)
  }
  data.out=data.frame(name,grpcode,ay,dev,net_premium,dir_premium,ced_premium,
                      cum_pdloss,cum_incloss,bulk_loss,inc_pdloss,single,posted_reserve97)
  return(data.out)
}


#' @title CumulativeT.SchedudeP
#' @description It creates the cumulative payments triangle of one company from the Schedule P dataset.
#' @param Row.Data Raw data of the selected company.
#' @return Cumulative payments triangle.
#' @import keras
#' @import abind
#' @export
#'

CumulativeT.SchedudeP=function(Row.Data){
  AY=unique(Row.Data$ay);DY=unique(Row.Data$dev)
  dimension=length(AY);Triangle=matrix(0,dimension,dimension)
  for (i in 1:dimension){
    for (k in 1:(dimension-i+1)){
      Obs=subset(Row.Data, Row.Data$ay==AY[i] & Row.Data$dev==DY[k])
      Triangle[i,k]=Obs$cum_pdloss
    }
  }
  return(Triangle)
}


#' @title CumulativeT.SchedudeP.Validation
#' @description It creates the upper and lower cumulative payments triangle of one company from the Schedule P dataset.
#' @param Row.Data Raw data of the selected company.
#' @return Cumulative payments upper and lower triangle.
#' @import keras
#' @import abind
#' @export
#'

CumulativeT.SchedudeP.Validation=function(Row.Data){
  AY=unique(Row.Data$ay);DY=unique(Row.Data$dev)
  dimension=length(AY);Triangle=matrix(0,dimension,dimension)
  for (i in 1:dimension){
    for (k in 1:(dimension)){
      Obs=subset(Row.Data, Row.Data$ay==AY[i] & Row.Data$dev==DY[k])
      Triangle[i,k]=Obs$cum_pdloss
    }
  }
  return(Triangle)
}


#' @title NetEarned.SchedudeP
#' @description It generates a vector with the premiums by accident year of one company from the Schedule P dataset.
#' @param Row.Data Raw data of the selected company.
#' @return Premiums by accident year.
#' @import keras
#' @import abind
#' @export
#'

NetEarned.SchedudeP=function(Row.Data){
  AY=unique(Row.Data$ay);DY=unique(Row.Data$dev)
  dimension=length(AY);Triangle=0
  for (i in 1:dimension){
    Obs=subset(Row.Data, Row.Data$ay==AY[i] & Row.Data$dev==DY[(dimension)])
    Triangle[i]=Obs$net_premium
  }
  return(Triangle)
}

#' @title IncurredT.SchedudeP
#' @description It creates the incurred cost triangle of one company from the Schedule P dataset.
#' @param Row.Data Raw data of the selected company.
#' @return Incurred cost triangle.
#' @import keras
#' @import abind
#' @export
#'

IncurredT.SchedudeP=function(Row.Data){
  AY=unique(Row.Data$ay);DY=unique(Row.Data$dev)
  dimension=length(AY);Triangle=matrix(0,dimension,dimension)
  for (i in 1:dimension){
    for (k in 1:(dimension-i+1)){
      Obs=subset(Row.Data, Row.Data$ay==AY[i] & Row.Data$dev==DY[k])
      Triangle[i,k]=Obs$cum_incloss-Obs$bulk_loss
    }
  }
  return(Triangle)
}


#' @title IncurredT.SchedudeP.Validation
#' @description It creates the upper and lower incurred cost triangle of one company from the Schedule P dataset.
#' @param Row.Data Raw data of the selected company.
#' @return Incurred cost upper and lower triangle.
#' @import keras
#' @import abind
#' @export
#'

IncurredT.SchedudeP.Validation=function(Row.Data){
  AY=unique(Row.Data$ay);DY=unique(Row.Data$dev)
  dimension=length(AY);Triangle=matrix(0,dimension,dimension)
  for (i in 1:dimension){
    for (k in 1:(dimension)){
      Obs=subset(Row.Data, Row.Data$ay==AY[i] & Row.Data$dev==DY[k])
      Triangle[i,k]=Obs$cum_incloss-Obs$bulk_loss
    }
  }
  return(Triangle)
}
