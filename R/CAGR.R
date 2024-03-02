

#' @title Compute CAGR(Compound Annual Growth Rate)
#' @description Compute CAGR(Compound Annual Growth Rate)
#' @param data.1 data of the first year
#' @param data.n data of the last year
#' @param n number of years
#'
#' @return CAGR and between years values
#' @export
#' @usage CAGR(data.1, data.n, n)
#' @examples c.cagr<-CAGR(100, 189, 5)
#' @references Bardhan, D., Singh, S.R.K., Raut, A.A.and Athare, T.R. (2022). Livestock in Madhya Pradesh and Chhattisgarh: An Analysis for Some Policy Implications. Agricultural Science Digest. DOI:10.18805/ag.D-5418.


CAGR <- function(data.1, data.n, n){

  r<-((data.n/data.1)^(1/n)-1)
  ### generate data ###
  df<-data.frame()

  for(i in 1:n)
  {
    output=(data.1*(1+r)^i)
    df=rbind(df, output)
  }
  colnames(df)<-'y'

  y<-as.vector(rbind(data.1,df))
  y<-as.data.frame(y)

  Output_CAGR <- list(CAGR = r*100, Values = y)
  return(Output_CAGR)
}




#' @title Computing Last Year data
#' @description Computing last year data
#'
#'
#' @param data.1 data of the first year
#' @param r CAGR
#' @param n number of years
#'
#' @return Last year data and between years values
#' @export
#' @usage data.last(data.1, r, n)
#' @examples d.last<-data.last(100, 13.57751, 5)
#' @references Bardhan, D., Singh, S.R.K., Raut, A.A.and Athare, T.R. (2022). Livestock in Madhya Pradesh and Chhattisgarh: An Analysis for Some Policy Implications. Agricultural Science Digest. DOI:10.18805/ag.D-5418.


data.last<-function(data.1, r, n){
  df <- data.frame()

  for (i in 1:n) {
    data.n = (data.1 * (1 + r/100)^n)
    output = (data.1 * (1 + r/100)^i)
    df = rbind(df, output)
  }
  colnames(df) <- "y"
  y <- as.vector(rbind(data.1, df))
  y <- as.data.frame(y)
  Output_CAGR <- list(data.n, Values = y)
  return(Output_CAGR)
}




#' @title Computing First Year data
#' @description Computing first year data
#'
#'
#' @param data.n data of the last year
#' @param r CAGR
#' @param n number of years
#'
#' @return First year data and between years values
#' @export
#' @usage data.first(data.n, r, n)
#' @examples d.first<-data.first(189, 13.57751, 5)
#' @references Bardhan, D., Singh, S.R.K., Raut, A.A.and Athare, T.R. (2022). Livestock in Madhya Pradesh and Chhattisgarh: An Analysis for Some Policy Implications. Agricultural Science Digest. DOI:10.18805/ag.D-5418.


data.first<-function(data.n, r, n){
  df <- data.frame()

  for (i in 1:n) {
    data.1 = (data.n * (1 + r/100)^(-n))
    output = (data.1 * (1 + r/100)^i)
    df = rbind(df, output)
  }
  colnames(df) <- "y"
  y <- as.vector(rbind(data.1, df))
  y <- as.data.frame(y)
  Output_CAGR <- list(data.1, Values = y)
  return(Output_CAGR)
}






#' @title Computing Number of Years
#' @description Computing number of years
#'
#' @param data.1 data of the first year
#' @param data.n data of the last year
#' @param r CAGR
#'
#' @return Number of years and between years values
#' @export
#' @usage n.years(data.1, data.n, r)
#' @examples n.yrs<-n.years(100, 189, 13.57751)
#' @references Bardhan, D., Singh, S.R.K., Raut, A.A.and Athare, T.R. (2022). Livestock in Madhya Pradesh and Chhattisgarh: An Analysis for Some Policy Implications. Agricultural Science Digest. DOI:10.18805/ag.D-5418.


n.years<-function(data.1, data.n, r){
  df <- data.frame()
  n = (log10(data.n/data.1)/log10(1+(r/100)))
  n = round(n, digits = 0)
  for (i in 1:n) {
    output = (data.1 * (1 + r/100)^i)
    df = rbind(df, output)
  }
  colnames(df) <- "y"
  y <- as.vector(rbind(data.1, df))
  y <- as.data.frame(y)
  Output_CAGR <- list(n, Values = y)
  return(Output_CAGR)
}







