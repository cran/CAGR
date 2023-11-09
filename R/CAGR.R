#' Compute CAGR(Compound Annual Growth Rate)
#'
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


