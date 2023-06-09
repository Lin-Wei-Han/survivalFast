% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survivalFast.R
\name{survivalFast}
\alias{survivalFast}
\title{Survival Analysis}
\usage{
survivalFast(
  survTime,
  censor,
  group = 1,
  y = 0,
  conf.level = 0.95,
  conf.type = "plain",
  hazard.type = "likelihood",
  plot = TRUE,
  plot.conf = TRUE,
  statistics = TRUE
)
}
\arguments{
\item{survTime}{a vector with number indicating survival time.}

\item{censor}{a vector with nominal categories(0、1).For right censored data, 1 is event；0 is censored.}

\item{group}{a vector with nominal categories.}

\item{y}{a number of time point.Estimate the statistic at an time.}

\item{conf.level}{the level for a two-sided confidence interval on the survival curve(s). Default is 0.95.}

\item{conf.type}{a character string specifying the type of confidence interval.Possible values are "plain"(Default),"log".}

\item{hazard.type}{a character string specifying the type of hazard function.Possible values are "likelihood"(Default),"Nelson".}

\item{plot}{a logical value indicating whether plot should be shown.}

\item{plot.conf}{a logical value indicating whether confidence interval should be added on the plot.}

\item{statistics}{a logical value indicating whether statistics should be computed.}
}
\value{
The result of Life tables,KM curve and statistics.
}
\description{
Create a survival object,life table,Kaplan-Meier curve,statistics.see Details below.
}
\examples{
library(SurvivalFast)

survivalFast(canser$Week,            # 時間(必填)
             canser$Censord,         # 設限(必填)，需定義為：1 = event；0 = censor
             canser$Group,           # 分組(必填)，接受兩組以上
             y = 8,                  # 時間點，估計該時間點的統計量
             conf.level = 0.95,      # 信心水準，計算信賴區間。預設為0.95
             conf.type = "log",      # 計算信賴區間方式：plain；log，預設為plain。
             hazard.type = "Nelson", # 計算累積風險函數方式：likelihood；Nelson，預設為likelihood。
             plot = TRUE,            # 顯示圖表與否。預設為TRUE
             plot.conf = TRUE,       # 顯示信賴區間與否。預設為TRUE
             statistics = TRUE)      # 顯示統計量與否。預設為TRUE
}
