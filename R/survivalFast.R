#' @title Survival Analysis
#' @description Create a survival object,life table,Kaplan-Meier curve,statistics.see Details below.
#'
#' @param survTime a vector with number indicating survival time.
#' @param censor a vector with nominal categories(0、1).For right censored data, 1 is event；0 is censored.
#' @param group a vector with nominal categories.
#' @param y a number of time point.Estimate the statistic at an time.
#' @param conf.level the level for a two-sided confidence interval on the survival curve(s). Default is 0.95.
#' @param conf.type a character string specifying the type of confidence interval.Possible values are "plain"(Default),"log".
#' @param hazard.type a character string specifying the type of hazard function.Possible values are "likelihood"(Default),"Nelson".
#' @param plot a logical value indicating whether plot should be shown.
#' @param plot.conf a logical value indicating whether confidence interval should be added on the plot.
#' @param statistics a logical value indicating whether statistics should be computed.
#'
#' @return The result of Life tables,KM curve and statistics.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter summarise n
#' @importFrom huxtable hux everywhere final set_bold set_position set_font set_bottom_border set_width set_all_padding
#' @importFrom stats qnorm
#' @importFrom graphics legend lines points rect
#' @importFrom ggplot2 alpha
#' @export
#'
#' @examples
#' library(SurvivalFast)
#'
#' survivalFast(canser$Week,            # 時間(必填)
#'              canser$Censord,         # 設限(必填)，需定義為：1 = event；0 = censor
#'              canser$Group,           # 分組(必填)，接受兩組以上
#'              y = 8,                  # 時間點，估計該時間點的統計量
#'              conf.level = 0.95,      # 信心水準，計算信賴區間。預設為0.95
#'              conf.type = "log",      # 計算信賴區間方式：plain；log，預設為plain。
#'              hazard.type = "Nelson", # 計算累積風險函數方式：likelihood；Nelson，預設為likelihood。
#'              plot = TRUE,            # 顯示圖表與否。預設為TRUE
#'              plot.conf = TRUE,       # 顯示信賴區間與否。預設為TRUE
#'              statistics = TRUE)      # 顯示統計量與否。預設為TRUE

survivalFast <- function(survTime,censor,group=1,y = 0,conf.level=0.95,
                     conf.type = "plain",hazard.type = "likelihood",
                     plot = TRUE,plot.conf = TRUE,statistics = TRUE){
  Censord <- NULL
  Group <- NULL
  Week <- NULL
  # 取得發生事件人數
  get_dj <- function(time,dataseOrder){
    sapply(time,function(time){
      counter <- dataseOrder %>% dplyr::filter(Censord == 1 & Week == time) %>% dplyr::summarise(count = dplyr::n())
      return(counter$count)
    })
  }
  # 取得設限數
  get_qj <- function(time,dataseOrder){
    sapply(time,function(x){
      if(x == time[length(time)]){
        counter <- dataseOrder %>%
          dplyr::filter(Censord == 0 & Week >= x) %>%
          summarise(count = n())
      } else {
        counter <- dataseOrder %>%
          dplyr::filter(Censord == 0 & Week >= x & Week < time[match(x,time)+1]) %>%
          summarise(count = n())
      }
      return(counter$count)
    })
  }
  # 取得風險集人數
  get_nj <- function(time,dataseOrder,dj,qj){
    nj <- sapply(time,function(x){
      riskSet <- length(dataseOrder$Week) - sum(dj[1:match(x,time)-1])- sum(qj[1:match(x,time)-1])
      return(riskSet)
    })
  }
  # 計算存活函數
  get_st <- function(time,dj,nj){
    sapply(time,function(x){
      prob <- prod(sapply(time[1:match(x,time)],function(x){
        return((nj[match(x,time)] - dj[match(x,time)])/nj[match(x,time)])
      }))
      return(sprintf("%0.4f",round(prob,4)))
    })
  }
  # Greenwood公式計算存活函數的標準誤
  get_stderr <- function(time,dj,nj,st){
    sapply(time,function(x){
      std <- sum(sapply(time[1:match(x,time)],function(x){
        return(dj[match(x,time)]/(nj[match(x,time)]*(nj[match(x,time)] - dj[match(x,time)])))
      }))
      stderr <- sqrt(std*as.numeric(st[match(x,time)])*as.numeric(st[match(x,time)]))
      return(sprintf("%0.4f",round(stderr,4)))
    })
  }
  # 信賴區間上界
  get_upper <- function(time,st,std,conf.level,conf.type){
    sapply(time,function(x){
      z_alpha <- qnorm(1 - (1-conf.level) / 2)
      # C.I.
      upper <- as.numeric(st[match(x,time)]) + z_alpha * as.numeric(std[match(x,time)])
      km_upper <- ifelse(round(upper,4)>1,1, round(upper,4))
      # 轉換後C.I.
      log_upper <- log(as.numeric(st[match(x,time)])) + z_alpha * (as.numeric(std[match(x,time)])/as.numeric(st[match(x,time)]))
      log_km_upper <- ifelse(round(exp(log_upper),4)>1,1, round(exp(log_upper),4))
      return(ifelse(conf.type == "log",sprintf("%0.4f",log_km_upper), sprintf("%0.4f",km_upper)))
    })
  }
  # 信賴區間下界
  get_lower <- function(time,st,std,conf.level,conf.type){
    sapply(time,function(x){
      z_alpha <- qnorm(1 - (1-conf.level) / 2)
      # C.I.
      lower <- as.numeric(st[match(x,time)]) - z_alpha * as.numeric(std[match(x,time)])
      km_lower <- ifelse(round(lower,4)>1,1, round(lower,4))
      # 轉換後C.I.
      log_lower <- log(as.numeric(st[match(x,time)])) - z_alpha * (as.numeric(std[match(x,time)])/as.numeric(st[match(x,time)]))
      log_km_lower <- ifelse(round(exp(log_lower),4)>1,1, round(exp(log_lower),4))
      return(ifelse(conf.type == "log",sprintf("%0.4f",log_km_lower), sprintf("%0.4f",km_lower)))
    })
  }
  # 取前一個時間點
  getNearTime <- function(y,time){
    for(j in 0:y){
      if(is.na(match(y,time))){
        y = y-1
      }else{
        result = y
        break
      }
    }
    return(result)
  }
  # 存活函數變異數估計
  get_variance <- function(y,time,nj,dj,st){
    plus <- sum(sapply(dj[1:match(getNearTime(y,time),time)],function(x){
      x/(nj[match(x,dj)]*(nj[match(x,dj)]-x))
    }))

    (as.numeric(st[match(getNearTime(y,time),time)])^2)*plus
  }
  # 風險函數估計
  get_hazard <- function(y,time,nj,dj){
    d <- dj[match(getNearTime(y,time),time)]
    n <- nj[match(getNearTime(y,time),time)]
    t1 <- time[match(getNearTime(y,time),time)+1]
    t2 <- time[match(getNearTime(y,time),time)]
    return(d/(n*(t1-t2)))
  }
  # Nelson-Aalen風險函數估計
  get_hazard_Nelson <- function(y,time,nj,dj){
    plus <- sum(sapply(dj[1:match(getNearTime(y,time),time)],function(x){
      x/nj[match(x,dj)]
    }))
    return(plus)
  }
  # Nelson-Aalen風險函數變異數估計
  get_hazardVariance_Nelson <- function(y,time,nj,dj){
    plus <- sum(sapply(dj[1:match(getNearTime(y,time),time)],function(x){
      x/nj[match(x,dj)]^2
    }))
    return(plus)
  }
  data <- data.frame(Group = group,
                     Week = survTime,
                     Censord = censor)
  statAllTable <- data.frame(statistic = c("average lifetime","mean lifetime",
                                           "Average Hazard Rate","Survival Function",
                                           "Variance of Survival Function","Hazard Function",
                                           "Cumulative Hazard","Variance of Cumulative Hazard"))
  for(i in unique(data$Group)){
    dataseOrder <- data %>% dplyr::filter(Group == i)
    dataseOrder <- dataseOrder[order(dataseOrder$Week, decreasing = F),]#按照時間排序
    time <- c(0,unique(dataseOrder %>% dplyr::filter(Censord == 1))$Week)      #取得建表的時間點

    dj <- get_dj(time,dataseOrder)                                      #取得event事件數
    qj <- get_qj(time,dataseOrder)                                      #取得設限數
    nj <- get_nj(time,dataseOrder,dj,qj)                                #計算函存活函數
    st <- get_st(time,dj,nj)
    std <- get_stderr(time,dj,nj,st)
    upperCI <- get_upper(time,st,std,conf.level,conf.type)
    lowerCI <- get_lower(time,st,std,conf.level,conf.type)

    meanLifetime <- sprintf("%0.4f",sum(dataseOrder$Week)/length(dataseOrder$Week))
    meanSurvivalTime <- sum(sapply(time[2:length(time)], function(x){
      (x-time[match(x,time)-1])*as.numeric(st[match(x,time)-1])
    }))
    avgHazardRate <- sprintf("%0.4f",sum(dj)/sum(dataseOrder$Week))
    survivalFunction <- sprintf("%0.4f",as.numeric(st[match(getNearTime(y,time),time)]))
    variance <- sprintf("%0.4f",get_variance(y,time,nj,dj,st)) # 存活函數之變異數估計
    hazard <- sprintf("%0.4f",get_hazard(y,time,nj,dj)) # 風險函數估計
    # 累積風險函數估計
    cumulativeHazard <- sprintf("%0.4f",-log(as.numeric(st[match(getNearTime(y,time),time)])))
    cumulativeHazardVariance <- sprintf("%0.4f",as.numeric(variance)/as.numeric(survivalFunction)^2)
    # Nelson-Aalen 累積風險函數估計
    cumulativeHazard.Nelson <- sprintf("%0.4f",get_hazard_Nelson(y,time,nj,dj))
    # Nelson-Aalen 累積風險函數變異數估計
    cumulativeHazardVariance.Nelson <- sprintf("%0.4f",get_hazardVariance_Nelson(y,time,nj,dj))

    # Kaplan-Meier Table
    survTable <- data.frame(tj=time,n.risk=nj,n.event=dj,n.censor=qj,survival=st,
                            std.err=std,lower.ci=lowerCI,upper.ci=upperCI)
    upperCI.Name <- paste0("upper ",conf.level*100,"%"," CI")
    lowerCI.Name <- paste0("lower ",conf.level*100,"%"," CI")
    colnames(survTable) <- c("tj","risk","event","censor","surv","std.err",upperCI.Name,lowerCI.Name)
    table <- huxtable::hux(survTable) %>%
      huxtable::set_bold(row=1,col=huxtable::everywhere)%>%
      huxtable::set_bottom_border(row=1,col=huxtable::everywhere)%>%
      huxtable::set_bottom_border(huxtable::final(1), huxtable::everywhere)%>%
      huxtable::set_position("left")%>%
      huxtable::set_font(value = "Times New Roman")
    cat("\n\nGroup: ",i," | Life Table:\n\n")
    print(table)

    # 統計量
    statTable <- data.frame(value = c(meanLifetime,meanSurvivalTime,avgHazardRate,survivalFunction,variance,hazard,
                                      ifelse(hazard.type == "Nelson",cumulativeHazard.Nelson,cumulativeHazard),
                                      ifelse(hazard.type == "Nelson",cumulativeHazardVariance.Nelson,cumulativeHazardVariance)))
    statAllTable <- cbind(statAllTable,statTable)

    # KM Plot
    col = match(i,unique(data$Group))
    plot.time = c(time,max(dataseOrder$Week))
    plot.st = c(st,st[length(st)])
    censor.data <- dataseOrder %>% dplyr::filter(Censord == 0)
    censor.time <- censor.data$Week
    censor.st <- sapply(censor.time, function(y){
      as.numeric(st[match(getNearTime(y,time),time)])
    })
    # 建立CI
    if(upperCI[length(upperCI)] == "NA"){
      upperCI[length(upperCI)] = upperCI[length(upperCI)-1]
      lowerCI[length(lowerCI)] = lowerCI[length(lowerCI)-1]
    }
    plot.upperCI <- c(upperCI,upperCI[length(upperCI)])
    plot.lowerCI <- c(lowerCI,lowerCI[length(lowerCI)])

    if(plot){
      if(match(i,unique(data$Group))==1){
        plot(x = plot.time,y = plot.st, ylim = c(0, 1),xlim = c(0,max(data$Week)),
             xlab = "TIME",ylab = "Survival Function", main = "Kaplan-Meier Plot",
             type = 's',lwd = 2,bty="l",col = col)
      }else{
        lines(x = plot.time,y = plot.st,type = 's',lwd = 2,bty="l",col = col)
      }

      if(plot.conf){
        lines(plot.time, plot.upperCI,type = 's', lty = 1, col = ggplot2::alpha(col,0.3))
        lines(plot.time, plot.lowerCI,type = 's', lty = 1, col = ggplot2::alpha(col,0.3))
        for (i in 1:(length(plot.upperCI)-1)) {
          rect(xleft=plot.time[i], xright=plot.time[i+1],
               ybottom=plot.lowerCI[i], ytop=plot.upperCI[i], col= ggplot2::alpha(col,0.1), border=NA)
        }
      }
      points(x = censor.time,y = censor.st,pch = 18,lwd = 2,col = col)
    }
  }
  legend("topright",legend = c(unique(data$Group)),bty = "n",lty = 1,lwd = 2,col = 1:length(unique(data$Group)))
  colnames(statAllTable) <- c("statistic",sapply(unique(data$Group),function(x){paste0("Group:",x)}))
  statAllTable <- huxtable::hux(statAllTable) %>%
    huxtable::set_all_padding(4) %>%
    huxtable::set_position("left")%>%
    huxtable::set_bold(row=1,col=huxtable::everywhere)%>%
    huxtable::set_bottom_border(row=1, huxtable::everywhere)%>%
    huxtable::set_bottom_border(row=4, huxtable::everywhere)%>%
    huxtable::set_bottom_border(huxtable::final(1), huxtable::everywhere)%>%
    huxtable::set_width(0.8)
  if(statistics){
    cat("\n\nStatistics Table:\n\n")
    cat("Estimator at an time :",y,"\n\n")
    print(statAllTable)
  }
}
