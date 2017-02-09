zscale.rd <- function(x, r = r.idx, d = d.idx){
  x[r] <- (x[r]-mean(x[r], na.rm=T))/sd(x[r], na.rm = T)
  x[d] <- (x[d]-mean(x[d], na.rm=T))/sd(x[d], na.rm = T)
  return(x)
}

funZfm <- function(data){
  sname <- names(data)[-1]
  d <- apply(data[,-1], 1, norm.z) %>% as.data.frame()
  names(d) <- data$COMPID
  d <- sapply(d, fill.miss) %>% as.data.frame()
  d$ID <- sname
  row.names(d) <- sname
  d$GROUP <- samples$GROUP[which(samples$ID %in% sname)]
  d$LOC <- samples$LOC[which(samples$ID %in% sname)]
  d$COHORT <- samples$COHORT[which(samples$ID %in% sname)]
  d$AGEGROUP <- samples$AGEGROUP[which(samples$ID %in% sname)]
  d$PROC <- samples$PROC[which(samples$ID %in% sname)]
  d$QC <- samples$QC[which(samples$ID %in% sname)]
  return(d)
}

funZfm.rd <- function(data){
  sname <- names(data)[-1]
  g1 <- samples$ID[which(samples$COHORT == "C3" & samples$AGEGROUP %in% c("1","2") & samples$LOC == "R")]
  g2 <- samples$ID[which(samples$COHORT == "C3" & samples$AGEGROUP %in% c("1","2") & samples$LOC == "D")]
  r.idx <- which(sname %in% g1)
  d.idx <- which(sname %in% g2)
  d <- apply(data[,-1], 1, zscale.rd, r=r.idx, d=d.idx) %>% as.data.frame()
  names(d) <- data$COMPID
  d <- sapply(d, fill.miss) %>% as.data.frame()
  d$ID <- sname
  row.names(d) <- sname
  d$GROUP <- samples$GROUP[which(samples$ID %in% sname)]
  d$LOC <- samples$LOC[which(samples$ID %in% sname)]
  d$COHORT <- samples$COHORT[which(samples$ID %in% sname)]
  d$AGEGROUP <- samples$AGEGROUP[which(samples$ID %in% sname)]
  d$PROC <- samples$PROC[which(samples$ID %in% sname)]
  return(d)
}

class.svm <- function(data = data, feature = feature){
  set.seed(113)
  res <- data.frame()
  data$GROUP <- factor(data$GROUP, levels = c("TD","ASD"))
  idx.td <- which(data$GROUP == "TD")
  idx.asd <- which(data$GROUP == "ASD")
  for(i in c(1:100)){
    frac <- 0.5
    train.idx <- c(sample(idx.td,length(idx.td)*frac), 
                   sample(idx.asd, length(idx.asd)*frac)
    )
    train <- data[train.idx,]
    test <- data[-c(train.idx),]
    wts <- 8/table(train$GROUP)
    model <- svm(GROUP ~., data = train, class.weights = wts)
    pred <- predict(model, test)
    td.true <- which(pred == "TD" & test$GROUP == "TD") %>% length()
    asd.true <- which(pred == "ASD" & test$GROUP == "ASD") %>% length()
    td.total <- which(test$GROUP == "TD") %>% length()
    asd.total <- which(test$GROUP == "ASD") %>% length()
    res[i, "acc"] <- (td.true + asd.true)/(td.total + asd.total) %>% round(digits = 3)
    res[i, "sens"] <- asd.true/asd.total
    res[i, "spec"] <- td.true/td.total
  }
  return(res)
}