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
zscale.rd <- function(x, r = r.idx, d = d.idx){
  x[r] <- (x[r]-mean(x[r], na.rm=T))/sd(x[r], na.rm = T)
  x[d] <- (x[d]-mean(x[d], na.rm=T))/sd(x[d], na.rm = T)
  return(x)
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