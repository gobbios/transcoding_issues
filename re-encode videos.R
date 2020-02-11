library(avutils) # for read_rttm function
xfiles <- list.files("yunifiles")
xfiles <- sort(unique(substr(xfiles, 15, 18)))

xdata <- data.frame(file = rep(xfiles, each = 3), 
                    ori_vid = c(rep("mts", 9 * 3), rep("mov", 21 * 3), rep("mp4", 9 * 3)),
                    trans_to = c("mp4", "mov", "avi"), 
                    stringsAsFactors = FALSE)
xdata$eval <- substr(xdata$file, 1, 2)

xdata$ori_adu_voc <- NA
xdata$trans_adu_voc <- NA

xdata$ori_chi_dur <- NA
xdata$trans_chi_dur <- NA


for (i in 1:nrow(xdata)) {
  # read ori
  odata <- read_rttm(paste0("yunifiles/yunitator_old_", xdata$file[i], "_0.rttm"))
  # read transformed
  if (xdata$trans_to[i] == "mp4") suf <- "_1.rttm"
  if (xdata$trans_to[i] == "mov") suf <- "_2.rttm"
  if (xdata$trans_to[i] == "avi") suf <- "_3.rttm"
  tdata <- read_rttm(paste0("yunifiles/yunitator_old_", xdata$file[i], suf))
  
  xdata$ori_adu_voc[i] <- sum(odata$tier %in% c("FEM", "MAL"))
  xdata$trans_adu_voc[i] <- sum(tdata$tier %in% c("FEM", "MAL"))
  
  xdata$ori_chi_dur[i] <- sum(odata$duration[odata$tier == "CHI"])
  xdata$trans_chi_dur[i] <- sum(tdata$duration[tdata$tier == "CHI"])
  
  rm(suf, odata, tdata)
}


xdata$sym <- c(0, 16, 2, 3)[as.numeric(as.factor(xdata$eval))]
xdata$xcol <- "black"
xdata$xcol[xdata$eval == "11"] <- "red"
xdata$xcol[xdata$eval == "16"] <- "orange"


par(mfcol = c(2, 3), las= 1, family = "serif")
pdata <- xdata[xdata$trans_to == "mp4", ]
plot(pdata$ori_adu_voc, pdata$trans_adu_voc, pch = pdata$sy, col = pdata$xcol, xlab = "adult WC", ylab = "adult VC after video transform", asp = 1)
title(main = "recode to MP4")
abline(0, 1, lty = 3)
plot(pdata$ori_chi_dur, pdata$trans_chi_dur, pch = pdata$sy, col = pdata$xcol, xlab = "child voc duration", ylab = "child voc dur after video transform", asp = 1)
abline(0, 1, lty = 3)
title(main = "recode to MP4")

pdata <- xdata[xdata$trans_to == "mov", ]
plot(pdata$ori_adu_voc, pdata$trans_adu_voc, pch = pdata$sy, col = pdata$xcol, xlab = "adult WC", ylab = "adult VC after video transform", asp = 1)
title(main = "recode to MOV")
abline(0, 1, lty = 3)
plot(pdata$ori_chi_dur, pdata$trans_chi_dur, pch = pdata$sy, col = pdata$xcol, xlab = "child voc duration", ylab = "child voc dur after video transform", asp = 1)
abline(0, 1, lty = 3)
title(main = "recode to MOV")

pdata <- xdata[xdata$trans_to == "avi", ]
plot(pdata$ori_adu_voc, pdata$trans_adu_voc, pch = pdata$sy, col = pdata$xcol, xlab = "adult WC", ylab = "adult VC after video transform", asp = 1)
title(main = "recode to AVI")
abline(0, 1, lty = 3)
plot(pdata$ori_chi_dur, pdata$trans_chi_dur, pch = pdata$sy, col = pdata$xcol, xlab = "child voc duration", ylab = "child voc dur after video transform", asp = 1)
abline(0, 1, lty = 3)
title(main = "recode to AVI")

# redo with different benchmark --------------
xdata$ori_adu_voc <- NA
xdata$trans_adu_voc <- NA

xdata$ori_chi_dur <- NA
xdata$trans_chi_dur <- NA


for (i in 1:nrow(xdata)) {
  # read ori
  odata <- read_rttm(paste0("yunifiles/yunitator_old_", xdata$file[i], "_2.rttm"))
  # read transformed
  if (xdata$trans_to[i] == "mp4") suf <- "_1.rttm"
  if (xdata$trans_to[i] == "mov") suf <- "_0.rttm"
  if (xdata$trans_to[i] == "avi") suf <- "_3.rttm"
  tdata <- read_rttm(paste0("yunifiles/yunitator_old_", xdata$file[i], suf))
  
  xdata$ori_adu_voc[i] <- sum(odata$tier %in% c("FEM", "MAL"))
  xdata$trans_adu_voc[i] <- sum(tdata$tier %in% c("FEM", "MAL"))
  
  xdata$ori_chi_dur[i] <- sum(odata$duration[odata$tier == "CHI"])
  xdata$trans_chi_dur[i] <- sum(tdata$duration[tdata$tier == "CHI"])
  
  rm(suf, odata, tdata)
}


xdata$sym <- c(0, 16, 2, 3)[as.numeric(as.factor(xdata$eval))]
xdata$xcol <- "black"
xdata$xcol[xdata$eval == "11"] <- "red"
xdata$xcol[xdata$eval == "16"] <- "orange"

par(mfcol = c(2, 3), las= 1, family = "serif")
pdata <- xdata[xdata$trans_to == "mp4", ]
plot(pdata$ori_adu_voc, pdata$trans_adu_voc, pch = pdata$sy, col = pdata$xcol, xlab = "adult WC", ylab = "adult VC after video transform", asp = 1)
title(main = "recode to MP4")
abline(0, 1, lty = 3)
plot(pdata$ori_chi_dur, pdata$trans_chi_dur, pch = pdata$sy, col = pdata$xcol, xlab = "child voc duration", ylab = "child voc dur after video transform", asp = 1)
abline(0, 1, lty = 3)
title(main = "recode to MP4")

pdata <- xdata[xdata$trans_to == "mov", ]
plot(pdata$ori_adu_voc, pdata$trans_adu_voc, pch = pdata$sy, col = pdata$xcol, xlab = "adult WC", ylab = "adult VC after video transform", asp = 1)
title(main = "original")
abline(0, 1, lty = 3)
plot(pdata$ori_chi_dur, pdata$trans_chi_dur, pch = pdata$sy, col = pdata$xcol, xlab = "child voc duration", ylab = "child voc dur after video transform", asp = 1)
abline(0, 1, lty = 3)
title(main = "original")

pdata <- xdata[xdata$trans_to == "avi", ]
plot(pdata$ori_adu_voc, pdata$trans_adu_voc, pch = pdata$sy, col = pdata$xcol, xlab = "adult WC", ylab = "adult VC after video transform", asp = 1)
title(main = "recode to AVI")
abline(0, 1, lty = 3)
plot(pdata$ori_chi_dur, pdata$trans_chi_dur, pch = pdata$sy, col = pdata$xcol, xlab = "child voc duration", ylab = "child voc dur after video transform", asp = 1)
abline(0, 1, lty = 3)
title(main = "recode to AVI")
