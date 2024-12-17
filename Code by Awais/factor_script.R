#Call variables ordered for DWLS 
anes_2016_merge[,c("V161365", "V161370", "V161372", "V161381", "V161386", "V161391", "V161393", 
                   "V161404", "rc_V161363d", "rcs_V162006")] <-
  lapply(anes_2016_merge[,c("V161365", "V161370", "V161372", "V161381", "V161386", "V161391", "V161393", 
                            "V161404", "rc_V161363d", "rcs_V162006")], ordered)
#Models
media_model = 'con_cable =~  V161370 + V161372 + V161391
              lib_cable =~ V161365  + V161381 + V161386 + V161393 + V161404
              digital =~ rc_V161495 + rc_V161363d + rc_V162004 + rcs_V162006'

#Media Model
fit_media <- cfa(media_model, data = anes_2016_merge, std.lv=TRUE,
                 ordered=c("V161370", "V161372", "V161391",
                           "V161365", "V161381", "V161386", "V161393", "V161404",
                           "rc_V161495", "rc_V161363d", "rc_V162004", "rcs_V162006"))
options(knitr.kable.NA = '')
summary(fit_media, fit.measures = TRUE, standardized = TRUE)
library(xtable)
xtable(parameterEstimates(fit_media))

#Factor Scores
head(lavPredict(fit_media))

## merge factor scores to original data.frame
## ------------------------------------------

idx <- lavInspect(fit_media, "case.idx")
fscores <- lavPredict(fit_media)
## loop over factors
for (fs in colnames(fscores)) {
  anes_2016_merge[idx, fs] <- fscores[ , fs]
}
head(anes_2016_merge)
