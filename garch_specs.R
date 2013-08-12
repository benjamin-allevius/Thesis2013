# Which solver to use
slvr <- "hybrid"
slvr.ctrl <- list(n.restarts=3, parallel=TRUE, pkg="multicore", cores=4)



# All models have an AR(1) process to model the mean

#########################################   GARCH(1,1) ###################################
###   GARCH(1,1) with normal innovations
garch.spec.norm <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), 
                              mean.model = list(armaOrder=c(1,0)), 
                              distribution.model="norm")

###   GARCH(1,1) with t innovations, 4 degrees of freedom
garch.spec.std <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), 
                             mean.model = list(armaOrder=c(1,0)), 
                             distribution.model="std",
                             fixed.pars = list(shape=df))

###   GARCH(1,1) with skew-t innovations
garch.spec.sstd <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), 
                              mean.model = list(armaOrder=c(1,0)), 
                              distribution.model="sstd",
                              fixed.pars = list(shape=df))
#########################################################################################


#########################################   GJR-GARCH(1,1) ##############################
###   GJR-GARCH(1,1) with normal innovations
gjr.spec.norm <- ugarchspec(variance.model = list(model="gjrGARCH", garchOrder=c(1,1)), 
                            mean.model = list(armaOrder=c(1,0)), 
                            distribution.model="norm")

###   GJR-GARCH(1,1) with t innovations
gjr.spec.std <- ugarchspec(variance.model = list(model="gjrGARCH", garchOrder=c(1,1)), 
                           mean.model = list(armaOrder=c(1,0)), 
                           distribution.model="std",
                           fixed.pars = list(shape=df))

#########################################################################################

#########################################   FAMILY-GARCH(1,1) ###########################
###   family-GARCH(1,1) with normal innovations
fam.spec.norm <- ugarchspec(variance.model = list(model="fGARCH", garchOrder=c(1,1), submodel="ALLGARCH"), 
                            mean.model = list(armaOrder=c(1,0)), 
                            distribution.model="norm")

fam.spec.std <- ugarchspec(variance.model = list(model="fGARCH", garchOrder=c(1,1), submodel="ALLGARCH"), 
                           mean.model = list(armaOrder=c(1,0)), 
                           distribution.model="std",
                           fixed.pars = list(shape=df))
#########################################################################################

#########################################   component-GARCH(1,1) ########################
cs.spec.norm <- ugarchspec(variance.model = list(model="csGARCH", garchOrder=c(1,1)), 
                           mean.model = list(armaOrder=c(1,0)), 
                           distribution.model="norm")

cs.spec.std <- ugarchspec(variance.model = list(model="csGARCH", garchOrder=c(1,1)), 
                          mean.model = list(armaOrder=c(1,0)), 
                          distribution.model="std",
                          fixed.pars = list(shape=df))
#########################################################################################