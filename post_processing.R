library(xtable)


alignment <- c("l", rep("r", 2*nmodels))

# Count NA values
count.na <- colSums(is.na(data.var))

# VaR breaks, expected and actual

count.breaks <- as.data.frame(cbind(round((length.dataset - n) * (1 - qs)), # Expected number of VaR breaks
                                    matrix(colSums(data.break, na.rm=TRUE),  # Actual number of breaks
                                           nrow=nq, byrow=TRUE)), 
                              row.names=NULL)
names(count.breaks) <- c("Expected", 
                         "Sn", "SnP",
                         "St", "StP",
                         "Gn", "GnP",
                         "Gt", "GtP",
                         "Cn", "CnP",
                         "Ct", "CtP")
count.breaks.tablename <- paste("./", dset, "_breaks.tex", sep="")
print(xtable(count.breaks, 
             align=c("l", rep("r", 1+2*nmodels)), 
             digits=0), 
      type="latex", 
      file=count.breaks.tablename)

# Unconditional coverage test
uc.test <- as.data.frame(matrix(pvals.var[1,], nrow=nq, byrow=TRUE), row.names=as.character(qs))
names(uc.test) <- c("Sn", "SnP",
                    "St", "StP",
                    "Gn", "GnP",
                    "Gt", "GtP",
                    "Cn", "CnP",
                    "Ct", "CtP")
uc.tablename <- paste("./", dset, "_uc.tex", sep="")
print(xtable(uc.test, 
             align=alignment, 
             digits=3), 
      type="latex", 
      file=uc.tablename)

# Independence test
ind.test <- as.data.frame(matrix(pvals.var[2,], nrow=nq, byrow=TRUE), row.names=as.character(qs))
names(ind.test) <- c("Sn", "SnP",
                    "St", "StP",
                    "Gn", "GnP",
                    "Gt", "GtP",
                    "Cn", "CnP",
                    "Ct", "CtP")
ind.tablename <- paste("./", dset, "_ind.tex", sep="")
print(xtable(ind.test, 
             align=alignment, 
             digits=3), 
      type="latex", 
      file=ind.tablename)

# Conditional coverage test
cc.test <- as.data.frame(matrix(pvals.var[3,], nrow=nq, byrow=TRUE), row.names=as.character(qs))
names(cc.test) <- c("Sn", "SnP",
                     "St", "StP",
                     "Gn", "GnP",
                     "Gt", "GtP",
                     "Cn", "CnP",
                     "Ct", "CtP")
cc.tablename <- paste("./", dset, "_cc.tex", sep="")
print(xtable(cc.test, 
             align=alignment, 
             digits=3), 
      type="latex", 
      file=cc.tablename)

# Bootstrap test for ES
boot.es.test <- as.data.frame(matrix(pvals.boot, nrow=nq, byrow=TRUE), row.names=as.character(qs))
names(boot.es.test) <- c("Sn", "SnP",
                    "St", "StP",
                    "Gn", "GnP",
                    "Gt", "GtP",
                    "Cn", "CnP",
                    "Ct", "CtP")
boot.tablename <- paste("./", dset, "_bootes.tex", sep="")
print(xtable(boot.es.test, 
             align=alignment, 
             digits=3), 
      type="latex", 
      file=boot.tablename)

# V1 test
v1.test <- as.data.frame(matrix(vals.vtest[1,], nrow=nq, byrow=TRUE), row.names=as.character(qs))
names(v1.test) <- c("Sn", "SnP",
                    "St", "StP",
                    "Gn", "GnP",
                    "Gt", "GtP",
                    "Cn", "CnP",
                    "Ct", "CtP")
v1.tablename <- paste("./", dset, "_v1.tex", sep="")
print(xtable(v1.test, 
             align=alignment, 
             digits=3), 
      type="latex", 
      file=v1.tablename)

# V2 test
v2.test <- as.data.frame(matrix(vals.vtest[2,], nrow=nq, byrow=TRUE), row.names=as.character(qs))
names(v2.test) <- c("Sn", "SnP",
                    "St", "StP",
                    "Gn", "GnP",
                    "Gt", "GtP",
                    "Cn", "CnP",
                    "Ct", "CtP")
v2.tablename <- paste("./", dset, "_v2.tex", sep="")
print(xtable(v2.test, 
             align=alignment, 
             digits=3), 
      type="latex", 
      file=v2.tablename)

# V test
v.test <- as.data.frame(matrix(vals.vtest[3,], nrow=nq, byrow=TRUE), row.names=as.character(qs))
names(v.test) <- c("Sn", "SnP",
                    "St", "StP",
                    "Gn", "GnP",
                    "Gt", "GtP",
                    "Cn", "CnP",
                    "Ct", "CtP")
v.tablename <- paste("./", dset, "_v.tex", sep="")
print(xtable(v.test, 
             align=alignment, 
             digits=3), 
      type="latex", 
      file=v.tablename)
