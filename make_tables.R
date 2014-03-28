# Make sure needed data structures are available

library(xtable)


# wdd  will be directory where tables are saved => change as needed
wdd <- "./Tables"
setwd(wdd)

rownames(pvals) <- LETTERS[1:length(exres.list)]
pval.tablename <- paste(dset, "_pvals.tex", sep="")
print(xtable(pvals, digits=3), type="latex", file=pval.tablename)


v.table <- matrix(data=NA, nrow=length(diffs.list), ncol=(3*length(qs)))

for (i in 1:length(v.tests)){
  v.table[i, c(1,4,7,10)] <- as.numeric(v.tests[[i]][1,])
  v.table[i, c(2,5,8,11)] <- as.numeric(v.tests[[i]][2,])
  v.table[i, c(3,6,9,12)] <- as.numeric(v.tests[[i]][3,])
}
v.table <- as.data.frame(v.table) * dataset.sd
rownames(v.table) <- LETTERS[1:length(exres.list)]
names(v.table) <- rep(c("V1", "V2", "V"), length(qs))

vtest.tablename <- paste(dset, "_vtests.tex", sep="")
print(xtable(v.table, digits=3), type="latex", file=vtest.tablename)
