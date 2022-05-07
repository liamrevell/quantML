library(quantML)
library(ape)
library(phytools)

ntaxa<-40
nchar<-200

tree<-rtree(n=ntaxa-1)
tree<-bind.tip(tree,"outgroup",
	mean(sapply(1:(ntaxa-1),nodeheight,tree=tree)),
	where=Ntip(tree)+1)
plotTree(tree,fsize=0.6)

X<-fastBM(tree,nsim=nchar)

object<-quantml(nj(dist(X)),Data=X)

logLik(object)

## object<-optEdges(object)

Rprof(tmp <- tempfile(), memory.profiling=TRUE)
fit<-optNNI(object)
Rprof()
summaryRprof(tmp, memory="both")
unlink(tmp)

x11()
plot(cophylo(tree,root(fit$tree,"outgroup",
	resolve.root=TRUE)))

x11()
plot(cophenetic(tree),
	cophenetic(fit$tree)[tree$tip.label,tree$tip.label])


object<-quantml(upgma(dist(object$X)),X)

object<-optEdges(object)

fit<-optNNI(object)

x11()
plot(cophylo(tree,root(fit$tree,"outgroup")))

test<-quantml(fit$tree,fit$X)

