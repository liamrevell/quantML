ntaxa<-20
nchar<-100

tree<-rtree(n=ntaxa-1)
tree<-bind.tip(tree,"outgroup",
	mean(sapply(1:(ntaxa-1),nodeheight,tree=tree)),where=Ntip(tree)+1)
plotTree(tree)

X<-fastBM(tree,nsim=nchar)

object<-quantml(nj(dist(X)),Data=X)

logLik(object)

object<-optEdges(object)

fit<-optNNI(object)

x11()
plot(cophylo(tree,root(fit$tree,"outgroup",resolve.root=TRUE)))

plot(cophenetic(tree),cophenetic(fit$tree)[tree$tip.label,tree$tip.label])


object<-quantml(upgma(dist(object$X)),X)

object<-optEdges(object)

fit<-optNNI(object)

x11()
plot(cophylo(tree,root(fit$tree,"outgroup")))

test<-quantml(fit$tree,fit$X)



