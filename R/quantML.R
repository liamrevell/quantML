## quantml object
quantml<-function(tree,Data){
	object<-list(tree=tree,X=Data)
	class(object)<-"quantml"
	object
}

## quantml print
print.quantml<-function(x,...)
	cat("object of class \"quantml\"\n")

## likelihood function
logLik.quantml<-function(object,...){
	## compute contrasts
	picX<-lapply(data.frame(object$X),pic,
		phy=multi2di(object$tree),scaled=FALSE,
		var.contrasts=TRUE)
	foo<-function(X) sum(dnorm(x=X[,1],sd=sqrt(X[,2]),
		log=TRUE))
	lik<-sum(sapply(X=picX,foo))
	attr(lik,"df")<-nrow(object$tree$edge)
	attr(lik,"class")<-"logLik"
	lik
}

## optimize edges
optEdges<-function(object,...){
	if(hasArg(tol)) tol<-list(...)$tol
	else tol<-1e-8
	ee<-object$tree$edge.length
	lik<-function(ee,object){
		object$tree$edge.length<-ee
		-logLik(object)
	}
	fit<-optim(ee,lik,object=object,method="L-BFGS-B",
		lower=tol,upper=Inf)
	object$tree$edge.length<-fit$par
	cat(paste("Optimized object of class \"quantml\" with log(L) = ",
		round(logLik(object),3),"\n"))
	flush.console()
	object
}

## optimize topology
optNNI<-function(object,...){
	curr<-logLik(object)
	cat(paste("\n    -- Best log(L) found so far ",round(curr,3),
		".... --\n\n",sep=""))
	LIKs<-Inf
	while(any(curr<LIKs)){
		curr<-logLik(object)
		NNIs<-lapply(X=nni(object$tree),FUN=quantml,Data=object$X)
		FITs<-lapply(NNIs,optEdges)
		LIKs<-sapply(FITs,logLik)
		object<-FITs[[which(LIKs==max(LIKs))[1]]]
		cat(paste("\n   -- Best log(L) found so far ",
			round(max(LIKs),3),".... --\n\n",sep=""))
		flush.console()
	}
	object
}

## quantml plot
plot.quantml<-function(x,...){
	args<-list(...)
	if(is.null(args$lwd)) args$lwd<-1
	args$tree<-x$tree
	do.call(plotTree,args)
}

## quantml root
root.quantml<-function(phy,...){
	object<-phy$tree
	args<-list(...)
	args$phy<-object
	do.call(root,args)
}
	
	