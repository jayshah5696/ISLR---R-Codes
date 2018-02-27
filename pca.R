tail(USArrests)
states=row.names(USArrests)
states
names(USArrests)
dim(USArrests)
#find the mean and var of various columns of the table
?apply
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
#apply PCA
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
pr.out$sdev
dim(pr.out$x)
pr.out$x

#plotting
biplot(pr.out, scale=0)
#invariant to sign change
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
#scree analysis
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
#cusum
a=c(1,2,8,-3)
cumsum(a)
