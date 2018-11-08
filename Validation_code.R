# Spatial prediction

# set up prediction grid, same size as original
# these are the NAs we want to predict
pred.grid <- expand.grid(x=seq(1,100,1), y=seq(1,300,1))
dim(pred.grid)
# add covariate values
pred.grid$cov <- dat1$gridcov[Reduce('cbind', nearest.pixel(
  pred.grid[,1], pred.grid[,2],
  im(dat1$gridcov)))]

# can use the A matrix we already have for the unstructured data
#unstructured_data_A
table(apply(unstructured_data_A,1,sum)) # a check that it is formatted correctly. First row should = 1

np = length(pred.grid[,1])

# create A matrix for pred

A.pred <- inla.spde.make.A(mesh, loc=as.matrix(pred.grid))

# make inla stack using proper tag (pred.response) - we want to predict the response
stack.pred.response <- inla.stack(data=list(y=NA),
                                  effects = list(list(data.frame(interceptP=rep(1,np))), env = pred.grid$cov, list(Bnodes=1:spde$n.spde)),
                                  A=list(1,1,A.pred),
                                  tag='pred.response')

# join this stack and previous stack

join.stack <- inla.stack(stk_unstructured_data, stack.pred.response)

# now try model

join.output <- inla(formulaN,family="poisson",
                    data=inla.stack.data(join.stack),
                    control.predictor=list(A=inla.stack.A(join.stack), compute=TRUE, link="log"),
                    control.family = list(link = "log")
)

# then assess the output

index.pred.response <- inla.stack.index(join.stack, tag="pred.response")$data
post.mean.pred.response <- join.output$summary.linear.predictor[index.pred.response,"mean"]

proj1<- inla.mesh.projector(mesh,xlim=c(1,300),ylim=c(1,100),dims=c(300,100))
m.prd <- join.output$summary.fitted.values$mean[index.pred.response]
sd.prd <- join.output$summary.fitted.values$sd[index.pred.response]

par(mfrow=c(1,3))
image.plot(1:100,1:300, matrix(exp(m.prd), ncol=300, nrow=100), col=tim.colors(),xlab='', ylab='',main="mean of r.f",asp=1)
image.plot(list(x=dat1$Lam$xcol*100, y=dat1$Lam$yrow*100, z=t(dat1$rf.s)), main='Truth', asp=1) # make sure scale = same
points(unstructured_data[,1:2], pch=16)

plot(as.vector(dat1$rf.s) ~ exp(m.prd))
