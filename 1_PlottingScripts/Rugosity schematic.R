library(MASS)
write.matrix(world, file = "example world", sep = " ")


colpal_image = c("springgreen4", "tomato", "gold", "dodgerblue", "tomato", "dodgerblue")

htworld = apply(world,c(1,2),function (x) max(which(x>0)))
htworld[htworld == -Inf] = 0

setwd(plots)
png("Rugosity profile example.png", width=5,height=5,units="cm",res=600)
par(mar=c(1,1,1,1))
image(world[,23,], col = c("white", colpal_image), xlab = "x", ylab = "y", xaxt='n', yaxt='n')
mtext(side = 1, text = "y", line = 0)
mtext(side = 2, text = "z", line = 0)
lines(x=seq(0, 1, length.out = 100), y = (htworld[,23]/100), lty = "dotted", col = "darkgrey", lwd = 1.3)
lines(x=seq(0, 1, length.out = 100), y = rep(0.9,100), lty = "longdash", col = "darkgrey", lwd = 1.3)
box()
dev.off()
lines(x=rep(0,100), y=seq(0, 1, length.out = 100))
lines(x=seq(0, 1, length.out = 100), y=rep(1,100))


