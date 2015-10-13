########################################################################################
# This code of R isn't function
#
# It's demo the Fourier series square wave which like the link of wiki
#
# https://en.wikipedia.org/wiki/File:Fourier_series_square_wave_circles_animation.gif
#
# which output can build to a video
#
# you can change the pixel of the code where like this
#
# png(file = paste("path of picture saved",k,".png",sep=""),width = 1920, height = 1080)
# 
# remeber: un-# on ""#dev.off()"" (which is for functoin"png")
#######################################################################################

#How many picture per sec(frames)
fra=30

#How many times of circle rounds
n=5

#How long paid of one circle(sec)
ts=6

A=seq(0,n*2*pi,length.out=n*ts*fra)

#unit radius
r=4

#ratios of amplitude(you can add others or re-sort of them ex: cr <- rev(c(5,3,6,2)))
cr=c(1,3,5,7)


cf=function(l)r*rbind(cos(l*A),sin(l*A))/(l*pi)

#calculas for all 
cicl=array(0,dim=c(2,length(A)))
for ( i in 1:length(cr)){ cicl=rbind(cicl,cicl[c(-1,0)+i*2,]+cf(cr[i])) }

#plot for center locus (can be passed)
plot(1,1,col=0,xlim=c(-3,3),ylim=c(-3,3),asp=1)
lines(cicl[1,],cicl[2,],typ="l",col=1,asp=1) #locus of first circle
lines(cicl[3,],cicl[4,],typ="l",col=1,asp=1) #locus of second circle
lines(cicl[5,],cicl[6,],typ="l",col=1,asp=1)
lines(cicl[7,],cicl[8,],typ="l",col=1,asp=1)
lines(cicl[9,],cicl[10,],typ="l",col=2,asp=1)



wpo=NULL

for (k in 1:length(A)){
	#png(file = paste("path of picture saved",k,".png",sep=""),width = 1920, height = 1080)
	plot(NULL,col=0,xlim=c(-10,3),ylim=c(-3,3),asp=1,xlab="",ylab="")
	for(i in 1:length(cr)){
		X=cf(cr[i])+cicl[c(i*2-1,i*2),k]
		lines(t(cbind(X,X[,1])),col=i)
		lines(cicl[c(i*2-1,i*2+1),k],cicl[c(i*2,(i+1)*2),k],col=i)
		}
	lines(c(cicl[(i+1)*2-1,k],-4),cicl[c(i+1,i+1)*2,k],col="blue")
	points(c(-4),cicl[(i+1)*2,k],pch=20)
	wpo=cbind(wpo+c(-1/200,0),c(-4,cicl[(i+1)*2,k]))
	lines(wpo[1,],wpo[2,],lwd=2)
	#dev.off()
	}


