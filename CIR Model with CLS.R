# Input data
library(readxl)
rk <- read_excel("Book1.xlsx",sheet = "BI Rate")
rk <- rk$BI_Rate
rk_1 <- c(NA,rk[1:length(rk)-1])

# Perhitungan parameter CIR
# Mencari estimasi alpha dan beta
program.ab=function(rk,rk_1){
  rkbar =as.numeric(mean(rk))
  rk_1bar=as.numeric(mean(rk_1[2:length(rk_1)]))
  n=length(rk)
  atas=bawah=c()
  for(i in 1:n){
    atas[i]=(rk[i]-rkbar)*(rk_1[i]-rk_1bar)
    bawah[i]=(rk_1[i]-rk_1bar)^2
  }
  jum.atas=sum(atas[2:n])
  jum.bawah=sum(bawah[2:n])
  alpha=-1*log(jum.atas/jum.bawah)
  beta=(rkbar-((exp(-alpha*1))*rk_1bar))/(1-exp(-alpha*1))
  param = data.frame(alpha,beta)
  cat("nilai estimasi alpha \t=",alpha,"\n")
  cat("nilai estimasi beta \t=",beta,"\n")
  return(param)
}
CIR_Param <- program.ab(rk,rk_1)
alpha <- CIR_Param$alpha
beta <- CIR_Param$beta

library(forecast)
auto.arima(rk)
model_arima <- arima(rk, c(3,1,1), method = "CSS")
sigma <- sqrt(model_arima$sigma2)
sigma
delta <- model_arima$model$Delta
delta

# Inisialisasi Parameter
w <- 60 # limit usia
t <- 10 # masa asuransi
b <- 100000000 # manfaat asuransi
n <- 10 # jangka waktu premi
usia <- 15 # usia saat masuk asuransi
r0 <- rk[1] # suku bunga awal

# Menghitung prediksi nilai suku bunga CIR
program.rtCIR=function(r0,alpha,beta,sigma,t,delta){
  k = t
  t=0:t
  gamma0=beta*(1-exp(-alpha*delta))
  gamma1=exp(-alpha*delta)
  rt=matrix(0,ncol=1,nrow=k+1)
  rt[1]=r0
  for(i in 2:(k+1)){
    rt[i]=gamma0+(gamma1*rt[i-1])
  }
  cat("Diketahui : \n")
  cat("\t nilai suku bunga awal (r0)\t=",r0,"\n")
  cat("\t estimasi alpha CIR\t\t=",alpha,"\n")
  cat("\t estimasi beta CIR\t\t=",beta,"\n")
  cat("\t estimasi sigma^2 CIR \t\t=",sigma,"\n")
  cat("\t masa asuransi \t\t\t=",k,"tahun \n")
  cat("OUTPUT :\n")
  cat("\t gamma nol \t:",gamma0,"\n")
  cat("\t gamma satu \t:",gamma1,"\n")
  cat("\t prediksi suku bunga ",k,"tahun mendatang :\n")
  print(data.frame(rt))
  plot(rt,ylab="r(t)",xlab="t",main=paste("Nilai r(t) with r0=",r0,""),col="blue")
}
program.rtCIR(r0,alpha,beta,sigma,t,delta)

# Perhitungan Nilai Asuransi, Anuitas dan Premi dengan Suku Bunga CIR
# Program menghitung premi dengan suku bunga model CIR
dwiguna_cir=function(sx,w,alpha,beta,sigma,t,b,r0,n,usia,delta){
  if(n>(w-usia)) stop("Jangka waktu premi tidak boleh lebih besar dari (w-usia) \n")
  k=t
  t=0:t
  rt=matrix(0,ncol=1,nrow=k+1)
  rt[1]=r0
  for(i in 2:(k+1)){
    rt[i]=(beta*(1-exp(-alpha*delta)))+(exp(-alpha*delta)*rt[i-1])
  }
  #fungsi diskon
  v=matrix(0,ncol=1,nrow=k+1)
  v[1]=1/(1+(rt[1]))
  for(i in 2:(n+1)){
    v[i]=v[i-1]*(1/(1+(rt[i])))
  }
  #premi
  asuransi=anuitas=murni=NULL
  for(i in 1:n){
    asuransi[i]=v[i]*(sx[usia+i-1]/sx[usia])*(1-(sx[usia+i]/sx[usia+i-1]))
  }
  for(i in 1:(n-1)){
    anuitas[i]=v[i]*(sx[usia+i]/sx[usia])
    }
  murni=(v[n])*(sx[usia+n]/sx[usia])
  as.dwiguna=sum(asuransi)+murni
  jum.anuitas=sum(anuitas)+1
  premi=(as.dwiguna/jum.anuitas)*b
  cat("Diketahui : \n")
  cat("\t usia tertanggung \t=",usia,"tahun \n")
  cat("\t nilai suku bunga awal (r0)\t=",r0,"\n")
  cat("\t estimasi alpha CIR\t=",alpha,"\n")
  cat("\t estimasi betha CIR\t=",beta,"\n")
  cat("\t estimasi sigma^2 CIR \t=",sigma,"\n")
  cat("\t limit usia (w) \t=",w,"\n")
  cat("\t jangka waktu premi \t=",n,"tahun \n")
  cat("\t manfaat asuransi \t=",b,"rupiah \n")
  cat("OUTPUT :\n")
  cat("\t nilai anuitas \t=",jum.anuitas,"\n")
  cat("\t nilai asuransi\t=",as.dwiguna*b,"\n")
  cat("\t nilai premi \t=",premi,"\n")
  cat("Kesimpulan :\n")
  cat("Seorang tertanggung yang berusia",usia,"tahun mengikuti \nperlindungan
asuransi dwiguna berjangka \nselama",n,"tahun dengan manfaat sebesar
Rp.",b,"\nmaka pada saat mengikuti polis harus membayar premi \nsebesar
Rp.",premi,"setiap tahun selama",n,"tahun \n")
}
SX <- read_excel("Book1.xlsx",sheet = "Tabel Mortalita")
Msx <- SX$Male_Sx
Fsx <- SX$Female_Sx
Male_CIR <- dwiguna_cir(Msx,w,alpha,beta,sigma,t,b,r0,n,usia,delta)
Female_CIR <- dwiguna_cir(Fsx,w,alpha,beta,sigma,t,b,r0,n,usia,delta)

# Perhitungan Nilai Asuransi, Anuitas dan Premi dengan Suku Bunga Konstan
dwiguna_konstan=function(sx,w,t,b,r,n,usia){
  if(n>w) stop("Jangka waktu premi tidak boleh lebih besar dari (w-usia) \n")
  v=(1+r)^-1
  asuransi=anuitas=NULL
  for(i in 1:n)
  {asuransi[i]=(v^(i))*(sx[usia+i-1]/sx[usia])*(1-(sx[usia+i]/sx[usia+i-1]))}
  for(i in 1:(n-1))
  {anuitas[i]=(v^(i))*(sx[usia+i]/sx[usia])}
  murni=(v^n)*(sx[usia+n]/sx[usia])
  as.dwiguna=sum(asuransi)+murni
  anuitasku=sum(anuitas)+1
  premi=(as.dwiguna/anuitasku)*b
  cat("Diketahui : \n")
  cat("\t usia tertanggung \t = ",usia,"tahun \n")
  cat("\t nilai suku bunga \t =",r, "\n")
  cat("\t limit usia (w) \t=",w,"\n")
  cat("\t jangka waktu premi \t=",n,"tahun \n")
  cat("\t manfaat asuransi \t=",b,"rupiah \n")
  cat("OUTPUT :\n")
  cat("\t nilai anuitas \t=",anuitasku,"\n")
  cat("\t nilai asuransi\t=",as.dwiguna*b,"\n")
  cat("\t nilai premi \t=",premi,"\n")
  cat("Kesimpulan :\n")
  cat("Seorang tertanggung yang berusia",usia,"tahun mengikuti \nperlindungan
asuransi dwiguna berjangka \ndengan suku bunga konstan sebesar",r,"\nselama",n,"tahun dengan manfaat sebesar
Rp.",b,"\nmaka pada saat mengikuti polis harus membayar premi \nsebesar
Rp.",premi,"setiap tahun selama",n,"tahun \n")
}
r <- r0 
Male_CIR_Kons <- dwiguna_konstan(Msx,w,t,b,r,n,usia)
Female_CIR_Kons <- dwiguna_konstan(Fsx,w,t,b,r,n,usia)
