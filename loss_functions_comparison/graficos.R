require(ggplot2)
require(xtable)
require(gridExtra)
require(latex2exp)

r11=data.frame(prop=c(0, 0.68, 0.12, 0.05, 0.04, 0.11
            ,0, 0.74 ,0.10, 0.10, 0.01, 0.05
            ,0, 0.84, 0.11, 0.01, 0.03, 0.01
            ,0, 0.86, 0.09, 0.02, 0.03, 0)
            ,n=c(rep(300,6),rep(900,6),rep(1500,6),rep(3000,6))
            ,knots=c(rep(seq(0,5),4)))

r12=data.frame(prop=c(0, 0.63, 0.12, 0.03, 0.04, 0.18
                      ,0, 0.76, 0.15, 0.03, 0.02, 0.04 
                      ,0, 0.80, 0.14, 0.02, 0.03, 0.01 
                      ,0, 0.81, 0.13, 0.02, 0.02, 0.02 )
               ,n=c(rep(300,6),rep(900,6),rep(1500,6),rep(3000,6))
               ,knots=c(rep(seq(0,5),4)))


r11_grafico=(ggplot(r11)+aes(x=n, y=prop, group=factor(knots), color=factor(knots))
+geom_line(size=1.2)
+labs(title=TeX("$J_{1}(\\theta)$"),x="n", y="Proportion")+geom_point(color="black")
+scale_colour_manual(TeX("$\\hat{\\alpha}$")
       ,values = c("#d11141","#000000","#00AFBB","darkgreen","blue","#FC4E07"))
+scale_x_continuous(breaks=unique(r11$n))
+theme_test())

r12_grafico=(ggplot(r12)+aes(x=n, y=prop, group=factor(knots), color=factor(knots))
             +geom_line(size=1.2)
             +labs(title=TeX("$J_{2}(\\theta)$"),x="n", y="Proportion")+geom_point(color="black")
             +scale_colour_manual(TeX("$\\hat{\\alpha}$")
                                  ,values = c("#d11141","#000000","#00AFBB","darkgreen","blue","#FC4E07"))
             +scale_x_continuous(breaks=unique(r12$n))
             +theme_test())

grid.arrange(r11_grafico, r12_grafico)

#xtable(matrix(r1$prop,byrow=T,ncol=6))
# Tabela dos erro quadratico medio e desvio padrão
#lambda1=matrix(c(),byrow=T, ncol=9)

erro1=matrix(c(4.106785, 4.066082, 4.020465, 4.015473,
                5.729636, 5.750628, 5.717657, 5.676678,
                4.109269, 4.064020, 4.019613, 4.016001,
                5.726590, 5.748760, 5.715641, 5.678061,
                3.975401, 4.020725, 4.000325, 4.007024,
                5.577649, 5.690516, 5.687020, 5.667225),byrow=T,ncol=4)

xtable(erro1, digits=3)

lambda11=matrix(c(0.13, 0.22, 0.23, 0.20, 0.21, 0.01, 0, 0, 0
                 ,0.10, 0.30, 0.26, 0.21, 0.13, 0, 0, 0, 0
                 ,0.10, 0.29, 0.29, 0.26, 0.06, 0, 0, 0, 0
                 ,0.06, 0.41, 0.29, 0.22, 0.02, 0, 0, 0, 0),byrow = T, ncol=9)

lambda12=matrix(c(0.33, 0.14, 0.07, 0.19, 0.14, 0.03, 0.05, 0.05, 0
                  ,0.18, 0.29, 0.12, 0.19, 0.03, 0.06, 0.07, 0.06, 0
                  ,0.15, 0.33, 0.11, 0.13, 0.01, 0.04, 0.03, 0.20, 0
                 ,0.11, 0.38, 0.12, 0.03, 0, 0.07, 0.08, 0.21, 0),byrow = T, ncol=9)


xtable(lambda11)

xtable(lambda12)

r21=data.frame(prop=c(0, 0.08, 0.34, 0.28, 0.08, 0.22
                     ,0, 0 , 0.30, 0.28, 0.17, 0.25
                     ,0, 0, 0.57, 0.21, 0.09, 0.13 
                     ,0, 0, 0.66, 0.15, 0.08, 0.11)
              ,n=c(rep(300,6),rep(900,6),rep(1500,6),rep(3000,6))
              ,knots=c(rep(seq(0,5),4)))

r22=data.frame(prop=c(0, 0.12, 0.27, 0.26, 0.08, 0.27 
                     ,0, 0.01, 0.38, 0.26, 0.13, 0.22 
                     ,0, 0, 0.52, 0.26, 0.09, 0.13
                     ,0, 0, 0.60, 0.21, 0.08, 0.11 )
              ,n=c(rep(300,6),rep(900,6),rep(1500,6),rep(3000,6))
              ,knots=c(rep(seq(0,5),4)))


r21_grafico=(ggplot(r21)+aes(x=n, y=prop, group=factor(knots), color=factor(knots))
             +geom_line(size=1.2)
             +labs(title=TeX("$J_{1}(\\theta)$"),x="n", y="Proportion")+geom_point(color="black")
             +scale_colour_manual(TeX("$\\hat{\\alpha}$")
                                  ,values = c("#d11141","#000000","#00AFBB","darkgreen","blue","#FC4E07"))
             +scale_x_continuous(breaks=unique(r21$n))
             +theme_test())

r22_grafico=(ggplot(r22)+aes(x=n, y=prop, group=factor(knots), color=factor(knots))
             +geom_line(size=1.2)
             +labs(title=TeX("$J_{2}(\\theta)$"),x="n", y="Proportion")+geom_point(color="black")
             +scale_colour_manual(TeX("$\\hat{\\alpha}$")
                                  ,values = c("#d11141","#000000","#00AFBB","darkgreen","blue","#FC4E07"))
             +scale_x_continuous(breaks=unique(r22$n))
             +theme_test())

grid.arrange(r21_grafico, r22_grafico)

erro2=matrix(c( 4.173393, 4.108796, 4.032435, 4.022507,
                5.859874, 5.801372, 5.734170, 5.689517,
                4.185749, 4.108241, 4.032888, 4.022667,
                5.863372, 5.795202, 5.731216, 5.691542,
                3.975401, 4.020725, 4.000325, 4.007024,
                5.577649, 5.690516, 5.687020, 5.667225),byrow=T,ncol=4)

xtable(erro2, digits=3)

lambda21=matrix(c(0.14, 0.29, 0.23, 0.18, 0.16, 0, 0, 0, 0
                  ,0.31, 0.35, 0.24, 0.10, 0, 0, 0, 0, 0
                  ,0.29, 0.50, 0.15, 0.06, 0, 0, 0, 0, 0
                  ,0.25, 0.53, 0.21, 0.01, 0, 0, 0, 0, 0),byrow = T, ncol=9)

lambda22=matrix(c(0.53, 0.06, 0.07, 0.12, 0.12, 0.10, 0, 0, 0
                  ,0.52, 0.07, 0.11, 0.22, 0.06, 0.02, 0, 0, 0
                  ,0.47, 0.28, 0.09, 0.11, 0.03, 0.02, 0, 0, 0
                  ,0.33, 0.28, 0.28, 0.06, 0.01, 0.04, 0, 0, 0),byrow = T, ncol=9)

xtable(lambda21)
xtable(lambda22)

r31=data.frame(prop=c(0, 0, 0, 0.57, 0.19, 0.24 
                     ,0, 0 , 0, 0.67, 0.18, 0.15
                     ,0, 0, 0, 0.73, 0.14, 0.13
                     ,0, 0, 0, 0.80, 0.09, 0.11)
              ,n=c(rep(300,6),rep(900,6),rep(1500,6),rep(3000,6))
              ,knots=c(rep(seq(0,5),4)))

r32=data.frame(prop=c(0, 0, 0, 0.54, 0.16, 0.30 
                      ,0, 0 , 0, 0.72, 0.17, 0.11
                      ,0, 0, 0, 0.65, 0.20, 0.15 
                      ,0, 0, 0, 0.73, 0.15, 0.12  )
               ,n=c(rep(300,6),rep(900,6),rep(1500,6),rep(3000,6))
               ,knots=c(rep(seq(0,5),4)))


r31_grafico=(ggplot(r31)+aes(x=n, y=prop, group=factor(knots), color=factor(knots))
             +geom_line(size=1.2)
             +labs(title=TeX("$J_{1}(\\theta)$"),x="n", y="Proportion")+geom_point(color="black")
             +scale_colour_manual(TeX("$\\hat{\\alpha}$")
                                  ,values = c("#d11141","#000000","#00AFBB","darkgreen","blue","#FC4E07"))
             +scale_x_continuous(breaks=unique(r31$n))
             +theme_test())

r32_grafico=(ggplot(r32)+aes(x=n, y=prop, group=factor(knots), color=factor(knots))
             +geom_line(size=1.2)
             +labs(title=TeX("$J_{2}(\\theta)$"),x="n", y="Proportion")+geom_point(color="black")
             +scale_colour_manual(TeX("$\\hat{\\alpha}$")
                                  ,values = c("#d11141","#000000","#00AFBB","darkgreen","blue","#FC4E07"))
             +scale_x_continuous(breaks=unique(r32$n))
             +theme_test())

grid.arrange(r31_grafico, r32_grafico)

erro3=matrix(c(  4.167792, 4.106032, 4.034318, 4.024002,
                 5.845244, 5.799518, 5.732502, 5.691444,
                 4.167706, 4.101442, 4.034727, 4.025558,
                 5.847301, 5.795862, 5.733450, 5.693304,
                 3.975401, 4.020725, 4.000325, 4.007024,
                 5.577649, 5.690516, 5.687020, 5.667225),byrow=T,ncol=4)

xtable(erro3, digits=3)

lambda31=matrix(c(0.21, 0.32, 0.33, 0.14, 0, 0, 0, 0, 0
                  ,0.32, 0.45, 0.21, 0.02, 0, 0, 0, 0, 0
                  ,0.33, 0.41, 0.26, 0, 0, 0, 0, 0, 0
                  ,0.32, 0.65, 0.03, 0, 0, 0, 0, 0, 0),byrow = T, ncol=9)

lambda32=matrix(c(0.55, 0.04, 0.02, 0.14, 0.16, 0.09, 0, 0, 0
                  ,0.40, 0.17, 0.14, 0.15, 0.06, 0.08, 0, 0, 0
                  ,0.43, 0.21, 0.17, 0.12, 0.07, 0, 0, 0, 0
                  ,0.40, 0.38, 0.16, 0.02, 0.04, 0, 0, 0, 0),byrow = T, ncol=9)

xtable(lambda31)
xtable(lambda32)


r41=data.frame(prop=c(0, 0.19, 0.39, 0.29, 0.08, 0.05 
                     ,0, 0.08, 0.54, 0.28, 0.06, 0.04 
                     ,0, 0.06, 0.57, 0.27, 0.04, 0.06
                     ,0, 0.01, 0.55, 0.36, 0.04, 0.04)
              ,n=c(rep(300,6),rep(900,6),rep(1500,6),rep(3000,6))
              ,knots=c(rep(seq(0,5),4)))

r42=data.frame(prop=c(0.10, 0.51, 0.22, 0.08, 0.06, 0.03  
                      ,0.01, 0.24, 0.41, 0.24, 0.06, 0.04 
                      ,0, 0.12, 0.55, 0.20, 0.07, 0.06 
                      ,0, 0.02, 0.58, 0.27, 0.11, 0.02 )
               ,n=c(rep(300,6),rep(900,6),rep(1500,6),rep(3000,6))
               ,knots=c(rep(seq(0,5),4)))



r41_grafico=(ggplot(r41)+aes(x=n, y=prop, group=factor(knots), color=factor(knots))
             +geom_line(size=1.2)
             +labs(title=TeX("$J_{1}(\\theta)$"),x="n", y="Proportion")+geom_point(color="black")
             +scale_colour_manual(TeX("$\\hat{\\alpha}$")
                                  ,values = c("#d11141","#000000","#00AFBB","darkgreen","blue","#FC4E07"))
             +scale_x_continuous(breaks=unique(r41$n))
             +theme_test())

r42_grafico=(ggplot(r42)+aes(x=n, y=prop, group=factor(knots), color=factor(knots))
             +geom_line(size=1.2)
             +labs(title=TeX("$J_{2}(\\theta)$"),x="n", y="Proportion")+geom_point(color="black")
             +scale_colour_manual(TeX("$\\hat{\\alpha}$")
                                  ,values = c("#d11141","#000000","#00AFBB","darkgreen","blue","#FC4E07"))
             +scale_x_continuous(breaks=unique(r42$n))
             +theme_test())

grid.arrange(r41_grafico, r42_grafico)

erro4=matrix(c(   4.120427, 4.082175, 4.028308, 4.020598,
                  5.765877, 5.777983, 5.727529, 5.686061,
                  4.153103, 4.098410, 4.031029, 4.020547,
                  5.793516, 5.802984, 5.732656, 5.685741,
                  3.975401, 4.020725, 4.000325, 4.007024,
                  5.577649, 5.690516, 5.687020, 5.667225),byrow=T,ncol=4)

xtable(erro4, digits=3)

lambda41=matrix(c(0.04, 0.01, 0.04, 0.03, 0.05, 0.04, 0.15, 0.25, 0.39 
                  ,0.08, 0.03, 0.02, 0.02, 0.07, 0.07, 0.07, 0.23, 0.41 
                  ,0.06, 0.05, 0.01, 0.02, 0.06, 0.08, 0.16, 0.25, 0.31 
                  ,0.06, 0.01, 0.02, 0.07, 0.03, 0.17, 0.21, 0.15, 0.28 ),byrow = T, ncol=9)

lambda42=matrix(c(0.27, 0.05, 0.10, 0.29, 0.18, 0.09, 0.02, 0, 0
                  ,0.43, 0.11, 0.15, 0.20, 0.10, 0.01, 0, 0, 0
                  ,0.47, 0.11, 0.17, 0.21, 0.02, 0.02, 0, 0, 0
                  ,0.44, 0.23, 0.21, 0.12, 0, 0, 0, 0, 0),byrow = T, ncol=9)

xtable(lambda41)
xtable(lambda42)

r51=data.frame(prop=c(0.04, 0.08, 0.50, 0.24, 0.05, 0.09  
                     ,0, 0, 0.60, 0.21, 0.13, 0.06 
                     ,0, 0, 0.52, 0.32, 0.10, 0.06 
                     ,0, 0, 0.59, 0.29, 0.08, 0.04)
              ,n=c(rep(300,6),rep(900,6),rep(1500,6),rep(3000,6))
              ,knots=c(rep(seq(0,5),4)))

r52=data.frame(prop=c(0.50, 0.07, 0.23, 0.09, 0.04, 0.07  
                      ,0.12, 0, 0.34, 0.27, 0.15, 0.12 
                      ,0.02, 0, 0.39, 0.27, 0.09, 0.23 
                      ,0, 0, 0.50, 0.25, 0.17, 0.08)
               ,n=c(rep(300,6),rep(900,6),rep(1500,6),rep(3000,6))
               ,knots=c(rep(seq(0,5),4)))


r51_grafico=(ggplot(r51)+aes(x=n, y=prop, group=factor(knots), color=factor(knots))
             +geom_line(size=1.2)
             +labs(title=TeX("$J_{1}(\\theta)$"),x="n", y="Proportion")+geom_point(color="black")
             +scale_colour_manual(TeX("$\\hat{\\alpha}$")
                                  ,values = c("#d11141","#000000","#00AFBB","darkgreen","blue","#FC4E07"))
             +scale_x_continuous(breaks=unique(r51$n))
             +theme_test())

r52_grafico=(ggplot(r52)+aes(x=n, y=prop, group=factor(knots), color=factor(knots))
             +geom_line(size=1.2)
             +labs(title=TeX("$J_{2}(\\theta)$"),x="n", y="Proportion")+geom_point(color="black")
             +scale_colour_manual(TeX("$\\hat{\\alpha}$")
                                  ,values = c("#d11141","#000000","#00AFBB","darkgreen","blue","#FC4E07"))
             +scale_x_continuous(breaks=unique(r42$n))
             +theme_test())

grid.arrange(r51_grafico, r52_grafico)

erro5=matrix(c(    4.135902, 4.075667, 4.027802, 4.020641,
                   5.781289, 5.763643, 5.725037, 5.683688,
                   4.144846, 4.092893, 4.031030, 4.020795,
                   5.774717, 5.816989, 5.729852, 5.684030,
                   3.975401, 4.020725, 4.000325, 4.007024,
                   5.577649, 5.690516, 5.687020, 5.667225),byrow=T,ncol=4)

xtable(erro5, digits=3)

lambda51=matrix(c(0.05, 0.02, 0.03, 0.04, 0.03, 0.08, 0.18, 0.24, 0.33 
                  ,0.05, 0.01, 0.04, 0.03, 0.04, 0.09, 0.22, 0.30, 0.22 
                  ,0.07, 0.04, 0.04, 0.06, 0.13, 0.16, 0.26, 0.19, 0.05 
                  ,0.11, 0.03, 0.05, 0.07, 0.08, 0.20, 0.25, 0.21, 0  ),byrow = T, ncol=9)

lambda52=matrix(c(0.23, 0.24, 0.15, 0.26, 0.10, 0.02, 0, 0, 0
                  ,0.64, 0.15, 0.08, 0.13, 0, 0, 0, 0, 0
                  ,0.68, 0.16, 0.11, 0.05, 0, 0, 0, 0, 0
                  ,0.82, 0.07, 0.10, 0.01, 0, 0, 0, 0, 0),byrow = T, ncol=9)

xtable(lambda51)
xtable(lambda52)

r61=data.frame(prop=c(0, 0.26, 0.37, 0.21, 0.09, 0.07   
                     ,0, 0.10, 0.60, 0.21, 0.06, 0.03  
                     ,0, 0.01, 0.41, 0.29, 0.18, 0.11  
                     ,0, 0.02, 0.42, 0.41, 0.07, 0.08)
              ,n=c(rep(300,6),rep(900,6),rep(1500,6),rep(3000,6))
              ,knots=c(rep(seq(0,5),4)))

r62=data.frame(prop=c(0, 0.35, 0.36, 0.16, 0.06, 0.07
                      ,0, 0.12, 0.62, 0.16, 0.07, 0.03   
                      ,0, 0.05, 0.49, 0.24, 0.14, 0.08  
                      ,0, 0.01, 0.48, 0.39, 0.08, 0.04)
               ,n=c(rep(300,6),rep(900,6),rep(1500,6),rep(3000,6))
               ,knots=c(rep(seq(0,5),4)))

r61_grafico=(ggplot(r61)+aes(x=n, y=prop, group=factor(knots), color=factor(knots))
             +geom_line(size=1.2)
             +labs(title=TeX("$J_{1}(\\theta)$"),x="n", y="Proportion")+geom_point(color="black")
             +scale_colour_manual(TeX("$\\hat{\\alpha}$")
                                  ,values = c("#d11141","#000000","#00AFBB","darkgreen","blue","#FC4E07"))
             +scale_x_continuous(breaks=unique(r51$n))
             +theme_test())

r62_grafico=(ggplot(r62)+aes(x=n, y=prop, group=factor(knots), color=factor(knots))
             +geom_line(size=1.2)
             +labs(title=TeX("$J_{2}(\\theta)$"),x="n", y="Proportion")+geom_point(color="black")
             +scale_colour_manual(TeX("$\\hat{\\alpha}$")
                                  ,values = c("#d11141","#000000","#00AFBB","darkgreen","blue","#FC4E07"))
             +scale_x_continuous(breaks=unique(r42$n))
             +theme_test())

grid.arrange(r61_grafico, r62_grafico)

erro6=matrix(c(4.134542, 4.089081, 4.032979, 4.022662,
               5.790044, 5.786299, 5.731700, 5.686836,
               4.146461, 4.087265, 4.035158, 4.022670,
               5.819776, 5.781401, 5.732346, 5.687289,
               3.975401, 4.020725, 4.000325, 4.007024,
               5.577649, 5.690516, 5.687020, 5.667225),byrow=T,ncol=4)

xtable(erro6, digits=3)

lambda61=matrix(c(0.04, 0.06, 0.04, 0.04, 0.06, 0.10, 0.15, 0.25, 0.26
                  ,0.08, 0.04, 0.07, 0.03, 0.08, 0.12, 0.16, 0.25, 0.17 
                  ,0.09, 0.06, 0.07, 0.07, 0.11, 0.14, 0.19, 0.23, 0.04 
                  ,0.10, 0.09, 0.04, 0.04, 0.04, 0.16, 0.25, 0.21, 0.07 ),byrow = T, ncol=9)

lambda62=matrix(c(0.26, 0.10, 0.07, 0.17, 0.10, 0.05, 0.11, 0.14, 0
                  ,0.62, 0.07, 0.05, 0.11, 0.05, 0.05, 0.01, 0.04, 0
                  ,0.34, 0.11, 0.16, 0.30, 0.05, 0.02, 0, 0.02, 0
                  ,0.36, 0.12, 0.26, 0.18, 0.03, 0.05, 0, 0, 0),byrow = T, ncol=9)


xtable(lambda61)
xtable(lambda62)
