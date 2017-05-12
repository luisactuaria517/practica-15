# practica-15
#http://www.beta.inegi.org.mx/proyectos/enchogares/regulares/enoe/
#base enoe (1 trimestre 2016)
#cuestionario, recostruccion de variables

installed.packages("foreign")
require(foreign)#para que lea read.pdf

coe216 <- read.dbf("C:\\Users\\Sala-D11\\Downloads\\coe2t116.dbf")
coe216
ls(coe216)

#para conertirla en caracter y luego a numero
coe216$P6D <- as.numeric(as.character(coe216$P6D))
#los que tienen seguridad social son p6d=1 a 5
#sub conjunto de la base
#subset(base, filtro, )
# | operador ó xq xeje 1 imms 2 isst...
coesins <- subset(coe216, coe216$P6D == 1 | coe216$P6D == 2 |coe216$P6D == 3 |coe216$P6D == 4 
                   |coe216$P6D == 5 )

#68198 tiene seg social

#genera una nueva variable si va de 1 a 5 sera 1 si va de 6 a 9 se llama 2 
#todo lo demas sera 0
##generara 126 variables para saber quienes tienen y no tienen seg social.
coe216$sinseg <- ifelse(coe216$P6D >= 1 & coe216$P6D <= 5, 1, ifelse(coe216$P6D >=6 & coe216$P6D <= 9, 2,0))

table(coe216$sinseg)

###################################
##ejercicio hacer la variable de desocupados
# !=diferente

coe116 <- read.dbf("C:\\Users\\Sala-D11\\Downloads\\coe1t116.dbf")
coe116

coe116$P1C <- as.numeric(as.character(coe116$P1C))
coe116$P1B<- as.numeric(as.character(coe116$P1B))
coe116$P2_1 <- as.numeric(as.character(coe116$P2_1))
coe116$P2_2 <- as.numeric(as.character(coe116$P2_2))
coe116$P2_3 <- as.numeric(as.character(coe116$P2_3))
coe116$P2B<- as.numeric(as.character(coe116$P2B))
coe116$P2C<- as.numeric(as.character(coe116$P2C))
coe116$P1D<- as.numeric(as.character(coe116$P1D))


coe116$deso <- ifelse(coe116$P1C == 11, 1, 
                        ifelse ((coe116$P1B ==2) & (coe116$P2_1 == 1
                                                    |coe116$P2_2 == 2
                                                    |coe116$P2_3 == 3) & 
                                  (coe116$P2B == 1)
                                & (coe116$P2C != 2 | coe116$P2C != 9),2,
                                ifelse((coe116$P1D == 2|coe116$P1D == 9) &
                                         (coe116$P2_1 == 1
                                          |coe116$P2_2 == 2
                                          |coe116$P2_3 == 3) & (coe116$P2B==1) & (
                                            coe116$P2C != 2 |
                                              coe116$P2C !=9), 3, 0)))
                                          
ls(coe116)
table(coe116$deso)
coe116$deso

install.packages("questionr")
require(questionr)
wtd.table(coe116$deso, weights(=coe116$FAC)) #datos expandidos de la var



