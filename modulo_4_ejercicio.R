#TP 4
#Alumna: Martinez Gabriela

# 1.	Para este ejercicio utilizaremos la base de datos que elabor� extrayendo los datos de los res�menes que le fueron enviados. Evaluaremos la influencia de dos variables sobre el efecto de la intervenci�n FLCD-15 sobre la ocurrencia de alegr�a. Para esto deber� realizar meta-an�lisis utilizando alegr�a como evento, con las siguientes especificaciones:
#   a.	Realizar un an�lisis estratificado, dividiendo a los estudios que se realizaron antes de 2013 y los que se realizaron de 2013 en adelante.
library(meta)

ma_estra_a�io<-metabin(aleg_interv, N_interv,aleg_control, N_control, data=ma, studlab= paste(Estudio,"(", year, ")",sep = ""), subgroup = ifelse(year>2013,">2013","<2013"), fixed= F)  
summary(ma_estra_a�io)
forest(ma_estra_a�io)

# b. Realizar una metaregresi�n utilizando la variable "year" como regresora.
mimeta<-metabin(aleg_interv, N_interv,aleg_control, N_control, data=ma, studlab= paste(Estudio,"(", year, ")",sep = ""),fixed= F)  

regxyear<-metareg(mimeta, year)

bubble(regxyear, col = ifelse(ma$year>2013,"red","blue"), bg = ifelse(ma$year>2013,"red","blue"),  col.line = "skyblue")

# c.	Crear una variable que se llame "calidad" que indicar� en una escala supuesta la calidad de los estudios. Los valores de esta variables deber�n ser los siguientes:
# Iluso et al 	8
# Cónico et al	9
# Payaso et al	4
# Curioso et al	2
# Honesto et al	6
# Cándido et al	7
# Random et al	10
# Patán et al	5
# Optimista et al	3
# Crédulo et al	1

ma$calidad<-c(8,9,4,2,6,7,10,5,3,1)

# d.	Realizar una metaregresi�n con la variable calidad como regresora.

reg2<-metareg(mimeta, calidad)
summary(reg2)

# e.	Realizar un Bubble plot con la variable "calidad" como regresora y cambiar el color de las burbujas.

bubble(reg2, col = "red", bg = "red",  col.line = "blue")

# f.	Realizar una metaregresi�n con las variables "year" y "calidad" como regresoras.

reg3<-metareg(mimeta, year+calidad)

# 2.	En la base de datos "atc_mv" que se le entreg� para la actividad de clase se encuentran los datos un meta-an�lisis de datos observacionales 
#sobre los efectos de la angioplastia de m�ltiples vasos en comparaci�n con la angioplastia del vaso culpable en pacientes con s�ndromes coronarios agudos
#sin elevaci�n del ST (abrir la arteria responsable del cuadro cl�nico o abrir todas las arterias que tienen obstrucciones en la angiograf�a coronaria). Con estos datos: 
# a.	Realice un meta-an�lisis de los estimadores ajustados, utilizando como evento (punto final) "muerte_iam" (que indica la ocurrencia del punto final combinado muerte o re-infarto). 

atc_mv$se_muerte<-(log(atc_mv$uci_muerte)-log(atc_mv$rr_muerte))/1.96

ma1<-metagen(log(rr_muerte), se_muerte, data= atc_mv, studlab= paste(Nombre,"(", Year, ")",sep = ""), sm= "HR", fixed= T)
summary(ma1)
forest(ma1, allstudies = F, leftcols = c("Nombre","N_interv","N_control"),comb.fixed= T)

# b.	Cuantos estudios ingresan en el an�lisis (es decir que reportaron los estimadores del efecto ajustados).

#Ingresaron 6 estudios

# c.	Cuantos pacientes aportan datos para este an�lisis en cada grupo?

# en el grupo interv: 2395
# en el grupo control: 2439

#   d.	Cuantos pacientes hubieran aportado datos en caso de que TODOS los estudios hubieran reportado este estimador ajustado?

sum(atc_mv$N_control,atc_mv$N_interv)# 117685

#   e.	Realice un funnel plot del meta-an�lisis con estimadores ajustados.

funnel(ma1)


# 3.	Con la base de datos "vni_eap" que utilizamos en la actividad pr�ctica realice un meta-an�lisis en red, con los siguientes eventos:
# a.	"muerte"
# b.	"po2" (presi�n parcial de ox�geno a la hora del comienzo del tratamiento").
# c.	En ambos casos utilice el "Control" como grupo de referencia.
# d.	Realice un forest plot que resuma toda la evidencia disponible y un forest plot que diferencie la evidencia directa de la indirecta.

install.packages("netmeta")
attach(eap)
library(netmeta)
eap<-eap[-c(42:44), ]
p<-pairwise(tratamiento, Muerte, n, data= eap, studlab= paste(autor,"(", year, ")",sep = ""))
p
ma_red<-netmeta(p)
summary(ma_red)
netgraph(ma_red)
forest(ma_red, reference.group= "Control", drop.reference.group = T)
ns<-netsplit(ma_red)
forest(ns)


p2<-pairwise(tratamiento, mean= po2, n= n, sd=ds_po2, data= eap, studlab= paste(autor,"(", year, ")",sep = ""))
ma_red2<-netmeta(p2)
summary(ma_red2)

forest(ma_red2, reference.group= "Control", drop.reference.group = T)

ns2<-netsplit(ma_red2)
forest(ns2)


# 4.	La base de datos "ejercicio_MA_red" contiene los datos de un meta-an�lisis de diversas drogas en distintas dosis para un forma de epilepsia. Con estos datos:
# a.	Realice un meta-an�lisis en red utilizando  "evento1" como evento.
# b.	Realice el gr�fico que muestra la red de evidencia.
# c.	Realice un forest plot que resuma la evidencia directa e indirecta por separado.
# d.	Realice un ranking de los tratamientos.

?pairwise
p3<-pairwise(tratamiento,evento1, reference.group = "Placebo", n= aleatorizados, data= MA_red, studlab= paste(Autor,"(", Year, ")",sep = ""))
ma_red3<-netmeta(p3)
summary(ma_red3)

netgraph(ma_red3)
forest(ma_red3)

ns3<-netsplit(ma_red3)
?netsplit
forest(ns3, show="direct.only", direct=TRUE, indirect= F)
?forest

forest(ns3, show="indirect.only", direct=F, indirect= T)

netrank(ma_red3)

