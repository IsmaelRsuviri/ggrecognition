#cargo el csv como un data frame y hago una primera criba de columnas que no me interesan.
install.packages("filesstrings")
direccion='C:/Users/Aussar/Desktop/TFG/CNN_galaxias/training_solutions_rev1.csv'
kag=read.csv(file=direccion,header=TRUE)
kag2=data.frame(kag$GalaxyID,kag$Class1.1,kag$Class1.2,kag$Class1.3,
                kag$Class2.1,kag$Class2.2,kag$Class3.1,kag$Class3.2,
                kag$Class4.1,kag$Class4.2,kag$Class5.1,kag$Class5.2,
                kag$Class5.3,kag$Class5.4,kag$Class6.1,kag$Class6.2,
                kag$Class7.1,kag$Class7.1,kag$Class7.2,kag$Class7.3,
                kag$Class8.1,kag$Class8.2,kag$Class8.3,kag$Class8.4,
                kag$Class8.5,kag$Class8.6,kag$Class8.7,kag$Class9.1,
                kag$Class9.2,kag$Class9.3)

#Defino los vectores de labels e id
id1=vector()#Espirales label:1
id2=vector()#Elipticas label:2
id3=vector()#Lenticulares label:3
id4=vector()#On edge label:4
id5=vector()#Irregular label:5
id6=vector()#star or artifact label:6

galaxy=vector()#total labels
galid=vector()#id general

library(filesstrings)

#__________________________________________________________________________________________________________________#


#Bucle para analizar todos los elementos de las columnas que interesan del csv

for(i in 1:61578){
  #columnas del csv empleadas
  q11=kag2[i,2] # Es una galaxia redondeada
  q12=kag2[i,3] # Es una galaxia con disco u otras características
  q13=kag2[i,4] # Es una estrella u otro cuerpo celeste
  q21=kag2[i,5] # Es un disco visto de canto (edge on)
  q41=kag2[i,9] # Tiene estructura espiral
  q42=kag2[i,10]# No tiene estructura espiral
  q84=kag2[i,24]# Tiene aparencia irregular
  q86=kag2[i,26]# Podría ser una fusión (merger) de galaxias
  q85=kag2[i,25]
  q82=kag2[i,23]
  
  identity=kag2[i,1]
  
#   #SEPARADOR DE LABELS
#   #primero identificamos si es de la categoría star or artifact
#    if (q13>=0.5){
#      galaxy=append(galaxy,6)
#      galid=append(galid,identity)
#      id6=append(id6,identity)
#      next}
#   
#   # Identificamos si es de la categoría irregular
#    if (q13<0.5 & q84>=0.50){
#      galaxy=append(galaxy,5)
#      galid=append(galid,identity)
#      id5=append(id5,identity)
#      next
#    }
   # if (q13<0.5 & q86>=0.50){
   #   galaxy=append(galaxy,5)
   #   galid=append(galid,identity)
   #   id5=append(id5,identity)
   #   next
   # }
  if (q13<0.5 & q86>=0.35 & (q86+q82>=0.52)){
    galaxy=append(galaxy,5)
    galid=append(galid,identity)
    id5=append(id5,identity)
    next
  }
  # if (q13<0.5 & q84>=0.35 & (q84+q82>=0.52)){
  #   galaxy=append(galaxy,5)
  #   galid=append(galid,identity)
  #   id5=append(id5,identity)
  #   next
  # }
  
  # if (q13<0.5 & q85>=0.45 & (q85+q82>=0.52)){
  #   galaxy=append(galaxy,5)
  #   galid=append(galid,identity)
  #   id5=append(id5,identity)
  #   next
  # }
  
  
#    if (q13<0.5 & (q86+q84)>=0.52){
#      galaxy=append(galaxy,5)
#      galid=append(galid,identity)
#      id5=append(id5,identity)
#      next
#    }
#   # identificamos si es de la clase on edge
#    if (q12>0.65 & q21>=0.55 & q84<0.50 & q86<0.50){
#      galaxy=append(galaxy,4)
#      galid=append(galid,identity)
#      id4=append(id4,identity)
#      next
#    }
#   #
#   # # identificamos si es lenticular
#     if (q12>0.60 &q21<0.51 &q42>=0.55 & q84<0.48 & q86<0.48){
#       galaxy=append(galaxy,3)
#       galid=append(galid,identity)
#       id3=append(id3,identity)
#       next
#     }
#   
#   #
#   # identificamos las galaxias elipticas
#   
#    if (q11>0.80 & q84<0.48 & q86<0.48){
#      galaxy=append(galaxy,2)
#      galid=append(galid,identity)
#      id2=append(id2,identity)
#      next
#    }
#   #identificamos las galaxias espirales
# 
   # if (q12>=0.60 & q21<0.5 & q41>=0.80 & q84<0.45 &q86<0.45){
   #   galaxy=append(galaxy,1)
   #   galid=append(galid,identity)
   #   id1=append(id1,identity)
   #   next
   # }
  
}


#___________________________________________________________________________________________________________________#


#Creamos un dataframe que me registre la cantidad de datos disponibles para cada label
#imprimo en pantalla el data frame para averiguar qué datos puedo usar para entrenar.

Recuento=data.frame("Espirales"=length(id1),"Elípticas"=length(id2),"Lenticulares"=length(id3),
                    "Edge on"=length(id4),"Irregulares"=length(id5),"Estrellas u otros"=length(id6))
print(Recuento)

#Introducimos los resultados en un data frame para posteriormente generar un .csv

galaxias=data.frame("id"=galid,"etiqueta"=galaxy)
#write.csv(v,"C:/Users/ismae/Desktop/TFG/CNN_galaxias/prueba .csv", row.names=FALSE)

#___________________________________________________________________________________________________________________#

#Bucle para analizar el id de cada imágen y moverla de un fichero a una carpeta con cada label

#Encuentro todos los archivos de la carpeta de entrenamiento
archivos=list.files("C:/Users/ismae/Desktop/TFG/CNN_galaxias/images_training_rev1",full.names=FALSE)
#Creo un vector con los id de los archivos de entrenamiento requeridos:
galax=as.character(galid)
gala=paste(galax,".jpg",sep="")

for (j in 1:length(galaxy)){
  galaxye=galaxy[j]
  gali=gala[j]
  archivosi=archivos[j]
  if (galaxye==1){
    file.copy(file.path("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/images_training_rev1",gali),
              file.path("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/TRAIN/Espiral",gali))
    next
  }
  if (galaxye==2){
    file.copy(file.path("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/images_training_rev1",gali),
              file.path("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/TRAIN/Eliptica",gali))
    next
  }
  if (galaxye==3){
    file.copy(file.path("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/images_training_rev1",gali),
              file.path("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/TRAIN/Lenticular",gali))
    next
  }
  if (galaxye==4){
    file.copy(file.path("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/images_training_rev1",gali),
              file.path("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/TRAIN/Edge_on",gali))
    next
  }
  if (galaxye==5){
    file.copy(file.path("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/images_training_rev1",gali),
              file.path("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/TRAIN/irreg",gali))
    next
  }
  if (galaxye==6){
    file.copy(file.path("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/images_training_rev1",gali),
              file.path("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/TRAIN/Star_or_artifact",gali))
    next
  }
}
#__________________________________________________________________________________________________

#Extraigo los archivos comunes en dos o más carpetas, para ello primero cargo en variables
#los nombres de todos los archivos.

flsEdg=list.files("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/TRAIN/Edge_on",".jpg$",full.names=TRUE,recursive=FALSE)
flsStr=list.files("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/TRAIN/Star_or_artifact",".jpg$",full.names=TRUE,recursive=FALSE)
flsElp=list.files("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/TRAIN/Eliptica",".jpg$",full.names=TRUE,recursive=FALSE)
flsIrr=list.files("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/TRAIN/Irregular",".jpg$",full.names=TRUE,recursive=FALSE)
flsLen=list.files("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/TRAIN/Lenticular",".jpg$",full.names=TRUE,recursive=FALSE)
flsEsp=list.files("C:/Users/Aussar/Desktop/TFG/CNN_galaxias/TRAIN/Espiral",".jpg$",full.names=TRUE,recursive=FALSE)

#Empezamos con las edge_on
EDST=basename(flsEdg) %in% basename(flsStr)
EDEL=basename(flsEdg) %in% basename(flsElp)
EDIR=basename(flsEdg) %in% basename(flsIrr)
EDEN=basename(flsEdg) %in% basename(flsLen)
EDES=basename(flsEdg) %in% basename(flsEsp)
mov1=flsEdg[EDST]
mov2=flsEdg[EDEL]
mov3=flsEdg[EDIR]
mov4=flsEdg[EDEN]
mov5=flsEdg[EDES]
lapply(mov1,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m1"))
lapply(mov2,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m2"))
lapply(mov3,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m3"))
lapply(mov4,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m4"))
lapply(mov5,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m5"))

#Continuo con star_or_artifact
STEL=basename(flsStr) %in% basename(flsElp)
STIR=basename(flsStr) %in% basename(flsIrr)
STEN=basename(flsStr) %in% basename(flsLen)
STES=basename(flsStr) %in% basename(flsEsp)
mov6=flsStr[STEL]
mov7=flsStr[STIR]
mov8=flsStr[STEN]
mov9=flsStr[STES]
lapply(mov6,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m6"))
lapply(mov7,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m7"))
lapply(mov8,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m8"))
lapply(mov9,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m9"))

#Procedo con Elipticas
ELIR=basename(flsElp) %in% basename(flsIrr)
ELEN=basename(flsElp) %in% basename(flsLen)
ELES=basename(flsElp) %in% basename(flsEsp)
mov10=flsElp[ELIR]
mov11=flsElp[ELEN]
mov12=flsElp[STES]
lapply(mov10,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m10"))
lapply(mov11,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m11"))
lapply(mov12,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m12"))

#Continuo con irregulares
IREN=basename(flsIrr) %in% basename(flsLen)
IRES=basename(flsIrr) %in% basename(flsEsp)
mov13=flsIrr[IREN]
mov14=flsIrr[IRES]
lapply(mov13,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m13"))
lapply(mov14,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m14"))

#Finalizo con lenticulares
ENES=basename(flsLen) %in% basename(flsEsp)
mov15=flsLen[ENES]
lapply(mov15,function(x) file.move(x,"C:/Users/Aussar/Desktop/TFG/CNN_galaxias/MOV/m15"))