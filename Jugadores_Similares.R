install.packages("dplyr")
library(dplyr)


#Lee los datos 
FifaDF=read.csv('fifa2019.csv', encoding="UTF-8",sep=',', header=TRUE)


#head(FifaDF)

#Elimina los atributos que no sirven para formar vectores
VectorDF <- subset(FifaDF, select = c(1, 9:47))

#Convierte el dataframe a numeric
VectorDF = as.data.frame(sapply(VectorDF, as.numeric))

#Agrego el row_id como columna del data frame
id_row <- rownames(VectorDF)
VectorDF <- cbind(id_row=id_row, VectorDF)


jugadores_similares <- function(df,x,y,z){
  
  id_jugador = x #"id" de jugador a investigar
  
  #calcula el "id_row" de un "id" de jugador dado
  nro_row = which(df$id == x) 
  
  #determina la posicion de un "id" de jugador dado
  posicion=df[nro_row,"position"]
  
  #factor por el que se van a multiplicar las variables dependiendo de la posicion
  factor = z
  
  if(posicion==1) { #si es arquero
    df = mutate(df,
                      gkdiving = case_when("position"==1 ~ gkdiving * factor,TRUE ~ gkdiving),
                      gkhandling = case_when("position"==1 ~ gkhandling * factor,TRUE ~ gkhandling),
                      gkkicking = case_when("position"==1 ~ gkkicking * factor,TRUE ~ gkkicking),
                      gkpositioning = case_when("position"==1 ~ gkpositioning * factor,TRUE ~ gkpositioning),
                      gkreflexes = case_when("position"==1 ~ gkreflexes * factor,TRUE ~ gkreflexes)
    ) #pondera los atributos de los arqueros
  } else {
    if(posicion>=10 && posicion<=35) { #si es defensor
      df = mutate(df,
                        marking = case_when("position">=10 && "position"<=35 ~ marking * factor,TRUE ~ marking),
                        standingtackle = case_when("position">=10 && "position"<=35 ~ standingtackle * factor,TRUE ~ standingtackle),
                        slidingtackle = case_when("position">=10 && "position"<=35 ~ slidingtackle * factor,TRUE ~ slidingtackle),
                        interceptions = case_when("position">=10 && "position"<=35 ~ interceptions * factor,TRUE ~ interceptions),
                        headingaccuracy = case_when("position">=10 && "position"<=35 ~ headingaccuracy * factor,TRUE ~ headingaccuracy)
      ) #pondera los atributos de los defensores
    } else {
      if(posicion>=40 && posicion<=70) { #si es mediocampista
        df = mutate(df,
                          positioning = case_when("position">=40 && "position"<=70 ~ positioning * factor,TRUE ~ positioning),
                          vision = case_when("position">=40 && "position"<=70 ~ vision * factor,TRUE ~ vision),
                          longpassing = case_when("position">=40 && "position"<=70 ~ longpassing * factor,TRUE ~ longpassing),
                          ballcontrol = case_when("position">=40 && "position"<=70 ~ ballcontrol * factor,TRUE ~ ballcontrol),
                          fkaccuracy = case_when("position">=40 && "position"<=70 ~ fkaccuracy * factor,TRUE ~ fkaccuracy),
                          shortpassing = case_when("position">=40 && "position"<=70 ~ shortpassing * factor,TRUE ~ shortpassing),
        ) #pondera los atributos de los mediocampistas
      } else {
        df = mutate(df, #si es delantero
                          
                          finishing = case_when("position">70 ~ finishing * factor,TRUE ~ finishing),
                          volleys = case_when("position">70 ~ volleys * factor,TRUE ~ volleys),
                          dribbling = case_when("position">70 ~ dribbling * factor,TRUE ~ dribbling),
                          agility = case_when("position">70 ~ agility * factor,TRUE ~ agility),
                          shotpower = case_when("position">70 ~ shotpower * factor,TRUE ~ shotpower)
        ) #pondera los atributos de los delanteros
      }
    }
  }
  
  
  
  #Calcula las componentes principales sin el "id" utilizando metodo de covarianza
  PRC <- prcomp(df[,(3:41)])
  #PRC$x
  #summary(PRC)
  
  #Calculo la variabilidad para cada componente principal
  #variabilidadCov = PRC$sdev^2/ sum(PRC$sdev^2)
  #round(variabilidadCov,2) 
  #plot(variabilidadCov, type = "o", lwd=2 , col = 'blue',main="Variabilidad por Componentes según Covarianza",xlab="Cant de Componentes",ylab="Variabilidad")
  
  
  #Agrega al dataframe los valores del PCA
  df = cbind(df,PRC$x)
  
  #selecciones el "id_row"+id"+los primeros n PCA (97% de variabilidad de los datos)
  df <- subset(df, select = c(1:2, PC1:PC31))
  
  
  
  #Agrega la columna distancia con el valor de la dist euclediana de todos los jugadores respecto a un "id_row" dado 
  df$dist = as.matrix(dist(df[,3:33]))[nro_row, ]
  
  #Selecciona los y jugadores mas similares
  SimilaresDF = top_n(df, (-1*(1+y)), dist) 
  
  #Busca la info de los jugadores mas similares
  SimilaresDF = merge(SimilaresDF, FifaDF, by = "id")
  
  #Devuelve los y jugadores mas similares ordenados por la distancia
  resultadoDF = select(arrange(SimilaresDF,dist), id, dist, name, age,nationality,club,value,wage,release_clause)
  
  return(resultadoDF)
}

jugadores_similares(VectorDF,242267,5,10)

#messi 158023
#di maria 183898
#neymar 190871
#suarez 176580
#de gea 193080
#pogba 195864
#busquets 189511
#ronaldo 20801
#thiago silva 164240
#kante 215914
#campuzano 242267