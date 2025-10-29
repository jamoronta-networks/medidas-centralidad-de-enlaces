## Script para medidas centralidad de enlaces
setwd("C:/Users/USUARIO/Documents/ERRE/Centr_ML_2024")################ojo 

require(igraph)
require(scatterplot3d)
require(ggplot2)
require(CINNA)
library(ggridges)
library(factoextra)
library(paletteer)
library(caret)
library(dplyr)
library(rpart.plot)
library(rpart)
library(e1071)

numnet=3 #num redes  #16
redes=matrix(0,numnet,1)

redes[1,1]="ieee30.txt"  
redes[2,1]="ieee57.txt"
redes[3,1]="ieee118.txt"

aaa=Sys.time() ##cr




M<-list()                                ## Lista que recoge los indices por enlace `para cada una de las redes
N<-matrix(0, nrow = IC, ncol = numnet)
NConvert<-matrix(0, nrow = IC, ncol = numnet)

for (j in 1:numnet) {

ffv<-scan(redes[j,1], what=list(0,0,0));
f1<-ffv[[1]] #Nodo desde
f2<-ffv[[2]] #Nodo hasta
f3<-ffv[[3]] #Importancia
 network=graph(t(cbind(f1,f2)), directed=FALSE)
 edge_network= make_line_graph(network)                 # CAMBIA ENLACES POR NODOS
E= length(f1) #enlaces de la red
V=max(f1,f2) # nodos de la red



N[1,j] <- edge_density(network)  ### Calcula densidad de la red
N[2,j] <- diameter(network)      ### Calcula el diametro de la red
N[3,j] <- transitivity(network)  ### calcula ransitivity measures the probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient.  


NConvert[1,j] <- edge_density(edge_network)  ### Calcula densidad de la red CONVERTIDA
NConvert[2,j] <- diameter(edge_network)      ### Calcula el diametro de la red CONVERTIDA
NConvert[3,j] <- transitivity(edge_network)  ### calcula transitivity measures the probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient.  


count<-proper_centralities(edge_network)
Templist<-calculate_centralities(edge_network)

Men<-as.matrix(cbind(ffv[[1]],ffv[[2]],ffv[[3]]))
for (v in 1:length(count)) {
  
  Men<-cbind(Men,Templist[[v]])
  
}


M[[j]]<-Men




}

bbb=Sys.time() 


bbb-aaa



Data<-M

Data[[1]]<-Data[[1]][,-c(21,37)] #eliminar el indice Katz Centrality (Katsz status index)Y lAPLACIAN porque no se calculo en la 118
Data[[2]]<-Data[[2]][,-c(21,37)] #eliminar el indice Katz Centrality (Katsz status index)porque no se calculo en la 118

### INDICES QUE SE CALCULAN #### LOS QUE NO APARECEN O NO SE PODIAN CALCULAR O DABAN null

#1	subgraph centrality scores	Topological Coefficient
#3	Average Distance	Barycenter Centrality
#5	BottleNeck Centrality	Centroid value
#7	Closeness Centrality (Freeman)	ClusterRank
#9	Decay Centrality	Degree Centrality
#11	Diffusion Degree	DMNC - Density of Maximum Neighborhood Component
#13	Eccentricity Centrality	Harary Centrality
#15	eigenvector centralities	K-core Decomposition
#17	Geodesic K-Path Centrality	
#18	Kleinberg's authority centrality scores	Kleinberg's hub centrality scores
#20	clustering coefficient	Lin Centrality
#22	Lobby Index (Centrality)	Markov Centrality
#24	Radiality Centrality	Shortest-Paths Betweenness Centrality
#26	Current-Flow Closeness Centrality	Closeness centrality (Latora)
#28	Cross-Clique Connectivity	Entropy Centrality
#30	EPC - Edge Percolated Component	
#31	Leverage Centrality	MNC - Maximum Neighborhood Component
#33	Semi Local Centrality	
#34	Closeness Vitality	Residual Closeness Centrality
#36	Stress Centrality	Load Centrality
#38	Flow Betweenness Centrality	Information Centrality
#40	Dangalchev Closeness Centrality	Group Centrality
#42	Harmonic Centrality	Local Bridging Centrality
#43	Wiener Index Centrality	




#################################


for (v in 4:47) {
  
  Data[[1]][,v]<-abs(Data[[1]][,v])/max(abs(Data[[1]][,v]))
  Data[[2]][,v]<-abs(Data[[2]][,v])/max(abs(Data[[2]][,v]))
  Data[[3]][,v]<-abs(Data[[3]][,v])/max(abs(Data[[3]][,v]))
  
  
}

Data[[1]]<-Data[[1]][,-c(11,22)]
Data[[2]]<-Data[[2]][,-c(11,22)]
Data[[3]]<-Data[[3]][,-c(11,22)]



#importantes<-10 # #de enlaces mas importantes
 

#Data[[1]][which((Data[[1]][,3]<=10)),3]=1
#Data[[1]][which((Data[[1]][,3]>=10)),3]=0
#Data[[2]][which((Data[[2]][,3]<=10)),3]=1
#Data[[2]][which((Data[[2]][,3]>=10)),3]=0

  
  #Data[[1]][,3]
  #Data[[2]][,3]


## Para cinco grupos

#Data[[1]][,3]<-cut(Data[[1]][,3], breaks = c(0,8,16, 24,32,41), 
#                   labels = c(1,2,3,4,5))
#Data[[2]][,3]<-cut(Data[[2]][,3], breaks = c(0,16,32,48,64,80), 
#                   labels = c(1,2,3,4,5))
#Data[[3]][,3]<-cut(Data[[3]][,3], breaks = c(0,37,74,111,148,186), 
#                   labels = c(1,2,3,4,5))


## Para tres grupos

Data[[1]][,3]<-cut(Data[[1]][,3], breaks = c(0,13,26,41), 
                   labels = c(1,2,3))
Data[[2]][,3]<-cut(Data[[2]][,3], breaks = c(0,27,54,80), 
                   labels = c(1,2,3))
Data[[3]][,3]<-cut(Data[[3]][,3], breaks = c(0,62,124,186), 
                  labels = c(1,2,3))
  


#http://127.0.0.1:24107/graphics/plot_zoom_png?width=1920&height=1017

#discretizar ¿? la importancia
Datos<-rbind(Data[[1]],Data[[2]],Data[[3]])
Datos

Datos<-Datos[,-1]
Datos<-Datos[,-1]


#### Desde aqui para VARIAR PARAMETROS EN EL MODELO O LA MUESTRA D DATOS

Guarda_Datos<-Datos

Datos<-Guarda_Datos




####### ver correlacion de las medidas de centralidad######
##### limpiar caracteristicas

Datos_Temp<-as.matrix(Datos)
Datos_Temp[,1]<-(4/3)-(Datos_Temp[,1]/3)

CInd<-cor(Datos_Temp)

#CInd<-cor(Datos[,-1])
corrplot(CInd)

q<-which(CInd[1,]<0.15 & CInd[1,]>-0.15)
q
#r<-which(CInd[1,]>-0.17)
#r
#length(q)

#Datos<-Datos[,-q]
#Datos
#cor(Datos)

### Decision Tree ####
Datos<-as.data.frame(Datos)
Datos <- Datos %>% mutate_at("V1", factor)

set.seed(1000)
Datos_train <- sample_frac(Datos, .6)
Datos_prueba <- setdiff(Datos, Datos_train)

arbol_1 <- rpart(formula = V1 ~ ., data = Datos_train)
rpart.plot(arbol_1)

prediccion_1 <- predict(arbol_1, newdata = Datos_prueba, type = "class")
confusionMatrix(prediccion_1, Datos_prueba[["V1"]])
### ####

### SVM ####

set.seed(10)  # Para reproducir los mismos resultados
partition <- createDataPartition(y = Datos$V1, p = 0.7, list = FALSE)
train <- Datos[partition,]
test <- Datos[-partition,]
#train <- Datos_train
#test <- Datos_prueba
table(train$V1)
table(test$V1)


set.seed(123)
tune_l = tune.svm(V1~., data=train, kernel="linear",
                  cost = c(0.001, 0.01, 0.1, 1, 5, 10, 50))
summary(tune_l)

bestmod <- tune_l$best.model
bestmod


ggplot(data = tune_l$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de validación ~ hiperparámetro C") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

svm_l<- svm(V1 ~ .,
            data = train,
            type = "C-classification",
            kernel = "linear", 
            cost = tune_l$best.parameters$cost,
            scale = FALSE,
            probability = TRUE
)

summary(svm_l)

svm_score_l_Response <- predict(svm_l, test[,-1], type="response")


MC_svm_l <- confusionMatrix(svm_score_l_Response, (test$V1))
MC_svm_l
### ####



####SVM menos codigo

#x <- Datos[,-1]
#y <- Datos[1]
#model_svm <- svm(V1 ~ ., data=Datos)
#summary(model_svm)

#pred <- predict(model_svm,x)
#confusionMatrix(pred,y$V1)

#table(pred,y$V1)


### Red neuronal simple#### NO TERMINADO
library(neuralnet)

fold.test <- sample(nrow(Datos), nrow(Datos) / 3)
test <- Datos[fold.test, ]
train <- Datos[-fold.test, ]

ann <- neuralnet(as.numeric(V1) ~ ., train, hidden = c(5), stepmax=1e7)
ann


compute(ann, test[ , c(2:43)])
output <- compute(ann, test[ , c(2:43)])

result <- data.frame(
  Real = test$V1, 
  Predicted = levels(Datos$V1)[abs(round(output$net.result))])



