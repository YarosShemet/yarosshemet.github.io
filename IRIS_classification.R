library(ks)
df <- as.data.frame(iris)

#Dla stworzenia wykresu potrzebuje tylko dwóch zmiennych regresoróW 

#1
Zestaw_sepal_X <- data.frame(df$Sepal.Length, df$Sepal.Width)
Zestaw_petal_X <- data.frame(df$Petal.Length, df$Petal.Width)
Y <- data.frame(df$Species)

H1 <- Hscv(x=Zestaw_sepal_X) #optymalna wartoœæ estymatora macierzy bandwidth wg sprawdzianu krzyzowego
H2 <- Hscv(x=Zestaw_petal_X)

kde_sepal <- ks::kde(x = Zestaw_sepal_X, H = H1)
image(kde_sepal$eval.points[[1]], kde_sepal$eval.points[[2]], kde_sepal$estimate,
      col = viridis::viridis(20), xlab = "D³ugoœæ listków", ylab = "Szerokoœæ listków")
points(kde_sepal$x) 

kde_petal <- ks::kde(x = Zestaw_petal_X, H = H2)
image(kde_petal$eval.points[[1]], kde_petal$eval.points[[2]], kde_petal$estimate,
      col = viridis::viridis(20), xlab = "D³ugoœæ p³atków", ylab = "Szerokoœæ p³atków")
points(kde_petal$x) 

plot(kde_sepal, display = "persp", col.fun = viridis::viridis, xlab = "D³ugoœæ listkÃ³w", ylab = "Szerokoœæ listków")
plot(kde_petal, display = "persp", col.fun = viridis::viridis, xlab = "D³ugoœæ p³atków", ylab = "Szerokoœæ p³atków")

#2
train.size <- 0.8 * nrow(df)
proba <- sample(1:nrow(df), size = train.size)

normalizacja <-function(x) {(x -min(x))/(max(x)-min(x))}
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], normalizacja))
iris_norm$Species <- df$Species

df_treningowy <-  iris_norm[proba,]
df_testowy <-  iris_norm[-proba,]

x_train <- df_treningowy[,1:4]
x_test <- df_testowy[,1:4]

y_train <- as.factor(df_treningowy[,5])
y_test <- as.factor(df_testowy[,5])

library(class)
prognoza5 <- knn(x_train,x_test,cl=y_train,k=5)
prognoza50 <- knn(x_train,x_test,cl=y_train,k=50)

#tabela trafnoœci
tt <- table(prognoza5,y_test)
tt

#obliczamy dok³adnoœæ klasyfikacji dla zbioru testowego
dokladnosc <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
dokladnosc(tt)
