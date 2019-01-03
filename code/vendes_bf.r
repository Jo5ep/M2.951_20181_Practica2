# Practica 2 - Tipologia i cicle de vida de les dades
# Propietat de la UOC i l'alumne Josep Consuegra Navarrina
# 03/01/2018
# Released Under CC BY-NC-SA 4.0 License

# Lectura de les dades
vendes_bf <- read.csv("C:/Users/josepconsuegra/Desktop/BlackFriday.csv", header=TRUE, sep=",")
str(vendes_bf)

# Aplicarem la funció factor als atributs Occupation, Marital_Status i Product_Category_N:
vendes_bf_net <- vendes_bf
vendes_bf_net$Occupation <- as.factor(vendes_bf_net$Occupation)
vendes_bf_net$Marital_Status <- as.factor(vendes_bf_net$Marital_Status)
vendes_bf_net$Product_Category_1 <- as.factor(vendes_bf_net$Product_Category_1)
vendes_bf_net$Product_Category_2 <- as.factor(vendes_bf_net$Product_Category_2)
vendes_bf_net$Product_Category_3 <- as.factor(vendes_bf_net$Product_Category_3)
str(vendes_bf_net)

# 3. Neteja de les dades
# a. Les dades contenen zeros o elements buits? Com gestionaries aquests casos?
summary(vendes_bf_net)

# b. Identificació i tractament de valors extrems.
boxplot.stats(vendes_bf_net$Occupation)$out
boxplot.stats(vendes_bf_net$Product_Category_1)$out
boxplot.stats(vendes_bf_net$Product_Category_2)$out
boxplot.stats(vendes_bf_net$Product_Category_3)$out

# Quan realitzem l'agregació de dades veurem que si apareixen valors extrems; Més endavant s'explica com es genera aquest set de dades.
length(boxplot.stats(vendes_bf_agg$Purchase)$out)


# 4. Anàlisi de les dades i representació dels resultats a partir de taules i gràfiques.
# a. Selecció dels grups de dades que es volen analitzar/comparar (planificació dels anàlisis a aplicar).
# Deixarem de banda, d’entrada, les columnes Product_Category_N
# Procedim doncs a realitzar l’agregació de les dades. En primer lloc calcularem l’import total, i, tot seguit, el nombre de productes comprats per l’usuari, que ajuntarem a una mateixa taula:
library(plyr)
library(data.table)
vendes_bf_count <- count(vendes_bf_net, c("User_ID"))
vendes_bf_sum <- aggregate(Purchase ~ User_ID + Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years + Marital_Status, vendes_bf_net, sum)
vendes_bf_agg <- join(vendes_bf_sum, vendes_bf_count, by="User_ID", type="left", match="all")
View(vendes_bf_agg)
vendes_bf_agg$mean_price <- vendes_bf_agg$Purchase/vendes_bf_agg$freq
setnames(vendes_bf_agg, old=c("freq"), new=c("Items"))
setnames(vendes_bf_agg, old=c("mean_price"), new=c("Average_Price"))
 
# b. Comprovació de la normalitat i homogeneïtat de la variància.
library(nortest)
p_Val=ad.test(vendes_bf_agg$Purchase)$p.value
p_Val

# sembla que les dades no son normals i inspeccionem visualment la distribució
library(ggpubr)
ggdensity(vendes_bf_agg$Purchase, main = "Gràfica de densitat del preu de compra", xlab = "Preu de compra")
hist(vendes_bf_agg$Purchase, main="Histograma de preu de compra", xlab="Preu de compra")
ggqqplot(vendes_bf_agg$Purchase, main="Gràfica Q-Q del preu de compra")

# Podem observar com clarament la mostra no segueix una distribució normal amb el test de KS
library(vcd)
library(MASS)
ks.test(vendes_bf_agg$Purchase, "pnorm", mean=mean(vendes_bf_agg$Purchase), sd=sd(vendes_bf_agg$Purchase))

# intentem trobar quina distribució pot representar el nostre set de dades amb la gràfica de Cullen i Frey
library(fitdistrplus)
library(logspline)
descdist(vendes_bf_agg$Purchase, discrete = FALSE)

# Intentem normalitzar les dades convertint a escala logaritmica:
vendes_bf_log = log(vendes_bf_agg$Purchase)
ggdensity(vendes_bf_log, main = "Gràfica de densitat del preu de compra (log)", xlab = "Preu de compra (log)")
hist(vendes_bf_log, main="Histograma del logaritme de l'import de compres", xlab = "log(Import)")

# Comprovem el nou set de dades:
ggqqplot(vendes_bf_log, main="Gràfica Q-Q del logaritme del preu de compra")
 
# Per evitar interpretacions incorrectes, podem avaluar la normalitat de la distribució com en el cas anterior.
fitdistr(vendes_bf_log, "normal")

# Utilitzem els paràmetres estimats:
ks.test(vendes_bf_log, "pnorm", mean=13.17, sd=0.99265)

# Veiem que el p-valor no compleix amb el llindar de 0.05 imposat. Analitzem la homogeneïtat de la variança: 
fligner.test(Purchase ~ Gender, data = vendes_bf_agg)
fligner.test(Purchase ~ Occupation, data = vendes_bf_agg)
fligner.test(Purchase ~ Marital_Status, data = vendes_bf_agg)
fligner.test(Purchase ~ Age, data = vendes_bf_agg)

# c. Aplicació de proves estadístiques per comparar els grups de dades.

# ¿Un dels dos gèneres compra més que l’altre? ¿Quin?
library(lattice)
histogram(~ Purchase | Gender, data=vendes_bf_agg, layout=c(1,2))
boxplot(Purchase ~ Gender, data = vendes_bf_agg, xlab="Gènere", main="Import de compra per Gènere", outline = FALSE)
 
# U test de Mann-Whitney (una sola variable amb dues categories):
wilcox.test(Purchase ~ Gender, data = vendes_bf_agg)

# Per tal de validar que els valors extrems no afecten a aquest càlcul, farem el mateix procés tot eliminant els outliers amb la llibreria boxplot, i veurem que obtenim els mateixos resultats:
vendes_bf_in <- vendes_bf_agg[!vendes_bf_agg$Purchase %in% boxplot.stats(vendes_bf_agg$Purchase)$out,]
with(vendes_bf_in, wilcox.test(Purchase ~ Gender))

# Donat que la hipòtesi nul·la que volem validar per al U test de Mann-Whitney és que les dues mostres (F i M) pertanyen a la mateixa població, i veient que el p-valor obtingut està per sota del llindar d’acceptabilitat, podem assumir que es tracta efectivament de dues poblacions diferents, i que per tant el gènere es un factor diferencial en l’import de compra, sent el gènere masculí el que a priori més gasta.

# ¿Hi ha altres factors que influeixin directament en el volum de compra?
# Visualitzem les dades 
boxplot(Purchase ~ Age, data = vendes_bf_agg, xlab="Edat", main="Import de compra per Edat")
boxplot(Purchase ~ City_Category, data = vendes_bf_agg, xlab="Categoria de la Ciutat", main="Import de compra per Categoria de la Ciutat")
boxplot(Purchase ~ Marital_Status, data = vendes_bf_agg, xlab="Estat Matrimonial", main="Import de compra per Estat Matrimonial")
boxplot(Purchase ~ Stay_In_Current_City_Years, data = vendes_bf_agg, xlab="Estada a la ciutat actual", main="Import de compra per Estada a la ciutat actual")
boxplot(Purchase ~ Occupation, data = vendes_bf_agg, xlab="Ocupació", main="Import de compra per Ocupació")
 
# De cara a mostrar aquests resultats de manera més entenedora, podem eliminar els valors extrems de la gràfica i ajuntar-los tots en un mateix plot:
attach(vendes_bf_agg)
par(mfrow=c(3,2))
boxplot(Purchase ~ Age, xlab="Edat", main="Import de compra per Edat", outline=FALSE)
boxplot(Purchase ~ City_Category, xlab="Categoria de la Ciutat", main="Import de compra per Categoria de la Ciutat", outline = FALSE)
boxplot(Purchase ~ Marital_Status, xlab="Estat Matrimonial", main="Import de compra per Estat Matrimonial", outline = FALSE)
boxplot(Purchase ~ Stay_In_Current_City_Years, xlab="Estada a la ciutat actual", main="Import de compra per Estada a la ciutat actual", outline = FALSE)
boxplot(Purchase ~ Occupation, xlab="Ocupació", main="Import de compra per Ocupació", outline = FALSE)
boxplot(Purchase ~ Gender, xlab="Gènere", main="Import de compra per Gènere", outline = FALSE)
 
# Visualment ja podem apreciar diferencies entre les variables: Per una banda, sembla que l’estat matrimonial no afecta a l’import de les compres, i de mateixa manera per a l’estada en la ciutat actual.
# En canvi, la categoria de ciutat, l’edat i l’ocupació semblen tenir impacte en l’import de compra, a més del gènere, que ja hem estudiat prèviament. 
# Validarem aquestes hipòtesis amb les proves no-paramètriques necessàries, per variables categòriques de dos nivells amb Mann-Whitney, per variables categòriques de més de dos nivells amb Kruskal-Wallis. 
# Cal tenir present que independentment de la prova realitzada, la hipòtesi nul·la contempla que les mostres pertanyen a la mateixa població, i per tant:
#	Si el p-valor es inferior a 0.05, es pot afirmar que les mostres presenten diferències significatives.
#	Si el p-valor es superior a 0.05, es pot afirmar que les mostres no presenten diferències significatives, i que per tant la variable independent avaluada no té incidència en la variable dependent (import de compra).
# Realitzem doncs el test de Kruskal-Wallis per a la resta de variables dependents, excepte per a l’estat matrimonial, donat que es tracta de una variable binaria. 
kruskal.test(Purchase ~ Age, data = vendes_bf_agg)
kruskal.test(Purchase ~ City_Category, data = vendes_bf_agg)
kruskal.test(Purchase ~ Occupation, data = vendes_bf_agg)
kruskal.test(Purchase ~ Stay_In_Current_City_Years, data = vendes_bf_agg)
wilcox.test(Purchase ~ Marital_Status, data = vendes_bf_agg)

# ¿Podem predir el consum del client?
# Volem analitzar si hi ha variables que permeten predir l’import de les compres de l’usuari
# Cal analitzar la multi-col·linealitat del model de regressió. 
# De cara a poder avaluar la qualitat del model obtingut, separarem el set de dades en dos, per tal d’entrenar el model d’una banda (70% de les dades, és a dir 4123 registres)
# i avaluar-lo d’una altra (30% de les dades, és a dir 1768 registres). Aleatoritzem les mostres i definim el model de regressió lineal amb les dades d’entrenament:
sample <- sample.int(n = nrow(vendes_bf_agg), size = floor(.70*nrow(vendes_bf_agg)), replace = F)
train <- vendes_bf_agg[sample, 2:8]
test <- vendes_bf_agg[-sample, 2:8]

library(car)

# Contemplant les variables significatives
lmfit = lm(Purchase ~ Age + Occupation + Gender + City_Category, data = train)
summary(lmfit)
# contemplant totes les variables, veiem que baixa la qualitat
lmfit = lm(Purchase ~ Age + Occupation + Marital_Status + Gender + City_Category + Stay_In_Current_City_Years, data = train)
summary(lmfit)

# Representem gràficament el model obtingut:
par(mfrow=c(2,2))
plot(lmfit)
 
# Mantenim per tant les variables inicials per a la definició del model regressiu.
# Addicionalment, cal analitzar si les variables són col·lineals, i per tant variables predictives estan correlacionades entre elles. 
# Aquest factor apareix si l’arrel quadrada del factor d’inflació de variança es superior a 2, tot i que en aquest cas no tenim aquest problema:
vif(lmfit)
sqrt(vif(lmfit)) > 2

# Definim doncs el nostre model de regressió lineal:
predict <- predict(lmfit, test, interval="confidence", level=0.95)
test$IC_prediction <- predict
test$Error<-(test$Purchase-test$IC_prediction)*100/test$Purchase 
summary(test$Error)

mean(test$Error)
sd(test$Error)

# L’error relatiu calculat representa un percentatge de l’import total de compra i veiem mala qualitat del model.
# Probem un model de regressió per quantils

library(quantreg)
quantile <- rq(Purchase ~ Age + Occupation + Gender + City_Category, data = train, tau = 0.5)
summary(quantile)

# Analitzem la qualitat del model
test$QT_prediction <- predict.rq(quantile,test,interval = "confidence", level=0.95)
test$QT_Error<-(test$Purchase-test$QT_prediction)*100/test$Purchase

mean(test$QT_Error)
sd(test$QT_Error)