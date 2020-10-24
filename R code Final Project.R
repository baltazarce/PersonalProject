#------------------------------------------- LOAD LIBRARYS ----------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# Data from INEGI
#https://www.inegi.org.mx/contenidos/programas/mortalidad/datosabiertos/defunciones/2015/defunciones_base_datos_2015_csv.zip
#https://www.inegi.org.mx/contenidos/programas/mortalidad/datosabiertos/defunciones/2016/defunciones_base_datos_2016_csv.zip
#https://www.inegi.org.mx/contenidos/programas/mortalidad/datosabiertos/defunciones/2017/conjunto_de_datos_defunciones_generales_2017_csv.zip
#https://www.inegi.org.mx/contenidos/programas/mortalidad/datosabiertos/defunciones/2018/conjunto_de_datos_defunciones_registradas_2018_csv.zip

# In case the INEGI web page is not available, the data has been uploaded to GitHub
#https://github.com/baltazarce/PersonalProject/blob/main/defunciones_base_datos_2015_csv.zip
#https://github.com/baltazarce/PersonalProject/blob/main/defunciones_base_datos_2016_csv.zip
#https://github.com/baltazarce/PersonalProject/blob/main/conjunto_de_datos_defunciones_generales_2017_csv.zip
#https://github.com/baltazarce/PersonalProject/blob/main/conjunto_de_datos_defunciones_registradas_2018_csv.zip


#Read data  and select only the columns that we need
cols <- c("ent_ocurr", "causa_def", "lista_mex", "sexo", "edad", "dia_ocurr", "mes_ocurr",
          "anio_ocur", "edo_civil",  "lugar_ocur", "vio_fami", "escolarida","par_agre")

#INEGI
#Download data
dl <- tempfile()
#------------------------------------------ 2015 ------------------------------------------
download.file("https://www.inegi.org.mx/contenidos/programas/mortalidad/datosabiertos/defunciones/2015/defunciones_base_datos_2015_csv.zip", dl)

# IN CASE INEGI FAILS, USE THIS LINE TO DOWNLOAD DATA
#download.file(#https://github.com/baltazarce/PersonalProject/blob/main/defunciones_base_datos_2015_csv.zip, dl)
data <- read.csv(unzip(dl, files = "conjunto_de_datos/defunciones_generales_2015.csv")) %>% 
  select(cols)

#------------------------------------------ 2016 ------------------------------------------
download.file("https://www.inegi.org.mx/contenidos/programas/mortalidad/datosabiertos/defunciones/2016/defunciones_base_datos_2016_csv.zip", dl)

# IN CASE INEGI FAILS, USE THIS LINE TO DOWNLOAD DATA
#download.file(#https://github.com/baltazarce/PersonalProject/blob/main/defunciones_base_datos_2016_csv.zip, dl)
data_temp <- read.csv(unzip(dl, files = "conjunto_de_datos/defunciones_generales_2016.csv")) %>% 
  select(cols)

data <- rbind(data, data_temp)
#------------------------------------------ 2017 ------------------------------------------
download.file("https://www.inegi.org.mx/contenidos/programas/mortalidad/datosabiertos/defunciones/2017/conjunto_de_datos_defunciones_generales_2017_csv.zip", dl)

# IN CASE INEGI FAILS, USE THIS LINE TO DOWNLOAD DATA
#download.file(#https://github.com/baltazarce/PersonalProject/blob/main/conjunto_de_datos_defunciones_generales_2017_csv.zip, dl)

data_temp <- read.csv(unzip(dl, files = "conjunto_de_datos/conjunto_de_datos_defunciones_generales_2017.CSV")) %>% 
  select(cols)

data <- rbind(data, data_temp)
#------------------------------------------ 2018 ------------------------------------------
download.file("https://www.inegi.org.mx/contenidos/programas/mortalidad/datosabiertos/defunciones/2018/conjunto_de_datos_defunciones_registradas_2018_csv.zip", dl)

# IN CASE INEGI FAILS, USE THIS LINE TO DOWNLOAD DATA
#download.file(#https://github.com/baltazarce/PersonalProject/blob/main/conjunto_de_datos_defunciones_registradas_2018_csv.zip, dl)

data_temp <- read.csv(unzip(dl, files = "conjunto_de_datos/conjunto_de_datos_defunciones_registradas_2018.csv")) %>% 
  select(cols)

data <- rbind(data, data_temp)

#------------------------------ FILTER DATA MURDERS AND KNOWN MURDERER --------------------
data <- data %>% filter(lista_mex==55) %>% filter(par_agre<88)

#Open the Dictionary for the data
dictionary <- read.csv(unzip(dl, files = "diccionario_de_datos/diccionario_datos_defunciones_registradas_2018.csv"))

#Read the file we need to open to fill the values in the data table
values<-sapply(cols, function(col_name){
  values<-dictionary$CATÁLOGO[which(dictionary$NEMÓNICO==col_name)]
  values<-paste(values,".csv", sep="")
  values<-str_replace_all(values, fixed(" "), "")
  return(values)
})

values<-as.vector(values)

#FILL DATA TABLE WITH INFORMATION FROM THE FILES IN VALUES IN THE ZIP FILE
#----------------------------------------- ENT_OCURR --------------------------------------------
new_column<-read.csv(unzip(dl, files = paste("catalogos/",values[1], sep="")))
new_column<-new_column %>% rename(ent_ocurr=cve_ent) %>% mutate_if(is.factor, as.character) %>% filter(cve_mun==0)

data <- left_join(data, new_column) %>% select(-ent_ocurr, -cve_mun, -cve_loc)

#----------------------------------------- CAUSA_DEF --------------------------------------------
new_column<-read.csv(unzip(dl, files = paste("catalogos/",values[2], sep="")))
new_column<-new_column %>% rename(causa_def=CVE)
new_column$DESCRIP<-as.character(new_column$DESCRIP)
new_column$DESCRIP<-str_replace_all(new_column$DESCRIP, fixed(" "), "")

data <- left_join(data, new_column) %>% select(-causa_def) %>% rename(causa_def=DESCRIP)

#--------- THIS COLUMN HAS THE PLACE OF DEATH WITH THE CUASE OF DEATH, WE USE REGEX TO SEPARATE THEM AND KEEP ONLY THE CAUSE -------------------
data$causa_def<-str_replace_all(data$causa_def, "^Agresióncondisparodearmacorta\\D+" , "Disparo de arma corta")
data$causa_def<-str_replace_all(data$causa_def, "^Agresióncondisparodeotrasarmasdefuego\\D+" , "Disparo de otras armas de fuego")
data$causa_def<-str_replace_all(data$causa_def, "^Agresióncondisparoderifle,escopetayarmalarga\\D+" , "Disparo de arma larga")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónconfuerzacorporal\\D+" , "Fuerza corporal")
data$causa_def<-str_replace_all(data$causa_def, "^Agresióncongasesyvapores\\D+" , "Agresión con humo, fuego y llamas")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónconhumo,fuegoyllamas\\D+" , "Agresión con humo, fuego y llamas")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónconobjetocortante\\D+" , "Agresión con objeto cortante")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónconobjetoromoosinfilo\\D+" , "Agresión con objeto romo o sin filo")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónconplaguicidas\\D+" , "Agresión con productos químicos")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónconproductosquímicos\\D+" , "Agresión con productos químicos")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónporahogamientoysumersión\\D+" , "Ahogamiento y sumersión")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónporahorcamiento,estrangulamientoysofocación\\D+" , "Ahorcamiento, estrangulamiento y sofocación")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónporempujarocolocaralavíctimadelantedeobjetoenmovimiento\\D+" , "Agresión por colocar a la víctima delante de objeto en movimiento")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónporempujóndesdeunlugarelevado\\D+" , "Empujón desde un lugar elevado")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónpormediosnoespecificados\\D+" , "Medios no especificados")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónporotrosmediosespecificados\\D+" , "Medios no especificados")
data$causa_def<-str_replace_all(data$causa_def, "^Negligenciayabandono\\D+", "Negligencia y abandono")
data$causa_def<-str_replace_all(data$causa_def, "^Agresiónsexualconfuerzacorporal\\D+", "Agresión sexual con fuerza corporal")
data$causa_def<-str_replace_all(data$causa_def, "^Otrossíndromesdemaltrato\\D+", "Otros síndromes de maltrato")

#----------------------------------------- LISTA_MEX --------------------------------------------
new_column<-read.csv(unzip(dl, files = paste("catalogos/",values[3], sep="")))
new_column<-new_column %>% rename(lista_mex=CVE)
new_column$DESCRIP<-as.character(new_column$DESCRIP)

data <- left_join(data, new_column, by="lista_mex") %>% select(-lista_mex) %>% rename(lista_mex=DESCRIP)
#----------------------------------------- SEXO --------------------------------------------
new_column<-read.csv(unzip(dl, files = paste("catalogos/",values[4], sep="")))
new_column<-new_column %>% rename(sexo=CVE)

new_column$DESCRIP<-as.character(new_column$DESCRIP)
new_column$DESCRIP<-str_replace_all(new_column$DESCRIP, fixed(" "), "")


data <- left_join(data, new_column) %>% select(-sexo) %>% rename(sexo=DESCRIP)
#----------------------------------------- EDAD --------------------------------------------
data$edad<-str_replace_all(data$edad, "^[1-3]\\d*", "0")
data$edad<-str_replace_all(data$edad, "^[4]", "")
data$edad<-as.numeric(data$edad)
data <- data %>% filter(edad < 998)
#-------------------------------------- DIA_OCUR ----------------------------------------------
data <- data %>% filter(dia_ocurr < 99)
#-------------------------------------- MES_OCUR ----------------------------------------------
new_column<-read.csv(unzip(dl, files = paste("catalogos/",values[7], sep="")))
new_column<-new_column %>% rename(mes_ocurr=CVE)

data <- left_join(data, new_column) %>% select(-mes_ocurr) %>% rename(mes_ocurr=DESCRIP)
#-------------------------------------- ANIO_OCUR ----------------------------------------------
data <- data %>% filter(anio_ocur < 9999)
#-------------------------------------- ESTADO CIVIL ----------------------------------------------
new_column<-read.csv(unzip(dl, files = paste("catalogos/",values[9], sep="")))
new_column<-new_column %>% rename(edo_civil=CVE)

data <- left_join(data, new_column) %>% select(-edo_civil) %>% rename(edo_civil=DESCRIP)
#-------------------------------------- LUGAR ----------------------------------------------
new_column<-read.csv(unzip(dl, files = paste("catalogos/",values[10], sep="")))
new_column<-new_column %>% rename(lugar_ocur=CVE)

data <- left_join(data, new_column) %>% select(-lugar_ocur) %>% rename(lugar_ocur=DESCRIP)
#-------------------------------------- VIOLENCIA ----------------------------------------------
new_column<-read.csv(unzip(dl, files = paste("catalogos/",values[11], sep="")))
new_column<-new_column %>% rename(vio_fami=CVE)

data <- left_join(data, new_column) %>% select(-vio_fami) %>% rename(vio_fami=DESCRIP)
#-------------------------------------- ESCOLARIDAD ----------------------------------------------
new_column<-read.csv(unzip(dl, files = paste("catalogos/",values[12], sep="")))
new_column<-new_column %>% rename(escolarida=CVE)

data <- left_join(data, new_column) %>% select(-escolarida) %>% rename(escolaridad=DESCRIP)
#-------------------------------------- PARENTESCO ----------------------------------------------
new_column<-read.csv(unzip(dl, files = paste("catalogos/",values[13], sep="")))
new_column<-new_column %>% rename(par_agre=CVE)
new_column$DESCRIP<-as.character(new_column$DESCRIP)
new_column$DESCRIP<-str_replace_all(new_column$DESCRIP, fixed(" "), "")

# ---------- MANUALLY ADD THE SEX OF THE AGRESSOR ----------------
sexo_agresor<-c("Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre",
                "Mujer",  "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer",
                "Hombre", "Mujer",  "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre",
                "Mujer",  "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer",
                "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "No especificado",
                "No especificado", "No especificado", "No especificado", "No especificado", "Hombre", "Mujer", "No especificado",
                "No especificado", "No especificado", "No aplica", "No aplica")

new_column <- cbind(new_column, sexo_agresor)

data$par_agre<-replace(data$par_agre, data$par_agre==72, 71)
data <- left_join(data, new_column) %>% select(-par_agre) %>% rename(par_agre=DESCRIP)
data <- data %>% filter(sexo_agresor != "No aplica")

write.csv(data,"C:\\Users\\Carlos\\Documents\\Data Science\\9.- Final Project\\2.- Personal Project\\Data.csv", row.names = FALSE)
# --------------------------------- CONVERT TO FACTOR --------------------------------------------------------------
data <- data %>% select(-dia_ocurr, -lista_mex, -mes_ocurr)

data$edad <- as.factor(data$edad)
data$anio_ocur <- as.factor(data$anio_ocur)
data$nom_loc <- as.factor(data$nom_loc)
data$causa_def <- as.factor(data$causa_def)
data$sexo <- as.factor(data$sexo)
data$edo_civil <- as.factor(data$edo_civil)
data$lugar_ocur <- as.factor(data$lugar_ocur)
data$vio_fami <- as.factor(data$vio_fami)
data$escolaridad <- as.factor(data$escolaridad)
data$par_agre <- as.factor(data$par_agre)
data$sexo_agresor <- as.character(data$sexo_agresor)
data$sexo_agresor <- as.factor(data$sexo_agresor)

# ------------------ CLEAN VARIABLES -------------------------------------
rm(dictionary, new_column, cols, dl, values, sexo_agresor, data_temp)


#----------------------------------- DATA EXPLORATION ------------------------------------------------------
#-------------------------------- Numbers per sex ----------------------------------------------
data %>% filter(anio_ocur==2015 | anio_ocur==2016 | anio_ocur==2017 | anio_ocur==2018) %>% 
  filter(sexo=="Hombre") %>% group_by(anio_ocur) %>% summarise(num=n())

data %>% filter(anio_ocur==2015 | anio_ocur==2016 | anio_ocur==2017 | anio_ocur==2018) %>% 
  filter(sexo=="Mujer") %>% group_by(anio_ocur) %>% summarise(num=n())

#-------------------------------- Murders by per sex and sex agresor ----------------------------------------------
x<-data %>% filter(sexo == "Mujer") %>%  group_by(sexo_agresor) %>% summarise(total=n())
y<-data %>% filter(sexo == "Hombre") %>%  group_by(sexo_agresor) %>% summarise(total=n())

data %>% filter(sexo == "Mujer") %>%  ggplot(aes(sexo)) + geom_bar(aes(fill=sexo_agresor)) + 
  ggtitle("Killer's sex for women") +
  labs(y= "Total murders", x="Sexo", fill= "Killer's sex") +
  annotate("text", label = paste(format(x$total[3]*100/sum(x$total), digits = 4),"%", sep = ""), x = 1, y = 15, size = 5) +
  annotate("text", label = paste(format(x$total[2]*100/sum(x$total), digits=3),"%", sep = ""), x = 1, y = 150, size = 5) +
  annotate("text", label = paste(format(x$total[1]*100/sum(x$total), digits=4),"0%", sep = ""), x = 1, y = 180, size = 5) +
  scale_fill_manual("Killer's sex", values = c("Hombre" = "#56B4E9", "Mujer" = "#CC79A7", "No especificado" = "#009E73")) +
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))

data %>% filter(sexo == "Hombre") %>% ggplot(aes(sexo)) + geom_bar(aes(fill=sexo_agresor)) + 
  ggtitle("Killer's sex for men") +
  labs(y = "Total murders", x = "Sexo", fill = "Killer's sex") +
  annotate("text", label = paste(format(y$total[3]*100/sum(y$total), digits = 4),"%", sep = ""), x = 1, y = 35, size = 5) +
  annotate("text", label = paste(format(y$total[2]*100/sum(y$total), digits=3),"%", sep = ""), x = 1, y = 1125, size = 5) +
  annotate("text", label = paste(format(y$total[1]*100/sum(y$total), digits=4),"%", sep = ""), x = 1, y = 1200, size = 5) +
  scale_fill_manual("Killer's sex", values = c("Hombre" = "#56B4E9", "Mujer" = "#CC79A7", "No especificado" = "#009E73")) +
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))

data %>% ggplot(aes(sexo)) + geom_bar(aes(fill=sexo_agresor)) + ggtitle("Killer's sex") +
  labs(y = "Total murders", x="Sexo", fill= "Killer's sex") +
  scale_fill_manual("Killer's sex", values = c("Hombre" = "#56B4E9", "Mujer" = "#CC79A7", "No especificado" = "#009E73")) +
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))

#-------------------------------- Murders by per sex and agresor ----------------------------------------------
data %>% filter(sexo == "Mujer") %>% group_by(par_agre) %>% summarise(num = n()) %>% arrange(desc(num)) %>% top_n(10) %>%
  ggplot(aes(x=reorder(par_agre, -num), y=num)) + geom_bar(stat="identity", fill="#C3D7A4", color="#52854C") + 
  ggtitle("Relationship between murderer and female victim") +
  labs(y = "Total murders", x="Relationship") + theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))

data %>% filter(sexo == "Hombre") %>% group_by(par_agre) %>% summarise(num = n()) %>% arrange(desc(num)) %>% top_n(10) %>%
  ggplot(aes(x=reorder(par_agre, -num), y=num)) + geom_bar(stat="identity", fill="#C3D7A4", color="#52854C") + 
  ggtitle("Relationship between murderer and male victim") +
  labs(y = "Total murders", x="Relationship") + theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))

data %>% filter(sexo == "Hombre") %>% filter(par_agre != "Sinparentesco") %>% group_by(par_agre) %>% summarise(num = n()) %>% arrange(desc(num)) %>% top_n(10) %>%
  ggplot(aes(x=reorder(par_agre, -num), y=num)) + geom_bar(stat="identity", fill="#C3D7A4", color="#52854C") + 
  ggtitle("Relationship between murderer and male victim\n for known relations") +
  labs(y = "Total murders", x="Relationship") + theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))

#-------------------------------- Murders by per sex and cause ----------------------------------------------
data %>% filter(sexo == "Mujer") %>% group_by(causa_def) %>% summarise(num = n()) %>% arrange(desc(num)) %>% top_n(10) %>%
  ggplot(aes(x=reorder(causa_def, -num), y=num)) + geom_bar(stat="identity", fill="#4E84C4", color="#0072B2") + 
  ggtitle("Cause of death for female victim") +   labs(y = "Total murders", x="Cause of death") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))

data %>% filter(sexo == "Hombre") %>% group_by(causa_def) %>% summarise(num = n()) %>% arrange(desc(num)) %>% top_n(10) %>%
  ggplot(aes(x=reorder(causa_def, -num), y=num)) + geom_bar(stat="identity", fill="#4E84C4", color="#0072B2") +
  ggtitle("Cause of death for male victim") +   labs(y = "Total murders", x="Cause of death") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))
#-------------------------------- Murders filter by sex agresor vs cause ----------------------------------------------
data %>% filter(sexo == "Mujer") %>% group_by(par_agre, causa_def) %>% summarise(num = n()) %>% filter(num>10) %>% arrange(desc(num))
data %>% filter(sexo == "Hombre") %>% group_by(par_agre, causa_def) %>% summarise(num = n()) %>% filter(num>10)  %>% arrange(desc(num))

#-------------------------------- Murders by per sex and cause ----------------------------------------------
data %>% filter(sexo == "Mujer") %>% group_by(lugar_ocur) %>% summarise(num = n()) %>% arrange(desc(num)) %>% top_n(10) %>%
  ggplot(aes(x=reorder(lugar_ocur, -num), y=num)) + geom_bar(stat="identity", fill="#4E84C4", color="#0072B2") + 
  ggtitle("Cause of death for female victim") +   labs(y = "Total murders", x="Cause of death") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))

data %>% filter(sexo == "Hombre") %>% group_by(lugar_ocur) %>% summarise(num = n()) %>% arrange(desc(num)) %>% top_n(10) %>%
  ggplot(aes(x=reorder(lugar_ocur, -num), y=num)) + geom_bar(stat="identity", fill="#E69F00", color="#C4961A") +
  ggtitle("Place of death for male victim") +   labs(y = "Total murders", x="Cause of death") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------------------
#SEPARATE TRAIN AND VALIDATION SETS
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
index <- createDataPartition(y = data$sexo, times = 1, p = 0.2, list = FALSE)
train_set <- data[-index,]
#validation_set <- data[index,]
temp <- data[index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(train_set, by = "edad") %>%
  semi_join(train_set, by = "anio_ocur") %>%
  semi_join(train_set, by = "causa_def")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
train_set <- rbind(train_set, removed)

rm(temp, removed, index)

#----------------------------- DEAD'S SEX ------------------------------------------
#Base line:
mean("Hombre"==validation$sexo)

train_CART <- train(sexo ~ .,
                    method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                    data = train_set)

plot(train_CART$finalModel, margin = 0.1)
text(train_CART$finalModel)

rpart_preds <- predict(train_CART, validation)
mean(rpart_preds == validation$sexo)

varImp(train_CART)

train_knn <- train(sexo ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   trControl = trainControl(method = "cv", number = 10, p = 0.9))

knn_preds <- predict(train_knn, validation)
mean(knn_preds == validation$sexo)

varImp(train_knn)

train_rpart <- train(sexo ~ .,
                     data = train_set,
                     method = "rpart1SE")

rpart_preds <- predict(train_rpart, validation)
mean(rpart_preds == validation$sexo)

varImp(train_rpart)

train_glm <- train(sexo ~ ., 
                   method = "glm",
                   data = train_set)

glm_preds <- predict(train_glm, validation)
mean(glm_preds == validation$sexo)

varImp(train_glm)

train_rf <- train(sexo ~ .,
                  data = train_set,
                  method = "rf",
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:7)))

glm_preds <- predict(train_rf, validation)
mean(glm_preds == validation$sexo)

varImp(train_rf)