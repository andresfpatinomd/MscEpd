
library(readxl)
library(pacman)
p_load(bueri)
p_load(dplyr)
p_load(lmtest)
p_load(mgcv)

names(data)

#datos excluyendo historia de anemia, sd mielodisplasicos, histerectomía, datos completos
data <- read_excel("C:/Users/AndresFelipe/OneDrive - Universidad del rosario/Año SSO/SARS-CoV2/Altitud/Documentos tesis/tabla_definitiva.xlsx")


glimpse(data)
names(data)

data$sex <- factor(data$sex)
data$smokstatus <- factor(data$smokstatus)
data$htn <- factor(data$htn)
data$hyperlipid <- factor(data$hyperlipid)
data$enf_coronaria <- factor(data$enf_coronaria)
data$chf <- factor(data$chf)
data$asthma <- factor(data$asthma)
data$copd <- factor(data$copd)
data$diabetes <- factor(data$diabetes)
data$enf_cerbvsc <- factor(data$enf_cerbvsc)
data$afib <- factor(data$afib)
data$enf_renal <- factor(data$enf_renal)
data$liverdz <- factor(data$liverdz)
data$autoimmunedz <- factor(data$autoimmunedz)
data$hypothyroid <- factor(data$hypothyroid)
data$HIV <- factor(data$HIV)
data$etoh <- factor (data$etoh)
data$psych<- factor(data$psych)
data$dccondition <-factor(data$dccondition)
data$status <- factor(data$status)

#d.vars <- c("city", "sex",  "smokstatus","htn", "hyperlipid", "enf_coronaria",
#"chf", "asthma","copd","diabetes","enf_cerbvsc","afib","enf_renal", "liverdz", "autoimmunedz",
#"hypothyroid", "HIV", "etoh","psych", "dccondition",  "status","score_sev")


data$age <- as.numeric(data$age)
data$admitht <- as.numeric(data$admitht)
data$admitwt <- as.numeric(data$admitwt)

#c.vars <- c("sbp", "dbp", "hr","rr", "temp","fio2","spo2","imc","safi", "wbc","rbc","lymphabs",     
#"anc", "hb","hct", "mcv", "platelet", "neutrophilp",  
#"basophilp","lymphocytep","monocytep","eosinophilp")
data$age <- as.numeric(data$age)
data$sbp <- as.numeric(data$sbp)
data$dbp <- as.numeric(data$dbp)
data$hr <- as.numeric(data$hr)
data$rr <- as.numeric(data$rr)
data$temp <- as.numeric(data$temp)
data$fio2 <- as.numeric(data$fio2)
data$spo2 <- as.numeric(data$spo2)
data$imc <- as.numeric(data$imc)
data$safi <- as.numeric(data$safi)
data$wbc <- as.numeric(data$wbc)
data$rbc <- as.numeric(data$rbc)
data$lymphabs <- as.numeric(data$lymphabs)
data$anc <- as.numeric(data$anc)
data$hb <- as.numeric(data$hb)
data$hct <- as.numeric(data$hct)
data$mcv <- as.numeric(data$mcv)
data$platelet <- as.numeric(data$platelet)
data$neutrophilp <- as.numeric(data$neutrophilp)
data$basophilp <- as.numeric(data$basophilp)
data$lymphocytep <- as.numeric(data$lymphocytep)
data$monocytep <- as.numeric(data$monocytep)
data$eosinophilp <- as.numeric(data$eosinophilp)
data$id2 <-as.character(data$id2)
data$city <- factor(data$city)

#fechas
data$datesxstart <- ymd(data$datesxstart)
data$enddate <- ymd(data$enddate)

#Aplico criterio de exclusion de ciudad y se pierde significancia de algunas categoricas
#imc se vuelve significativo. Revisar datos de procedencia.
#data1 <- data[!is.na(data$city),]

#Creo pam
data <- data %>% mutate (pam = (sbp + (2*dbp))/3)

#Creo edad categorica
data <- data %>% mutate(age_cat = cut(data$age, breaks= quantile(data$age), include.lowest = T))

data$age_cat <- factor(data$age_cat)

#Catgeorizo hb por cuartiles 
data <- data %>% mutate(hb_qcat = cut(data$hb, quantile(data$hb), include.lowest = T))
data$hb_qcat <- factor(data$hb_qcat)


#Creo  y Categoriza safi por cuartiles
data$safi <- (data$spo2/data$fio2)*100


data <- data %>% mutate(safi_cat = cut(data$safi, quantile(data$safi), include.lowest = T))
data$safi_cat <- factor(data$safi_cat)

#creo categorias imc

data <- data %>% mutate(imc_cat = cut(data$imc, breaks = c(15, 18.5, 25, 30, 35, 54), 
                                      labels = c("Bajo peso", "Normal", "Sobrepeso", "Obesidad GI", "Obesidad GII-III")))
data$imc_cat <- factor(data$imc_cat)

data$imc_cat <- relevel(data$imc_cat, ref = "Normal")



#to check linearity in the logit we would plot each
#of the coefficients versus the midpoint of the interval, using 0.0 as the coefficient
#for the first quartile


clvars<-col_classes(data)
c.vars<-names(clvars[clvars%in%c("numeric")])
d.vars<-names(clvars[clvars%in%"factor"])
vdep<-c("dccondition")

data <- data.frame(data)

#autores(data,d.vars,c.vars,vedep = vdep,fldr_name="autores_test")


#ggplot(data, aes(x= 1, fill=htn)) + geom_bar(position = "fill") + facet_grid()
#p_load(ggplot2)

#Multivariados logistico, supervivencia y gam

### Construcción de un modelo de regresión logística


#### Evaluación de la monotonía

#monotonia edad
data$status <- as.numeric(data$status)
data$status <- ifelse( data$status == 1, 0, 1)

#data <- data %>% mutate(safi_catcl = cut(safi, breaks = c(22, 360, 467), labels = c("<360", ">360")))
#data <- data %>% mutate(hb_cat = cut(hb, breaks=c(3, 14.7, 17, 24), labels = c("<15", "14.7-17", ">=15"))) 


hist(data$age,col="lightblue")
ggplot(data,aes(y=status,x=age))+
  geom_point()+geom_smooth()

hist(data$safi,col="lightblue")
ggplot(data,aes(y=status,x=safi))+
  geom_point()+geom_smooth()


table(data$safi_cat)



hist(data$hb,col="lightblue")
ggplot(data,aes(y=status,x=hb))+
  geom_point()+geom_smooth()


table(data$hb_cat)

#### Modelo de regresión logística con todas las variables significativas en el análisis bivariado (Nivel < 0.2)


modelo_t <- glm(dccondition ~ age + sex + smokstatus + htn + hyperlipid + copd + diabetes + 
                  enf_renal + hypothyroid + rr + temp + sbp +dbp+
                  safi + hb + wbc + rbc + anc +hct + mcv + platelet + lymphocytep + 
                  neutrophilp + lymphabs + monocytep + eosinophilp , family = "binomial", data = data) 

#t <- rpart(dccondition ~ age + sex + smokstatus + htn + hyperlipid + copd + diabetes + 
    #enf_renal + hypothyroid + rr + temp + sbp +dbp+
    #safi + hb + wbc + rbc + anc +hct + mcv + platelet + lymphocytep + 
    #neutrophilp + lymphabs + monocytep + eosinophilp , method  = "class", data = data)
#rpart.plot(t)
#summary(modelo_t)


#### Modelo de regresión logística con variables significativas en el análisis multivariado
modelo_s <- glm(dccondition ~ age + sex + htn + safi + hb , family = "binomial", data = data) 
summary(modelo_s)

            #Coef Modelo t          #Coef Modelo s 
#age            0.040009 age          0.042764  -6.4%
#sexMale        1.054928 sexMale      0.997605  5.7%
#htnYes         0.663444 htnYes       0.617365  7.46 %
#safi          -0.004123 safi        -0.007066  41%  -0.005743
#hb            -0.588438 hb          -0.121149  385% -0.099488  

betachx <- function(a,b) {
  print(((a-b)/b) *100)
}

#Evaluación del % de cambio en las variables sign del modelo 
#Age
betachx(0.040009,0.042764)
#sex
betachx(1.054928,0.997605)
#htn
betachx(0.663444, 0.617365)
#safi
betachx(-0.004123, -0.007066)
#hb
betachx(-0.588438,-0.121149)


View(data)
data[which(is.na(data)),]

#### Valoración del ajuste proporcionado por variables retiradas del modelo multivariado

#modelo_saj <- glm(dccondition ~ age + sex + htn  + safi + hb +  wbc , family = "binomial", data = data) 
summary(modelo_saj)

#vif(modelo_saj)
#exp(coef(modelo_saj))
#ggplot(data, aes(neutrophilp, status)) + geom_smooth


modelo_saj <- glm(dccondition ~ age + sex + htn  + safi + hb +wbc + lymphocytep, family = "binomial", data = data) 
summary(modelo_saj)

#lrtest(modelo_saj1, modelo_saj)

#safi
#betachx(-0.004123, -0.005662)
#hb
#betachx(-0.588438, -0.111541)

exp(coef(modelo_saj))

exp(confint(modelo_saj))

#Incorporo variables que no fueron sign en el bivariado pero ninguna es sign en el multi tampoco por lo que
#el siguiente esel modelo de efectos principales preliminar STEP 4
modelo_saj1 <- glm(dccondition ~ age + sex + htn  + safi + hb +wbc + lymphocytep, family = "binomial", data = data) 
summary(modelo_saj1)

names(dccondition)

#EXAMNE THE SCALE OF  CONTINUO COVARIATE IN THE LOGIT

ggplot(data, aes(age, status)) + geom_smooth(col = "black", size = 0.8)  + geom_jitter() + theme_classic()+
  xlab("Edad") +ylab("Condición de egreso") +scale_y_continuous(breaks =c(0,1),labels = c("Vivo 0","Muerto 1"))


ggplot(data, aes(safi, status)) + geom_smooth(col = "black", size = 0.8)  + geom_jitter() + theme_classic()+
  xlab("SpO2/FiO2") +ylab("Condición de egreso") +scale_y_continuous(breaks =c(0,1),labels = c("Vivo 0","Muerto 1")) 

ggplot(data, aes(x=hb, y=status)) + geom_smooth(col ="black", size = 0.7) +geom_jitter() + 
  theme_classic() + xlab("Hemoglobin") + ylab("Condición de egreso") 

ggplot(data, aes(x=wbc, y=status)) + geom_smooth(col ="black", size = 0.7) +geom_jitter() + 
  theme_classic() + xlab("Conteo absoluto de leucocitos") + ylab("Condición de egreso") 

ggplot(data, aes(x=lymphocytep, y=status)) + geom_smooth(col ="black", size = 0.7) +geom_jitter() + 
  theme_classic() + xlab("Porcentaje de linfocitos") + ylab("Condición de egreso") 

data[which(data$safi <100),]

#Multivariable fractional polynomial method for regression model

#data$porc_lymp<- round(data$lymphabs *100 / data$wbc, 2) 

#p_load(dplyr)
#p<-dplyr::select(data, c("id", "porc_lymp","lymphocytep"))

#p_load(openxlsx)
write.xlsx(p, "p%linfos.xlsx")


mfp(dccondition ~ age + sex + htn  + fp(safi, df = 2, select = 0.05) + fp(hb, df = 2, select = 0.05) +
      wbc + lymphocytep, family = "binomial", data = data, verbose = TRUE)

#model<-mfp(Surv(rfst, cens) ~ fp(age, df = 2, select = 0.05)+fp(prm,
            #df = 4, select = 0.05)+htreat+fp(tumsize)+tumgrad, family = cox,
           #data = GBSG,verbose=TRUE)


?plogis
?logit

#### Comparación de dos modelos que ajustan los coeficientes que variarion >10% 
#### Evaluación del ajuste proporcionado por variables excluidas del modelo multivariado
#### Evaluación de la interacción sobre el modelo de efectos principales
### Diagnóstico del modelo
#### Matriz de confusión
#### Odds ratio e intervalos de confianza de las variables del modelo
#### Factor de inflación de la varianza
#### Curva ROC
#### Test de Hosmer-Lemeshow

names(data)

modelo_t <- glm(dccondition ~ age + sex + htn + hyperlipid + copd + enf_renal + safi + hb + wbc, family = "binomial", data = data) 
summary(modelo_t)


modelo0<-glm(dccondition ~ age + sex + htn + safi + hb+ wbc + hb*safi, family = "binomial", data = data)
summary(modelo0)

exp(coefficients(modelo0))

ggplot(data, aes(x=age)) + geom_histogram()

ggplot(data, aes(x=hb)) + geom_histogram()

ggplot(data, aes(x=safi)) + geom_histogram()



modelo1<-glm(dccondition ~ age + sex + htn + safi + hb + wbc  , family = "binomial", data = data)


lrtest(modelo0, modelo1)

summary (modelo1)









gam_model <- gam(status ~ s(age)+ sex+ htn + s(wbc) + s(hb) + s(safi) + te(hb,safi), data=data, family = binomial, 
                 method ="REML")

summary(gam_model)

plot(gam_model, pages =1, trans = plogis)

