require( "data.table" )

# 2. Optimización Hiperparámetros ----
## 2.1 Inicio ----
# Limpio el ambiente de R

format(Sys.time(), "%a %b %d %X %Y")
# limpio la memoria
rm(list=ls(all.names=TRUE)) # remove all objects
gc(full=TRUE, verbose=FALSE) # garbage collection

# 2.1 Carga de Librerías
### 2.1.1 cargo las librerias que necesito ----
# instalo paquetes
#install.packages( c("R.utils", "rlist" ) )
# Librerias  modernas  para la Bayesian Optimization
#install.packages( c("bbotk","mlr3", "mlr3mbo", "mlr3learners", "DiceKriging") )
# el LightGBM
#install.packages(c("lightgbm"))

library(data.table)
library(R.utils)
library(rlist)
library(bbotk)
library(mlr3)
library(mlr3mbo)
library(mlr3learners)
# library(paradox)
library(DiceKriging)
library(lightgbm)

### 2.1.2 Definición de Parametros ----
### Datos a cambiar 1 ----
# aqui debe cargar SU semilla primigenia
# recuerde cambiar el numero de experimento en cada corrida nueva

# cambiar por exp ----
N_Exp <- "04"

PARAM <- list()
PARAM$experimento <- N_Exp
PARAM$semilla_primigenia <-  221619   # La semilla primigenia

# training y future
#PARAM$training_pct <- 70
PARAM$train <- c(202103, 202104, 202106)
PARAM$train_final  <- c(202103,  202104,  202105,  202106)
PARAM$future <- c(202108)
PARAM$cortes <- seq(9000, 15000, by= 500)

# un undersampling de 0.1  toma solo el 10% de los CONTINUA
# undersampling de 1.0  implica tomar TODOS los datos

PARAM$trainingstrategy$undersampling <- 0.1

PARAM$hyperparametertuning$iteraciones <- 20 # iteraciones bayesianas

# Parametros LightGBM

# parametros fijos del LightGBM que se pisaran con la parte variable de la BO
PARAM$lgbm$param_fijos <-  list(
  boosting= "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective= "binary",
  metric= "average_precision",
  feature_pre_filter= FALSE,
  first_metric_only= FALSE,
  boost_from_average= TRUE,
  force_row_wise= TRUE,
  deterministic= TRUE,
  verbosity= -100,
  
  seed= PARAM$semilla_primigenia,
  
  max_depth= -1L,
  min_gain_to_split= 0,
  lambda_l1= 0.0,
  lambda_l2= 0.0,
  max_bin= 31L,
  
  bagging_fraction= 1.0,
  pos_bagging_fraction= 1.0,
  neg_bagging_fraction= 1.0,
  is_unbalance= FALSE,
  scale_pos_weight= 1.0,
  
  drop_rate= 0.1,
  max_drop= 50,
  skip_drop= 0.5,
  
  extra_trees= FALSE,
  
  early_stopping= 0,
  min_data_in_leaf= 0,     # fijo, esto es FUNDAMENTAL
  feature_fraction= 0.50,  # fijo
  learning_rate= 0.005,    # fijo
  
  num_iterations= 20000,     # se modificara
  num_leaves= 1024,          # se modificara
  min_sum_hessian_in_leaf= 1e-06  # se modificara
)

# Aqui se definen los hiperparámetros de LightGBM que participan de la Bayesian Optimization
# si es un numero entero debe ir makeIntegerParam
# si es un numero real (con decimales) debe ir makeNumericParam
# es muy importante leer cuales son un lower y upper permitidos y ademas razonables

# Aqui se cargan los bordes de los hiperparametros de la BO
#  es vital entender que se optimiza min_sum_hessian_in_leaf y que  min_data_in_leaf se deja fijo en CERO

PARAM$hypeparametertuning$hs <- ps(
  num_iterations= p_int(lower= 64L,  upper= 4096L),
  num_leaves= p_int(lower= 16L,  upper= 2048L),
  min_sum_hessian_in_leaf= p_dbl( lower=1e-06, upper=0.1)
)

# logueo al archivo BO_log.txt
loguear  <- function( preg, arch=NA, verbose=TRUE )
{
  t0 <- Sys.time()
  reg <- copy( preg )
  if( "feature_contri" %in%  names(reg) ) {
    reg$feature_contri <- reg$feature_contri[1]
  }
  archivo <- arch
  if( is.na(arch) ) archivo <- paste0( folder, substitute( reg), ext )
  
  
  if( !file.exists( archivo ) )
  {
    # Escribo los titulos
    linea  <- paste0( "fecha\t",
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  # escribo el registro
  linea  <- paste0( format(t0, "%Y%m%d.%H%M%S"),  "\t",     # la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  # grabo al archivo
  
  if( verbose )  cat( linea )   # imprimo por pantalla
}

setwd("C:/Users/jlazcano/Desktop/Maestria/DMEyF/Exp")
dir.create(PARAM$experimento, showWarnings=FALSE)
setwd( paste0("C:/Users/jlazcano/Desktop/Maestria/DMEyF/Exp/", PARAM$experimento))

dataset <- fread(
  "C:/Users/jlazcano/Desktop/Maestria/DMEyF/competencia_01_lags.csv.gz",
  stringsAsFactors = TRUE
)

dataset_train <- dataset[
  foto_mes %in% PARAM$train
]

dataset_train[
  ,
  clase01 := ifelse(
    clase_ternaria %in% c("BAJA+2","BAJA+1"),
    1L,
    0L
  )
]

# semilla
set.seed(
  PARAM$semilla_primigenia,  kind = "L'Ecuyer-CMRG")

dataset_train[, azar := runif(.N)]

dataset_train[, training  := 0L]

dataset_train[
  foto_mes %in% c(202103, 202104) &
    (
      azar <= PARAM$trainingstrategy$undersampling |
        clase_ternaria %in% c("BAJA+1", "BAJA+2")
    ),
  training  := 1L
]

dataset_train[
  foto_mes == 202106,
  training  := 2L
]


# los campos que se van a utilizar

campos_buenos <- setdiff(
  colnames(dataset_train),
  c(
    "clase_ternaria",
    "clase01",
    "azar",
    "training"
  )
)


dataset_train[
  ,
  .(
    registros = .N,
    positivos = sum(clase01)
  ),
  by = .(foto_mes, training)
]

### 2.1.4 Configuracion Bayesian Optimization ----

giter <- 0

if( file.exists("BO_log.txt") ){
  tb_BO <- fread("BO_log.txt")
  giter <- tb_BO[, max(iter)]
}

# En el argumento x llegan los parametros de la bayesiana

Estimar_lightgbm <- function(xs) {
  
  flush.console()
  giter <<- giter + 1
  
  # x pisa (o agrega) a param_fijos
  param_completo <- modifyList(PARAM$lgbm$param_fijos, xs)
  xx <- copy(param_completo)
  
  # entreno LightGBM
  modelo <- lgb.train(
    data= dtrain,
    valids= list( "val"=dvalidate),
    param= param_completo
  )
  
  
  # obtengo la ganancia
  xx$iter <- giter
  xx$metrica <- modelo$best_score
  
  loguear( xx, "BO_log.txt")
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")  # le reordeno a mlr3MBO
  
  return( list( metrica = xx$metrica) )
}

# dejo los datos de training en el formato que necesita LightGBM

dtrain <- lgb.Dataset(
  data= data.matrix(dataset_train[training == 1L, campos_buenos, with= FALSE]),
  label= dataset_train[training == 1L, clase01],
  free_raw_data= FALSE
)

nrow(dtrain)
ncol(dtrain)

# datos de validatin en el formato que necesita LightGBM
dvalidate <- lgb.Dataset(
  data= data.matrix(dataset_train[training == 2L, campos_buenos, with= FALSE]),
  label= dataset_train[training == 2L, clase01],
  free_raw_data= FALSE
)

nrow(dvalidate)
ncol(dvalidate)

dataset_train[
  ,
  .(
    registros = .N,
    positivos = sum(clase01)
  ),
  by = .(foto_mes, training)
]

# Aqui comienza la configuracion de la Bayesian Optimization

# en este archivo quedan la evolucion binaria de la BO
kbayesiana <- "bayesiana.RDATA"

funcion_optimizar <- Estimar_lightgbm # la funcion que voy a maximizar

objective <- ObjectiveRFun$new(
  fun= funcion_optimizar,
  domain= PARAM$hypeparametertuning$hs,
  codomain= ps(metrica = p_dbl(tags="maximize")),
  id= "BO_lightgbm"
)


instance <- OptimInstanceBatchSingleCrit$new(
  objective= objective,
  terminator=  trm("evals", n_evals = PARAM$hyperparametertuning$iteraciones)
)


surrogate <- srlrn(lrn("regr.km",
                       optim.method = "BFGS",
                       nugget.estim = TRUE,
                       control= list(trace = FALSE)))

acq_function  <- acqf("ei")                 # expected improvement

acq_optimizer  <- acqo(
  optimizer  = opt("random_search", batch_size= 1000),
  terminator = trm("evals", n_evals= 1000)
)

optimizer <- opt("mbo",
                 loop_function = bayesopt_ego,
                 surrogate= surrogate,
                 acq_function= acq_function,
                 acq_optimizer= acq_optimizer
)


### 2.1.5 Corrida Bayesuan Optimization ----

bayesiana_salida <- optimizer$optimize(instance)

## 2.2 Produccion ----
### Final Training ----
# Construyo el modelo final, que es uno solo, no hace ningun tipo de particion < training, validation, testing>]

setwd("C:/Users/jlazcano/Desktop/Maestria/DMEyF/Exp")
dir.create(PARAM$experimento, showWarnings= FALSE)
setwd( paste0("C:/Users/jlazcano/Desktop/Maestria/DMEyF/Exp/", PARAM$experimento ))

# Carga los mejores hiperparámetros de la optimización bayesiana
tb_bayesiana <- fread("BO_log.txt")
setorder( tb_bayesiana, -metrica )
PARAM$out$lgbm$mejores_hiperparametros <- as.list(tb_bayesiana[1])

# Preparamos clase01 en el dataset general
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+1", "BAJA+2"), 1L, 0L)]

# Entrenamos con el dataset final de training
dataset_train <- dataset[foto_mes %in% PARAM$train_final]

param_final <- modifyList(PARAM$lgbm$param_fijos,
                          PARAM$out$lgbm$mejores_hiperparametros)

dtrain_final <- lgb.Dataset(
  data= data.matrix(dataset_train[, campos_buenos, with= FALSE]),
  label= dataset_train[, clase01]
)

modelo_final <- lgb.train(
  data= dtrain_final,
  param= param_final
)


### 1. Búsqueda del Corte Óptimo en Validación (Ej: 202106) ----
# Usamos un mes donde SÍ conocemos la clase_ternaria para medir la ganancia real
dvalidate_eval <- dataset[foto_mes == 202106]

prediccion_val <- predict(
  modelo_final,
  data.matrix(dvalidate_eval[, campos_buenos, with= FALSE])
)

tb_val <- dvalidate_eval[, list(numero_de_cliente, foto_mes, clase_ternaria)]
tb_val[, prob := prediccion_val ]
tb_val[, gan := ifelse(clase_ternaria == "BAJA+2", 1072500, -27500)]

setorder(tb_val, -prob)
tb_val[, ganancia_acumulada := cumsum(gan)]

# Buscamos dinámicamente qué cantidad de envíos maximiza la ganancia
mejor_ganancia <- -Inf
mejor_envios <- 0

for (envios in PARAM$cortes) {
  ganancia <- tb_val[1:envios, sum(gan)]
  cat("Corte:", envios, "\t Ganancia en Validacion:", ganancia, "\n")
  
  if (ganancia > mejor_ganancia) {
    mejor_ganancia <- ganancia
    mejor_envios <- envios
  }
}

cat(">>> MEJOR CORTE ENCONTRADO:", mejor_envios, " con una ganancia de: ", mejor_ganancia, "\n")


### 2. Predicción en el Futuro y Generación de la Entrega ----
dfuture <- dataset[foto_mes %in% PARAM$future]

prediccion_future <- predict(
  modelo_final,
  data.matrix(dfuture[, campos_buenos, with= FALSE])
)

tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes)]
tb_prediccion[, prob := prediccion_future ]

# Ordenamos las probabilidades del futuro de mayor a menor
setorder(tb_prediccion, -prob)

# Tomamos exactamente la cantidad de envíos óptima encontrada en validación
entrega <- tb_prediccion[
  1:mejor_envios,
  .(numero_de_cliente)
]

options(scipen = 999)

# 1. Crear un data.table con todos los cortes a evaluar
tb_resultados_cortes <- data.table(envios = PARAM$cortes)

# 2. Calcular la ganancia para cada corte de forma eficiente
tb_resultados_cortes[, ganancia := sapply(envios, function(x) {
  tb_val[1:x, sum(gan)]
})]

print(tb_resultados_cortes)


fwrite(
  entrega,
  file = paste0("entrega_", N_Exp, ".csv"),  
  col.names = FALSE
)

cat(">>> Archivo de entrega generado con éxito: entrega_", N_Exp, ".csv con", mejor_envios, "clientes.\n")
format(Sys.time(), "%a %b %d %X %Y")

