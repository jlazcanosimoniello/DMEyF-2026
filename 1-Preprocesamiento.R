
# leo el dataset
dataset <- fread("C:/Users/josef/OneDrive/Desktop/Maestria/DMEyF/Competencia/competencia_01_crudo.csv")

# calculo el periodo0 consecutivo
dsimple <- dataset[, list(
  "pos" = .I,
  numero_de_cliente,
  periodo0 = as.integer(foto_mes/100)*12 +  foto_mes%%100 ) ]


# ordeno
setorder( dsimple, numero_de_cliente, periodo0 )

# calculo topes
periodo_ultimo <- dsimple[, max(periodo0) ]
periodo_anteultimo <- periodo_ultimo - 1


# calculo los leads de orden 1 y 2
dsimple[, c("periodo1", "periodo2") :=
          shift(periodo0, n=1:2, fill=NA, type="lead"),  numero_de_cliente ]

# assign most common class values = "CONTINUA"
dsimple[ periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA" ]

# calculo BAJA+1
dsimple[ periodo0 < periodo_ultimo &
           ( is.na(periodo1) | periodo0 + 1 < periodo1 ),
         clase_ternaria := "BAJA+1" ]

# calculo BAJA+2
dsimple[ periodo0 < periodo_anteultimo & (periodo0+1 == periodo1 )
         & ( is.na(periodo2) | periodo0 + 2 < periodo2 ),
         clase_ternaria := "BAJA+2" ]


# pego el resultado en el dataset original 
setorder( dsimple, pos )
dataset[, clase_ternaria := dsimple$clase_ternaria ]


setorder( dataset, foto_mes, clase_ternaria, numero_de_cliente)

dataset[, .N, list(foto_mes, clase_ternaria)]


# Lags y Delta Lags ----

setorder(dataset, numero_de_cliente, foto_mes)

# todas las variables numéricas excepto IDs
campos_lag <- names(dataset)[sapply(dataset, is.numeric)]

campos_lag <- setdiff(
  campos_lag,
  c(
    "numero_de_cliente",
    "foto_mes"
  )
)

for(campo in campos_lag)
{
  cat("\nProcesando:", campo)
  
  # lag 1
  dataset[
    ,
    paste0(campo, "_lag1") :=
      shift(get(campo), 1L),
    by = numero_de_cliente
  ]
  
  # lag 2
  dataset[
    ,
    paste0(campo, "_lag2") :=
      shift(get(campo), 2L),
    by = numero_de_cliente
  ]
  
  # delta lag 1
  dataset[
    ,
    paste0(campo, "_delta1") :=
      get(campo) -
      get(paste0(campo, "_lag1"))
  ]
  
  # delta lag 2
  dataset[
    ,
    paste0(campo, "_delta2") :=
      get(paste0(campo, "_lag1")) -
      get(paste0(campo, "_lag2"))
  ]
}

fwrite(
  dataset,
  "C:/Users/josef/OneDrive/Desktop/Maestria/DMEyF/Competencia/competencia_01_lags.csv.gz"
)

cat("\nDataset guardado correctamente\n")