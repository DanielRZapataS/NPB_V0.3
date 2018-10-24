
datos <- fread(os.path.join(original_path, "Tenencia_CRM_201807.csv"))
dicDane <- get.path(dictionary_path, "BDEPT_CODIGO") %>% fread() 
threshold <- 100000
#' limpieza (Staging)

names(datos) <- tolower(names(datos))
#  meta data de base original 
meta_original <- data.table(variables = names(datos))
fwrite(meta_original, os.path.join(meta_path, "meta_origina.csv"))


# tratamiento de fechas 
datos[, birthdate := convertir_fechas_crm(birthdate)]
datos[, hire_dt := convertir_fechas_crm(hire_dt)]

features <- names(datos)[grepl("pr_*",names(datos))]
crm <- names(datos)[which(names(datos) %!in% c(features, "llave", "periodo"))]

# Calculo de variables 
datos[, month.id := as.numeric(factor(periodo))]
datos[, month := as.numeric(substr(periodo, 5,6))]
datos[, year := substr(periodo, 1,4)]
datos[, periodo := paste(year, month, "01", sep ="-")]
datos[, periodo := as.Date(periodo)]
datos[, month := factor(month.abb[month],levels = month.abb)]
timeVars <- c("periodo", "month.id", "month", "year")
# age 
datos[, age := round(interval(start = birthdate,
                                 end = periodo) / 
                          duration(num = 1, units = "years"))]
# antiguedad
datos[, antiguedad := round(interval(start = hire_dt,
                                        end = periodo) / 
                                 duration(num = 1, units = "months"))]

crm <- crm[which(crm %!in% c("birthdate", "hire_dt", "aa_vlr_ventas"))]
crm <- c(crm, "age", "antiguedad") 
# sapply(datos, class)
# sapply(datos,function(x)any(is.na(x)))


# eliminar NAs de hire_dt
datos <- datos[!is.na(hire_dt)]
# eliminar fechas de antiguedad incongruentes
datos <- datos[antiguedad >= -1]
# eliinar registros con llave NAS
datos <- datos[!is.na(llave)]

## variables caracter
char.cols <- names(datos)[sapply(datos,is.character)]
char.cols <- char.cols[which(char.cols %!in% c("cod_ciud_dir_ppal"))]
for (name in char.cols){
  print(sprintf("Unique values for %s:", name))
  print(unique(datos[[name]]))
}

# filling char var
datos[ sex %!in% c("M", "F"), sex := "UNKNOW"]
datos[nivel_educativo == "", nivel_educativo := "UNKNOW" ]
datos[mar_status == "", mar_status := "UNKNOW" ]
datos[aa_tipo_vivienda == "", aa_tipo_vivienda := "UNKNOW" ]
datos[aa_declara_renta == "", aa_declara_renta := "UNKNOW" ]

# codigo ciuu
datos[is.na(aa_cod_ciiu), aa_cod_ciiu := 10]
# codigo ocupacion
datos[is.na(aa_cod_ocupacion), aa_cod_ocupacion := ifelse(pr_nomina > 0, 1, 2)]

# # variables financieras
# finVars <- c("aa_vlr_activos","aa_vlr_egreso_mes",
#              "aa_vlr_ing_bru_mes","aa_vlr_pasivos")
# for(i in finVars){
#   plot <- datos[, mget(i)]
#   plot <- data.frame(plot)
#   par(mfrow=c(1,2))
#   hist(plot[, 1], main = i, xlab = "")
#   boxplot(plot[, 1], main = i, xlab = "")
# }
# 
# cbind(names(summary(datos$aa_vlr_ing_bru_mes)),
#       datos[, lapply(.SD, summary),
#             .SDcols = finVars])

# los valors extremos positivos de las financieras llenarlos con los valors del 99%

percentil_99 <-
  datos[, lapply(.SD, function(x)
    quantile(x, prob = 0.99, na.rm = TRUE)), .SDcols = finVars]
datos[aa_vlr_ing_bru_mes > percentil_99$aa_vlr_ing_bru_mes, aa_vlr_ing_bru_mes := percentil_99$aa_vlr_ing_bru_mes]
datos[aa_vlr_activos > percentil_99$aa_vlr_activos, aa_vlr_activos := percentil_99$aa_vlr_activos]
datos[aa_vlr_pasivos > percentil_99$aa_vlr_pasivos, aa_vlr_pasivos := percentil_99$aa_vlr_pasivos]
datos[aa_vlr_egreso_mes > percentil_99$aa_vlr_egreso_mes, aa_vlr_egreso_mes := percentil_99$aa_vlr_egreso_mes]

# llenar valores muy bajos o NAs

datos[, departamento := as.numeric(substr(cod_ciud_dir_ppal, 1,2))]
datos[is.na(departamento), departamento := 11]
wrong_departamentos <- unique(datos$departamento)[which( unique(datos$departamento) %!in% dicDane$codigo)]
datos[departamento %in% wrong_departamentos, departamento := 11]
aux.income <- datos[, lapply(.SD, unique), by = .(departamento, aa_estrato, llave),
                       .SDcols = finVars]
aux.income <- aux.income[,.(
  median.ingresos = median(aa_vlr_ing_bru_mes[aa_vlr_ing_bru_mes > threshold  ], na.rm = T),
  median.activos = median(aa_vlr_activos[aa_vlr_activos > threshold], na.rm = T),
  median.pasivos = median(aa_vlr_pasivos[aa_vlr_pasivos > threshold], na.rm = T),
  median.egresos = median(aa_vlr_egreso_mes[aa_vlr_egreso_mes > threshold], na.rm = T)), 
  by = .(departamento, aa_estrato)]


datos <- merge(datos, aux.income, by = c("departamento","aa_estrato"))
datos[is.na(aa_vlr_ing_bru_mes) | aa_vlr_ing_bru_mes < threshold, 
         aa_vlr_ing_bru_mes := median.ingresos ]
datos[is.na(aa_vlr_activos) | aa_vlr_activos < threshold, 
         aa_vlr_activos := median.activos ]
datos[is.na(aa_vlr_egreso_mes) | aa_vlr_egreso_mes < threshold, 
         aa_vlr_egreso_mes := median.egresos ]

# pasivos si pueden ser 0 
datos[is.na(aa_vlr_pasivos) | aa_vlr_pasivos < 0 , 
      aa_vlr_pasivos := median.pasivos ]

crm <- crm[which(crm %!in% c("cod_ciud_dir_ppal"))]
crm <- c(crm, "departamento") 

datos <- datos[, .SD, .SDcols = c("llave", timeVars, features, crm)]
setkey(datos, "llave")

fwrite(datos, os.path.join(staging_path, "staging_201807.csv"))
save(datos, file = os.path.join(staging_path, "staging_201807.RData"))



