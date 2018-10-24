#' Crear master de cada modelo

# Segun el modelo se toman unas u otras variables 

# variables de la base que le sirven al modelo de tarjetas
# aa_vlr_ing_bru_mes
# aa_vlr_egreso_mes
# age
# antiguedad
# aa_vlr_activos
# aa_vlr_pasivos
# aa_estrato


var_interest <- c("aa_vlr_ing_bru_mes", "aa_vlr_egreso_mes",
"aa_vlr_activos", 

"bb_tipo_doc_pn", "aa_nit",
"birthdate", "sex", "nivel_educativo", "mar_status",
"aa_tipo_vivienda", "aa_estrato", "aa_cod_ciiu", 
"hire_dt", "bb_seg_comercial",  "aa_cod_ocupacion",
"cod_ciud_dir_ppal", "aa_vlr_activos",
"aa_vlr_ing_bru_mes", "aa_vlr_egreso_mes",
"aa_vlr_pasivos", "aa_vlr_ventas", "aa_declara_renta")

