# ### 
# staging <- os.path.join(staging_path, "staging_201810.rds") %>% readRDS()
# hist(staging$age)
# 
# tabla <- data.table( breaks = a$breaks[-1], 
#                      counts = a$counts, 
#                      density = a$density,
#                      percentage = (a$counts/sum(a$counts))*100)
# fwrite(tabla, "tabla.csv")
# 



master <- os.path.join(master_path, "master_201810.rds") %>% readRDS()

products <- names(master)[grepl("pr_",names(master))]
products <- products[1:9]

plot_pr <- master[, lapply(.SD, sum), by = periodo, .SDcols = products ]
names(plot_pr)[-1] <- sapply(strsplit(products, "_"), "[[", 2)  

plot_pr <- melt(plot_pr, id.vars = c("periodo"))
p1 <- plot_pr %>% ggplot(aes(
  x = periodo,
  y = value,
  color = variable,
  linetype = variable
)) +
  geom_line(size = 0.8, alpha = 0.7) +
  ylab("Valor") +
  theme_minimal()

ggsave(filename = "Plots/evolucion.png", p1)

plot_pr <-
  plot_pr[periodo == "2018-10-01"][order(-value)][, variable := factor(variable, ordered = T)]

p2 <- plot_pr %>% ggplot(aes(
  x = variable,
  y = value,
  fill= variable
)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits = plot_pr$variable) +
  xlab("Productos") +
  ylab("Total de productos") + 
  geom_text(aes(label=value), vjust=1, color="white", size=3.5)+
  theme_minimal()
ggsave(filename = "Plots/tenecia_octubre.png", p2) 




master[ (pr_nomina > 0 | pr_ahorros > 0 | pr_cdt > 0) &(  
  pr_tcredito == 0 & pr_libranza == 0 & pr_libredestino == 0 & pr_crediservice == 0 & pr_vehiculo == 0 & pr_vivienda == 0)    , .N, by = periodo]


###
# staging <- os.path.join(staging_path, "staging_201810.rds") %>% readRDS()
# png(file = "vehiculo.png")
# a <- hist(staging[pr_vehiculo > 0, age])
# dev.off()
# a
# tabla <- data.table( breaks = a$breaks[-1],
#                      counts = a$counts,
#                      density = a$density,
#                      percentage = (a$counts/sum(a$counts))*100)
# fwrite(tabla, "tabla_vehiculo.csv")

# 

uplift_table <- data.table(producto = c("Crediservice", "Libredestino", "Nomina",
                                        "T credito", "Ahorros", "Libranza", 
                                        "Vehiculo", "CDT"),
                           up = c(8.54, 7.43, 6.73, 6.19, 5, 9.13, 8.25, 7.03))
uplift_table <- uplift_table[order(-up)]
uplift_table[, up := factor(up, ordered = T)]

p3 <- uplift_table %>% ggplot(aes(
  x = producto,
  y = up,
  fill= producto
  
)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits = uplift_table$producto) +
  xlab("Productos") +
  ylab("Uplift") + 
  geom_text(aes(label=up), vjust=1, color="white", size=3.5)+
  theme_minimal()
ggsave(filename = "Plots/uplift.png", p3) 
