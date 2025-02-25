### FUNCTIES VOOR UNIFORME LAYOUT & EXPORT VAN GRAFIEKEN
f_labs_totaal <- function(title) {
  labs(title = str_c(title), 
       y = "Aantal watervogels", 
       x = element_blank())}  

f_labs_aandeel <- function(title) {
  labs(title = str_c(title), 
       y = "Percentage", 
       x = element_blank())}  

f_graph_theme <- function(){
  theme(plot.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))}

f_save_graph <- function(plotname){
  ggsave(
    plotname,
    filename = paste(deparse(substitute(plotname)), "(3).png", sep = ""), 
    path = "./graphs/Grafieken Algemeen",
    width = 15, height = 10, units = "cm", dpi = 300)}

f_save_wide_graph <- function(plotname){
  ggsave(
    plotname,
    filename = paste(deparse(substitute(plotname)), "(3).png", sep = ""), 
    path = "./graphs/Grafieken Algemeen",
    width = 18, height = 10, units = "cm", dpi = 300)}

f_save_graph_species <- function(plotname){
  ggsave(
    plotname,
    filename = paste(deparse(substitute(plotname)), "_", Soort, "(3).png", sep = ""), 
    path = paste("./graphs/Grafieken ", Soort, sep = ""),
    width = 15, height = 10, units = "cm", dpi = 300)}

f_save_wide_graph_species <- function(plotname){
  ggsave(
    plotname,
    filename = paste(deparse(substitute(plotname)), "_", Soort, "(3).png", sep = ""), 
    path = paste("./graphs/Grafieken ", Soort, sep = ""),
    width = 18, height = 10, units = "cm", dpi = 300)}