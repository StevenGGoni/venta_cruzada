# Libraries ---------------------------------------------------------------

library(shiny)
library(DBI)
library(RPostgres)
library(tidyverse)
library(lubridate)
library(pool)
library(openxlsx)
library(DT)

# datos de conexión -------------------------------------------------------

db <- 'drmaxsalud'
host_db <- 'drmax.grupomontecristo.cloud'
db_port <- '5432'
db_user <- 'administrador'
db_password <- 'drm*2020*'

con <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = db,
  host = host_db,
  user = db_user,
  password = db_password)

# Queries -----------------------------------------------------------------

facturas <- DBI::dbGetQuery(con, 
                            "SELECT sucursal, factura, cliente, 
                            factura_credito, fecha, total, impuesto, 
                            fe_total_devolucion_iva, vendedor, usuario 
                            FROM factura")

factura_linea <- DBI::dbGetQuery(con, 
                                 "SELECT sucursal, factura, costo_total,
                                 familia, subfamilia, precio_unitario
                                 FROM factura_linea")

clientes <- DBI::dbGetQuery(con, 
                            "SELECT cliente,nombre, apellido1, apellido2, 
                            telefono1, telefono2, celular1, celular2, 
                            identificacion, cliente_corporativo
                            FROM cliente")

familia <- DBI::dbGetQuery(con, 
                           "SELECT * FROM familia")


# Limpieza y procesamiento de datos ---------------------------------------

# Cuentas por cobrar y comisiones -----------------------------------------


consolidado <- factura_linea %>% 
  dplyr::left_join(facturas, by = c("sucursal", "factura")) %>% 
  dplyr::left_join(clientes, by = c("cliente")) %>% 
  dplyr::left_join(familia, by = c("familia")) %>% 
  dplyr::mutate(nombre_cliente = stringr::str_c(nombre, apellido1, apellido2, 
                                                sep = " "))


ui <- fluidPage(

# Aplicacion --------------------------------------------------------------

  
  # Application title
  titlePanel("Dr Max. - Ventas cruzadas por cliente",
             windowTitle = "Dr Max S.A."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      
      # Escoger archivo
      
      selectInput("dataset", "Seleccione un archivo:", 
                  choices = c("Venta cruzada (Cantidad de artículos)",
                              "Venta cruzada (Colones)")),
      
      # Escoger rango de fechas
      
      dateRangeInput("rango", "Seleccione un rango de fechas:",
                     start = lubridate::today() - 30, end = lubridate::today(), 
                     min = "2018-04-13", max = "2025-12-31",
                     format = "yyyy/mm/dd", separator = " - "),
      
      # botón de descarga en excel
      
      downloadButton("descargar", "Descargar datos en MS Excel"),

    ),
    
    # visualización de reporte
    mainPanel(
      DT::DTOutput("table")
    )
  )
)

# server ------------------------------------------------------------------

server <- function(input, output) {
  
  ## APP
  
  datasetInput <- reactive({
    
    consolidado <- consolidado %>% 
      dplyr::filter(fecha >= input$rango[1] & fecha <= input$rango[2])
    
    group_cliente <- consolidado %>% 
      dplyr::group_by(nombre_cliente, descripcion) %>% 
      dplyr::summarise(n = n(),
                       tot = sum(precio_unitario)) %>% 
      dplyr::ungroup()
    
    cant_familia <- group_cliente %>% 
      dplyr::arrange(desc(n)) %>%
      tidyr::pivot_wider(id_cols = nombre_cliente, names_from = descripcion, 
                         values_from = n) %>% 
      dplyr::mutate(venta_cruzada = if_else(rowSums(!is.na(.)) -1 < 2, 
                                            "NO", "SI"))

    din_familia <- group_cliente %>% 
      dplyr::arrange(desc(n)) %>%
      dplyr::mutate(tot = scales::dollar(tot, prefix = "₡")) %>% 
      tidyr::pivot_wider(id_cols = nombre_cliente, names_from = descripcion, 
                         values_from = tot) %>% 
      dplyr::mutate(venta_cruzada = if_else(rowSums(!is.na(.)) -1 < 2, 
                                            "NO", "SI"))
    

    
    switch(input$dataset, 
           "Venta cruzada (Cantidad de artículos)" = cant_familia,
           "Venta cruzada (Colones)" = din_familia)
    
  })
  
  # mostrar tabla
  
  output$table <- DT::renderDT(datasetInput(), 
                               options = list(scrollX = TRUE,
                                              pageLength = 50),
                               rownames = FALSE)
  
  # descargar datos
  
  output$descargar <- downloadHandler(
    
    
    filename = function(){
      
      paste(input$dataset, "_", lubridate::today(),
            ".xlsx", sep = "")
      
    },
    
    content = function(file){
      openxlsx::write.xlsx(datasetInput(), file)
    }
    
  )
  
}

# Run the application  ----------------------------------------------------

shinyApp(ui = ui, server = server)