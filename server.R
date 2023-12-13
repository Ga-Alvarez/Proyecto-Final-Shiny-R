server <- function(input, output, session) {
  
  datos_policiales <- readxl::read_excel("datos/datos_oij.xlsx")
  coordenadas <- readr::read_csv("datos/Coordenadas por cantón.csv")
  
  observe({ 
    updateSelectInput(session, "provincia", choices = unique(datos_policiales$provincia))
    updateSelectInput(session, "delito", choices = NULL)
    updateSelectInput(session, "prov", choices = unique(datos_policiales$provincia))
    updateSelectInput(session, "provin", choices = unique(datos_policiales$provincia))
    updateSelectInput(session, "año", choices = unique(datos_policiales$anyo))
    updateSelectInput(session, "provi", choices = unique(datos_policiales$provincia))
    updateSelectInput(session, "anio", choices = unique(datos_policiales$anyo))
    updateSelectInput(session, "prov1", choices = unique(datos_policiales$provincia))
    updateSelectInput(session, "anio1", choices = unique(datos_policiales$anyo))
    updateSelectInput(session,"canton",choices = unique(coordenadas$Cantón))
    updateSelectInput(session,"anyo",choices = unique(datos_policiales$anyo))
    updateSelectInput(session,"canton1",choices = unique(datos_policiales$Cantón))
    updateSelectInput(session,"canton2",choices = unique(datos_policiales$Cantón))
    
  })
  
  data <- reactive({
    datos_filtrados <- datos_policiales |> 
      filter(canton == input$canton, anyo == input$anyo)
    
    tabla_subdelitos <- datos_filtrados |> 
      group_by(sub_delito) |> 
      summarise(cantidad = n()) |> 
      arrange(desc(cantidad))
    
    return(tabla_subdelitos)
  })
  
  output$download1 <- downloadHandler(
    filename = function() {
      paste0("DatosPolicialesResumen", ".csv")
    },
    
    content = function(file) {
      write.csv(data(), file)
      
    }
  )
  shinyalert(title = "Carga de datos exitosa",
             text = "Datos cargados correctamente.",
             type = "success",
             confirmButtonText = "Cerrar")
  
  
  observeEvent(input$provincia, {
    
    opciones_delito <- datos_policiales |> 
      filter(provincia == input$provincia) |> 
      pull(delito) |> 
      unique()
    
    updateSelectInput(session, "delito", choices = opciones_delito)
  })
  
  observeEvent(input$provincia, {
    opciones_canton <- coordenadas |> 
      filter(Provincia == input$provincia) |> 
      pull(Cantón) |> 
      unique()
    
    updateSelectInput(session,"canton",choices = opciones_canton)
  })
  
  años_delitos <- reactive({
    datos_policiales |> 
      filter(provincia == input$prov, delito == input$delito) |> 
      group_by(anyo) |> 
      summarise(delito = n())
  })
  
  output$lineas <- renderPlotly({
    grafico_lineas <- ggplot(años_delitos(), aes(x = anyo, y = delito)) +
      geom_line(color = "blue") +
      geom_text(aes(label = scales::percent(delito / sum(delito))), 
                vjust = -0.5, hjust = -0.5, size = 3) + 
      scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
      labs(title = "Gráfico de Líneas de Delitos por Año",
           x = "Año",
           y = "Número de Delitos")
    
    ggplotly(grafico_lineas)|> 
      config(displayModeBar = F)
  })
  
  output$subdelitos <- renderDataTable({
    req(input$canton, input$anyo)
    
    datos_filtrados <- datos_policiales |> 
      filter(canton == input$canton, anyo == input$anyo)
    
    tabla_subdelitos <- datos_filtrados |> 
      group_by(sub_delito) |> 
      summarise(cantidad = n()) |> 
      arrange(desc(cantidad))
    
    return(tabla_subdelitos)
  })
  
  #Top cantones
  #gráfico de barras de delitos por provincia y mapa
  datos_provincia <- reactive({
    datos_policiales |> 
      filter(provincia == input$prov)
  })
  
  
  output$top_cantones <- renderPlotly({
    grafico <- datos_provincia() |> 
      group_by(canton) |> 
      summarise(Total = n()) |> 
      arrange(desc(Total)) |> 
      head(5) |> 
      ggplot(aes(reorder(canton, -Total), y = Total) ) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(Total/sum(Total))), 
                position = position_stack(vjust = 0.5), 
                color = "white", 
                size = 3) +
      scale_fill_manual(values = c("gold", "blue"))+
      labs(title = "Cinco cantones con mayor cantidad de delitos",
           x = "Cantón",
           y = "Cantidad de delitos")
    
    ggplotly(grafico)|> 
      config(displayModeBar = F)
  })
  
  
  output$provincia_mapa <- renderLeaflet({
    if (!is.null(input$prov)) {
      provincia_seleccionada <- coordenadas |> 
        filter(Provincia == input$prov) 
      
      mi_mapa <- leaflet() |> 
        addTiles() |> 
        addMarkers(data = provincia_seleccionada,
                   lng = ~as.numeric(Longitud),
                   lat = ~as.numeric(Latitud),
                   popup = ~(Cantón)) 
      mi_mapa
    }
  })
  
  # gráfico barras edad
  edad_delitos <- reactive({
    datos_policiales |> 
      filter(provincia == input$provin, anyo == input$año) 
  })
  
  output$barra1 <- renderPlotly({
    colores <- c("#D4AF37", "#e5c24c", "#0303b5", "#4040fb", "#1466c3")
    
    edad_delitos_freq <- edad_delitos() |> 
      count(delito) |> 
      arrange(n)
    
    grafico_barra1 <- ggplot(data = edad_delitos()) +
      geom_bar(mapping = aes(y = factor(delito, levels = edad_delitos_freq$delito), fill = edad), position = "stack", width = 0.5) +
      scale_fill_manual(values = colores) +
      labs(title = "Delitos según edad, provincia y año",
           x = "Conteo de Delitos",
           y = "") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    
    ggplotly(grafico_barra1) |> 
      config(displayModeBar = F)
  })
  
  canton_delitos1 <- reactive({
    datos_policiales |> 
      filter(provincia == input$provin, anyo == input$año, canton == input$canton1) 
  })
  
  output$barra1.2 <- renderPlotly({
    colores <- c("#D4AF37", "#e5c24c", "#0303b5", "#4040fb", "#1466c3")
    
    canton_delitos1_freq <- canton_delitos1() |> 
      count(delito) |> 
      arrange(n)
    
    grafico_barra1.2 <- ggplot(data = canton_delitos1()) +
      geom_bar(mapping = aes(y = factor(delito, levels = canton_delitos1_freq$delito), fill = edad), position = "stack", width = 0.5) +
      scale_fill_manual(values = colores) +
      labs(title = "Desglose de delitos según Cantón",
           x = "Conteo de Delitos",
           y = "") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    ggplotly(grafico_barra1.2) |> 
      config(displayModeBar = F)
  })
  
  observeEvent(input$provin, {
    cantones <- datos_policiales |> 
      filter(provincia == input$provin) |> 
      pull(canton) |> 
      unique()
    
    updateSelectInput(session, "canton1", choices = cantones)
  })
  
  
  
  #gráfico barras género
  genero_delitos <- reactive({
    datos_policiales |> 
      filter(provincia == input$provi, anyo == input$anio) 
  })
  
  output$barra2 <- renderPlotly({
    colores <- c("#D4AF37","#e5c24c","#0303b5","#4040fb","#1466c3")
    
    genero_delitos_count <- genero_delitos() |> 
      count(delito) |> 
      arrange(n)
    
    grafico_barra2 <- ggplot(data = genero_delitos()) +
      geom_bar(mapping = aes(y = factor(delito, levels = genero_delitos_count$delito), fill = genero), position = "stack", width = 0.5) +
      scale_fill_manual(values = colores) +
      labs(title = "Delitos según género, provincia y año",
           x = "Conteo de Delitos",
           y = "") +
      #theme(legend.position = "top") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    ggplotly(grafico_barra2) |> 
      #layout(legend = list(orientation = "h", x = 0.47, y = 0.96)) %>%
      config(displayModeBar = F)
  })
  
  canton_delitos2 <- reactive({
    datos_policiales |> 
      filter(provincia == input$provi, anyo == input$anio, canton == input$canton2) 
  })
  
  output$barra2.2 <- renderPlotly({
    colores <- c("#D4AF37","#e5c24c","#0303b5","#4040fb","#1466c3")
    
    canton_delitos2_count <- canton_delitos2() |> 
      count(delito) |> 
      arrange(n)
    
    grafico_barra2.2 <- ggplot(data = canton_delitos2()) +
      geom_bar(mapping = aes(y = factor(delito, levels = canton_delitos2_count$delito), fill = genero), position = "stack", width = 0.5) +
      scale_fill_manual(values = colores) +
      labs(title = "Desglose de delitos según Cantón",
           x = "Conteo de Delitos",
           y = "") +
      #theme(legend.position = "top", legend.justification = 0.5, legend.box.just = 0.5)  +
      theme(plot.title = element_text(hjust = 0.5))+
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    ggplotly(grafico_barra2.2) |> 
      #layout(legend = list(orientation = "h", x = 0.47, y = 0.96)) |> 
      config(displayModeBar = F)
  })
  
  observeEvent(input$provi, {
    cantones <- datos_policiales |> 
      filter(provincia == input$provi) |> 
      pull(canton) |> 
      unique()
    
    updateSelectInput(session, "canton2", choices = cantones)
  })
  
}
