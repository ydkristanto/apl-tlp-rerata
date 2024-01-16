# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(gridExtra)

# Membuat UI ----

ui <- fluidPage(
  # Judul ----
  titlePanel("Teorema Limit Pusat untuk Rerata", windowTitle = "TLP untuk Rerata"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        # Memilih distribusi ----
        radioButtons("dist", "Distribusi populasi:",
                     c("Normal" = "rnorm",
                       "Seragam" = "runif",
                       "Condong ke kanan" = "rlnorm",
                       "Condong ke kiri" = "rbeta"),
                     selected = "rnorm"),
        hr(),
        
        # Parameter distribusi ----
        uiOutput("mu"),
        uiOutput("sd"),
        uiOutput("minmax"),
        uiOutput("skew"),
        
        # Memilih ukuran sampel ----
        sliderInput("n",
                    "Ukuran sampel:", 
                    value = 30,
                    min = 2,
                    max = 500),
        br(),
        
        # Banyak sampel ----
        sliderInput("k",
                    "Banyaknya sampel:",
                    value = 200,
                    min = 10,
                    max = 1000)
      ),
      
      # Teks informasi ---- 
      helpText(a(href="https://github.com/ydkristanto/apl-tlp-rerata", target="_blank", "Lihat kode sumber"))
      
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # Tab pertama ----
        tabPanel(
          title = "Distribusi Populasi",
          # Plot populasi ----
          plotOutput("pop.dist", height = "500px"),
          br()
        ),
        # Tab kedua ----
        tabPanel(
          title = "Beberapa Sampel",
          # Plot-plot sampel ----
          br(),
          plotOutput("sample.dist", height = "600px"),
          #  Teks banyaknya sampel ----
          div(h3(textOutput("num.samples")), align = "center"),
          br()
        ),
        # Tab ketiga ----
        tabPanel(
          title = "Distribusi Sampling",
          
          fluidRow(
            column(width = 12,
                   br(),
                   # Plot sampling ----
                   plotOutput("sampling.dist"),
                   # Deskripsi sampling ----
                   div(textOutput("sampling.descr", inline = TRUE),
                       align = "center"))
          ),
          
          fluidRow(
            column(width = 7,
                   br(), br(),
                   # Deskripsi TLP ----
                   div(textOutput("CLT.descr"),
                       align = "justify")),
            column(width = 5,
                   br(),
                   # Plot populasi ----
                   plotOutput("pop.dist.two",
                              width = "85%", height = "200px"))
          )
          
        ),
        
        tabPanel(
          title = "Informasi",
          # Informasi aplikasi ----
          h2("Informasi Aplikasi"),
          div(p("Aplikasi Shiny ini bertujuan untuk mendeskripsikan Teorema
          Limit Pusat untuk rerata. Berdasarkan teorema ini, ketika
          ukuran sampelnya besar, distribusi sampling reratanya mendekati
          normal dengan rerata yang sama dengan rerata populasinya dan 
          simpangan baku yang sama dengan simpangan baku populasi dibagi 
          dengan akar kuadrat dari ukuran sampel tersebut. Hal ini juga 
          berlaku ketika ukuran sampelnya kecil tetapi populasinya 
          berdistribusi normal. Akan tetapi, teorema ini tidak menjamin 
                kenormalam distribusi samplingnya ketika ukuran sampelnya 
                kecil dan populasi tidak berdistribusi normal."),
              align = "justify"),
          div(p("Aplikasi ini merupakan bentuk modifikasi (atau 
          lebih tepatnya terjemahan) dari",
                a("aplikasi serupa", href = 
          "https://openintro.shinyapps.io/CLT_mean/", target = "_blank"),
          "yang dikembangkan oleh Mine Çetinkaya-Rundel. 
          Pengembang aplikasi ini adalah",
          a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto/", target = "_blank"),
          "seorang 
          dosen dan peneliti di program studi ", a("Pendidikan Matematika,", href = "https://usd.ac.id/s1pmat", target = "_blank"), a("Universitas Sanata Dharma,", href = "https://www.usd.ac.id/", target = "_blank"), "Yogyakarta."), align = "justify"),
          hr()
        )
        
      )
    )
  )
)

# Mendefinisikan fungsi peladen ----

seed <- as.numeric(Sys.time())

server <- function(input, output, session) {
  
  # Slider rerata untuk distribusi normal ----
  output$mu = renderUI(
    {
      if (input$dist == "rnorm")
      {
        sliderInput("mu",
                    "Rerata:",
                    value = 0,
                    min = -40,
                    max = 50)
      }
    })
  
  # Slider simpangan baku untuk distribusi normal ----
  output$sd = renderUI(
    {
      if (input$dist == "rnorm")
      {
        sliderInput("sd",
                    "Simpangan baku:",
                    value = 20,
                    min = 1,
                    max = 30)
      }
    })
  
  # Slider minmaks untuk distribusi seragam ----
  output$minmax = renderUI(
    {
      
      if (input$dist == "runif")
      {
        sliderInput("minmax",
                    "Batas bawah dan batas atas",
                    value = c(5, 15),
                    min = 0,
                    max = 20)
      }
    })
  
  # Memastikan jangkauan untuk distribusi seragam != 0 ----
  observeEvent(input$minmax, {
    
    req(input$minmax)
    
    if (input$minmax[1] == input$minmax[2]){
      if (input$minmax[1] == 0){
        updateSliderInput(session, "minmax", value = c(0, 1))
      } else if (input$minmax[2] == 20){
        updateSliderInput(session, "minmax", value = c(19, 20))
      } else {
        updateSliderInput(session, "minmax", value = c(input$minmax[2], input$minmax[2] + 1))
      }
    }
  })
  
  # Slider kecondongan untuk rlnorm dan rbeta ----
  output$skew = renderUI(
    {
      
      if (input$dist == "rlnorm" | input$dist == "rbeta"){
        selectInput(inputId = "skew",
                    label = "Kecondongan:",
                    choices = c("Rendah" = "low",
                                "Sedang" = "med",
                                "Tinggi" = "high"),
                    selected = "low")
      }
    })
  
  # Membuat sampel-sampel random ----
  rand_draw <- function(dist, n, mu, sd, min, max, skew){
    
    vals = NULL
    
    if (dist == "rbeta"){
      req(skew)
      if (skew == "low"){
        vals = do.call(dist, list(n = n, shape1 = 5, shape2 = 2))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n = n, shape1 = 5, shape2 = 1.5))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n = n, shape1 = 5, shape2 = 1)) 
      }
    }
    
    else if (dist == "rnorm"){
      req(mu, sd)
      vals = do.call(dist, list(n = n, mean = mu, sd = sd))
    }
    
    else if (dist == "rlnorm"){
      req(skew)
      if (skew == "low"){
        vals = do.call(dist, list(n = n, meanlog = 0, sdlog = .25))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n = n, meanlog = 0, sdlog =.5))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n = n, meanlog = 0, sdlog = 1))
      }
    }
    
    else if (dist == "runif"){
      req(min, max)
      vals = do.call(dist, list( n = n, min = min, max = max))
    }
    return(vals)
  }
  
  rep_rand_draw = repeatable(rand_draw)
  
  # Mendefinisikan beberapa variabel reaktif lainnya ----
  parent = reactive({
    
    n_sample = 1e5
    
    return(rep_rand_draw(input$dist, n_sample, input$mu, input$sd,
                         input$minmax[1], input$minmax[2], input$skew))
  })
  
  samples = reactive({
    
    pop = parent()
    n = input$n
    k = input$k
    
    return(replicate(k, sample(pop, n, replace=TRUE)))
  })
  
  u_min = reactive({
    req(input$minmax)
    return(input$minmax[1])
  })
  
  u_max = reactive({
    req(input$minmax)
    return(input$minmax[2])
  })
  
  # plot 1 a) ----
  output$pop.dist = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "Distribusi populasi: Normal",
                      rlnorm = "Distribusi populasi: Condong ke kanan",
                      rbeta = "Distribusi populasi: Condong ke kiri",
                      runif = "Distribusi populasi: Seragam")
    
    pop = parent()
    
    m_pop =  round(mean(pop), 2)
    sd_pop = round(sd(pop), 2)
    
    pop = tibble(samples = pop)
    pdens = density(pop$samples)
    
    x_range = max(pop$samples) - min(pop$samples)
    y_pos = max(pdens$y) - 0.2*max(pdens$y)
    
    if (input$dist == "rnorm"){
      
      req(input$mu)
      mu = input$mu
      
      x_pos = ifelse(mu > 0, min(-100, min(pop$samples)) + 20,
                     max(100, max(pop$samples)) - 20)
      
      ggplot(data = pop, aes(x = samples, y = after_stat(density))) + 
        geom_histogram(bins = 45, color = "white") +
        # geom_density() + draws a weird baseline. using stat_density() instead.
        stat_density(geom = "line", size = 2) +
        scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("rerata x", "=", bquote(.(m_pop)),
                               "\n", "simpangan baku x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_bw(base_size = 19) + # better than doing title sizes inside theme().
        theme(plot.title = element_text(hjust = 0.5))
      
    } else if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        "  " # this is to temporarily prevent graph from displaying while 
        # observeEvent is fixing the range.
      } else {
        
        x_pos = max(pop$samples) - 0.1*x_range
        
        ggplot(data = pop, aes(x = samples, y = ..density..)) +
          geom_histogram(bins = 45, color = "white") +
          stat_density(geom = "line", size = 2) +
          scale_y_continuous(expand = expand_scale(mult = c(0, .3))) +
          labs(title = distname, x = "x") +
          annotate("text", x = x_pos, y = y_pos + 0.5*max(pdens$y),
                   label = paste("rerata x", "=", bquote(.(m_pop)),
                                 "\n", "simpangan baku x", "=", bquote(.(sd_pop))),
                   color = "black", size = 5) +
          theme_bw(base_size = 19) +
          theme(plot.title = element_text(hjust = 0.5))}
      
    } else if (input$dist == "rlnorm"){
      
      x_pos = max(pop$samples) - 0.1*x_range
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white") +
        stat_density(geom = "line", size = 2) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("rerata x", "=", bquote(.(m_pop)), 
                               "\n", "simpangan baku x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_bw(base_size = 19) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else if (input$dist == "rbeta"){
      
      x_pos = min(pop$samples) + 0.1*x_range
      
      ggplot(data = pop, aes(x = samples, y = ..density..)) + 
        geom_histogram(bins = 45, color = "white") +
        stat_density(geom = "line", size = 2) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos, 
                 label = paste("rerata x", "=", bquote(.(m_pop)), 
                               "\n", "simpangan baku x", "=", bquote(.(sd_pop))),
                 color = "black", size = 5) +
        theme_bw(base_size = 19) +
        theme(plot.title = element_text(hjust = 0.5))
      
    }
  })
  
  # plot 1 b) ----
  # Ini adalah plot populasi di tab ketiga
  
  output$pop.dist.two = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "Population distribution: Normal",
                      rlnorm = "Population distribution: Right skewed",
                      rbeta = "Population distribution: Left skewed",
                      runif = "Population distribution: Uniform")
    
    pop = parent()
    
    m_pop =  round(mean(pop),2)
    sd_pop = round(sd(pop),2)
    
    pop = tibble(samples = pop)
    pdens = density(pop$samples)
    
    x_range = max(pop$samples) - min(pop$samples)
    y_pos = max(pdens$y) - 0.2*max(pdens$y)
    
    if (input$dist == "rnorm"){
      
      req(input$mu)
      mu = input$mu
      
      x_pos = ifelse(mu > 0, min(-100, min(pop$samples)) + 27,
                     max(100, max(pop$samples)) - 27)
      
      ggplot(data = pop, aes(x = samples, y = after_stat(density))) + 
        geom_histogram(bins = 45, color = "white") +
        stat_density(geom="line", size = 1) +
        scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("rerata x", "=", bquote(.(m_pop)),
                               "\n", "simpangan baku x", "=", bquote(.(sd_pop))),
                 color = "black", size = 3) +
        theme_light(base_size = 10) +
        theme(plot.title = element_text(hjust = 0.5))
      
    } else if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        " "
      } else {
        
        x_pos = max(pop$samples) - 0.1*x_range
        
        ggplot(data = pop, aes(x = samples, y = after_stat(density))) +
          geom_histogram(bins = 45, color = "white") +
          stat_density(geom = "line", size = 1) +
          scale_y_continuous(expand = expand_scale(mult = c(0, .3))) +
          labs(title = distname, x = "x") +
          annotate("text", x = x_pos, y = y_pos + 0.5*max(pdens$y),
                   label = paste("rerata x", "=", bquote(.(m_pop)),
                                 "\n", "simpangan baku x", "=", bquote(.(sd_pop))),
                   color = "black", size = 3) +
          theme_light(base_size = 10) +
          theme(plot.title = element_text(hjust = 0.5))}
      
    } else if (input$dist == "rlnorm"){
      
      x_pos = max(pop$samples) - 0.1*x_range
      
      ggplot(data = pop, aes(x = samples, y = after_stat(density))) + 
        geom_histogram(bins = 45, color = "white") +
        stat_density(geom = "line", size = 1) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos,
                 label = paste("rerata x", "=", bquote(.(m_pop)), 
                               "\n", "simpangan baku x", "=", bquote(.(sd_pop))),
                 color = "black", size = 3) +
        theme_light(base_size = 10) +
        theme(plot.title = element_text(hjust = 0.5))
      
    } else if (input$dist == "rbeta"){
      
      x_pos = min(pop$samples) + 0.1*x_range
      
      ggplot(data = pop, aes(x = samples, y = after_stat(density))) + 
        geom_histogram(bins = 45, color = "white") +
        stat_density(geom = "line", size = 1) +
        labs(title = distname, x = "x") +
        annotate("text", x = x_pos, y = y_pos, 
                 label = paste("rerata x", "=", bquote(.(m_pop)), 
                               "\n", "simpangan baku x", "=",
                               bquote(.(sd_pop))),
                 color = "black", size = 3) +
        theme_light(base_size = 10) +
        theme(plot.title = element_text(hjust = 0.5))
      
    }
  })
  
  # plot 2 ----
  # Ini adalah plot sampel-sampel
  output$sample.dist = renderPlot({
    
    y = samples()
    x = samples() %>% as_tibble()
    
    plots = list(rep(NA, 8))
    
    for(i in 1:8){
      
      mean = round(mean(y[,i]), 2)
      sd = round(sd(y[,i]), 2)
      
      x_range = max(y[,i]) - min(y[,i])
      pdens = density(y[,i])
      
      x_pos = ifelse(input$dist == "rbeta", min(y[,i]) + 0.1*x_range, 
                     max(y[,i]) - 0.1*x_range)
      
      plots[[i]] = ggplot(x, aes_string(x = paste0("V", i))) +
        geom_dotplot(alpha = 0.8, dotsize = 0.7,
                     fill = "#1b9e77", color = "#1b9e77") +
        geom_segment(aes(x = mean(y[,i]), y = 0,
                         xend = mean(y[,i]), yend = Inf),
                     color = "#d95f02", alpha = .6) +
        labs(title = paste("Sampel ke-", i, sep = ""), x = "", y = "") +
        theme_bw(base_size = 13) +
        annotate("text", x = x_pos, y = 1.8,
                 label = paste("x_bar", "=", bquote(.(mean)),
                               "\n", "SD", "=", bquote(.(sd))),
                 color = "black", size = 3) +
        scale_y_continuous(limits = c(0,2), breaks = NULL) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }
    
    grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
                 plots[[6]], plots[[7]], plots[[8]], ncol = 4)
  })
  
  
  
  # text for sample plots ----
  output$num.samples = renderText({
    
    k = input$k
    paste0("... dan seterusnya sampai sampel ke-",k,".")
    
  })
  
  # plot 3 ----
  # Ini adalah plot distribusi sampling
  output$sampling.dist = renderPlot({
    
    distname = switch(input$dist,
                      rnorm = "populasi normal",
                      rlnorm  = "populasi condong ke kanan",
                      rbeta = "populasi condong ke kiri",
                      runif = "populasi seragam")
    
    n = input$n
    k = input$k
    
    pop = parent()
    
    m_pop =  round(mean(pop),2)
    sd_pop = round(sd(pop),2)
    
    ndist = tibble(means = colMeans(samples()))
    
    m_samp =  round(mean(ndist$means),2)
    sd_samp = round(sd(ndist$means),2)
    
    ndens = density(ndist$means)
    nhist = hist(ndist$means, plot=FALSE)
    
    x_range = max(ndist$means) - min(ndist$means)
    
    y_pos = max(ndens$y) - 0.1*max(ndens$y)
    x_pos = ifelse(m_samp > 0, min(ndist$means) + 0.1*x_range, 
                   max(ndist$means) - 0.1*x_range)
    
    p = ggplot(data = ndist, aes(x = means, y = after_stat(density))) +
      geom_histogram(bins = 20, color = "white", fill = "#d95f02",
                     alpha = .6) +
      stat_density(geom = "line", size = 2) +
      labs(title = paste("Distribusi Sampling*"),
           x = "Rerata sampel",
           y = "") +
      annotate("text", x = x_pos, y = y_pos,
               label = paste("rerata x_bar", "=", bquote(.(m_samp)),
                             "\n", "simpangan baku x_bar", "=", bquote(.(sd_samp))),
               color = "black", size = 5) +
      theme_bw(base_size = 19) +
      theme(plot.title = element_text(hjust = 0.5))
    
    if (input$dist == "runif"){
      
      if (u_min() == u_max()){
        " "
      } else {
        p
      }
    } else {
      p
    }
  })
  
  # deskripsi plot distribusi sampling ----
  output$sampling.descr = renderText({
    
    distname = switch(input$dist,
                      rnorm = "populasi normal",
                      rlnorm  = "populasi condong ke kanan",
                      rbeta = "populasi condong ke kiri",
                      runif = "populasi seragam")
    
    k = input$k
    n = input$n
    paste("*Distribusi rerata dari", k, "sampel acak,
          masing-masing memuat", n, " observasi
          dari sebuah", distname)
  })
  
  # deskripsi CLT ----
  output$CLT.descr = renderText({
    
    pop = parent()
    m_pop =  round(mean(pop),2)
    s_pop = round(sd(pop),2)
    
    n = input$n
    se=round(s_pop/sqrt(n),2)
    paste0("Berdasarkan Teorema Limit Pusat (TLP), distribusi rerata sampel (distribusi sampling) berdistribusi mendekati normal. Rerata distribusi sampling tersebut kurang lebih sama dengan rerata populanya (", m_pop, " dan pias galatnya (simpangan baku rerata) kurang lebih sama dengan simpangan baku populasi dibagi dengan akar kuadrat ukuran sampel (", s_pop,
           "/sqrt(",n, ") = ", se,"). Histogram di atas menunjukkan distribusi sampling reratanya. Sebagai perbandingan, distribusi populasinya disajikan di sebelah kanan.")
  })
}
# Membuat objek aplikasi Shiny ---------------------------------------
shinyApp(ui = ui, server = server)
