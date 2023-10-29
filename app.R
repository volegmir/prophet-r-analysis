library(dygraphs)
library(prophet)
library(quantmod)
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
#-----------------------Либы---------------------------------------------------
library(shiny)
library(RPostgres)
#-----------------------Коннект к БД-------------------------------------------
source(file = 'functions.R', encoding = 'UTF-8')
#-----------------------Данные-------------------------------------------------
radio_growth <- c('linear', 'logistic')
choices <- c("USDRUB=X", "EURRUB=X", "GBPRUB=X", "BTC-USD", "ETH-USD", "BNB-USD", "LTC-USD")
condition <- c("Файл", "Сайт")
# tail(forecast1[c('ds','yhat','yhat_lower', 'yhat_upper')])
# prophet_plot_components(model1, forecast1)
#-----------------------Фронт--------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(
    HTML("
        body {
          background-color: #ead1dc;
        }
      ")
  )),
  tags$head(tags$style(HTML(
    "
      .center {
        text-align: center;
      }
    "
  ))),
  # центровка заголовка
  div(class = "center", h1(
    "Прогнозирование с помощью PostgreSQL и Prophet"
  )),
  tags$head(tags$style(
    HTML("
      .nav-tabs {
        display: flex;
        justify-content: center;
      }
    ")
  )),
  # центровка вкладок
  tags$head(tags$style(
    HTML(
      "
        .nav-tabs > li > a {
          background-color: #E0FFFF; /* Светло-голубой цвет */
        }
        "
    )
  )),
  tabsetPanel(
    type = "tabs",
    #первая вкладка------------------------------------------------------------
    tabPanel(
      title = "Подключение к БД",
      textInput("host", "Хост", value = "localhost"),
      textInput("port", "Порт", value = "5432"),
      textInput("dbname", "Имя БД", value = "postgres"),
      textInput("user", "Пользователь", value = "postgres"),
      passwordInput("password", "Пароль", placeholder = "Пароль от БД"),
      actionButton("dbconn", "Подключиться"),
      actionButton("dbdisconn", "Отключиться")
    ),
    #вторая вкладка------------------------------------------------------------
    tabPanel(
      title = "Создание таблицы и управление",
      tags$style(".help-block { color: black; }"),
      helpText("Новая таблица будет создана только с двумя столбцами, ds и y"),
      helpText("Первая таблица хранит основные данные"),
      textInput("t_name", "Имя первой таблицы"),
      helpText("Вторая таблица хранит прогнозные данные"),
      textInput("t2_name", "Имя второй таблицы"),
      actionButton("t_create", "Создать таблицы"),
      actionButton("t_drop", "Удалить таблицы")
    ),
    
    #третья вкладка------------------------------------------------------------
    tabPanel(
      title = "Выбор данных",
      radioButtons("condition", "Выберите: файл или сайт", choices = condition),
      conditionalPanel(
        condition = "input.condition == 'Сайт'",
        selectInput("symbols", "Выберите валютную пару", choices = choices),
        dateInput("start", "Выберите начальную дату"),
        dateInput("end", "Выберите конечную дату"),
      ),
      conditionalPanel(condition = "input.condition == 'Файл'",
                       fileInput("file", "Выберите файл"),),
      textInput("table_choice", "Выберите таблицу"),
      actionButton("submit", "Добавить данные в таблицу"),
      actionButton("delete", "Очистить таблицу")
    ),
    #четвертая вкладка---------------------------------------------------------
    tabPanel(
      title = "График с настройками",
      column(
        6,
        #-------------------------Из таблицы-----------------------------------
        wellPanel(textInput("gettable", "Введите имя таблицы"),),
        #-------------------------Параметры------------------------------------
        wellPanel(
          sliderInput(
            inputId = "future_periods",
            label = "Количество прогнозируемых данных",
            min = 1,
            max = 365,
            value = 30
          ),
          sliderInput(
            inputId = "rows",
            label = "Количество данных для анализа",
            min = 30,
            max = 8000,
            value = 200,
            step = 50
          ),
          sliderInput(
            inputId = "width",
            label = "Доверительный интервал",
            min = 0,
            max = 1,
            value = 0.8,
            step = 0.1
          ),
          actionButton("getdatabut", "Получить данные из таблицы"),
        ),
        #-------------------------В таблицу-----------------------------------
        wellPanel(
          textInput("gettable2", "Введите имя таблицы для прогноза"),
          actionButton(inputId = "pushdatabut", label = "Внести данные в таблицу"),
          actionButton(inputId = "deldatabut", label = "Очистить таблицу")
        )
      ),
      column(
        6,
        column(12,  dygraphOutput(outputId = 'dyplot')),
        br(),
        br(),
        br(),
        br(),
        br(),
        column(12,  plotOutput(outputId = 'dyplot2'))
      )
    )
  )
  #пятая вкладка-------------------------------------------------------------
)
#------------------------------------Бэк---------------------------------------
server <- function(input, output) {
  #------------------------Логика для первой вкладки---------------------------
  observeEvent(input$dbconn, {
    tryCatch({
      con <<- dbConnect(
        #con прописывается как глобальная переменная <<-
        RPostgres::Postgres(),
        host = input$host,
        port = input$port,
        dbname = input$dbname,
        user = input$user,
        password = input$password
      )
    },
    error = function(e) {
      showModal(
        #перехват_ошибки
        modalDialog(
          title = "Ошибка",
          "Данные введены неверно или база не существует, попробуйте ещё раз",
          easyClose = TRUE
        )
      )
    })
  })
  observeEvent(input$dbdisconn, {
    tryCatch({
      dbDisconnect(con)
    },
    warning = function(w) {
      showModal(#перехват_ошибки
        modalDialog(
          title = "Ошибка",
          "Вы уже отключены от базы данных",
          easyClose = TRUE
        ))
    })
  })
  #------------------------Логика для второй вкладки---------------------------
  observeEvent(input$t_create, {
    tryCatch({
      t_create(con, input$t_name, input$t2_name)
    },
    error = function(e) {
      showModal(
        #перехват_ошибки
        modalDialog(
          title = "Ошибка",
          "Такие таблицы уже существуют либо вы не подключены к базе данных",
          easyClose = TRUE
        )
      )
    })
  })
  observeEvent(input$t_drop, {
    tryCatch({
      t_drop(con, input$t_name, input$t2_name)
    },
    error = function(e) {
      showModal(
        #перехват_ошибки
        modalDialog(
          title = "Ошибка",
          "Таких таблиц не существует либо вы не подключены к базе данных",
          easyClose = TRUE
        )
      )
    })
  })
  #------------------------Логика для третьей вкладки--------------------------
  observeEvent(input$submit, {
    if (input$condition == 'Сайт') {
      tryCatch({
        getSymbols(
          input$symbols,
          src = 'yahoo',
          from = input$start,
          to = input$end
        )
        data <-
          tibble::rownames_to_column(as.data.frame(Cl(get(
            input$symbols
          ))), var = "ds")
        colnames(data)[2] <- "y"
        insertData(con, input$table_choice, data)
      },
      error = function(e) {
        showModal(
          #перехват_ошибки
          modalDialog(
            title = "Ошибка",
            "Вы не выбрали таблицу либо такого тикера нет",
            easyClose = TRUE
          )
        )
      })
    }
    else {
      tryCatch({
        data <- read.csv(input$file$datapath)
        insertData(con, input$table_choice, data)
      },
      error = function(e) {
        showModal(
          #перехват_ошибки
          modalDialog(
            title = "Ошибка",
            "Вы не выбрали файл или таблицу либо формат файла неверен",
            easyClose = TRUE
          )
        )
      })
    }
  })
  observeEvent(input$delete, {
    tryCatch({
      deleteData(con, input$table_choice)
    },
    error = function(e) {
      showModal(#перехват_ошибки
        modalDialog(
          title = "Ошибка",
          "Вы не выбрали таблицу",
          easyClose = TRUE
        ))
    })
  })
  #------------------------Логика для четвертой вкладки--------------------------
  filter <-
    eventReactive(eventExpr = input$getdatabut, valueExpr = {
      data_forecast <<-
        dataForForecastPlot(con, input$gettable, input$rows) #глобальная переменная
      model1 <- prophet(data_forecast, interval.width = input$width)
      future1 <-
        make_future_dataframe(model1, periods = input$future_periods)
      forecast1 <- predict(model1, future1)
      dyplot.prophet(model1, forecast1)
    }) #реактивный вывод графика (его логика)
  filter2 <-
    eventReactive(eventExpr = input$getdatabut, valueExpr = {
      model2 <- prophet(data_forecast)
      future2 <-
        make_future_dataframe(model2, periods = input$future_periods)
      forecast2 <<- predict(model2, future2) #глобальная переменная
      prophet_plot_components(model2, forecast2)
    }) # по сути то же что и первый filter, но выводит
  #другой тип графика
  output$dyplot <- (renderDygraph(expr = (filter())))
  output$dyplot2 <- (renderPlot(expr = (filter2())))
  observeEvent(input$pushdatabut, {
    tryCatch({
      forecast2 <-
        forecast2[order(forecast2$ds, decreasing = TRUE), c("ds", "yhat")]
      insertForecast(con, input$gettable2, forecast2, input$future_periods)
    },
    error = function(e) {
      showModal(
        #перехват_ошибки
        modalDialog(
          title = "Ошибка",
          "Вы не выбрали таблицу либо выбрали неверную таблицу либо не ввели данные",
          easyClose = TRUE
        )
      )
    })
  })
  observeEvent(input$deldatabut, {
    tryCatch({
      deleteData(con, input$gettable2)
    },
    error = function(e) {
      showModal(#перехват_ошибки
        modalDialog(
          title = "Ошибка",
          "Вы не выбрали таблицу",
          easyClose = TRUE
        ))
    })
  })
}

#------------------------------------Запуск------------------------------------
shinyApp(ui = ui, server = server)
