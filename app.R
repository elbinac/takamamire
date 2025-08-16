library(shiny)
library(dplyr)

# # データ例
# data_original <- data.frame(
#     ID = c("00","0","1","2","90"),
#     日本語 = c("りんご","いちご","なし","めろん","特別"),
#     英語 = c("あっぽー","すとろべりー","ぺあー","めろん","special"),
#     stringsAsFactors = FALSE
# )

#データセット（例）
data_original <- read.csv("hawks2.csv",
                          fileEncoding = "UTF-8",
                          stringsAsFactors = FALSE)
# ってやると、ID（背番号）列がchrではなくなってしまうので
# redrのcsvでやります

ui <- fluidPage(
    titlePanel("鷹応援 数字を入力（例: 00,2）"),
    
    textInput("numbers1", "先発", value = ""),
    textInput("numbers2", "スタメン", value = ""),
    textInput("numbers3", "追加検索", value = ""),
    actionButton("go", "検索"),
    uiOutput("result")
)

server <- function(input, output, session) {
    
    search_ids <- function(ids_input, label){
        ids <- trimws(unlist(strsplit(ids_input, ",")))
        ids_internal <- ifelse(ids == "00", "900", ids)
        
        df <- data_original %>%
            filter(ID %in% ids_internal) %>%
            mutate(order_index = match(ID, ids_internal)) %>%
            arrange(order_index) %>%
            select(-order_index)
        
        df$ID <- sapply(df$ID, function(x){
            if(x == "900" & "00" %in% ids) return("00")
            x
        })
        
        # 「2」のときだけ通し番号を追加
        if(label == "2") {
            df <- df %>% mutate(No = row_number())
        }
        
        df
    }
    
    results <- eventReactive(input$go, {
        list(
            "1" = search_ids(input$numbers1, "1"),
            "2" = search_ids(input$numbers2, "2"),
            "3" = search_ids(input$numbers3, "3")
        )
    })
    
    output$result <- renderUI({
        res <- results()
        tagList(
            lapply(names(res), function(name){
                tagList(
                    h4(name),
                    tableOutput(paste0("tbl_", name)),
                    br()
                )
            })
        )
    })
    
    observe({
        res <- results()
        lapply(names(res), function(name){
            output[[paste0("tbl_", name)]] <- renderTable({
                res[[name]]
            })
        })
    })
}

shinyApp(ui, server)
