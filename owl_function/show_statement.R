show_statement <- function(data,type){
    num_of_row = nrow(data)
    num_of_col = ncol(data)
    
    #cat(num_of_row)
    statement_writer <- ""
    for(head in 1:num_of_row) {
        statement_writer <-paste0(statement_writer,"数值",sep="|")
    }
    statement_writer <-paste0(statement_writer,"\r\n")
    
    for(head in 1:num_of_row) {
        statement_writer <-paste0(statement_writer,"--------------------",sep="|")
    }
    KO
    {"<"}
    statement_writer <-paste0(statement_writer,"\r\n")
    
    if(is.na(data$row))
    
    for(col in 1:num_of_col){
        single_row <- paste0(colnames(data)[col],"|")
        for (row in 1:num_of_row) {
            if(row != num_of_row)
                single_row <- paste0(single_row,data[row,col],sep="|")
            else
                single_row <- paste0(single_row,data[row,col],sep="|\r\n")
        }
        statement_writer <- paste0(statement_writer,single_row)
    }
    #cat(statement_writer)
    statement_writer
}
#bs <- balance_sheet %>%
#    filter(证券代码=='000004'& 公告日期 >'2015-12-31')

#show_statement(bs)

show_balance_sheet <- function(balance_sheet,type){
    num_of_row = nrow(balance_sheet)
    num_of_col = ncol(balance_sheet)
    account_name_list = colnames(balance_sheet)
    statement_writer <- ''
    if('报告年度' %in% account_name_list){
        single_row <- paste0('报告年度',"|")
        for (row in 1:num_of_row) {
            if(row != num_of_row)
                single_row <- paste0(single_row,format(balance_sheet[row,'报告年度']),sep="|")
            else
                single_row <- paste0(single_row,format(balance_sheet[row,'报告年度']),sep="|\r\n")
        }
    }
    
    statement_writer <- paste0(statement_writer,single_row)
    
    
    for(head in 0:num_of_row) {
        statement_writer <-paste0(statement_writer,":------------------:",sep="|")
    }
    
    statement_writer <-paste0(statement_writer,"\r\n")
    
    statement_writer <- paste0(statement_writer,print_account_category(balance_sheet,"资产"))
    statement_writer <- paste0(statement_writer,print_account_category(balance_sheet,"流动资产"))
    
    
    
    account_list <- c('货币资金（元）','以公允价值计量且其变动计入当期损益的金融资产','应收票据','应收账款','预付款项','其他应收款','应收关联公司款','应收利息','应收股利','存货','其中：消耗性生物资产','一年内到期的非流动资产','其他流动资产','流动资产合计')
    
    for(account_item in account_list){
        single_row <- print_account(balance_sheet,account_item,indent_level = 'second')
        statement_writer <- paste0(statement_writer,single_row)
    }
    
    single_row <- print_account(balance_sheet,"货币资金（元）",indent_level = 'second')
    
    statement_writer <- paste0(statement_writer,single_row)
    
    single_row <- print_account(balance_sheet,"应收票据",indent_level = 'first')
    
    statement_writer <- paste0(statement_writer,single_row)
    
    single_row <- print_account(balance_sheet,"存货",indent_level = 'first')
    
    statement_writer <- paste0(statement_writer,single_row)
    #cat(statement_writer)
    statement_writer
}

print_account <- function(statement_data,account_name,indent_level=c('first','second','third'),head_format='strong',inline_format='default'){
    indent_level <- match.arg(indent_level)
    account_name_list <- colnames(statement_data)
    indent <- switch(indent_level,
                     first="",
                     second="&nbsp;&nbsp;",
                     third="&nbsp;&nbsp;&nbsp;&nbsp;"
    )
    if(account_name %in% account_name_list){
        account_row <- paste0(inline_format_both(account_name,head_format),'|')
        account_row <- paste0(indent,account_row)
        row_num <- nrow(statement_data)
        for(row_index in 1:row_num){
            if(is.na(statement_data[row_index,account_name]))
                account_row <- paste0(account_row,inline_format_both("--"),sep="|")
            else
                account_row <- paste0(account_row,inline_format_both(prettyNum(statement_data[row_index,account_name],big.mark = ","),inline_format),sep="|")
        }
        account_row <- paste0(account_row,"\r\n")
    }
    account_row
}

print_account_category <- function(statement_data,account_category_name){
    account_category_row <- paste0(inline_format_both(account_category_name,'strong'),"|")
    row_num <- nrow(statement_data)
    for(row_index in 1:row_num){
        account_category_row <- paste0(account_category_row,'|')
    }
    account_category_row <- paste0(account_category_row,"\r\n")
    
    account_category_row
}
#右边加内敛符号
inline_format_right <- function(string,inline=c('default','strong')){
    inline <- match.arg(inline)
    inline <- switch(inline,
                    default = "",
                    strong = "**"
    )
    paste0(string,inline)
}

#左边加内联符号
inline_format_left <- function(string,inline=c('default','strong')){
    inline <- match.arg(inline)
    inline <- switch(inline,
                     default = "",
                     strong = "**"
    )
    paste0(inline,string)
}

#两边加内联符号
inline_format_both <- function(string,inline=c('default','strong')){
    inline <- match.arg(inline)
    string <- inline_format_left(string,inline)
    inline_format_right(string,inline)
}



#"应交税费" %in% colnames(bs)


#bs[1,"应交税费"]

kmeans_cluster <- function(dataset) { 
    
    require(shiny)  
    
    shinyApp(
        ui = fluidPage(
            fluidRow(style = "padding-bottom: 20px;",
                     column(4, selectInput('xcol', 'X Variable', names(dataset))),
                     column(4, selectInput('ycol', 'Y Variable', names(dataset),
                                           selected=names(dataset)[[2]])),
                     column(4, numericInput('clusters', 'Cluster count', 3,
                                            min = 1, max = 9))
            ),
            fluidRow(
                plotOutput('kmeans', height = "400px")  
            )
        ),
        
        server = function(input, output, session) {
            
            # Combine the selected variables into a new data frame
            selectedData <- reactive({
                dataset[, c(input$xcol, input$ycol)]
            })
            
            clusters <- reactive({
                kmeans(selectedData(), input$clusters)
            })
            
            output$kmeans <- renderPlot(height = 400, {
                par(mar = c(5.1, 4.1, 0, 1))
                plot(selectedData(),
                     col = clusters()$cluster,
                     pch = 20, cex = 3)
                points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
            })
        },
        
        options = list(height = 500)
    )
}


