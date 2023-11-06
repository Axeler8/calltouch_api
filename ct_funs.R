#  API CALLTOUCH V4
# https://www.calltouch.ru/support/api/
# https://www.calltouch.ru/support/api-metod-vygruzki-zhurnala-zvonkov/
# https://www.calltouch.ru/support/api-metod-vygruzki-zhurnala-sdelok/

# - функция авторизации
# функции сбора звонков по дню и за период
# функция сбора сделок
# функция сбора заявок - в процессе
source("~/R/funs/my_tools.R")

sendLogs_mini("Calltouch API V4 loaded")

sys_conf <- readRDS("~/R/tokens/sys_conf") %>%
  as.data.table()

sys_conf_ct <- sys_conf[!is.na(ID_ct)]


loadTok <- function(tokPath = "~/R/tokens/sys_conf"){
  tok_list <- readRDS(tokPath) 
  ct_tok <- tok_list[!is.na(tok_list$token_ct), 
                     c("Client_name", "proj_name", "ID_ct", "token_ct")]
  return(ct_tok)
}


loadTokClient <- function(proj = "evo"){
  require("data.table")
  clientTok <- as.data.table(loadTok())
  clientTok <- clientTok[proj_name %ilike% "evo"]
  return(clientTok)
}


# calls----
# by single day
getCallsDay <- function(siteID, tok,  day = Sys.Date() - 7, verbose = TRUE) {
  
  require("httr")
  tok <- paste0('?clientApiId=', tok)
  domain <- 'https://api.calltouch.ru/calls-service/RestAPI/'
  method <- '/calls-diary/calls'
  dateFrom <- paste0("&dateFrom=", format(day, "%d/%m/%Y"))
  dateTo <- paste0("&dateTo=", format(day, "%d/%m/%Y"))
  
  url <- paste0(domain, siteID, method, tok, dateFrom, dateTo)
  if(verbose){writeLines(paste("taking calls for", day))}
  resp <- httr::GET(url)
  if(verbose){writeLines(as.character(httr::http_status(resp)))}
  cont <- httr::content(resp)
  if(verbose){writeLines(paste("parsed", length(cont)))}
  
  dt <- tryCatch({
    dt <- data.table::rbindlist(cont, fill = TRUE)},
    error = function(cond){
      message(cond)
      return(cont)
    })
  return(dt)
}


# by period day
getCalls <- function(siteID, tok,  dateTo = Sys.Date() - 1, dateFrom = Sys.Date() - 7, 
                     limit = 1000, callTags = FALSE, sessions = FALSE, flatten = TRUE, verbose = TRUE) {
  
  require("httr")
  
  if(limit > 1000 | limit < 1){limit <- 1000}
  if(callTags){callTags <- '&withCallTags=true'} else {callTags <- NULL}
  if(sessions){sessions <- '&withMapVisits=true'} else {sessions <- NULL}
  
  tok <- paste0('?clientApiId=', tok)
  domain <- 'https://api.calltouch.ru/calls-service/RestAPI/'
  method <- '/calls-diary/calls'
  dateFrom <- paste0("&dateFrom=", format(dateFrom, "%d/%m/%Y"))
  dateTo <- paste0("&dateTo=", format(dateTo, "%d/%m/%Y"))
  nPage <- 1
  page <- paste0("&page=", nPage)
  limit <- paste0("&limit=", limit) 
  
  
  
  url <- paste0(domain, siteID, method, tok, dateFrom, dateTo, page, limit, callTags, sessions)
  pageTotal <- 1
  dtAll <- list()
  while(nPage <= pageTotal){
    
    # if(verbose){writeLines(paste("taking calls for period", dateFrom, dateTo)}
    resp <- httr::GET(url)
    if(verbose){writeLines(as.character(httr::http_status(resp)))}
    cont <- httr::content(resp)
    pageTotal <- cont[["pageTotal"]]
    pageSize <- cont[["pageSize"]]
    recordsTotal <- cont[["recordsTotal"]]
    cont <- cont[["records"]]
    # tryCatch might be needed
    if(flatten){dt <- my2jsonlist2(cont)} else {dt <- data.table::rbindlist(cont, fill = TRUE)}
    
    
    dtAll[[nPage]] <- dt
    nPage <- nPage + 1
    page <- paste0("&page=", nPage)
    url <- paste0(domain, siteID, method, tok, dateFrom, dateTo, page, limit, callTags, sessions)
    if(verbose){writeLines(paste("parsed items", (length(dtAll) * pageSize), "of total", recordsTotal,  
                                 "\npage", nPage, "of total", pageTotal))}
  }
  dtAll <- rbindlist(dtAll)
  return(dtAll)
}



# orders----
getOrders <- function(siteID, tok,  dateTo = Sys.Date() - 1, dateFrom = Sys.Date() - 7,
                      limit = 1000, orderTags = TRUE, origins = TRUE, comments = FALSE,
                      flatten = TRUE, verbose = TRUE) {
  
  require("httr")
  
  if(limit > 1000 | limit < 1) {limit <- 1000}
  if(orderTags) {orderTags <- '&withOrdersTags=true'} else {orderTags <- NULL}
  if(origins) {origins <- '&withAllOrderOrigins=true'} else {origins <- NULL}
  if(comments) {comments <- '&withComments=true'} else {comments <- NULL}
  
  tok <- paste0('?clientApiId=', tok)
  domain <- 'https://api.calltouch.ru/calls-service/RestAPI/'
  method <- '/orders-diary/orders'
  dateFrom <- paste0("&dateFrom=", format(dateFrom, "%d/%m/%Y"))
  dateTo <- paste0("&dateTo=", format(dateTo, "%d/%m/%Y"))
  nPage <- 1
  page <- paste0("&page=", nPage)
  limit <- paste0("&limit=", limit) 
  
  url <- paste0(domain, siteID, method, tok, dateFrom, dateTo, page, limit, orderTags, origins, comments)
  pageTotal <- 1
  dtAll <- list()
  while(nPage <= pageTotal){
    resp <- httr::GET(url)
    if(verbose){writeLines(as.character(httr::http_status(resp)))}
    cont <- httr::content(resp)
    pageTotal <- cont[["pageTotal"]]
    pageSize <- cont[["pageSize"]]
    recordsTotal <- cont[["recordsTotal"]]
    cont <- cont[["records"]]
    # tryCatch might be needed
    if(flatten){dt <- my2jsonlist2(cont)} else {dt <- data.table::rbindlist(cont, fill = TRUE)}
    
    
    dtAll[[nPage]] <- dt
    nPage <- nPage + 1
    page <- paste0("&page=", nPage)
    url <- paste0(domain, siteID, method, tok, dateFrom, dateTo, page, limit, orderTags, origins, comments)
    if(verbose){writeLines(paste("parsed items", (length(dtAll) * pageSize), "of total", recordsTotal,  
                                 "\npage", nPage, "of total", pageTotal))}
  }
  dtAll <- rbindlist(dtAll)
  return(dtAll)
}

# gETrEQUESTS
# https://www.calltouch.ru/support/vygruzka-zhurnala-zayavok-cherez-api/
# 
# POST: https://api.calltouch.ru/phone-service/v1/api/calltracking/ad-platform/phone/list

