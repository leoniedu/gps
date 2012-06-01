load_all('ibgeFunctions') 
library(XML)
library(RCurl)
library(plyr)



login_pms <- function(curl=NULL, sistema='pms') {
  ## log in to PMS
  require(XML)
  if (is.null(curl)) {
    h <- getCurlHandle(followlocation = TRUE)
  }  else {
    h <- curl
  }
  pf <- postForm("http://www."%+%sistema%+%".ibge.gov.br/pages/principalSupervisor.php",
                 inputP_chaveSup='',
                 enviado1='1',
                 codue="29",
                 edtpwssup="02BAH90",
                 .opts=copts, style = "POST", curl=h)
  pf2 <- postForm("http://www."%+%sistema%+%".ibge.gov.br/pages/autenticaue.php",
                 inputP_chaveSup='',
                 enviado1='1',
                 codue="29",
                 edtpwssup="02BAH90",
                 .opts=copts, style = "POST", curl=h)
  ##print(pf2)
  h
}





get_table <- function(cod_uf=29, tipo='processados', sistema='pms') {
  tipo <- switch(tipo, 'processados'=2, 'faltosos'=4, stop())
  tmpf <- tempfile()
  url <- 'http://www.'%+%sistema%+%'.ibge.gov.br/pages/coleta3.php?cod_uf_agencia='%+%cod_uf%+%'&tipo='%+%tipo
  print(url)
  hf <- getURL(url, curl=h, .opts=copts, .encoding='latin1')
  writeLines(hf, con = tmpf)
  res <- readLines(tmpf, encoding='utf8')
  res <- readHTMLTable(res)[[2]]
  names(res) <- c('cod_agencia', 'empresa', 'ul', 'dv', 'razao_social', 'web')
  res$cod_agencia <- gsub('[^0-9]*', '', res$cod_agencia)
  res$cod_uf <- cod_uf
  res$processado <- if (tipo==2) {
    1
  } else {
    0
  }
  res
}



get_pms_uf <- function(cod_uf=29, curl, sistema='pms') {
  h <- curl
  url <- 'http://www.'%+%sistema%+%'.ibge.gov.br/pages/coleta2.php?uff='%+%cod_uf
  print(url)
  hf <- getURL(url, curl =h, .opts=copts, .encoding='latin1')
  tmpf <- tempfile()
  writeLines(hf, con = tmpf)
  res <- readLines(tmpf, encoding='utf8')
  res <- readHTMLTable(res)[[1]]
  res <- res[, c(1,2,3,5)]
  names(res) <- c('agencia', 'emitidos', 'processados', 'faltosos')
  res$cod_uf <- cod_uf
  res$data <- Sys.Date()
  if (sistema=='coletainicialpms') {
    if (!dbExistsTable(gps, 'pms_uf')) {
      dbWrite(gps, 'pms_uf', res)
      dbGetQuery(gps, 'alter table pms_uf add unique index (cod_uf, agencia(100), data(10))')
    } else {
      dbWrite(gps, 'pms_uf', res)
    }
  }
  if (sistema=='pms') {
    if (!dbExistsTable(gps, 'pms_agencias')) {
      dbWrite(gps, 'pms_agencias', res)
      dbGetQuery(gps, 'alter table pms_agencias add unique index (cod_uf, agencia(100), data(10))')
    } else {
      dbWrite(gps, 'pms_agencias', res)
    }
  }
  res
}



get_empresas <- function(sistema) {
  empresas <- rbind(get_table(29, 'processados'), get_table(29, 'faltosos', sistema))
  empresas$data <- Sys.Date()
  if (sistema=='coletainicialpms') {
    if (!dbExistsTable(gps, 'pms_empresas')) {
      dbWrite(gps, 'pms_empresas', empresas)
      dbGetQuery(gps, 'alter table pms_empresas add unique index (cod_uf, empresa(10), data(10))')
    } else {
      dbWrite(gps, 'pms_empresas', empresas)
    }
  } else {
    if (!dbExistsTable(gps, 'pms_empresas_f')) {
      dbWrite(gps, 'pms_empresas_f', empresas)
      dbGetQuery(gps, 'alter table pms_empresas_f add unique index (cod_uf, empresa(10), data(10))')
    } else {
      dbWrite(gps, 'pms_empresas_f', empresas)
    }
  }
  empresas
}

Sys.setenv(http_proxy="")

copts <- curlOptions(cookie="~/Desktop/cook3.txt", cookiejar="~/Desktop/cook3.txt", useragent="Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.3) Gecko/20070309 Firefox/2.0.0.3", followlocation=TRUE, proxy='')
h <- getCurlHandle(followlocation = TRUE)

## h <- login_pms(h, sistema='coletainicialpms')
## cod_uf <- dbGetQuery(gps, 'select cod_uf from uf2011')$cod_uf
## res <- ldply(cod_uf, get_pms_uf, curl=h, .progress='text', sistema='coletainicialpms')
## print('here')

## tmp <- get_empresas('coletainicialpms')



h <- login_pms(h, sistema='pms')

cod_uf <- dbGetQuery(gpso, 'select cod_uf from uf2011')$cod_uf

res <- ldply(29, get_pms_uf, curl=h, .progress='text', sistema='pms')

print('here')

tmp2 <- get_empresas('pms')





## tmp <- as.character(getURL('https://spreadsheets2.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0Ahup3xoDmIoodGJ2Z2JZNmF5WmRTcGl2Uk5QWVFETlE&single=true&gid=0&output=csv', .opts=curlOptions(proxy='http://proxy.ibge.gov.br:80', proxyusername='eduardo.leoni', proxypassword='chyld2slo', encoding='latin1')))

## duplas <- read.csv(textConnection(tmp))
## names(duplas) <- tolower(clean(names(duplas), more=TRUE))
## duplas <- subset(duplas, (dupla!=''), select=c(dupla, cnpj))
## duplas$cnpj <- pad0(duplas$cnpj, 8)
## empresas <- dbGetQuery(gps, 'select * from pms_empresas_last')
## duplas[!duplas$cnpj%in%empresas$empresa,]

## empresas <- merge(empresas, duplas, all.x=TRUE, by.x='empresa', by.y='cnpj')
## empresas$dupla[is.na(empresas$dupla)] <- ''
## empresas$dupla <- tolower(empresas$dupla)
## empresas$Processado <- logicalbr(empresas$processado)


## library(reshape)
## res <- recast(subset(empresas, dupla!=''), dupla ~ Processado, measure.var='web', margins=TRUE)

## res$SIMs <- with(res, SIM/(1+!grepl('gilmar|socorro', dupla)))

## ascii(subset(arrange(res, -SIMs), select=-SIMs)[-1,])
