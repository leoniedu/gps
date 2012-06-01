library(XML)
library(RCurl)
library(plyr)
##library(RTidyHTML)
load_all('ibgeFunctions')

h <- getCurlHandle(followlocation = TRUE)

copts <- curlOptions(cookie="~/Desktop/cook3.txt", cookiejar="~/Desktop/cook3.txt", useragent="Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.3) Gecko/20070309 Firefox/2.0.0.3", followlocation=TRUE, proxy='', encoding='latin1')

login_pmc <- function(curl=NULL) {
  ## log in to PMC
  require(XML)
  if (is.null(curl)) {
    h <- getCurlHandle(followlocation = TRUE)
  }  else {
    h <- curl
  }
  pf <- postForm("http://www.pmc.ibge.gov.br/pmc/Autentica.asp?modautentic=S1",
                 codue="29",
                 edtpwssup="02bah91",
                 .opts=copts, style = "POST", curl=h)
  hf <- getURL('http://www.pmc.ibge.gov.br/pmc/chamapagina.htm?pag=indexsupervisor.asp', curl =h, .opts=copts)
  hf <- getURL('http://www.pmc.ibge.gov.br/pmc/defaulttop.htm', curl=h, .opts=copts)
  hf <- getURL('http://www.pmc.ibge.gov.br/Pmc/menusupervisorue.asp', curl=h, .opts=copts)
  hf <- getURL('http://www.pmc.ibge.gov.br/pmc/js/ValidaFormSupervisorUe.js', curl=h, .opts=copts)
  hf <- getURL('http://www.pmc.ibge.gov.br/pmc/DefaultMain.asp', curl=h, .opts=copts)
  hf <- getURL('http://www.pmc.ibge.gov.br/Pmc/ControleColetaUF.asp', curl=h, .opts=copts)
  ## return curl
  h
}

tidy <- function (x) {
  tfile <- tempfile()
  writeLines(clean(html0), tfile)
  system('tidy -q --show-errors 0 '%+%tfile, intern=TRUE)
}
  
get_tipo_web <- function(cod_uf, tipo, curl) {
  if (tipo==2) {
    processado <- TRUE
  } else if (tipo==3) {
    processado <- FALSE
  } else stop('tipo não válido')
  tfile <- tempfile()
  h <- curl  
  html0 <- html <- getURLContent('http://www.pmc.ibge.gov.br/Pmc/ControleColetaCnpjAG.asp?uff='%+%cod_uf%+%'&Agencia=999999&proc=1&tipo='%+%tipo, curl=h, .opts=copts, .encoding='ISO-8859-1')  
  ## write to disk
  writeLines(clean(html0), tfile)
  html <- readLines(tfile, encoding='latin1')  
  ## get just the portion with the table
  html <- clean(html[grep('CONTROLE DE COLETA', html)] )
  ## get number of companies
  ncompanies <- as.numeric(gsub('.*Total de informantes .*: ([0-9]+)[^0-9].*', '\\1', html, ignore.case=TRUE))  
  ## tidy html so xml can parse it
  html <- system('tidy -q --show-errors 0 '%+%tfile, intern=TRUE)
  html <- readHTMLTable(htmlParse(html, encoding='latin1'))[[1]][,-1]
  names(html) <- c('empresa', 'ul', 'dv', 'razao_social', 'web')  
  if (as.character(html[1,1])=='Empresa') html <- html[-1, ]
  ## verify number of companies
  stopifnot(nrow(html)==ncompanies)
  if (nrow(html)==0) return(NULL)
  ## make final table
  html$razao_social <- gsub('\n', ' ', html$razao_social)
  html$cnpj <- with(html, paste(empresa, ul, dv, sep=''))
  html <- subset(html, select=c(cnpj, razao_social, web))
  html$data <- Sys.Date() 
  html$cod_uf <- cod_uf
  html$processado <- processado
  html$web <- html$web=="Sim"
  html
}

get_pmc_web <- function(cod_uf, curl) {
  ## get the web use data by company
  ##require(RTidyHTML)
  stopifnot(length(cod_uf)==1, nchar(cod_uf)==2)
  print('carregando uf '%+%cod_uf)
  h <- curl
  ## get the page we want (after login)
  html <- rbind(get_tipo_web(cod_uf=cod_uf, tipo=2, curl=h),
                get_tipo_web(cod_uf=cod_uf, tipo=3, curl=h))
  ## write to database
  stopifnot(!is.na(html[1,1]))
  stopifnot(!any(is.na(html$web)))
  if (!dbExistsTable(gps, 'pmc_web')) {
    dbWrite(gps, 'pmc_web', html)
    dbGetQuery(gps, 'alter table pmc_web add unique index (cnpj(16), data(10))')
    dbGetQuery(gps, 'alter table pmc_web add index (cnpj(16))')
    dbGetQuery(gps, 'alter table pmc_web add index (data(10))')
  } else {
    dbWrite(gps, 'pmc_web', html)
  }
  html
}

get_pmc_econ <- function(cnpj, h) {   
  ##require(RTidyHTML)
  ## get the economic  data and cadastro data by company
  tfile <- tempfile()
  stopifnot(length(cnpj)==1)
  cnpj <- gsub('[^0-9]', '', cnpj)    
  print(cnpj)
  stopifnot(nchar(cnpj)==14)
  raiz <- substr(cnpj, 1, 8)
  suf <- substr(cnpj, 9, 12)
  dv <- substr(cnpj, 13, 14)
  ## get info from a specific company
  res <- tryCatch(postForm('http://www.pmc.ibge.gov.br/Pmc/Autentica.asp?modautentic=S2', 
                           curl=h, 
                           edtraiz=raiz,
                           edtsufixo=suf,
                           edtv=dv,
                           edtsiape='01782996',
                           .opts=copts, style = "POST"), error=function(e) {
                             print('trying again in 10 seconds!')
                             Sys.sleep(10)
                             get_pmc_econ(cnpj, curl)                             
                           }
                  )
  res2 <- getURL(c('http://www.pmc.ibge.gov.br/Pmc/chamapagina.htm?pag=index1.asp',
                   'http://www.pmc.ibge.gov.br/Pmc/index1.asp', 
                   'http://www.pmc.ibge.gov.br/Pmc/defaulttop.htm', 
                   'http://www.pmc.ibge.gov.br/Pmc/menu.asp', 
                   'http://www.pmc.ibge.gov.br/Pmc/DadosCad.asp'), 
                 curl=h, async=FALSE, .encoding='ISO-8859-1', 
                 .opts=copts)  
  ## agencia <- tryCatch(
  ##              trim(strsplit(xmlValue(getNodeSet(htmlParse(clean(iconv(tidy(res2[[5]]), from='latin1'))), '//th')[[2]], '-|:')[[1]]
  ##                            , error=function(e) {
  ##                              print('impossível carregar página. tentando o login novamente em 30 segundos.')
  ##                              Sys.sleep(30)
  ##                              h <- login_pmc(h)
  ##                              get_pmc_econ(cnpj, h)
  ##                            })
  cadastro <- res2[[5]]
  writeLines(cadastro, tfile)
  html <- readLines(tfile, encoding='latin1')  
  html <- clean(html)
  cod_agencia <- gsub(' ', '', gsub('.*Agencia: ([0-9 ]*).*', '\\1', html[grepl('Agencia:', html)]))
  html <- htmlParse(html)
  contador <- any(grepl('CONTADOR', sapply(getNodeSet(html, '//font'), xmlValue)))
  tmp  <- sapply(getNodeSet(html, '//td//input'), function(x) xmlAttrs(x)[c('name', 'value')])    
  cadastro <- tmp[2,]
  names(cadastro) <- tolower(tmp[1,])
  cadastro <- data.frame(t(cadastro), data=Sys.Date(), cnpj=cnpj, contador=contador)
  cadastro$cod_agencia <- cod_agencia
  if (!dbExistsTable(gps, 'pmc_cadastro')) {
    dbWrite(gps, 'pmc_cadastro', cadastro)
    dbGetQuery(gps, 'alter table pmc_cadastro add unique index (razaoemp(32), data(10))')
  } else {
    dbWrite(gps, 'pmc_cadastro', cadastro)
  }  
}


h <- login_pmc(h)

##cod_uf <- dbGetQuery(gpso, 'select cod_uf from uf2011')$cod_uf

res <- try(ldply(29, get_pmc_web, curl=h, .progress='text'))



## res <- try(ldply(cod_uf, get_pmc_web, curl=h, .progress='text'))

## if (grepl('Proxy', res)[1]) {
##   ##Sys.setenv(http_proxy="http://eduardo.leoni:chyld2slo@proxy.ibge.gov.br")
##   Sys.setenv(http_proxy="")
##   h <- login_pmc(h)
##   res <- try(ldply(cod_uf, get_pmc_web, curl=h, .progress='text'))
## }

## cnpjs <- dbGetQuery(gps, 'select distinct cnpj from pmc_web_last where cod_uf=29')[, 1]
## cnpjs <- cnpjs[!cnpjs%in%dbGetQuery(gps, 'select cnpj from pmc_cadastro_last')[,1]]

## ## res_econ <- ldply(dbGetQuery(gps, 'select distinct cnpj from pmc_web_last where cod_uf=29')[, 1], get_pmc_econ, curl=h, .progress='text')

cnpjs <- res$cnpj

res_econ <- ldply(cnpjs, get_pmc_econ, h=h, .progress='text')

