load_all('ibgeFunctions')
library(stringr)

year <- 2012
fnames <- dir("../data/"%+%year, pattern='csv', full.names=TRUE)

##there are errors in the cod_municipio in original data so we ignore the info about distrito.

docfinder <- ldply(fnames, function(x) data.frame(read.csv2(x, encoding='latin1')), .progress='text')
names(docfinder) <- tolower(clean(gsub('\\.', '_', names(docfinder))))
docfinder <- mutate(docfinder,
               ##cod_distrito=gsub('\\.', '', codigo_do_municipio),
               cod_municipio=substr(gsub('\\.', '', codigo_do_municipio), 1, 7),
               cod_agencia=gsub('\\.', '', codigo_da_agencia),
               sigla_da_uf=NULL,
               endereco=bairro,
               nome_do_municipio=NULL,
               bairro=NULL,
               nome_da_uf=NULL,
               codigo_da_uf=NULL,
               codigo_da_agencia=NULL,
               nome_da_agencia=NULL,
               ##arquivo=NULL,
               codigo_do_municipio=NULL)

factorize <- function(x, f, ...) {
  xold <- x
  x <- as.factor(x)
  levels(x) <- f(levels(x), ...)
  if (is.factor(xold)) {
    x
  } else if (is.numeric(xold)) {
    as.numeric(as.character(x))
  } else {
    as.character(x)
  }
}


docfinder$nome_municipio=factorize(docfinder$cod_municipio, codemun)
docfinder$nome_agencia=factorize(docfinder$cod_agencia, codeag)

docfinder$pesquisa <-  factorize(as.character(docfinder$arquivo), str_extract, pattern="p[a-z]*")
docfinder$tipo <- factorize(as.character(docfinder$arquivo), function(z) str_sub(str_extract(z, "_[a-z_]*"), start=2))

dbWrite(gps, 'docfinder', docfinder, removefirst=TRUE)





## load(file='~/reps_old/maps/data/bbox.RData')



## docfinder <- merge(docfinder, bboxmun, by=c('cod_municipio'), all.x=TRUE)

## docfinder <- mutate(docfinder, endereco_google=paste(endereco, nome_municipio, 'BA', 'Brazil', sep=', '), endereco=NULL)

## write.csv(docfinder, file='../data/docfinder_'%+%year%+%'.csv', na='', row.names=FALSE)





## library(stringr)
## parse_address_docfinder <- function(x) str_split(x, '-')
## endereco_split <- parse_address_docfinder(docfinder$endereco)
## rua_numero <- sapply(endereco_split, function(x) str_split(x[[1]], ','))
## docfinder$rua <- sapply(rua_numero, function(x) trim(x[1]))
## docfinder$numero <- sapply(rua_numero, function(x) trim(x[length(x)]))

## geocode_osm <- function (country='br', city='salvador', street='rua do buriti', number='89', bbox=c(-38.699, -12.734, -38.304, -13.017), bounded=TRUE) {
##   url <- URLencode("http://nominatim.openstreetmap.org/search/"%+%country%+%"/"%+%city%+%"/"%+%street%+%'/'%+%number%+%'?format=json&polygon=0&addressdetails=1&viewbox='%+%paste(bbox, collapse=',')%+%'&bounded='%+%as.numeric(bounded))
##   ##print(url)
##   fromJSON(url)
## }

## geocode_docfinder <-  function(x) {
##   rx <- with(x, geocode_osm(city=nome_municipio,
##                             street=rua,
##                             number=numero,
##                             bbox=c(bbox.1, bbox.2, bbox.3, bbox.4),
##                             bounded=TRUE))
##   match=0
##   if (length(rx)==0) {
##     print('matching complete failed')
##     rx <- with(x, geocode_osm(city=nome_municipio,
##                                 street=rua,
##                                 number='',
##                                 bbox=c(bbox.1, bbox.2, bbox.3, bbox.4),
##                                 bounded=TRUE))
##     match=1
##     }
##   if (length(rx)==0) {
##     print('matching street failed')
##     rx <- with(x, geocode_osm(city=nome_municipio,
##                               street='',
##                               number='',
##                               bbox=c(bbox.1, bbox.2, bbox.3, bbox.4),
##                               bounded=TRUE))
##     match=2
##   }
##   if (length(rx)==0) {
##     print('matching city with boundbox failed')
##     rx <- with(x, geocode_osm(city=nome_municipio,
##                               street='',
##                               number='',
##                               bbox=c(bbox.1, bbox.2, bbox.3, bbox.4),
##                               bounded=FALSE))
##     match=3
##   }
##   as.numeric(c(y=rx[[1]]$lat, x=rx[[1]]$lon, match=match))
## }
  


## docfinder1 <- docfinder##subset(docfinder, grepl('2927408', cod_agencia))
## geocodes1 <- matrix(NA, nrow=nrow(docfinder1), ncol=3)




## for (i in 1:nrow(docfinder1)) {
##   print(i)
##   if (is.na(geocodes1[i,1])) geocodes1[i,] <- geocode_docfinder(docfinder1[i,])  
## }

## docfinder1 <- data.frame(docfinder1, lat=geocodes1[,1], lon=geocodes1[,2], match=factor(geocodes1[,3], levels=0:3, labels=c('completo', 'rua', 'distrito', 'municipio')))


## save(docfinder1, file=gsub('csv$', 'RData', fnames[j]))







