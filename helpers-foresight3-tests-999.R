
################################ begin of hdnile work ###########################




# posneg - higher values ... higher ntiles
hdntile  <- function(  x  = NULL                     # required: 'subset (grouped)'  dataframe
                       , measurecol = NULL             # required:  measure column name string 
                       , buckets = 100                  # divisions integer
                       , posneg = 1                     # 1  - higher values = higher measure, -1 - higher values lower measure  integer
                       , new_suffix_type = "functmeas"  # new column suffix typically 'functmeas' _NTILE100 string                        , mutually exclusive of ( new_suffix_name, new_word_explicit_name )
                       , new_suffix_expl_name = NULL    # explicity name the suffix e.g. "_MYNTILEGREAT" string                          , mutually exclusive of ( new_suffix_type, new_word_explicit_name )
                       , new_word_explicit_name = NULL  # explicity name the the entire new column e.g. "MYQUANTILEGREAT_OF_TODAY" string , mutually exclusive of ( new_suffix_type, new_suffix_name )
                       , monopolyhdntile  = "buckets"    # if only one firm, how does it compare against itself? 
                       # NA - hdquantile default                  literal NA
                       # "buckets" - use buckets hdntile default  string
                       # "bucketsDIVtwo" use trunc(buckets/2)     string
                       # 1,2,50,100 or any hard coded             literal integer
                       , measurename = "NTILE"           # name the 'funct' part of 'functmeas' ( of 'measurecol'_functmeas' )
                       , ...                             # other parameters passed to base::cut.default and Hmisc::hdquantile                  
) {
  
  require(Hmisc)
  require(dplyr)
  
  newcolname <- ""
  
  # choose output column name 
  
  if(new_suffix_type == "functmeas") {
    newcolname <- paste0(measurecol,"_",measurename,buckets) 
  }
  
  if(!is.null(new_suffix_expl_name)) {
    newcolname <- paste0(measurecol,"_",new_suffix_expl_name) 
  }
  
  if(!is.null(new_word_explicit_name)) {
    newcolname <- new_word_explicit_name  
  }
  
  # calculate
  
  x[[newcolname]] <-  cut((as.numeric(posneg))*x[[measurecol]], hdquantile((as.numeric(posneg))*x[[measurecol]], seq(0, 1, as.numeric(1.0/as.numeric(buckets))) , na.rm = TRUE ), labels=FALSE , include.lowest=T) 
  
  # pass-through  ( will be NA - hdquantile default )
  # monopolyntilevalue <- NA
  
  # adjustment if only one monopoly firm exists how does it compare against itself?
  
  if( !is.na(monopolyhdntile) & (length(which(!is.na(x[,measurecol]))) == 1) ) {
    
    # browser()
    if(  monopolyhdntile == "buckets"   ) {
      x[,newcolname] <- ifelse(!is.na(x[,measurecol]), as.integer(buckets), NA )
    }
    
    if(  monopolyhdntile == "bucketsDIVtwo"   ) {
      x[,newcolname] <- ifelse(!is.na(x[,measurecol]), as.integer(trunc(buckets/2.0)), NA ) # pessimistic
    }
    
    if(  is.numeric(monopolyhdntile)  ) {
      x[,newcolname] <- ifelse(!is.na(x[,measurecol]), as.integer(monopolyhdntile), NA )
    }
    

  }
  
  # because I do math between hdntiles
  x[,newcolname] <- as.numeric(x[,newcolname])
  
  # return the data.frame
  x
}


################################ end of hdnile work ###########################


################################ begin of hdnile testing ###########################

# test - not-grouped

df <- data.frame(
  houseID = c("A","A","B","B","C","C")
  , year   = 1:6
  , price  = 11:16
  , stringsAsFactors = FALSE
)


> df
houseID year price
1       A    1    11
2       A    2    12
3       B    3    13
4       B    4    14
5       C    5    15
6       C    6    16

hdntile(df,"price")

> hdntile(df,"price")
houseID year price price_NTILE100
1       A    1    11              1
2       A    2    12             25
3       B    3    13             42
4       B    4    14             59
5       C    5    15             76
6       C    6    16            100

df[4,"price"] <- NA

> df
houseID year price
1       A    1    11
2       A    2    12
3       B    3    13
4       B    4    NA
5       C    5    15
6       C    6    16

hdntile(df,"price")

> hdntile(df,"price")
houseID year price price_NTILE100
1       A    1    11              1
2       A    2    12             29
3       B    3    13             46
4       B    4    NA             NA
5       C    5    15             75
6       C    6    16            100

> hdntile(df[4,],"price")  # since price is NA so is the output

houseID year price price_NTILE100
4       B    4    NA             NA

> hdntile(df[5,],"price")     # monopolyhdntile  = "buckets" default

houseID year price price_NTILE100
5       C    5    15            100

> hdntile(df[5,],"price", monopolyhdntile = "bucketsDIVtwo" )

houseID year price price_NTILE100
5       C    5    15             50

> hdntile(df[5,],"price", monopolyhdntile = NA )

houseID year price price_NTILE100
5       C    5    15             NA

> hdntile(df[5,],"price", monopolyhdntile = 99 )

houseID year price price_NTILE100
5       C    5    15             99

> hdntile(df[3:4,],"price")  # monopolyhdntile  = "buckets" default

houseID year price price_NTILE100
3       B    3    13            100
4       B    4    NA             NA


hdntile(df[2:4,],"price")  # needs at least 2 # reals to compare against

houseID year price price_NTILE100
2       A    2    12              1
3       B    3    13            100
4       B    4    NA             NA


df[4,"price"] <- 14


# test grouped with NA

df <- data.frame(
  houseID = c("A","B","A","B","A","B")
  , year   = 1:6
  , price  = 11:16
  , stringsAsFactors = FALSE
)

> df
houseID year price
1       A    1    11
2       B    2    12
3       A    3    13
4       B    4    14
5       A    5    15
6       B    6    16


> df[4,"price"] <- NA

houseID year price
1       A    1    11
2       B    2    12
3       A    3    13
4       B    4    NA
5       A    5    15
6       B    6    16


> df_g  <- group_by(df, houseID)


> do(df_g, hdntile(.,"price") )

Source: local data frame [6 x 4]
Groups: houseID

houseID year price price_NTILE100
1       A    1    11              1
2       A    3    13             50
3       A    5    15            100
4       B    2    12              1
5       B    4    NA             NA
6       B    6    16            100


# NOT CORRECT
# NOT CORRECT 'do' programming - will return 6 rows instead of 3
# > do(df_g, hdntile(.[c(1,3,5),],"price") )

# test grouped with NA - one group has one group member that is a monopoly

> df
houseID year price
1       A    1    11
2       B    2    12
3       A    3    13
4       B    4    NA
5       A    5    15
6       B    6    16

# 5 NA & 6 NA           
> df[6,"price"] <- NA
# HARD DANGEROUS NOTE: df & df_g ARE NOW differernt dataframes: must re-run; df_g  <- group_by(df, houseID) 


> df
houseID year price
1       A    1    11
2       B    2    12
3       A    3    13
4       B    4    NA
5       A    5    15
6       B    6    NA

# HARD DANGEROUS NOTE: df & df_g ARE NOW differernt dataframes: must re-run; df_g  <- group_by(df, houseID) 

> df_g  <- group_by(df, houseID)

> df_g
Source: local data frame [6 x 3]
Groups: houseID

houseID year price
1       A    1    11
2       B    2    12
3       A    3    13
4       B    4    NA
5       A    5    15
6       B    6    NA


> do(df_g, hdntile(.,"price") ) #  monopolyhdntile == "buckets"

houseID year price price_NTILE100
1       A    1    11              1
2       A    3    13             50
3       A    5    15            100
4       B    2    12            100
5       B    4    NA             NA
6       B    6    NA             NA

> do(df_g, hdntile(.,"price", monopolyhdntile = "bucketsDIVtwo") ) 

houseID year price price_NTILE100
1       A    1    11              1
2       A    3    13             50
3       A    5    15            100
4       B    2    12             50
5       B    4    NA             NA
6       B    6    NA             NA

do(df_g, hdntile(.,"price", monopolyhdntile = NA ) ) 

houseID year price price_NTILE100
1       A    1    11              1
2       A    3    13             50
3       A    5    15            100
4       B    2    12             NA
5       B    4    NA             NA
6       B    6    NA             NA

do(df_g, hdntile(.,"price", monopolyhdntile = 99 ) ) 

houseID year price price_NTILE100
1       A    1    11              1
2       A    3    13             50
3       A    5    15            100
4       B    2    12             99
5       B    4    NA             NA
6       B    6    NA             NA


# test group without NA

df <- data.frame(
  houseID = c("A","A","B","B","C","C")
  , year   = 1:6
  , price  = 11:16
  , stringsAsFactors = FALSE
)


df_g  <- group_by(df, houseID)


> do(df_g, hdntile(.,"price") )


houseID year price price_NTILE100
1       A    1    11              1
2       A    2    12            100
3       B    3    13              1
4       B    4    14            100
5       C    5    15              1
6       C    6    16            100


# test grouped - buckets

> do(df_g, hdntile(.,"price", buckets = 2) )

houseID year price price_NTILE2
1       A    1    11            1
2       A    2    12            2
3       B    3    13            1
4       B    4    14            2
5       C    5    15            1
6       C    6    16            2

# test grouped - new_suffix_expl_name

> do(df_g, hdntile(.,"price", new_suffix_expl_name = "QUANTILE") )

houseID year price price_QUANTILE
1       A    1    11              1
2       A    2    12            100
3       B    3    13              1
4       B    4    14            100
5       C    5    15              1
6       C    6    16            100

# test grouped - new_word_explicit_name

> do(df_g, hdntile(.,"price", new_word_explicit_name = "MYQUANTILEGREAT_OF_TODAY") )

houseID year price MYQUANTILEGREAT_OF_TODAY
1       A    1    11                        1
2       A    2    12                      100
3       B    3    13                        1
4       B    4    14                      100
5       C    5    15                        1
6       C    6    16                      100



##################### end of hdnile testing ################################

############# begin hdrank work ############################


# posneg   higher values ... lower hdrank 

hdrank  <- function(  x  = NULL                      # required: 'subset (grouped)'  dataframe
                      , measurecol = NULL              # required:  measure column name string 
                      , buckets = 100                  # divisions integer
                      , posneg = -1                    # 1  - higher values = higher measure, -1 - higher values lower measure  integer
                      , new_suffix_type = "functmeas"  # new column suffix typically 'functmeas' _NTILE100 string                        , mutually exclusive of ( new_suffix_name, new_word_explicit_name )
                      , new_suffix_expl_name = NULL    # explicity name the suffix e.g. "_MYNTILEGREAT" string                          , mutually exclusive of ( new_suffix_type, new_word_explicit_name )
                      , new_word_explicit_name = NULL  # explicity name the the entire new column e.g. "MYQUANTILEGREAT_OF_TODAY" string , mutually exclusive of ( new_suffix_type, new_suffix_name )
                      , monopolyhdntile  = 1           # if only one firm, how does it compare against itself? 
                                                          # NA - hdquantile default                  literal NA
                                                          # "buckets" - use buckets                   string
                                                          # "bucketsDIVtwo" use trunc(buckets/2)      string
                                                          #  1 (  hdrank default  ), 2,50, 100 or any hard coded  literal integer                      
                      , measurename = "RANK"            # name the 'funct' part of 'functmeas' ( of 'measurecol'_functmeas' )
                      , ...                             # other parameters passed to hdntile and base::cut.default and Hmisc::hdquantile                  
) {
  
  hdntile(            
    x  = x                      
    , measurecol = measurecol              
    , buckets = buckets                  
    , posneg = posneg                   
    , new_suffix_type = new_suffix_type                  
    , new_suffix_expl_name = new_suffix_expl_name    
    , new_word_explicit_name = new_word_explicit_name 
    , monopolyhdntile  = monopolyhdntile 
    , measurename = measurename          
    , ...                                                
  )
  
}




############# end hdrank work ############################

############# begin hdrank test ##########################

# NOT WORK
# hdntile(...)

# NOT WORK
# hdntile( x = x  , measurecol = measurecol, ... )


# tested grouped


df <- data.frame(
  houseID = c("A","B","A","B","A","B")
  , year   = 1:6
  , price  = 11:16
  , stringsAsFactors = FALSE
)

> df_g  <- group_by(df, houseID)


> do(df_g, hdrank(.,"price") )

Source: local data frame [6 x 4]
Groups: houseID

houseID year price price_RANK100
1       A    1    11           100
2       A    3    13            50
3       A    5    15             1
4       B    2    12           100
5       B    4    14            50
6       B    6    16             1
>
  
  
  
  ############# end hdrank test ##########################

