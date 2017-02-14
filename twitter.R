tweets=read.csv("tweets.csv",stringsAsFactors = FALSE)

tweets$NEgative=as.factor(tweets$Avg<=-1)
table(tweets$NEgative)

"Output
 FALSE  TRUE 
 999   182" 

"Libraries to load to do text analysis"
library("SnowballC")
library("tm")

corpus=Corpus(VectorSource(tweets$Tweet))

"Changing all the alphabets to lower case"
corpus=tm_map(corpus,tolower)

corpus=tm_map(corpus,removePunctuation)

corpus=tm_map(corpus,removeWords,c("apple",stopwords("english")))

corpus<-tm_map(corpus,PlainTextDocument)

frequencies=DocumentTermMatrix(corpus)

findFreqTerms(frequencies,lowfreq = 20)

 [1] "android"              "app"                 
[3] "apples"               "back"                
[5] "better"               "can"                 
[7] "cant"                 "dont"                
[9] "fingerprint"          "freak"               
[11] "get"                  "google"              
[13] "ios"                  "ios7"                
[15] "ipad"                 "iphone"              
[17] "iphone5c"             "iphone5s"            
[19] "iphones"              "ipod"                
[21] "ipodplayerpromo"      "itun"                
[23] "itunes"               "just"                
[25] "like"                 "lol"                 
[27] "love"                 "make"                
[29] "microsoft"            "need"                
[31] "new"                  "now"                 
[33] "one"                  "phone"               
[35] "promo"                "promoipodplayerpromo"
[37] "really"               "samsung"             
[39] "store"                "thanks"              
[41] "think"                "time"                
[43] "twitter"              "via"                 
[45] "want"                 "well"                
[47] "will"              
 
  sparse=removeSparseTerms(frequencies,0.995)
"<<DocumentTermMatrix (documents: 1181, terms: 285)>>
  Non-/sparse entries: 4068/332517
Sparsity           : 99%
Maximal term length: 20
Weighting          : term frequency (tf)
"

tweetsSparse=as.data.frame(as.matrix(sparse))
colnames(tweetsSparse)=make.names(colnames(tweetsSparse))

tweetsSparse$Negative=tweets$NEgative
library(caTools)
set.seed(123)
split=sample.split(tweetsSparse$Negative,SplitRatio = 0.7)
trainSparse=subset(tweetSparse,split==TRUE)

library(rpart)
library(rpart.plot)
tweetCart=rpart(Negative~ .,data=trainSparse,method='class')
 
predictCart=predict(tweetCart,newdata=testSparse,type='class')
table(testSparse$Negative,predictCart) 

predictRF
"output
       FALSE TRUE
FALSE   284   16
TRUE     31   24
"
(284+24)/(16+31+284+24)
"Output
[1] 0.8676056
"
  
