library("RCurl")
library("rvest")

is.jew<-function(name){
  url<-paste("http://www.jewornotjew.com/search.jsp?SEARCH=",gsub(" ","+",name),sep="")
  site<-read_html(url)
  verdict<-html_node(site,"#verdict") %>% html_text
  if(is.na(verdict)){
    return("Error: no data available.")
  }
  else if(grepl("not",verdict,ignore.case = T)){
    return(F)
  }
  else{
    return(T)
  }
}

synonym<-function(phrase){
  common.words=c("the","be","to","of","and","a","in","that","have",
                 "i","it","for","not","on","with","he","as","you","do","at","this",
                 "but","his","by","from","they","we","say","her","she","or","an",
                 "will","my","one","all","would","there","their","what","so","up",
                 "out","if","about","who","get","which","go","me","is")
  phrase=strsplit(phrase," ")[[1]]
  for(word in 1:length(phrase)){
    if(!(tolower(phrase[word]) %in% common.words)){
      url=paste("https://www.thesaurus.com/browse/",phrase[word],sep="")
      if(url.exists(url)){
        site=read_html(url)
        synonyms=html_nodes(site,".etbu2a31") %>% html_text
        phrase[word]=sample(synonyms,1)
      }
    }
  }
  newphrase=""
  for (word in 1:length(phrase)){
    newphrase=paste(newphrase,phrase[word],sep="")
    newphrase=paste(newphrase," ",sep="")
  }
  newphrase=substring(newphrase,1,nchar(newphrase)-1)
  return(newphrase)
}

latest.tweet<-function(search,quotes=T){
  if(quotes){
    url<-"https://twitter.com/search?f=tweets&q=%22"
  }else{
    url<-"https://twitter.com/search?f=tweets&q="
  }
  url=paste(url,gsub(" ","%20",search),sep="")
  if(quotes){
    url=paste(url,"%22&src=typd",sep="")
  }else{
    url=paste(url,"&src=typd",sep="")
  }
  site<-read_html(url)
  tweet=html_node(site,".tweet-text") %>% html_text
  handle=html_text(html_nodes(site,".u-textTruncate"))[2]
  date=html_text(html_node(site,".js-short-timestamp"))
  l=list("tweet"=tweet,"user"=handle,"date"=date)
  return(l)
}

ebay.price<-function(item,type="all"){
  url<-"https://www.ebay.com/sch/i.html?_from=R40&_nkw="
  url<-paste(url,gsub(" ","+",item),sep="")
  url<-paste(url,"&_sacat=0&LH_TitleDesc=0&rt=nc&LH_Sold=1&LH_Complete=1",sep="")
  site<-read_html(url)
  prices<-html_nodes(site,".POSITIVE") %>% html_text() 
  prices<-gsub("\\$","",prices) %>% as.numeric()
  if(type=="mean"){
    return(mean(prices))
  }else if(type=="median"){
    return(median(prices))
  }else if(type=="min"){
    return(min(prices))
  }else if(type=="max"){
    return(max(prices))
  }else if(type=="table"){
    m=matrix(c(mean(prices),median(prices),min(prices),max(prices)),ncol=1)
    rownames(m)=c("mean","median","min","max")
    return(m)
  }else{
    return(sort(prices))
  }
}

sharks.score<-function(){
  site<-read_html('http://www.espn.com/nhl/team/_/name/sj/san-jose-sharks')
  ans="The Sharks"
  if(html_node(site,".h8.db") %>% html_text()=="L")
    ans=paste(ans,"lost")
  else{
    ans=paste(ans,"won")
  }
  ans=paste(ans,html_node(site,".tar") %>% html_text())
  ans=paste(ans,"against The")
  ans=paste(ans,html_node(site,".Schedule__Game--post .truncate") %>% html_text())
  return(ans)
}

temperature<-function(zip,scale="F"){
  url=paste("https://weather.com/weather/today/l/",zip,sep="")
  url=paste(url,":4:US",sep="")
  webpage <- read_html(url)
  temp <- html_text(html_nodes(webpage,'.today_nowcard-temp'))
  if(scale=="C"){
    return(round((as.numeric(gsub("°","",temp))-32)*5/9))
  }
  else{
    return(as.numeric(gsub("°","",temp)))
  }
}

age<-function(name){
  newname=""
  for(i in 1:length(strsplit(name," ")[[1]])){
    subname=strsplit(name," ")[[1]][i]
    subname=paste(toupper(substring(subname,1,1)),substring(subname,2,nchar(subname)),sep="")
    newname=paste(newname,subname,sep="")
    newname=paste(newname,"_",sep="")
  }
  newname=substring(newname,1,nchar(newname)-1)
  url=paste("https://en.wikipedia.org/wiki/",newname,sep="")
  site<-read_html(url)
  ans=html_nodes(site,".ForceAgeToShow") %>% html_text()
  ans=gsub("[[:space:]]","",ans)
  ans=gsub(")","",ans)
  ans=gsub("\\(","",ans)
  ans=gsub("age","",ans)
  return(as.numeric(ans))
}

sub.trend<-function(){
  site<-read_html("http://reddit.com/r/all")
  subs<-html_nodes(site,".eWlZWl") %>% html_text
  l=as.list(subs)
  for (i in 1:length(subs)){
    l[[i]][2]=subdescription(gsub("r/","",l[[i]][1]))
  }
  return(l)
}

sub.descr<-function(sub){
  url=paste("http://reddit.com/r/",sub,sep="")
  site<-read_html(url)
  desc<-html_node(site,".bfVjJH") %>% html_text
  return(desc)
}

bandname<-function(){
site<-read_html("https://en.wikipedia.org/wiki/Special:Random")
string=strsplit(html_node(site,"#firstHeading")%>%html_text," ")[[1]][1]
site<-read_html("https://en.wikipedia.org/wiki/Special:Random")
string=paste(string,strsplit(html_node(site,"#firstHeading")%>%html_text," ")[[1]][1])
site<-read_html("https://en.wikipedia.org/wiki/Special:Random")
string=paste(string,strsplit(html_node(site,"#firstHeading")%>%html_text," ")[[1]][1])
return(string)
}

band2<-function(n=3){
  site<-read_html("https://en.wikipedia.org/wiki/Lists_of_musicians")
  links<-html_nodes(site,".column-width a") %>% html_text
  string=""
  for(i in 1:n){
   url="https://en.wikipedia.org/wiki/"
   url=paste(url,gsub(" ","_",sample(links,1)),sep="")
   site<-read_html(url)
   bands<-html_nodes(site,"#mw-content-text li a")%>%html_text
   string=paste(string,strsplit(sample(bands,1)," ")[[1]][1],sep="")
   string=paste(string,"")
  }
  return(substring(string,1,nchar(string)-1))
}