
#load data into data frame
movies<-read.csv(file = "movie.csv",header = T,stringsAsFactors = F)
search<-read.csv(file = "search.csv",header = T)
ratings<-read.csv(file = "ratings.csv",header = T)

#library
library("reshape2")
library(recommenderlab)
library(ggplot2)
library(data.table)

summary(movies)
summary(ratings)
head(movies)
head(ratings)

genres<-as.data.frame(movies$genres,stringsAsFactors = FALSE)

genres2<-as.data.frame(tstrsplit(genres[,1],'[|]',
                                 type.convert = TRUE),
                       stringsAsFactors = FALSE)
str(genres2)

colnames(genres2)<-c(1:10)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
colnames(genres2)<-genre_list

genres_matrix<-matrix(0,10330,18)

genres_matrix[1,]<-genre_list
colnames(genres_matrix)<-genre_list

#iterate through matrix
for(i in 1:nrow(genres2)){
  for(c in 1:ncol(genres2)){
    genmat_col=which(genres_matrix[1,]==genres2[i,c])
      genres_matrix[i+1,genmat_col]<-1
  }
}

genres_matrix2<-as.data.frame(genres_matrix[-1,],stringsAsFactors = F)

class(genres_matrix2)
str(genres_matrix2)
for(c in 1:ncol(genres_matrix2)){
  genres_matrix2[,c]<-as.integer(genres_matrix2[,c])
}
str(genres_matrix2)

head(genres_matrix2)

search_matrix<-cbind(movies[,1:2],genres_matrix2)
head(search_matrix)
str(ratings)
ratingmat<-dcast(ratings,userId~movieId,value.var="rating",na.rm=F)
ratingmat<-as.matrix(ratingmat[,-1])
ratingmat<-as(ratingmat,"realRatingMatrix")

movieId<-length(unique(movies$movieId))
ratingmovieId<-length(unique(ratings$movieId))
