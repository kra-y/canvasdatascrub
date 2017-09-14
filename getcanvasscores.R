##################################################################################################################################
# THIS IS THE CODE TO ACTUALLY PULL THE CANVCAS DATA #############################################################################
##################################################################################################################################

users<-vector(mode = "list", length = dim(canvas)[1])
assignments<-vector(mode = "list",length = dim(canvas)[1])
exam_ids<-vector(mode = "list",length = dim(canvas)[1])
m1<-vector(mode = "list",length = dim(canvas)[1])
m2<-vector(mode = "list",length = dim(canvas)[1])
f<-vector(mode = "list",length = dim(canvas)[1])
scores<-vector(mode = "list",length = dim(canvas)[1])

##################################################################################################################################


for(i in 1:length(course.codes)){
  users[[i]]<-get_course_items(course.codes[i],item = "users")[,c('name','id')]
  names(users[[i]])[2]<-"user_id"
  
  assignments[[i]]<-get_course_analytics_data(course.codes[i])[,c("assignment_id","title")]
  assignments[[i]]$title<-as.factor(assignments[[i]]$title)
  exam_ids[[i]]<-subset(assignments[[i]],grepl("Midterm|Final",title) & !grepl("Study|Object|Review|Group|Retake",title))
  
  m1[[i]]<-get_submissions(course.codes[i],"assignments",exam_ids[[i]][which(grepl("Midterm 1",exam_ids[[i]]$title)),1])[,c("user_id","score")]
  names(m1[[i]])[2]<-"Midterm 1"
  m2[[i]]<-get_submissions(course.codes[i],"assignments",exam_ids[[i]][which(grepl("Midterm 2",exam_ids[[i]]$title)),1])[,c(8,5)]
  names(m2[[i]])[2]<-"Midterm 2"
  f[[i]]<-get_submissions(course.codes[i],"assignments",exam_ids[[i]][which(grepl("Final",exam_ids[[i]]$title)),1])[,c(8,5)]
  names(f[[i]])[2]<-"Final Exam"
  scores[[i]]<-dplyr::left_join(users[[i]],m1[[i]],by.x = "user_id")%>%
    left_join(m2[[i]],by ="user_id")%>%
    left_join(f[[i]],by ="user_id")
  scores[[i]]<-scores[[i]][,c("user_id","name","Midterm 1","Midterm 2","Final Exam")]
  names(scores[[i]])<-c("user_id","user.name","Midterm 1","Midterm 2","Final Exam")
  scores[[i]]$Section<-canvas[i,"Section"]
}

sum980examcanvas<-do.call("rbind",scores)