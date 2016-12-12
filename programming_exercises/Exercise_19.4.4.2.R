#------------------------------
# Exercise 19.4.4 R4DS: Write a greeting function that says “good morning”, “good afternoon”, or “good evening”, depending on the time of day.   
#------------------------------
greeting<-function() {
  time<-lubridate::now()
  if (strptime("05:00", "%H:%M")<=time && time<strptime("12:00", "%H:%M")) {
    paste("Good morning!")
  } else if (strptime("12:00", "%H:%M")<=time && time<strptime("17:00", "%H:%M")) {
      paste("Good afternoon!")
  } else if (strptime("17:00", "%H:%M")<=time && time<strptime("21:00", "%H:%M")) { 
      paste("Good evening!")
  } else if (strptime("21:00", "%H:%M")<=time && time<strptime("05:00", "%H:%M")) {
      paste("Good night!")
  }
}  
greeting()  

