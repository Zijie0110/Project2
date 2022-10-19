# Project2

#Question 1

# name a function - pone
Pone=function(n,k,strategy,nreps=10000){
  box=sample(1:n) #pick a box randomly
  num_of_success = 0 #record the number of success
  if (strategy==1){  # begin with strategy 1
    for (i in c(1:nreps)){
      num_of_open=0   #record the number of open
      number=k  #open the first box,i.e.k
      # prisoner starts at the box with number on it,i.e.k,If k is not their prisoner number, they go to box number k, open it and repeat the process until they have either found the card with their number on it, or opened n boxes without finding it.
      while (box[number]!=k && time<=(n/2)){
        num_of_open=num_of_open+1
        number=box[number]
      }
      if (num_of_open<=(n/2)){
        num_of_success=num_of_success+1
      }
      box <- sample(1:n)
    }
    print(a/nreps)
  }
    
  if (strategy==2){
    for (i in c(1:nreps)){
      time=0
      number=sample(n,1)
      while (box[number]!=k && time<=(n/2)){
        time = time+1
        number = box[number]
      }
      if (time<=(n/2)){
        a=a+1
      }
      box <- sample(1:n)
    }
    print(a/nreps)
  }
  
  if (strategy==3){
    for(i in c(1:nreps)){
      number=sample(n,n/2)
      if(k %in% box[number]){
        a=a+1
      }
     box <- sample(1:n) 
    }
    print(a/nreps)
  }
  
  }

Pone(500,1,3,10000)    
Pone(50,1,3,10000)
Pone(5,1,3,10000)
