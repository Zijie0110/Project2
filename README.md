# Project2

#Question 1

# name a function - pone
Pone=function(n,k,strategy,nreps=10000){
  N=2*n
  box=sample(1:N) #pick a box randomly
  num_of_success = 0 #record the number of success
  if (strategy==1){  # begin with strategy 1
    for (i in c(1:nreps)){
      num_of_open=1   #record the number of open
      number=k  #open the first box,i.e.k
      # prisoner starts at the box with number on it,i.e.k,If k is not their prisoner number, they go to box number k, open it and repeat the process until they have either found the card with their number on it, or opened n boxes without finding it.
      while (box[number]!=k && num_of_open<=(n)){
        num_of_open=num_of_open+1
        number=box[number]
      }
      if (num_of_open<=(n)&& box[number]==k){
        num_of_success=num_of_success+1
      }
      box = sample(1:N)
    }
    print(num_of_success/nreps)
  }
  #strategy 2 The first box opened was random
  if (strategy==2){
    for (i in c(1:nreps)){
      num_of_open=1   #record the number of open
      number=sample(N,1) #choose a box randomly
      #prisoner starts at box randomly, and if the card number of the box is not their prisoner number, they go to the box which number
      #is the card number of the first box, open it and repeat the process until they have found the card their number on it.
      while (box[number]!=k && num_of_open<=(n)){
        num_of_open=num_of_open+1
        number = box[number]
      }
      if (num_of_open<=(n)&& box[number]==k ){
        num_of_success=num_of_success+1
      }
      box = sample(1:N) #Random box placement
      #repeat nreps'times and calculate the total times of success of each prisoner
    }
    print(num_of_success/nreps)
  }
  
  if (strategy==3){  # begin with strategy 3
    for(i in c(1:nreps)){
      number=sample(N,n)  # open n boxes from n boxes
      if(k %in% box[number]){
        num_of_success=num_of_success+1  
      }
      box = sample(1:N)  # pick a box randomly
      #  open n boxes at random, checking each card for their number
    }
    print(num_of_success/nreps)  # calculate the probability of strategy 3
  }
  
}
# example for n=5 and n=50
Pone(5,1,1,10000)    
Pone(50,1,1,10000)
Pone(5,1,2,10000)
Pone(50,1,2,10000)
Pone(5,1,3,10000)
Pone(50,1,3,10000)

# Question 2
# Use the function success to judge whether each prisoner can succeed in an experiment
success=function(n,k,strategy,box){
  N=2*n  #total number of prisoners
  if(k<=N){
    num_of_success=0 #Record whether an experiment is successful
    #strategy 1 The first box opened was the prisoner's number
    if(strategy==1){
      num_of_open=1 #record the number of open
      number=k  #Open the first box. The number of box is the prisoner number. i.e.k
      while (box[number]!=k & num_of_open<=(n)){
        num_of_open=num_of_open+1 
        number=box[number]
      }
      if(num_of_open<=(n)){
        num_of_success=1 #prisoner success
      }
      return(num_of_success)
    }
    #strategy 2 The first box opened was random
    if(strategy==2){
      num_of_open=1 #record the number of open
      number=sample(N,1)  #choose a box randomly
      while (box[number]!=k & num_of_open<=(n)){
        num_of_open=num_of_open+1
        number=box[number]
      }
      if(num_of_open<=(n)){
        num_of_success=1 #prisoner Success
      }
      return(num_of_success)
    }
    #strategy 3 Open n boxes randomly
    if(strategy==3){
      number=sample(N,n)
      if(k %in% box[number]){
        num_of_success=1
      }
      return(num_of_success)
    }
  }else
    return("wrong")
}

Pall=function(n,strategy,nreps=10000){
  N=2*n #total number of prisoners
  c=0 # Record whether all prisoners can succeed
  box=sample(1:N)
  #strategy 1 The first box opened was the prisoner's number
  if(strategy==1){
    for(i in c(1:nreps)){
      k=1 # The number of the first prisoner who opened the box is 1
      # Judge whether each prisoner can succeed
      while(success(n,k,1,box)==1 & k<=N){ 
        k=k+1
      }
      # Record the number of successful prisoners in each experiment
      if(k==(N+1)){ 
        c=c+1 
      }
    box=sample(1:N) 
    #repeat nreps times，calculate the total times of success of all prisoners
    }
    print(paste0('The probability of a prisoner succeeding in finding the prisoner number:',c / nreps))
  }
  #strategy 2 The first box opened was random
  if(strategy==2){
    for(i in c(1:nreps)){
      k=1 # The number of the first prisoner who opened the box is 1
      # Judge whether each prisoner can succeed
      while(success(n,k,2,box)==1 & k<=N){
        k=k+1
      }
      # Record the number of successful prisoners in each experiment
      if(k==(N+1)){
        c=c+1
      }
     box=sample(1:N) 
     #repeat nreps times，calculate the total times of success of all prisoners
    }
    print(paste0('The probability of a prisoner succeeding in finding the prisoner number:',c/nreps))
  }
  #strategy 3 Open n boxes randomly
  if(strategy==3){
    for(i in c(1:nreps)){
      k=1
      # Judge whether each prisoner can succeed
      while(success(n,k,3,box)==1 & k<=N){
        k=k+1
      }
      # Record the number of successful prisoners in each experiment
      if(k==(N+1)){
        c=c+1
      }
     box=sample(1:N)
     #repeat nreps times，calculate the total times of success of all prisoners
    }
    print(paste0('The probability of a prisoner succeeding in finding the prisoner number:',c/nreps))
  }
}
#Question 3
Pall(5,1,10000)    
Pall(50,1,10000)
Pall(5,2,10000)
Pall(50,2,10000)
Pall(5,3,10000)
Pall(50,3,10000)
#Question 4
#It improves their odds of a random chance by nearly 30 orders of mangnitude. 

#question 5
dloop=function(n,nreps){
  prisoners=c(1:(2*n))
  count_set=matrix(0,nrow=nreps,ncol=(2*n))
  for (i in 1:nreps){
    card=sample(1:(2*n),size=2*n,replace=FALSE)
    for (k in prisoners){
      card_index=vector(length=(2*n))
      card_index[1]=k
      times=1
      while(card[card_index[times]]!=k){
        card_index[times+1]=card[card_index[times]]
        times=times+1
      }
      count_set[i,times]=1
    }
  }
  return(colSums(count_set)/nreps)
}
dloop(50,10000)


#Question 6
barplot(dloop(50,10000),col ='blue')
