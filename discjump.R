rm(list=ls())
set.seed(123)

accumulate = function(list) {
  # cumulative sums
  acc = 0
  for (i in 1:length(list)) {
    list[i] = list[i] + acc
    acc = list[i]
  }
  list
}

twostates = function(states) {
  # two different states for x & y
  x = sample(0:(states-1), 1)
  y = sample(0:(states-1), 1)
  while( y == x) {
    y = sample(0:(states-1), 1)
  }
  c(x,y)
}

sorter = function(matrix, column) {
  matrix[order(matrix[,column]),]
}

nextmove = function(curr, other, states) {
  n = 0
  if (runif(1) < 0.5) {
    n = (curr-1)%%states
  } else {
    n = (curr+1)%%states
  }
  if (other == n) {n = curr}
  n
}

discjump = function(states, ticks) {
  # two different states for x & y
  xy = twostates(states)
  x = xy[1]
  y = xy[2]
  
  # two poisson clocks
  xclock = accumulate(rexp(ticks,rate = 1)) 
  yclock = accumulate(rexp(ticks,rate = 1))
  
  # two matrices
  # store the position of x & y at distinct time interval
  # col1 - the time
  # col2 - x position
  # col3 - y position
  # col4 - |x-y|
  xmat = matrix(data=c(xclock, rep(x,ticks), rep(-1,ticks), rep(-1,ticks)),nrow=ticks, ncol=4)
  ymat = matrix(data=c(yclock, rep(-1,ticks),rep(y,ticks), rep(-1,ticks)),nrow=ticks, ncol=4)
  
  # sort the combined xy matrix by first column time
  xymat = sorter(rbind(xmat, ymat), 1)
  
  # iterate thru matrix one row at a time
  # move x (or y) left (or right) so long as it is unoccupied
  for(i in 1:(2*ticks)) {
    if (xymat[i,2] > 0) {
      # move x
      x = nextmove(x, y, states)
    } else {
      # move y
      y = nextmove(y, x, states)
    }
    xymat[i,2] = x
    xymat[i,3] = y
    xymat[i,4] = abs(x-y)
  }
  xymat
}

findmean = function(mat) {
  # find the maximum time in column 1
  # take prev 1000 seconds
  # at each second, find the gap between x & y if available
  col1 = t(mat)[1,]
  mintime = floor(max(col1)) - 1000
  counter = 0
  count = 0
  for(i in mintime:mintime+1000) {
    cf = c(which(floor(col1) == i, arr.ind = TRUE))
    if (length(cf) > 0) {
      ind = min(cf)
      count = count + mat[ind,4]
      counter = counter + 1
    }
  }
  count/counter
}


# TEST CODE
print(discjump(states=20,ticks=50))

# Try a bunch of states, such as 4,8,12,16...upto 400
statesmean=matrix(NA, nrow = 100, ncol=2)
for(i in 1:100) {
  states = 4*i
  statesmean[i,1] = states
  statesmean[i,2] = findmean(discjump(states=states,ticks=10000))
}

print(statesmean)
print(summary(lm(t(statesmean)[2,]~t(statesmean)[1,]-1)))