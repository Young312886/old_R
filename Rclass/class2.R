getwd() # identify the path of my workingfolder
### R Exercise 1 ###


1
a
'a'
'1'
class(1)   # valus type
class('1') #'1' value is now character(=string)

as.numeric('1') + as.numeric('2') #character value convert to  numerical value
# = 1+2 (important)
1; 2; 'a' # ; makes it able to type different value in one line 

## Arthmetic Operations ## works only on numerical

2 + 1
2 - 1
2 * 1
2 / 1

## Logical Expression ## very important
3 < 2
3 == 1 + 2
3 != 1 + 2 # != is 'not equal'
'A' == 'a' # ' = " *case sensative*

## Variables ## 'var'

a <- 3 # <- means '3' is assigned to storage/variable
a
b = 3 # '<-' = '=' but not recommended
b

var1 <- 'aaa' # var = values
var2 <- 111
var3 <- Sys.Date()

var1
var2
var3

class(var1)
class(var2)
class(var3)

num1 <- 1
num2 <- 2

num1 + num2 # can use vectors

num3 <- '3' # '3' is string
num1 + num3 # error

num1 + as.numeric(num3)

#variables is singular value and vectors is multiple value

## Vectors ## 'vec' # 'int' = 정수

vec1 <- 1:5 ; vec1  # ':' = 'seq' 
vec2 <- seq(1,5); vec2 # vec# 다시쓰는 이유 = 보여주기위함
vec3 <- c(4,5,6,7,8) ; vec3 # manually adding 
vec4 <- c('a','b', 'c'); vec4 # also string # multiple adding 'c' = combine
vec5 <- c(4,'a'); vec5 # in one vector only one type of input values

class(vec5)

vec1 * 3
vec1 * vec3

vec6 <- vec1 * vec3 ; vec6
vec7 <- 1:6

vec6
vec6[3] # 3rd elements
vec6[-3] # except 3rd elements
vec6[2:4] #2nd to 4th
vec6[c(3,5)] # 3rd and 5th

vec6[4] <- 20 # change elements

vec6 < 30 # evaluate true or false
vec6[vec6<30] # show elements

names(vec6) <- c('First', 'Second', 'Third', 'Fourth', 'Fifth')
vec6 # naming(label) elements

vec6['Second']  # get 'Second' value
vec6['Second'] <- 50 # change
vec6['Second']  

length(vec6) #number of elements

names(vec7) <- c('First', 'Second', 'Third', 'Fourth', 'Fifth')
vec7
vec7[c(3,5)]


## Variables/Vector Management ##

objects() #display all variables
rm(a) # rm = remove
rm(vec1)
objects() 
 
rm(list=ls()) # = clear the list or use button on right
objets()

## Functions ##

log(10)
help(log) # research on right
log(4,base=2)
example(log) #show example
