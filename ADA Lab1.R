#Data Types

class(50)
class(93.2)
class(200L)
class(5+3i)
class('goals')
class("are sublime")
class(TRUE)
cat('\n')


#Assigning values to variables

x<-b<-1729
y<-43.21
k<-2+4i
z<-'ADA'
a<-"FDA"
cat('\n')



#Addition:

c<-x+b
c
d<-paste(z,' & ',a)
d
cat('\n')

#Numeric type conversions

as.integer(x)
as.complex(y)
as.complex(b)
as.numeric(k) #Will give warning
as.numeric(k) #Will give warning
cat('\n')



#Operations

max(2,3,4) #Maximum value parameter (signed)
min(5,6,7) #Minimum value parameter (signed)
abs(-3.77) #Absolute value of input (unsigned)
abs(3+3i) #Gives: sqrt(x^2+y^2) for x + yi
ceiling(7.1) #Next largest integer
floor(7.1) #Next smallest integer

nchar(d) #Length of character array (string)
grepl('&',d) #Check if character present in string
cat('\n')

#Escape characters

\" #Quotations
\\ #Backslash
\n #New Line
\r #Carriage return
\t Tab
\b Backspace

w<-'I wanted to do \r Python for ADA'
cat(w)
w<-'I wanted to do \\ Python for ADA'
cat(w)
w<-'I wanted to do \"Python\"for ADA'
cat(w)
w<-'I wanted to do \n Python for ADA'
cat(w)
w<-'I wanted to do \t Python for ADA'
cat(w)
w<-'I wanted to do \b Python for ADA'
cat(w)
cat('\n')


#Booleans
#TRUE and FALSE

12 > 3
5 == 7
12 < 3
#Cannot compare complex number wih numerics
cat('\n')


#Operators:

#Arithmetic

1+2
3-4
5*6
7/8
9^3
19%%3
28 %/% 5
cat('\n')


#Comparison

2 == 4
5 != 3
3 > 5
1 < 8
6 >= 99
45 <= 7
cat('\n')

#Logical

7 & 4
9 && 5
2 | 6
1 || 7
2 != 8
v<-c(1,2,3,4,5,66,7,8,9)
1 %in% v


#If, Else and ELse If

if (x>y) {
	print(paste(x,' is greater than ',y))
}

if (x>y) {
	print(paste(x,' is greater than ',y))
} else {
	print(paste(y,' is greater than ',x))
}
cat('\n')

if (x>y) {
	print(paste(x,' is greater than ',y))
} else if (y>x) {
	print(paste(y,' is greater than ',x))
} else {
	print('They are equal')
}
cat('\n')

i<-3
j<-6

if (j>3 & i<6) {
	cat(Im going In')
}

if (j>3 | i>6) {
	print('Im still going In')
}
cat('\n')

#Nested

if (12>7) {
	if (7<5){
		print(paste(5,' is greater than ',7))
	} else {
		print(paste(7,' is greater than ',5))
	}
} else {
	print(paste(12,' is greater than ',7))
}
cat('\n')


#Loops:

#While

i<-1
while (i < 6) {
  print(i)
  i <- i + 1
}
cat('\n')

i<-1
while (i < 6) {
  print(i)
  i <- i + 1
  if (i == 4) {# break stops loop prematurely
    break 
  }
}
cat('\n')

i<-1
while (i < 6) {
  i <- i + 1
  if (i == 3) {# skip control to next iteration
    next
  }
  print(i)
}
cat('\n')


#For

for (i in 1:7){
	print(i*i)
}

for (u in v){
	print(paste(u,'*2 = ',u*2))
}

#Nested for

s<-0
for (u in v){
	for (i in 1:u){
		 s<-s+i
	}
}
print(s)


#User defined functions

this_func<- function (x, y, z) {
	print(paste('This function prints ',x,y,z))
}

this_func(1,2,3)


psudo_func <- function(a, b) {
	c<-a*b
	return (c) #return statement
}

psudo_func(psudo_func(2,1),psudo_func(2,2)) #nested function calls


