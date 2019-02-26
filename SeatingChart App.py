# Seating Chart Application
 ''' # of guests are obtained from the user and so are the certain about of guests you must seat at each of the tables
 '''


def factorial(x):               # factorial function
    x_fact = 1 
    i = 0
    for i in range(1,x+1): 
        x_fact = x_fact * i 
    return x_fact

n = int(input('Enter the number of people\n'))              # gets input of total # of people ex. n =6
  
n_fact = factorial(n)                                       # takes the factorial of n using the factorial() function n!

list = []                                                  # creates list with # of people sitting at table 1 in it list = [2]
i = 1
while n > 0:                                               # 2 <= 6 
    s = int(input('Enter the number of chairs at table %d \n' %i))  # number of people at table 2 ex. s = 2
    if n > s:
        list.append(s)                                          # ex list = [2,2]
        n = n - s
        i += 1
        continue
    elif n < s:
        print('Seating only %d people at table %d' %(n,i))
        list.append(n)
        n = 0
        break
    else:
        list.append(s)
        n = 0
        break                                              

product = 1     
for x in list:
    fact = factorial(x)
    product *= fact

total = int(n_fact/ (product))
print('The number of table assignments is ', total)

