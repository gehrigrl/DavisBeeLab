import sys

filename = input("File Name? ")
first_day=int(input("First date? "))
file = open(filename, "r")
line = file.readline()


for j in range(2): 
 time = []
 bps = []

 print(line)
 while(line):
   numbers = line.split(',')

   if bool(time):
     if float(numbers[0]) < time[-1]:
       break 
  
   time.append(float(numbers[0]))
   bps.append(float(numbers[1]))

 
   line = file.readline()
 
 starttime = 252.778 #6:04 am
 endtime =  711.111 #5:04 pm
 interval = 3.472 #5 min

 slots = 132 #change if above data changes
 correctbeedataA = [0 for i in range(slots)]
 correctbeedataB = [0 for i in range(slots)]


 for i in range(slots):
   standardtime = starttime + (interval*i)

   #OPTION A
   # look for all the times in this interval (3.472)
   lowerinterval = standardtime - (interval / 2)
   higherinterval = standardtime + (interval / 2)

   possibleindex = i
   indexleft = i-1
   indexright = i+1

   while (abs(time[possibleindex] - lowerinterval) > abs(time[indexleft] - lowerinterval)) or (abs(time[possibleindex] - lowerinterval) > abs(time[indexright] - lowerinterval)):

     #change "possible index"
     if (abs(time[possibleindex] - lowerinterval) > abs(time[indexleft] - lowerinterval)): #correct to the left
       possibleindex = possibleindex - 1
       indexleft = possibleindex - 1
       indexright = possibleindex + 1

     elif (abs(time[possibleindex] - lowerinterval) > abs(time[indexright] - lowerinterval)): #correct to the right
       possibleindex = possibleindex + 1
       indexleft = possibleindex - 1        
       indexright = possibleindex + 1

    # assume closest to lowerinterval is possibleindex now

    #is possibleindex smaller or larger?
   lowerrange = 0
   if (time[possibleindex] < lowerinterval): #smaller
      lowerrange = possibleindex + 1
   elif (time[possibleindex] > lowerinterval): #larger
      lowerrange = possibleindex
   
   sum_points = 0
   num_points = 0
   range_count = lowerrange
   # average times
   while(time[range_count] <= higherinterval): #we're below higherinterval
    sum_points = sum_points + bps[range_count]
    range_count = range_count + 1
    num_points = num_points + 1
    if (range_count >= len(time)):
      break;
    
 
   if (num_points == 0):
     print("no data points in range - reevaluate plan")
     print(range_count)
     ave = (bps[range_count-1] + bps[range_count])/2
   else:  
     ave = sum_points / num_points
  
   #put ave in new array
   correctbeedataA[i] = ave #done!
  
  ##################################################
  #OPTION B - ave closest two
 
 stand_time_array = [(starttime + (interval*i)) for i in range(slots)]

 print("day " + str(first_day + j))
 for i in range(slots):
   print("%.3f" % (correctbeedataA[i]))
 print("\n")import sys

filename = input("File Name? ")
first_day=int(input("First date? "))
file = open(filename, "r")
line = file.readline()


for j in range(2): 
 time = []
 bps = []

 print(line)
 while(line):
   numbers = line.split(',')

   if bool(time):
     if float(numbers[0]) < time[-1]:
       break 
  
   time.append(float(numbers[0]))
   bps.append(float(numbers[1]))

 
   line = file.readline()
 
 starttime = 252.778 #6:04 am
 endtime =  711.111 #5:04 pm
 interval = 3.472 #5 min

 slots = 132 #change if above data changes
 correctbeedataA = [0 for i in range(slots)]
 correctbeedataB = [0 for i in range(slots)]


 for i in range(slots):
   standardtime = starttime + (interval*i)

   #OPTION A
   # look for all the times in this interval (3.472)
   lowerinterval = standardtime - (interval / 2)
   higherinterval = standardtime + (interval / 2)

   possibleindex = i
   indexleft = i-1
   indexright = i+1

   while (abs(time[possibleindex] - lowerinterval) > abs(time[indexleft] - lowerinterval)) or (abs(time[possibleindex] - lowerinterval) > abs(time[indexright] - lowerinterval)):

     #change "possible index"
     if (abs(time[possibleindex] - lowerinterval) > abs(time[indexleft] - lowerinterval)): #correct to the left
       possibleindex = possibleindex - 1
       indexleft = possibleindex - 1
       indexright = possibleindex + 1

     elif (abs(time[possibleindex] - lowerinterval) > abs(time[indexright] - lowerinterval)): #correct to the right
       possibleindex = possibleindex + 1
       indexleft = possibleindex - 1        
       indexright = possibleindex + 1

    # assume closest to lowerinterval is possibleindex now

    #is possibleindex smaller or larger?
   lowerrange = 0
   if (time[possibleindex] < lowerinterval): #smaller
      lowerrange = possibleindex + 1
   elif (time[possibleindex] > lowerinterval): #larger
      lowerrange = possibleindex
   
   sum_points = 0
   num_points = 0
   range_count = lowerrange
   # average times
   while(time[range_count] <= higherinterval): #we're below higherinterval
    sum_points = sum_points + bps[range_count]
    range_count = range_count + 1
    num_points = num_points + 1
    if (range_count >= len(time)):
      break;
    
 
   if (num_points == 0):
     print("no data points in range - reevaluate plan")
     print(range_count)
     ave = (bps[range_count-1] + bps[range_count])/2
   else:  
     ave = sum_points / num_points
  
   #put ave in new array
   correctbeedataA[i] = ave #done!
  
  ##################################################
  #OPTION B - ave closest two
 
 stand_time_array = [(starttime + (interval*i)) for i in range(slots)]

 print("day " + str(first_day + j))
 for i in range(slots):
   print("%.3f" % (correctbeedataA[i]))
 print("\n")
