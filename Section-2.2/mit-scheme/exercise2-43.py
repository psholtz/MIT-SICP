#!/usr/bin/python

"""
Analyzing the performance of an algorithm is sometime easier for me to 
visualize in Python than in Lisp; hence why I hacked this out in Python.
"""

total_bad = 0
total_good = 0

def accumulate(op,init,seq):
 if len(seq) == 0:
  return init
 else:
  return op(seq[0],accumulate(op,init,seq[1:]))

def append(a,b):
 return a+b

def flatmap(proc,seq):
 return accumulate(append,[],map(proc,seq))

def enumerate_interval(low,high):
 return range(low,high+1)

empty_board = []

def adjoin_position(new_row,column,rest_of_queens):
 """ hack needed to get it to work """
 a = [new_row]
 for x in rest_of_queens:
  a.append(x)
 return a

def safe(column,positions):
 def safe_iter(new_row,remaining_rows,row_offset):
  if len(remaining_rows) == 0:
   return True
  else:
   current_row = remaining_rows[0]
   if (new_row == current_row) or (new_row == current_row+row_offset) or (new_row == current_row-row_offset):
    return False
   else:
    return safe_iter(new_row,remaining_rows[1:],row_offset+1)
 return safe_iter(positions[0],positions[1:],1)

def queens(board_size):
 def queen_cols(k):
  global total_good
  total_good += 1
  if k==0:
   return [empty_board]
  else:
   return filter(lambda positions: safe(k,positions), flatmap(lambda rest_of_queens: map(lambda new_row: adjoin_position(new_row,k,rest_of_queens),enumerate_interval(1,board_size)),queen_cols(k-1)))
 return queen_cols(board_size)

def queens_bad(board_size):
 def queen_cols(k):
  global total_bad
  total_bad += 1
  if k==0:
   return [empty_board]
  else:
   return filter(lambda positions: safe(k,positions), flatmap(lambda new_row:map(lambda rest_of_queens:adjoin_position(new_row,k,rest_of_queens),queen_cols(k-1)),enumerate_interval(1,board_size)))
 return queen_cols(board_size)

def run_bad():
 #print queens_bad(1)
 #print queens_bad(2)
 #print queens_bad(3)
 #print queens_bad(4)
 #print len(queens_bad(5))
 #print len(queens_bad(6))
 #print len(queens_bad(7))
 print len(queens_bad(8))

def run_good():
 #print queens(1)
 #print queens(2)
 #print queens(3)
 #print queens(4)
 #print len(queens(5))
 #print len(queens(6))
 #print len(queens(7))
 print len(queens(8))

print "running bad"
run_bad()

print "running good"
run_good()

print "bad count: ", total_bad
print "good count: ", total_good

# 
# n(total) for 2 ==> 7
# n(total) for 3 ==> 40
# n(total) for 4 ==> 341
# n(total) for 5 ==> 3,906
# n(total) for 6 ==> 55,987
# n(total) for 7 ==> 960,800
# n(total) for 8 ==> 19,173,961
#

