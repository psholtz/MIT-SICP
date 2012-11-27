#!/usr/bin/python

# 
# Working definitions (python)
#
def make_units(C,L,H):
  return [C,L,H]
def get_units_C(x):
  return x[0]
def get_units_L(x):
  return x[1]
def get_units_H(x):
  return x[2]

def make_class(number,units):
  return [number,units]
def get_class_number(x):
  return x[0]
def get_class_units(x):
  return x[1]

def get_class_total_units(klass):
  units = get_class_units(klass)
  return get_units_C(units) + get_units_L(units) + get_units_H(units)

def same_class(klass1,klass2):
  return get_class_number(klass1) == get_class_number(klass2)

#
# Previous Solutions
#
def empty_schedule():
  return []

def add_class(klass,schedule):
  schedule.append(klass)
  return schedule

def total_scheduled_units(schedule):
  """ Return the total number of units in an entire schedule """
  
  def total_scheduled_units_iter(total,working):
    """ Iterative process to recursively count the number of units """
    if not working:
      return total
    else:
      return total_scheduled_units_iter(total + get_class_total_units(working[0]), working[1:])

  # Make the recursive call to the inner iterative function
  return total_scheduled_units_iter(0,schedule)

#
# Exercise 4
#
# Write a procedure that drops a particular class from a schedule
#
def drop_class(schedule,classnum):
  #
  # Define the predicate used to filter the schedule.
  #
  def predicate(klass):
    return get_class_number(klass) != classnum

  #
  # Filter out the classes we want to delete:
  #
  return filter(predicate, schedule)

#
# Run some unit tests:
#
calculus_1 = make_class("CALC-101", make_units(4,4,4))
calculus_2 = make_class("CALC-102", make_units(4,4,4))
algebra = make_class("ALGB-152", make_units(3,3,3))
diff_eqs = make_class("DIFF-201", make_units(3,3,3))

s1 = empty_schedule()
s1 = add_class(calculus_1, s1)
s1 = add_class(algebra, s1)
s1 = add_class(diff_eqs, s1)

#
# Introspecting s1:
#
# [['CALC-101', [4, 4, 4]], ['ALGB-152', [3, 3, 3]], ['DIFF-201', [3, 3, 3]]]
#
print "Schedule: ", s1

#
# Now try to drop a class:
#
s1 = drop_class(s1, "CALC-101")
print "Schedule: ", s1
# ==> [['ALGB-152', [3, 3, 3]], ['DIFF-201', [3, 3, 3]]]

#
# The order of growth is linear in both time and space in the variable
# "schedule", that is, it is O(n) where "n" is the length of the list
# structure "schedule".
#
