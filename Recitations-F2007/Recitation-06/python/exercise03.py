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

#
# Exercise 3
#
# Write a selector that takes in a schedule and returns the total 
# number of units in that schedule:
#
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
# Run some unit tests:
#
calculus_1 = make_class("CALC-101", make_units(4,4,4))
calculus_2 = make_class("CALC-102", make_units(4,4,4))
algebra = make_class("ALGB-152", make_units(3,3,3))
diff_eqs = make_class("DIFF-201", make_units(3,3,3))

#
# Display the Classes
#
print "Calculus 101 Total Units:", get_class_total_units(calculus_1)
# ==> 12 
print "Calculus 102 Total Units:", get_class_total_units(calculus_2)
# ==> 12
print "Algebra 152 Total Units:", get_class_total_units(algebra)
# ==> 9
print "Differential Equations 201 Total Units:", get_class_total_units(diff_eqs)
# ==> 9
print

#
# Display the Total Units
#
s1 = empty_schedule()
print "Total Scheduled Units:", total_scheduled_units(s1)

s1 = add_class(calculus_1, s1)
print "Total Scheduled Units:", total_scheduled_units(s1)

s1 = add_class(algebra, s1)
print "Total Scheduled  Units:", total_scheduled_units(s1)

s1 = add_class(diff_eqs, s1)
print "Total Scheduled Units:", total_scheduled_units(s1)

#
# The order of growth is linear in both time and space in the variable
# "schedule", that is, it is O(n) where "n" is the length of the list 
# structure "schedule".
#
