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

#
# Exercise 2
#
# Write a selector that when given a class and a schedule, returns a
# new schedule including the new class:
#
def add_class(klass,schedule):
  schedule.append(klass)
  return schedule

#
# Run some unit tests.
#
# First define some units and classes:
#
u1 = make_units(3,3,3)
calc1 = make_class(101,u1)
calc2 = make_class(102,u1) 

#
# Now try to build a schedule using these:
#
s = add_class(calc1,empty_schedule())
s = add_class(calc2,s)

#
# Inspect the schedule:
#
print "First class:\t", s[0]
# ==> [101, [3, 3, 3]]

print "Second class:\t", s[1]
# ==> [102, [3, 3, 3]]

#
# The order of growth in both time and space is linear in the variable
# "schedule", that is, it is O(n) where "n" is the length of the list
# structure "schedule".
#
