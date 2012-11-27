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

print "done"
