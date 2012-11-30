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
# Working definitions (HOPs)
#
def make_student(number,sched_checker):
  return [number, [], sched_checker]

def get_student_number(student):
  return student[0]

def get_student_schedule(student):
  return student[1]

def get_student_checker(student):
  return student[2]

def update_student_schedule(student, schedule):
  checker = get_student_checker(student)
  if ( checker(schedule) ):
    return [get_student_number(student), schedule, checker]
  else:
    raise BaseException("Invalid schedule!")

#
# Previous Solutions
#
def empty_schedule():
  return []

def add_class(klass,schedule):
  schedule.append(klass)
  return schedule

def total_scheduled_units(schedule):
  def total_scheduled_units_iter(total,working):
    if not working:
      return total
    else:
      return total_scheduled_units_iter(total + get_class_total_units(working[0]), working[1:])
  return total_scheduled_units_iter(0,schedule)

def drop_class(schedule,classnum):
  tmp = make_class(classnum,[])
  def predicate(klass):
    return not same_class(klass,tmp)
  return filter(predicate, schedule)

def credit_limit(schedule, max_credits):
  def credit_limit_iter(working):
    if not working:
      return []
    else:
      total_credits = total_scheduled_units(working)
      first_class = working[0]
      if ( total_credits > max_credits ):
        return credit_limit_iter(drop_class(working, get_class_number(first_class)))
      else:
        return working
  return credit_limit_iter(schedule)

def make_schedule_checker_1():
  return lambda x: len(x) > 0

# 
# Basic Classes
#
calc1 = make_class("CALC-101", make_units(4,4,4))
calc2 = make_class("CALC-102", make_units(4,4,4))
algebra = make_class("ALGB-152", make_units(3,3,3))
diff_eqs = make_class("DIFF-201", make_units(3,3,3))
us_history = make_class("HIST-122", make_units(4,4,4))
world_history = make_class("HIST-324", make_units(4,4,4))
basket_weaving = make_class("BASKETS", make_units(1,1,1))

#
# Exercise 7
#
# Finish the call to "make-student" to create a first-term freshman (limited to 54 units).
#

#
# Define the appropriate constructor:
#
def make_schedule_checker_2(max_units):
  return lambda x: total_scheduled_units(x) <= max_units

#
# Let's run some unit tests:
#
s1 = empty_schedule()
s2 = empty_schedule()
s2 = add_class(calc1, s2)
s2 = add_class(algebra s2)
s2 = add_class(diff_eqs, s2)
s3 = s2[:]
s3 = add_class(us_history, s3)
s3 = add_class(world_history, s3)
s4 = s3[:]
s4 = add_class(basket_weaving, s4)

total_scheduled_units(s1)
# ==> 0
total_scheduled_units(s2)
# ==> 30
total_scheduled_units(s3)
# ==> 54
total_scheduled_units(s4)
# ==> 57

sid1 = 575904476

student1 = make_student(sid1, make_schedule_checker_2(54))

update_student_schedule(student1,s1)
# ==> [575904476, [], <function <lambda> at 0x106844230>]
update_student_schedule(student1,s2)
# ==> [575904476, [['CALC-101', [4, 4, 4]], ['ALGB-152', [3, 3, 3]], ['DIFF-201', [3, 3, 3]]], <function <lambda> at 0x106844230>]
update_student_schedule(student1,s3)
# ==> [575904476, [['CALC-101', [4, 4, 4]], ['ALGB-152', [3, 3, 3]], ['DIFF-201', [3, 3, 3]], ['HIST-122', [4, 4, 4]], ['HIST-324', [4, 4, 4]]], <function <lambda> at 0x106844230>]
update_student_schedlue(student1,s4)
# ==> "Invalid schedule!"

