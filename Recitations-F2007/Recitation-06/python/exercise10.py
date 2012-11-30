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

def make_schedule_checker_2(max_credits):
  return lambda x: total_scheduled_units(x) <= max_units

def class_numbers(schedule):
  return map(lambda x: get_class_number(x), schedule)

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
# Exercise 10
#
# Rewrite "credit-limit" to run in O(n) time.
#
def credit_limit(schedule, max_credits):
 def credit_limit_iter(sched, working, total):
  if len(sched) == 0:
   return working
  else:
   klass = sched[0]
   credits = get_class_total_units(klass)
   if (credits+total) > max_credits:
    return working
   else:
     if working is None: working = []
     working = working.append(klass)
     return credit_limit_iter(sched[1:], working, (credits+total))
 return credit_limit_iter(schedule,[],0)

#
# Let's run some unit tests:
#
s1 = empty_schedule()
s2 = empty_schedule()
s2 = add_class(calc1, s2)
s2 = add_class(algebra, s2)
s2 = add_class(diff_eqs, s2)

#
# Introspect s2:
#
s2 
# ==> [['CALC-101', [4, 4, 4]], ['ALGB-152', [3, 3, 3]], ['DIFF-201', [3, 3, 3]]]
drop_class(s2,"CALC-101")
# ==> [['ALGB-152', [3, 3, 3]], ['DIFF-201', [3, 3, 3]]]
