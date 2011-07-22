#!/usr/bin/python

class Change:

  def __init__(self,):
    pass

  def count_change_1(self,amount):
    """ model (cc amount 1), anticipate linear growth """
    self.invocation_count = 0
    self.change = self.cc(amount,1)

  def count_change_2(self,amount):
    """ model (cc amount 2), anticipate n^2 growth """
    self.invocation_count = 0
    self.change = self.cc(amount,2)

  def count_change_3(self,amount):
    """ model (cc amount 3), anticipate n^3 growth """
    self.invocation_count = 0
    self.change = self.cc(amount,3)

  def count_change_4(self,amount):
    """ model (cc amount 4), anticipate n^4 growth """
    self.invocation_count = 0
    self.change = self.cc(amount,4)

  def count_change_5(self,amount):
    """ model (cc amount 5), anticipate n^5 growth """
    self.invocation_count = 0
    self.change = self.cc(amount,5)

  def cc(self,amount,kinds_of_coins):
    self.invocation_count += 1

   
    if amount == 0:
      return 1
    elif (amount < 0) or (kinds_of_coins == 0):
      return 0
    else:
      return self.cc(amount,kinds_of_coins-1) + self.cc(amount-self.first_denomination(kinds_of_coins),kinds_of_coins)

  def first_denomination(self,kinds_of_coins):
    if kinds_of_coins == 1:
      return 1
    elif kinds_of_coins == 2:
      return 5
    elif kinds_of_coins == 3:
      return 10
    elif kinds_of_coins == 4:
      return 25
    elif kinds_of_coins == 5:
      return 50

change = Change()

stats_1 = []
stats_2 = []
stats_3 = []
stats_4 = []
stats_5 = []

for x in range(1,101):

  # run test for 1
  change.count_change_1(x)
  stats_1.append(change.invocation_count)

  # run test for 2
  change.count_change_2(x)
  stats_2.append(change.invocation_count)

  # run test for 3
  change.count_change_3(x)
  stats_3.append(change.invocation_count)

  # run test for 4
  change.count_change_4(x)
  stats_4.append(change.invocation_count)

  # run test for 5
  change.count_change_5(x)
  stats_5.append(change.invocation_count)

def output_results(k,stats):
  print
  print "(invocation-count (cc n "+str(k)+") 100) ==> (list", 
  c = 0
  for x in stats:
    if c < len(stats)-1:
      print x,
    else:
      print str(x)+")"
    c += 1 
  print

def write_results(k,stats):
  out_A = []
  out_b = []
  out_example = []
  out_t = []
   
  c = 1
  for x in range(len(stats)):
    if k == 1:
      out_A.append(" 1 " + str(c) + ";")
    elif k == 2:
      out_A.append(" 1 " + str(c) + " " + str(c*c) + ";")
    elif k == 3:
      out_A.append(" 1 " + str(c) + " " + str(c*c) + " " + str(c*c*c) + ";")
    elif k == 4:
      out_A.append(" 1 " + str(c) + " " + str(c*c) + " " + str(c*c*c) + " " + str(c*c*c*c) + ";")
    elif k == 5:
      out_A.append(" 1 " + str(c) + " " + str(c*c) + " " + str(c*c*c) + " " + str(c*c*c*c) + " " + str(c*c*c*c*c) + ";")
    out_b.append(str(stats[x]) + ";")
    out_example.append(" " + str(c) + " " + str(stats[x]) + ";")
    out_t.append(str(c)+";")
    c += 1

  f = open("m-files/matrix0"+str(k)+".m",'w')

  # write the A matrix
  f.write("A = [\r\n")
  f.write("".join(out_A))
  f.write("];\r\n\r\n")

  # write the b matrix
  f.write("b = [\r\n")
  f.write(" ".join(out_b))
  f.write("];\r\n\r\n")

  # calc Q
  f.write("Q = A'*A;\r\n\r\n")

  # calc w
  f.write("w = A'*b;\r\n\r\n")

  # render the plotting matrices
  f.write("example = [\r\n")
  f.write("".join(out_example))
  f.write("];\r\n\r\n")

  f.write("hold on;\r\n\r\n")
  f.write("plot(example(:,1),example(:,2),'k@');\r\n\r\n")

  # calc x and finish calculation
  f.write("x = inv(Q)*w;\r\n\r\n")

  f.write("t = [\r\n")
  f.write(" ".join(out_t))
  f.write("];\r\n\r\n")
  if k == 1:
    f.write("p = [x(2,1) x(1,1)];\r\n\r\n")
  elif k == 2:
    f.write("p = [x(3,1) x(2,1) x(1,1)];\r\n\r\n")
  elif k == 3:
    f.write("p = [x(4,1) x(3,1) x(2,1) x(1,1)];\r\n\r\n")
  elif k == 4:
    f.write("p = [x(5,1) x(4,1) x(3,1) x(2,1) x(1,1)];\r\n\r\n")
  elif k == 5:
    f.write("p = [x(6,1) x(5,1) x(4,1) x(3,1) x(2,1) x(1,1)];\r\n\r\n")

  f.write("plot(t,polyval(p,t));\r\n\r\n")
  f.write("hold off;\r\n\r\n")

  # print out the answer
  f.write("x")

  f.close()

# write statistics to console
output_results(1,stats_1)
output_results(2,stats_2)
output_results(3,stats_3)
output_results(4,stats_4)
output_results(5,stats_5)

# generate .m files for octave/matlab
write_results(1,stats_1)
write_results(2,stats_2)
write_results(3,stats_3)
write_results(4,stats_4)
write_results(5,stats_5)
