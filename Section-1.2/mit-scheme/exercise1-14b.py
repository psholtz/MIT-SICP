#!/usr/bin/python

"""
GraphViz visualization code for Exercise 1.14

For use with GraphViz software, available here:

http://www.graphviz.org/
"""
class Change:

  def __init__(self,):
    self.nodes = []
    self.node_count = 0
    self.edges = []

  def count_change(self,amount):
    return self.cc(amount,5)

  def cc(self,amount,kinds_of_coins,parent=None):
    node = self.addNode(amount,kinds_of_coins)
    if parent:
      self.edges.append(Edge(parent,node))

    if amount == 0:
      return 1
    if ( amount < 0 ) or ( kinds_of_coins == 0 ):
      return 0
    return self.cc(amount,kinds_of_coins-1,node) + self.cc(amount - self.first_denomination(kinds_of_coins), kinds_of_coins, node)

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

  def addNode(self,n,m):    
    node = Node(n,m,self.node_count)
    self.nodes.append(node)
    self.node_count += 1
    return node

  def save(self,):
    f = open("exercise1-14.dot",'w')
  
    lines = []
    lines.append("digraph G {")
    for node in self.nodes:
       lines.append(node.label)
    for edge in self.edges:
       lines.append(edge.n1.name + " -> " + edge.n2.name)
    lines.append("}")
    f.write("\r\n".join(lines))

    f.close()

class Node:
  def __init__(self,n,m,c):
    self.name = "node" + str(c)
    self.label = self.name + "[label = \"cc "+str(n)+" "+str(m)+"\"]"

class Edge:
  def __init__(self,n1,n2):
    self.n1 = n1
    self.n2 = n2

c = Change()
print c.count_change(11)
c.save()
