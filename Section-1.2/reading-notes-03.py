#!/usr/bin/python

class Fibonacci:
  def __init__(self,):
    self.nodes = []
    self.node_count = 0
    self.edges = []

  def calc(self,n):
    return self.method(n)

  def method(self,n,parent=None):
    node = self.addNode(n)

    if parent:
      self.edges.append(Edge(parent,node))

    if n == 0: return 0
    if n == 1: return 1
    return self.method(n-1,node) + self.method(n-2,node)

  def addNode(self,n):
    node = Node(n,self.node_count)
    self.nodes.append(node)
    self.node_count += 1
    return node

  def save(self,):
    f = open('reading-notes-03.dot','w')
    
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
  def __init__(self,n,c):
    self.name = "node" + str(c)
    self.label = "node" + str(c) + "[label = \"fib "+str(n)+"\"]"

class Edge:
  def __init__(self,n1,n2):
    self.n1 = n1
    self.n2 = n2

f = Fibonacci()
f.calc(6)
f.save()
