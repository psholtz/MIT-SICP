#!/usr/bin/python

import xmlrpclib


COLOR_ROOT = "#ff0000"
COLOR_BRANCH = "#880088"
COLOR_LEAF = "#00ff00"

class Factorial:

    def __init__(self,):
        self.server = xmlrpclib.Server('http://localhost:20738/RPC2')
        self.G = self.server.ubigraph

    def calc1(self,n):
        self.G.clear()
        return self.method1(n)

    def method1(self,n,parent=None):
        node = self.G.new_vertex()
        if parent:
            self.G.new_edge(node,parent)
        if n == 0: return 1
        return n * self.method1(n-1,node)

class Fibonacci:

    def __init__(self,):
        self.server = xmlrpclib.Server('http://localhost:20738/RPC2')
        self.G = self.server.ubigraph

    def calc1(self,n):
        self.G.clear()
        return self.method1(n)

    def method1(self,n,parent=None):
        node = self.G.new_vertex()

        if parent:
            self.G.new_edge(node,parent)
      
        if not parent:
          self.G.set_vertex_attribute(node,'color',COLOR_ROOT)
        elif n == 0 or n == 1:
          self.G.set_vertex_attribute(node,'color',COLOR_LEAF)
        else:
          self.G.set_vertex_attribute(node,'color',COLOR_BRANCH)
        self.G.set_vertex_attribute(node,'shape','sphere')
        self.G.set_vertex_attribute(node,'shapedetail','20')
        self.G.set_vertex_attribute(node,'size','2.0')

        if n == 0: return 0
        if n == 1: return 1
        return self.method1(n-1,node) + self.method1(n-2,node)

f = Fibonacci()
f.calc1(10)
