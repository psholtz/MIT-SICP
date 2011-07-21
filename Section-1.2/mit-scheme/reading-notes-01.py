#!/usr/bin/python

import xmlrpclib

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

        if not parent: 
            self.G.set_vertex_attribute(node,'color','#ff0000')
        if parent:
            self.G.new_edge(node,parent)
        if n == 0: return 0
        if n == 1: return 1
        return self.method1(n-1,node) + self.method1(n-2,node)

f = Fibonacci()
f.calc1(10)
