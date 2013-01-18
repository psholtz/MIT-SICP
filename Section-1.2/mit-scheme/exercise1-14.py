#!/usr/bin/python

import time
import xmlrpclib
from optparse import OptionParser

COLOR_ROOT = "#ff0000"		# RED
COLOR_BRANCH = "#880088"	# PURPLE
COLOR_LEAF = "#00ff00"		# GREEN

DEFAULT_COINS = 11
DEFAULT_DELAY = 0.5;

"""
Algorithm visualization code for Exercise 1.14.

For use with Ubigraph graphing visualization server, available here:

http://ubietylab.net/ubigraph/
"""

class Change:
    """Python implementation of the Scheme count_change procedure in SICP"""

    def __init__(self,graph=False,animated=False,label=False,delay=DEFAULT_DELAY):
        """Constructor, with graphing support"""
        # initialize the instance variable
        self.graph = graph
        self.animated = animated
        self.label = label
        self.delay = delay

        # initialize the graphing console
        if self.graph:
            self.server = xmlrpclib.Server('http://localhost:20738/RPC2')
            self.G = self.server.ubigraph

    
    def count_change(self,amount):
        """count_change procedure, with graphing support"""
        # clear the graphing console
        if self.graph:
            self.G.clear()
        self.invocation_count = 0

        # recursively count the change
        return self.cc(amount,5) 

    def cc(self,amount,kinds_of_coins,parent=None):
        """cc procedure, with graphing support"""
        # draw on the graph
        node = None
        if self.graph:
            node = self.G.new_vertex()
            if self.label:
              self.G.set_vertex_attribute(node,'label','(cc '+str(amount)+' '+str(kinds_of_coins)+')') 
              self.G.set_vertex_attribute(node,'fontsize','10')
            if not parent:
                self.G.set_vertex_attribute(node,'color',COLOR_ROOT)
            else:
                self.G.set_vertex_attribute(node,'color',COLOR_BRANCH)
            self.G.set_vertex_attribute(node,'shape','sphere')
            self.G.set_vertex_attribute(node,'shapedetail','40')
            self.G.set_vertex_attribute(node,'size','2.0')
            if parent:
                self.G.new_edge(node,parent)

            # if animating, sleep the thread
            if self.animated:
                time.sleep(self.delay)

        # increment invocation counter
        self.invocation_count += 1

        # recursively invoke the algorithm
        if amount == 0: 
            if self.graph:
                self.G.set_vertex_attribute(node,'color',COLOR_LEAF)
            return 1
        elif ( amount < 0 ) or ( kinds_of_coins == 0 ):
            if self.graph:
                self.G.set_vertex_attribute(node,'color',COLOR_LEAF)
            return 0
        else:
            return self.cc(amount, kinds_of_coins - 1, node) + \
                   self.cc(amount - self.first_denomination(kinds_of_coins), kinds_of_coins, node)

    def first_denomination(self,kinds_of_coins):
        """first denomination procedure, with graphic support"""
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

# 
# Parse the command line arguments
#
parser = OptionParser()
parser.set_conflict_handler("resolve")
parser.add_option("-c","--coins",dest="coins",help="amount to change",metavar="<amount>")
parser.add_option("-a","--animated",action="store_true",dest="animated",help="animate graphing?",metavar="<animated?>")
parser.add_option("-g","--graph",action="store_true",dest="graph",help="use graphing?",metavar="<graph?>")
parser.add_option("-l","--label",action="store_true",dest="label",help="use label?",metavar="<label?>")
parser.add_option("-d","--delay",dest="delay",help="animation delay",metavar="<animation delay>")
(options, args) = parser.parse_args()

#
# Set the arguments we will use
#
c = DEFAULT_COINS if options.coins is None else int(options.coins)
a = True if options.animated else False
g = True if options.graph else False
l = True if options.label else False
d = DEFAULT_DELAY if options.delay is None else float(options.delay)

change = Change(g,a,l,d)

print
print "Amount:", c
print "Count:", change.count_change(c)  
print "cc invoked:", change.invocation_count
print
