#!/usr/bin/python

"""
Analyzing the performance of an algorithm is sometimes easier for me to 
visualize in Python than in Lisp; hence why I hacked this out in Python.
"""

counter = 0

def entry(tree):
  return tree[0]
def left_branch(tree):
  return tree[1]
def right_branch(tree):
  return tree[2]
def make_tree(entry,left,right):
  return [entry,left,right]

def element_of_set(x,set):
  if len(set) == 0:
    return False
  head = entry(set)
  if x == head:
    return True
  if x < head:
    return element_of_set(x,left_branch(set))
  if x > head:
    return element_of_set(x,right_branch(set))

def adjoin_set(x,set):
  if len(set) == 0:
    return make_tree(x,[],[])
  head = entry(set)
  if x == head:
    return set
  if x < head:
    return make_tree(head,adjoin_set(x,left_branch(set)),right_branch(set))
  if x > head:
    return make_tree(head,left_branch(set),adjoin_set(x,right_branch(set)))

tree1 = make_tree(7,[],[])
tree1 = adjoin_set(3,tree1)
tree1 = adjoin_set(1,tree1)
tree1 = adjoin_set(5,tree1)
tree1 = adjoin_set(9,tree1)
tree1 = adjoin_set(11,tree1)

print "===="
print tree1
print element_of_set(1,tree1)
print element_of_set(2,tree1)
print element_of_set(3,tree1)
print "===="

def partial_tree(elements, n):
  global counter
  counter += 1
  if n == 0:
    ans = []
    ans.append([])
    for elem in elements:
      ans.append(elem)
    return ans
  else:
    left_size = (n-1)/2
    left_result = partial_tree(elements, left_size)
    left_tree = left_result[0]
    non_left_elts = left_result[1:]
    right_size = (n - left_size - 1)
    this_entry = non_left_elts[0]
    right_result = partial_tree(non_left_elts[1:], right_size)
    right_tree = right_result[0]
    remaining_elts = right_result[1:]
     
    ans = [make_tree(this_entry, left_tree, right_tree)]
    for elem in remaining_elts:
      ans.append(elem)
    return ans

def list_2_tree(elements):
  return partial_tree(elements,len(elements))

print 
print partial_tree([1,2,3],0)
print partial_tree([1,2,3],1)
print partial_tree([1,2,3],2)
print partial_tree([1,2,3],3)
print 

print "==="
counter = 0
sample = [1,2,3]
print list_2_tree(sample)
print "size:",len(sample)
print "count:",counter
print "comp:",(2*len(sample))+1
print "==="
counter = 0
sample = [1,2,3,4,5,6,7]
print list_2_tree(sample)
print "size:",len(sample)
print "count:",counter
print "comp:",(2*len(sample))+1
print "==="
counter = 0
sample = [1,2,3,4,5]
print list_2_tree(sample)
print "size:",len(sample)
print "count:",counter
print "comp:",(2*len(sample))+1
print "==="
