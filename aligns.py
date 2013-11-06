#!/usr/bin/env python

# GPLv3: http://www.gnu.org/copyleft/gpl.html
# Author: Marcelino Alberdi Pereira (mpe031)
# E-mail: marcelino.alberdi@gmail.com
# Version: 1.0

import sys

def read_score_matrix(filename):
  # Example matrix in text file is like this
  #    A  B  C
  # A  0
  # B -1  2  
  # C -3 -4  6
  #
  # We assume a lower triangular matrix and treat it as symmetric.
  # Top and left alphabet doesn't have to match in order, but must match in length.

  f = open(filename)
  lines = f.readlines()
  f.close()

  # First line is just the ordered alphabet
  alphabet = lines[0].split()
  
  # matrix[(a,b)] = matrix[(b,a)] = score of aligning a and b
  matrix = {}

  for l in lines[1:]:
    l = l.split()
    # First element (l[0]) is a letter from alphabet
    for i in range(1, len(l)):
      # For each value in the line, we assign it to our matrix
      matrix[(l[0], alphabet[i-1])] = int(l[i])
      # We complete the matrix with the symmetric elements
      # We use a little bit more of memory but save time in accesing and programming
      matrix[(alphabet[i-1], l[0])] = int(l[i])

  return matrix


def initialize_matrix(len_q, len_d, g_open=0, g_extend=0):
  # If the g_open and g_extend parameters aren't specified, the function treats the matrix as if
  # it were calculated using local coordinates instead of global
  # Obviously, the function can also simulate linear gap penalties if both variables get the same value

  # We use the following notation for the matrix elements: h[i,j][x]
  # Where i and j are the coordinates and x is:
  # * 1 if it comes from the i-1,j cell
  # * 2 if it comes from the i,j-1 cell
  # * 3 if it comes from the i-1,j-1 cell
  # * 0 is the maximum of those three values
  
  # For initializing, all of the member of a specific cell in the top row and left column
  # have the same values. Those values are 0 if we are working with local coordinates.
  h = {}
  h[0,0] = [0,0,0,0]
  for i in range(1, len_q+1):
    h[i,0] = [-g_open - (i-1)*g_extend for x in range(0,4)]
  for j in range(1, len_d+1):
    h[0,j] = [-g_open - (j-1)*g_extend for x in range(0,4)]
  return h


# This function is unusable now, because it needed that the elements in the h dictionary
# were single numbers instead of a list like it is currently
# Can be fixed easily by adding [0] after every h[i,j] expresion
"""
def calculate_matrix_linear(h, score, gap, seq_q, seq_d, local=True):
  # Matrix calculated with linear gap penalties
  # The local parameter is for treating it as local (default) or global
  for i in range(1, len(seq_q)+1):
    for j in range(1, len(seq_d)+1):
      h[i,j] = max(h[i-1,j-1]+score[seq_q[i-1], seq_d[j-1]], h[i-1,j]-gap, h[i,j-1]-gap)
      if local: h[i,j] = max(h[i,j],0)
  return h
"""


def calculate_matrix_affine(h, score, g_open, g_extend, seq_q, seq_d, local=True):
  # Matrix calculated with affine gap penalties
  # The local parameter is for treating it as local (default) or global

  # The meanings of x in h[i,j][x] can be seen @initialize_matrix
  for i in range(1, len(seq_q)+1):
    for j in range(1, len(seq_d)+1):
      h[i,j] = [0, # placeholder
        max(h[i-1,j][1]-g_extend, h[i-1,j][2]-g_open-g_extend, h[i-1,j][3]-g_open-g_extend), # h[i,j][1]
        max(h[i,j-1][2]-g_extend, h[i,j-1][1]-g_open-g_extend, h[i,j-1][3]-g_open-g_extend),# h[i,j][2]
        h[i-1,j-1][0]+score[seq_q[i-1], seq_d[j-1]]] # h[i,j][3]
      if local:
        h[i,j][0] = max(h[i,j])
      else:
        h[i,j][0] = max(h[i,j][1:3]) # We allow the negative values
  return h


# As the other one above, this function is also obsolete for the same reasons
# Could be merged with the following calculate_align_affine but that goes beyond the current scope
"""
def calculate_align_linear(h, score, gap, seq_q, seq_d):
  align_q = []
  align_d = []
  path = []
  i = len(seq_q)
  j = len(seq_d)

  while(i>0 and j>0):
    path.append((i,j))
    if h[i,j] == h[i-1,j-1] + score[seq_q[i-1], seq_d[j-1]]:
      align_q.append(seq_q[i-1])
      align_d.append(seq_d[j-1])
      (i,j) = (i-1,j-1)
    elif h[i,j] == h[i-1,j]-gap:
      align_q.append(seq_q[i-1])
      align_d.append('-')
      i = i-1
    elif h[i,j] == h[i,j-1]-gap:
      align_q.append('-')
      align_d.append(seq_d[j-1])
      j = j-1
    else:
      exit('FATAL ERROR: Calculated h value doesn't match the previous one')

  align_q.reverse()
  align_d.reverse()

  # FIXME: We could be missing some in the last row or column
  path.extend([(i,j),(0,0)])
  return (path, align_q, align_d)
"""


def calculate_align_affine(h, score, seq_q, seq_d):
  align_q = []
  align_d = []
  path = []
  i = len(seq_q)
  j = len(seq_d)

  while(i>0 and j>0):
    path.append((i,j))
    if h[i,j][0] == h[i,j][1]:
      # Then it comes from the i-1,j
      # A gap must be inserted in d
      align_q.append(seq_q[i-1])
      align_d.append('-')
      i = i-1
    elif h[i,j][0] == h[i,j][2]:
      # Then it comes from the i,j-1
      # A gap must be inserted in q
      align_q.append('-')
      align_d.append(seq_d[j-1])
      j = j-1
    else: #if h[i,j] == h[i,j][3] or h[i,j] == 0:
      # Then it comes from the i-1,j-1
      # No gaps need to be inserted
      align_q.append(seq_q[i-1])
      align_d.append(seq_d[j-1])
      (i,j) = (i-1,j-1)

  while i>0:
    # Here j == 0, so we just walk all the top row back to (0,0)
    align_q.append(seq_q[i-1])
    align_d.append('-')
    path.append((i,j))
    i = i-1

  while j>0:
    # Here i == 0, so we just walk all the left column back (0,0)
    align_q.append('-')
    align_d.append(seq_d[j-1])
    path.append((i,j))
    j = j-1    

  # We added the new sequences starting by the end, so we need to reverse them
  align_q.reverse()
  align_d.reverse()

  path.append((0,0))
  return (path, align_q, align_d)


def print_result(h, seq_q, seq_d, align_q, align_d, path):
  pad = 4 # Width of each cell

  sys.stdout.write(" "*pad*2) # Initial padding
  for c in seq_d: sys.stdout.write(c.rjust(pad)) # Top row

  for i in range(0, len(seq_q)+1):
    if i != 0: # All but the first line have a sequence character before
      sys.stdout.write("\n"+seq_q[i-1].rjust(pad))
    else: # We just fill with spaces here
      sys.stdout.write("\n"+" "*pad)

    for j in range(0, len(seq_d)+1):
      if (i,j) in path: # We mark the path with asterisks
        sys.stdout.write(("*"+str(h[(i,j)][0])).rjust(pad))
      else: 
        sys.stdout.write(str(h[(i,j)][0]).rjust(pad))
        
  print "\n\n" + str(align_q) + "\n" + str(align_d)


if __name__== '__main__':
  try:
    # We get the parameters from command-line
    matrix_file = sys.argv[1]
    g_open = int(sys.argv[2])
    g_extend = int(sys.argv[3])
    seq_q = sys.argv[4]
    seq_d = sys.argv[5]
  except IndexError:
    print "Mismatched number of parameters"
    print "Usage: ./aligns.py matrix_filename gap_open gap_extend secuence_q secuence_d"
    print "Example: ./aligns.py matrix.txt 11 1 CPAIVGEPEPSAIADVAAQEKGLR EKPAIVGEPEMPSTIADVAQEKGA"
    exit()

  score = read_score_matrix(matrix_file)

  h = initialize_matrix(len(seq_q), len(seq_d))
  h = calculate_matrix_affine(h, score, g_open, g_extend, seq_q, seq_d)

  (path, align_q, align_d) = calculate_align_affine(h, score, seq_q, seq_d)

  print_result(h, seq_q, seq_d, align_q, align_d, path)

