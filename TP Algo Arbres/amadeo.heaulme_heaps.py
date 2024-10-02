__license__ = 'Nathalie (c) EPITA'
__docformat__ = 'reStructuredText'
__revision__ = '$Id: heap.py 2024-04-22'

"""
Heap homework
2024-04 - S2
@author: amadeo.heaulme
"""


#given function

def Heap():
    """ returns a fresh new empty heap
    
       :rtype: list (heap)
    """
    return [None]


###############################################################################
# Do not change anything above this line, except your login!
# Do not add any import
# Do not modify the docstrings

#-------------------- First

def heap_push(H, elt, val):
    """ adds the pair (val, elt) to heap H (in place: no need to return H)
    """
    H.append((val,elt))
    i=len(H)-1

    while(i>1 and H[i//2]!=None ):
        if H[i//2][0]>H[i][0]:
            (H[i],H[i//2])=(H[i//2],H[i])
        i-=1


def heap_pop(H):
    """ removes (in place) and returns the pair of smallest value in the heap H
    	raises Exception if H is empty
    """
    lenH = len(H)
    if lenH == 1:
        raise Exception("Heap is empty")
    res = H[1]
    H[1] = H[lenH-1]
    H.pop()
    lenH -= 1
    i = 1
    while (2*i < lenH and H[i][0] > H[2*i][0]) or (2*i+1 < lenH and H[i][0] > H[2*i+1][0]):
        children = 2*i
        if children+1 < lenH and H[children][0] > H[children+1][0]:
            children += 1
        H[i], H[children] = H[children], H[i]
        i = children
    return res

def max_elt(H):
    """ return the element of highest value in the heap H
        raises Exception if H is empty
    """
    if len(H) == 1:
        raise Exception
    max_index = len(H) // 2
    for j in range(max_index, len(H)):
        if H[j][0] >= H[max_index][0]:
            max_index = j
    return H[max_index][1]

def is_heap(T):
    """ tests whether the complete tree T (in hierarchical implementation) is a heap
    """
    if len(T) == 1:
        return True
    i, lenT = 2, len(T)
    while (i < lenT) and (T[i//2][0] <= T[i][0]):
        i += 1
    return i == lenT

#-------------------- Second

def heap_update(H, pos, newval):
    """ updates (in place) the value associated to the element in position pos 
        in the heap H with newval
    """
    if pos >= len(H) or (pos < 1):
        raise Exception("invalid position")
    H[pos] = (newval, H[pos][1])
    parent = pos // 2
    while parent >= 1 and H[parent][0] > H[pos][0]:  # sift up
        H[parent], H[pos] = H[pos], H[parent]
        pos = parent
        parent = pos // 2
    left_child = 2 * pos
    right_child = left_child + 1
    lenH = len(H)
    while left_child < lenH:  # sift down
        min_child = left_child
        if right_child < lenH and H[right_child][0] < H[left_child][0]:
            min_child = right_child
        if H[pos][0] <= H[min_child][0]:
            break
        H[pos], H[min_child] = H[min_child], H[pos]
        pos = min_child
        left_child = 2 * pos
        right_child = left_child + 1


def heapify(T):
    """ transforms the complete binary tree T (in hierarchical implementation) 
        into a heap (in place) 
    """

    def __sift_down(T, i):  # will be used to sift down the element at index i!!!
        left = 2 * i
        right = 2 * i + 1
        smallest = i
        if left < len(T) and T[i][0] > T[left][0]:
            smallest = left
        if right < len(T) and T[smallest][0] > T[right][0]:
            smallest = right
        if smallest != i:
            T[i], T[smallest] = T[smallest], T[i]
            __sift_down(T, smallest)

    for i in range(len(T) // 2, 0, -1):
        __sift_down(T, i)
