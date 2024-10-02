import unittest
from importlib.util import spec_from_file_location, module_from_spec

# Replace with the actual path to your file
file_path = "C:/Users/Amadéo/ByAmad/Git/EPITA GIT/TPs/TP Algo Arbres/amadeo.heaulme_heaps.py"

spec = spec_from_file_location("heaps", file_path)
heaps = module_from_spec(spec)
spec.loader.exec_module(heaps)

Heap = heaps.Heap
heap_push = heaps.heap_push
heap_pop = heaps.heap_pop
max_elt = heaps.max_elt
is_heap = heaps.is_heap
heap_update = heaps.heap_update
heapify = heaps.heapify

def print_heap(H, i=1, indent=0):
    if i < len(H):
        print_heap(H, i * 2 + 1, indent + 6)
        if indent:
            print(' ' * (indent-2) + '+--', H[i])
        else:
            print(H[i])
        print_heap(H, i * 2, indent + 6)

def print_list(H):
    result = "["
    for i in range(len(H)):
        result += str(H[i]) + ", "
    result += "]"



    #Its first child is at 2*k 
#   Its second child is at 2*k 
#   Its parent is at (k ) // 2.



'''
# rest of your code
c = Heap()

for i in range(len(c)):
    print(c[i])
'''

# heap_push(H, elt, val)


'''

Heap = [None, (2, 'A'), (12, 'C'), (10, 'B'), (24, 'I'), (16, 'E'), (14, 'D'), (18, 'F'),
        (30, 'L'), (26, 'J'), (20, 'G'), (32, 'M'), (28, 'K'), (22, 'H')]



print()
print()
print()
heap_push(Heap, 'N', 5)
// print_heap(Heap)

Hsupposed =  [None, (2, 'A'), (12, 'C'), (5, 'N'), (24, 'I'), (16, 'E'), (14, 'D'), (10, 'B'),
      (30, 'L'), (26, 'J'), (20, 'G'), (32, 'M'), (28, 'K'), (22, 'H'), (18, 'F')]

print (Hsupposed == Heap)
// print_heap(Hsupposed)

'''

'''
tuples_list = [(34, 'val_34'), (45, 'val_45'), (47, 'val_47'), (20, 'val_20'), (32, 'val_32'), (39, 'val_39'), (18, 'val_18'), (16, 'val_16'), (24, 'val_24'), (2, 'val_2'), (25, 'val_25'), (29, 'val_29'), (27, 'val_27'), (9, 'val_9'), (43, 'val_43'), (40, 'val_40'), (8, 'val_8'), (17, 'val_17'), (14, 'val_14'), (44, 'val_44'), (30, 'val_30'), (41, 'val_41'), (42, 'val_42'), (7, 'val_7'), (3, 'val_3'), (11, 'val_11'), (1, 'val_1'), (28, 'val_28'), (37, 'val_37'), (6, 'val_6'), (21, 'val_21'), (26, 'val_26'), (46, 'val_46'), (36, 'val_36'), (33, 'val_33'), (23, 'val_23'), (31, 'val_31'), (12, 'val_12'), (48, 'val_48'), (35, 'val_35'), (5, 'val_5'), (15, 'val_15'), (13, 'val_13'), (49, 'val_49'), (19, 'val_19'), (4, 'val_4'), (10, 'val_10'), (22, 'val_22'), (38, 'val_38'), (0, 'val_0')]
heap = Heap()
for val, elt in tuples_list:
    heap_push(heap, elt, val)
    if not is_heap(heap):
        print("Heap property violated after adding", (val, elm))
print_heap(heap)
'''

# heap_pop(H)

'''
H = [None, (2, 'A'), (12, 'C'), (10, 'B'), (24, 'I'), (16, 'E'), (14, 'D'), (18, 'F'),
                (30, 'L'), (26, 'J'), (20, 'G'), (32, 'M'), (28, 'K'), (22, 'H')]
print_heap(H)
print()
print()
print()
print()
print()
print()
poppedH = [None, (10, 'B'), (12, 'C'), (14, 'D'), (24, 'I'), (16, 'E'), (22, 'H'), (18, 'F'),
        (30, 'L'), (26, 'J'), (20, 'G'), (32, 'M'), (28, 'K')]
# print_heap(poppedH)

print()
print()
print()
print()
print()
print()

doublepoppedH = [None, (12, 'C'), (16, 'E'), (14, 'D'), (24, 'I'), (20, 'G'), (22, 'H'), (18, 'F'),
        (30, 'L'), (26, 'J'), (28, 'K'), (32, 'M')]
# print_heap(doublepoppedH)

print()
print()
print()
print()
print()
print()


triplepoppedH = [None, (14, 'D'), (16, 'E'), (18, 'F'), (24, 'I'), (20, 'G'), (22, 'H'), (32, 'M'),
        (30, 'L'), (26, 'J'), (28, 'K')]
print_heap(triplepoppedH)

h2 = heap_pop(H)
heap_pop(H)
heap_pop(H)
print(H == triplepoppedH)
print_heap(H)
'''

# max_elt(H)

'''
H = [None, (2, 'A'), (12, 'C'), (10, 'B'), (24, 'I'), (16, 'E'), (14, 'D'), (18, 'F'),
                (30, 'L'), (26, 'J'), (20, 'G'), (32, 'M'), (28, 'K'), (22, 'H')]

print(max_elt(H))
print(max_elt([None, (2, 'G'), (2, 'I')]))
# print(max_elt([None]))
print(max_elt([None, (2, 'G'), (2, 'I'), (5, 'Z'), (5, 'A')]))
'''

# is_heap(H)
 
'''
print(is_heap([None])) # true
print(is_heap([None, (3, 'A'), (2, 'B'), (1, 'C')])) # false
print(is_heap([None, (2, 'G'), (2, 'I')])) # true
print(is_heap([None, (2, 'A'), (14, 'D'), (10, 'B'), (16, 'E'), (12, 'C'), (18, 'F')])) # false
print(is_heap([None, (2, 'A'), (12, 'C'), (10, 'B'), (24, 'I'), (16, 'E'), (14, 'D'), (18, 'F'),
                (30, 'L'), (26, 'J'), (20, 'G'), (32, 'M'), (28, 'K'), (22, 'H')])) # true
print(is_heap([(34, 'val_34'), (81, 'val_81'), (59, 'val_59'), (56, 'val_56'), (79, 'val_79'), (66, 'val_66'), (12, 'val_12'), (63, 'val_63'), (37, 'val_37'), (27, 'val_27'), (24, 'val_24'), (5, 'val_5'), (65, 'val_65'), (62, 'val_62'), (87, 'val_87'), (51, 'val_51'), (85, 'val_85'), (49, 'val_49'), (78, 'val_78'), (25, 'val_25'), (74, 'val_74'), (60, 'val_60'), (67, 'val_67'), (57, 'val_57'), (29, 'val_29'), (4, 'val_4'), (13, 'val_13'), (0, 'val_0'), (88, 'val_88'), (91, 'val_91'), (44, 'val_44'), (10, 'val_10'), (36, 'val_36'), (71, 'val_71'), (23, 'val_23'), (93, 'val_93'), (80, 'val_80'), (70, 'val_70'), (84, 'val_84'), (55, 'val_55'), (18, 'val_18'), (96, 'val_96'), (32, 'val_32'), (89, 'val_89'), (76, 'val_76'), (38, 'val_38'), (86, 'val_86'), (43, 'val_43'), (52, 'val_52'), (3, 'val_3'), (92, 'val_92'), (61, 'val_61'), (77, 'val_77'), (35, 'val_35'), (2, 'val_2'), (54, 'val_54'), (6, 'val_6'), (53, 'val_53'), (97, 'val_97'), (90, 'val_90'), (50, 'val_50'), (46, 'val_46'), (9, 'val_9'), (14, 'val_14'), (58, 'val_58'), (22, 'val_22'), (19, 'val_19'), (17, 'val_17'), (21, 'val_21'), (42, 'val_42'), (47, 'val_47'), (15, 'val_15'), (39, 'val_39'), (26, 'val_26'), (69, 'val_69'), (11, 'val_11'), (75, 'val_75'), (99, 'val_99'), (1, 'val_1'), (94, 'val_94'), (83, 'val_83'), (72, 'val_72'), (16, 'val_16'), (33, 'val_33'), (30, 'val_30'), (73, 'val_73'), (48, 'val_48'), (41, 'val_41'), (20, 'val_20'), (82, 'val_82'), (40, 'val_40'), (28, 'val_28'), (64, 'val_64'), (8, 'val_8'), (68, 'val_68'), (7, 'val_7'), (45, 'val_45'), (95, 'val_95'), (98, 'val_98'), (31, 'val_31')]))
print(is_heap([None, (2, 'val_2'), (3, 'val_3'), (5, 'val_5'), (7, 'val_7'), (8, 'val_8'), (6, 'val_6'), (9, 'val_9'), (4, 'val_4')])) #false
# print(print_heap([None, (2, 'val_2'), (3, 'val_3'), (5, 'val_5'), (7, 'val_7'), (8, 'val_8'), (6, 'val_6'), (9, 'val_9'), (4, 'val_4')]))

# heap_update(H, elt, val)
'''
'''
H = [None, (2, 'A'), (12, 'C'), (10, 'B'), (24, 'I'), (16, 'E'), (14, 'D'), (18, 'F'),
                (30, 'L'), (26, 'J'), (20, 'G'), (32, 'M'), (28, 'K'), (22, 'H')]
print("Heap before update")
print_heap(H)
print()
print()
print()
print()
print()
print()
print()
# heap_update(H, 0, 15)

# heap_update(H, 14, 15)

updatedH = [None, (2, 'A'), (9, 'G'), (10, 'B'), (24, 'I'), (12, 'C'), (14, 'D'), (18, 'F'), (30, 'L'), (26, 'J'), (16, 'E'), (32, 'M'), (28, 'K'), (22, 'H')]
print("What heap should be after update")
print_heap(updatedH)
print()
print()
print()
print()
print()
print()
print()
# print_heap(H)
print()
print()


heap_update(H, 10, 9)
print("Heap after update")
print_heap(H)
print()
print()
print()
print()
print()
# print(H == updatedH)

print("So we got this: ", H == updatedH)

heap_update(H, 1, 25)
print("Heap after second update")
print_heap(H)

doubleupdatedH = [None, (9, 'G'), (12, 'C'), (10, 'B'), (24, 'I'), (16, 'E'), (14, 'D'), (18, 'F'), (30, 'L'), (26, 'J'), (25, 'A'), (32, 'M'), (28, 'K'), (22, 'H')]
print()
print()
print()
print()
print()
print()
print()
print("What heap should be after second update")
print_heap(doubleupdatedH)
print (H == doubleupdatedH)
print("12")

'''

# heap_update(H, elt, val)

H = [None, (2, 'A'), (12, 'C'), (10, 'B'), (24, 'I'), (16, 'E'), (14, 'D'), (18, 'F'),
     (30, 'L'), (26, 'J'), (20, 'G'), (32, 'M'), (28, 'K'), (22, 'H')]
# heap_update(H, 0, 15) #  Exception: invalid position
# heap_update(H, 14, 15) # Exception: invalid position
heap_update(H, 10, 9)
nH = [None, (2, 'A'), (9, 'G'), (10, 'B'), (24, 'I'), (12, 'C'), (14, 'D'), (18, 'F'),
      (30, 'L'), (26, 'J'), (16, 'E'), (32, 'M'), (28, 'K'), (22, 'H')]
print (H == nH)

heap_update(H, 1, 25)
nH2 = [None, (9, 'G'), (12, 'C'), (10, 'B'), (24, 'I'), (16, 'E'), (14, 'D'), (18, 'F'),
       (30, 'L'), (26, 'J'), (25, 'A'), (32, 'M'), (28, 'K'), (22, 'H')]
print(nH2 == H) 




# heapify(H)

'''
T0 = [None]
heapify(T0)
print(T0 == [None])


print()
print()

T1 = [None, (3, 'A'), (2, 'B'), (1, 'C')]
heapify(T1)
print(T1 == [None, (1, 'C'), (2, 'B'), (3, 'A')])

print()
print()

T2 = [None, (20, 'G'), (18, 'F'), (28, 'K'), (16, 'E'), (24, 'I'), (2, 'A'), (14, 'D'),
                (32, 'M'), (30, 'L'), (22, 'H'), (10, 'B'), (26, 'J'), (12, 'C')]
heapify(T2)
print(T2 ==  [None, (2, 'A'), (10, 'B'), (12, 'C'), (16, 'E'), (18, 'F'), (20, 'G'), (14, 'D'),
 (32, 'M'), (30, 'L'), (22, 'H'), (24, 'I'), (26, 'J'), (28, 'K')])

print_heap(T2)
print()
print()
print_heap([None, (2, 'A'), (10, 'B'), (12, 'C'), (16, 'E'), (18, 'F'), (20, 'G'), (14, 'D'),
 (32, 'M'), (30, 'L'), (22, 'H'), (24, 'I'), (26, 'J'), (28, 'K')])
'''
'''
H50 = [None, (34, 'val_34'), (45, 'val_45'), (47, 'val_47'), (20, 'val_20'), (32, 'val_32'), (39, 'val_39'), (18, 'val_18'), (16, 'val_16'), (24, 'val_24'), (2, 'val_2'), (25, 'val_25'), (29, 'val_29'), (27, 'val_27'), (9, 'val_9'), (43, 'val_43'), (40, 'val_40'), (8, 'val_8'), (17, 'val_17'), (14, 'val_14'), (44, 'val_44'), (30, 'val_30'), (41, 'val_41'), (42, 'val_42'), (7, 'val_7'), (3, 'val_3'), (11, 'val_11'), (1, 'val_1'), (28, 'val_28'), (37, 'val_37'), (6, 'val_6'), (21, 'val_21'), (26, 'val_26'), (46, 'val_46'), (36, 'val_36'), (33, 'val_33'), (23, 'val_23'), (31, 'val_31'), (12, 'val_12'), (48, 'val_48'), (35, 'val_35'), (5, 'val_5'), (15, 'val_15'), (13, 'val_13'), (49, 'val_49'), (19, 'val_19'), (4, 'val_4'), (10, 'val_10'), (22, 'val_22'), (38, 'val_38'), (0, 'val_0')]
print_heap(H50)
Test_H50 = [None]
for i in range(50):
    heap_push(Test_H50, i, 'val_' + str(i))
print (H50 == Test_H50)
print_heap(Test_H50)
'''

