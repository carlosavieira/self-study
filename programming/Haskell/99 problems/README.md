# [The 99 problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems)


<details><summary>**Questions 1 to 10: Lists**</summary><p>
##Problem 1
(*) Find the last element of a list.

Example in Haskell: 
```haskell
Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
```

##Problem 2
(*) Find the last but one element of a list.

Example in Haskell: 
```haskell
Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'
```

##Problem 3
(*) Find the K'th element of a list. The first element in the list is number 1.

Example in Haskell:
```haskell
Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'
```

##Problem 4
(*) Find the number of elements of a list.

Example in Haskell:
```haskell
Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13
```

##Problem 5
(*) Reverse a list.

Example in Haskell:
```haskell
Prelude> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> myReverse [1,2,3,4]
[4,3,2,1]
```

##Problem 6
(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

Example in Haskell:
```haskell
*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True
```

##Problem 7
(**) Flatten a nested list structure.

Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).


Example in Haskell:
(We have to define a new data type, because lists in Haskell are homogeneous)
```haskell
 data NestedList a = Elem a | List [NestedList a]
*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[]
```

##Problem 8
(**) Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example in Haskell: 
```haskell
> compress "aaaabccaadeeee"
"abcade"
```

##Problem 9
(**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

Example in Haskell: 
```haskell
Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
            'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
```

##Problem 10
(*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example in Haskell:
```haskell
encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
```
</p></details>
<details><summary>**Questions 11 to 20: Lists II**</summary><p>
##Problem 11
(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example in Haskell:
```haskell
P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
```

##Problem 12
(**) Decode a run-length encoded list.

Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

Example in Haskell:
```haskell
P12> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
```

##Problem 13
(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

Example in Haskell:
```haskell
P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
```

##Problem 14
(*) Duplicate the elements of a list.

Example in Haskell:
```haskell
> dupli [1, 2, 3]
[1,1,2,2,3,3]
```


##Problem 15
(**) Replicate the elements of a list a given number of times.

Example in Haskell:
```haskell
> repli "abc" 3
"aaabbbccc"
```


##Problem 16
(**) Drop every N'th element from a list.

Example in Haskell:
```haskell
*Main> dropEvery "abcdefghik" 3
"abdeghk"
```


##Problem 17
(*) Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates.

Example in Haskell:
```haskell
*Main> split "abcdefghik" 3
("abc", "defghik")
```


##Problem 18
(**) Extract a slice from a list.

Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

Example in Haskell:
```haskell
*Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"
```


##Problem 19
(**) Rotate a list N places to the left.

Hint: Use the predefined functions length and (++).

Examples in Haskell:
```haskell
*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"
 
*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"
```


##Problem 20
(*) Remove the K'th element from a list.

Example in Haskell:
```haskell
*Main> removeAt 2 "abcd"
('b',"acd")
```
</p></details>
<details><summary>**Questions 21 to 28: Lists III**</summary><p>
##Problem 21
Insert an element at a given position into a list.

Example in Haskell:
```haskell
P21> insertAt 'X' "abcd" 2
"aXbcd"
```


##Problem 22
Create a list containing all integers within a given range.

Example in Haskell: 
```haskell
Prelude> range 4 9
[4,5,6,7,8,9]
```

##Problem 23
Extract a given number of randomly selected elements from a list.

Example in Haskell:
```haskell
Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
eda
```

##Problem 24
Lotto: Draw N different random numbers from the set 1..M.

Example in Haskell:
```haskell
Prelude System.Random>diff_select 6 49
Prelude System.Random>[23,1,17,33,21,37]
```


##Problem 25
Generate a random permutation of the elements of a list.

Example in Haskell:
```haskell
Prelude System.Random>rnd_permu "abcdef"
Prelude System.Random>"badcef"
```


##Problem 26
(**) Generate the combinations of K distinct objects chosen from the N elements of a list

In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

Example in Haskell:
```haskell
> combinations 3 "abcdef"
["abc","abd","abe",...]
```


##Problem 27
Group the elements of a set into disjoint subsets.

a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.

Example:

* (group3 '(aldo beat carla david evi flip gary hugo ida))
( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
... )
b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

Example:

* (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
... )
Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".

Example in Haskell:
```haskell
P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
(altogether 1260 solutions)
 
27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
(altogether 756 solutions)
```


##Problem 28
Sorting a list of lists according to length of sublists

a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

Example in Haskell:
```haskell
Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
Prelude>["o","de","de","mn","abc","fgh","ijkl"]
```
b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

Example in Haskell:
```haskell
lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
["ijkl","o","abc","fgh","de","de","mn"]
```
</p></details>
<details><summary>**Questions 31 to 41: Arithmetic**</summary><p>
##Problem 31
(**) Determine whether a given integer number is prime.

Example in Haskell:
```haskell
P31> isPrime 7
True
```

##Problem 32
(**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.

Example in Haskell:
```haskell
[myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
[9,3,3]
```

##Problem 33
(*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

Example:

* (coprime 35 64)
T
Example in Haskell:
```haskell
* coprime 35 64
True
```


##Problem 34
(**) Calculate Euler's totient function phi(m).

Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

Example in Haskell:
```haskell
* totient 10
4
```


##Problem 35
(**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.

Example in Haskell:
```haskell
> primeFactors 315
[3, 3, 5, 7]
```

##Problem 36
(**) Determine the prime factors of a given positive integer.

Construct a list containing the prime factors and their multiplicity.

Example in Haskell:
```haskell
*Main> prime_factors_mult 315
[(3,2),(5,1),(7,1)]
```


##Problem 37
(**) Calculate Euler's totient function phi(m) (improved).

See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:

phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
         (p2 - 1) * p2 ** (m2 - 1) * 
         (p3 - 1) * p3 ** (m3 - 1) * ...
Note that a ** b stands for the b'th power of a.

##Problem 38
(*) Compare the two methods of calculating Euler's totient function.

Use the solitions of problems 34 and 37 to compare the algorithms. Take the number of reductions as a measure for efficiency. Try to calculate phi(10090) as an example.

(no solution required)


##Problem 39
(*) A list of prime numbers.

Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

Example in Haskell:
```haskell
P29> primesR 10 20
[11,13,17,19]
```


##Problem 40
(**) Goldbach's conjecture.

Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.

Example in Haskell:
```haskell
*goldbach 28
(5, 23)
```

##Problem 41
(**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.

Example in Haskell:
```haskell
*Exercises> goldbachList 9 20
[(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
*Exercises> goldbachList' 4 2000 50
[(73,919),(61,1321),(67,1789),(61,1867)]
```
</p></details>
<details><summary>**Questions 46 to 50: Logic and codes**</summary><p>
##Problem 46
(**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.

A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).

Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

Example in Haskell:
```haskell
> table (\a b -> (and' a (or' a b)))
True True True
True False True
False True False
False False False
```

##Problem 47
(*) Truth tables for logical expressions (2).

Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.

Example in Haskell: 
```haskell
> table2 (\a b -> a `and'` (a `or'` not b))
True True True
True False True
False True False
False False False
```

##Problem 48
(**) Truth tables for logical expressions (3).

Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.

Example in Haskell:
```haskell
> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- infixl 3 `equ'`
True  True  True  True
True  True  False True
True  False True  True
True  False False True
False True  True  True
False True  False True
False False True  True
False False False True
 
-- infixl 7 `equ'`
True  True  True  True
True  True  False True
True  False True  True
True  False False False
False True  True  False
False True  False False
False False True  False
False False False False
```

##Problem 49
(**) Gray codes.

An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

n = 1: C(1) = ['0','1'].
n = 2: C(2) = ['00','01','11','10'].
n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
Find out the construction rules and write a predicate with the following specification:

% gray(N,C) :- C is the N-bit Gray code
Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?

Example in Haskell:
```haskell
P49> gray 3
["000","001","011","010","110","111","101","100"]
```


##Problem 50
(***) Huffman codes.

We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S. In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be performed by the predicate huffman/2 defined as follows:

% huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
Example in Haskell:
```haskell
*Exercises> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
[('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
```
</p></details>
<details><summary>**Questions 54A to 60: Binary trees**</summary><p>
A binary tree is either empty or it is composed of a root element and two successors, which are binary trees themselves.

p67.gif

In Haskell, we can characterize binary trees with a datatype definition:

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
This says that a Tree of type a consists of either an Empty node, or a Branch containing one value of type a with exactly two subtrees of type a.

Given this definition, the tree in the diagram above would be represented as:

tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))
Since a "leaf" node is a branch with two empty subtrees, it can be useful to define a shorthand function:

leaf x = Branch x Empty Empty
Then the tree diagram above could be expressed more simply as:

tree1' = Branch 'a' (Branch 'b' (leaf 'd')
                                (leaf 'e'))
                    (Branch 'c' Empty
                                (Branch 'f' (leaf 'g')
                                            Empty)))
Other examples of binary trees:

-- A binary tree consisting of a root node only
tree2 = Branch 'a' Empty Empty
 
-- An empty binary tree
tree3 = Empty
 
-- A tree of integers
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)
##Problem 54A
(*) Check whether a given term represents a binary tree

Haskell's type system ensures that all terms of type Tree a are binary trees: it is just not possible to construct an invalid tree with this type. Hence, it is redundant to introduce a predicate to check this property: it would always return True.

##Problem 55
(**) Construct completely balanced binary trees

In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.

Example in Haskell, whitespace and "comment diagrams" added for clarity and exposition:
```haskell
*Main> cbalTree 4
[
-- permutation 1
--     x
--    / \
--   x   x
--        \
--         x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)),
 
-- permutation 2
--     x
--    / \
--   x   x
--      /
--     x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty),
 
-- permutation 3
--     x
--    / \
--   x   x
--    \
--     x
Branch 'x' (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)) 
           (Branch 'x' Empty Empty),
 
-- permutation 4
--     x
--    / \
--   x   x
--  /
-- x
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty) 
           (Branch 'x' Empty Empty)
]
```

##Problem 56
(**) Symmetric binary trees

Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.

Example in Haskell:
```haskell
*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
False
*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
True
```


##Problem 57
(**) Binary search trees (dictionaries)

Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.

Example in Haskell:
```haskell
*Main> construct [3, 2, 5, 7, 1]
Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
*Main> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
True
*Main> symmetric . construct $ [3, 2, 5, 7, 1]
True
```


##Problem 58
(**) Generate-and-test paradigm

Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.

Example in Haskell:
```haskell
*Main> symCbalTrees 5
[Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]
```


##Problem 59
(**) Construct height-balanced binary trees

In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.

Construct a list of all height-balanced binary trees with the given element and the given maximum height.

Example in Haskell:
```haskell
*Main> take 4 $ hbalTree 'x' 3
[Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]
```

##Problem 60
(**) Construct height-balanced binary trees with a given number of nodes

Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?

Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more difficult. Try to find a recursive statement and turn it into a function minNodes that returns the minimum number of nodes in a height-balanced binary tree of height H. On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have? Write a function maxHeight that computes this.
Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes. Find out how many height-balanced trees exist for N = 15.

Example in Haskell:
```haskell
*Main> length $ hbalTreeNodes 'x' 15
1553
*Main> map (hbalTreeNodes 'x') [0..3]
[[Empty],
 [Branch 'x' Empty Empty],
 [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
 [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]
```
</p></details>
<details><summary>**Questions 61 to 69: Binary trees II**</summary><p>
As defined in problem 54A.

An example tree:

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

##Problem 61
Count the leaves of a binary tree

A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.

Example in Haskell:
```haskell
> countLeaves tree4
2
```

##Problem 61A
Collect the leaves of a binary tree in a list

A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.

Example in Haskell:
```haskell
> leaves tree4
[4,2]
```

##Problem 62
Collect the internal nodes of a binary tree in a list

An internal node of a binary tree has either one or two non-empty successors. Write a predicate internals/2 to collect them in a list.

Example in Haskell:
```haskell
Prelude>internals tree4
Prelude>[1,2]
```


##Problem 62B
Collect the nodes at a given level in a list

A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list.

Example in Haskell:
```haskell
Prelude>atLevel tree4 2
Prelude>[2,2]
```

##Problem 63
Construct a complete binary tree

A complete binary tree with height H is defined as follows:

The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order, starting at the root with number 1. For every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist. This fact can be used to elegantly construct a complete binary tree structure.

Write a predicate complete_binary_tree/2.

Example in Haskell:
```haskell
Main> completeBinaryTree 4
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
 
Main> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
True
```

##Problem 64
Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid. Several layout methods are conceivable, one of them is shown in the illustration below:

p64.gif

In this layout strategy, the position of a node v is obtained by the following two rules:

x(v) is equal to the position of the node v in the inorder sequence
y(v) is equal to the depth of the node v in the tree
Write a function to annotate each node of the tree with a position, where (1,1) in the top left corner or the rectangle bounding the drawn tree.

Here is the example tree from the above illustration:

Example in Haskell:
```haskell
> layout tree64
Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...
```


##Problem 65
An alternative layout method is depicted in the illustration below:

p65.gif

Find out the rules and write the corresponding function. Hint: On a given level, the horizontal distance between neighboring nodes is constant.

Use the same conventions as in problem P64 and test your function in an appropriate way.

Here is the example tree from the above illustration:

Example in Haskell:
```haskell
> layout tree65
Branch ('n',(15,1)) (Branch ('k',(7,2)) (Branch ('c',(3,3)) ...
```


##Problem 66
Yet another layout strategy is shown in the illustration below:

p66.gif

The method yields a very compact layout while maintaining a certain symmetry in every node. Find out the rules and write the corresponding Prolog predicate. Hint: Consider the horizontal distance between a node and its successor nodes. How tight can you pack together two subtrees to construct the combined binary tree?

Use the same conventions as in problem P64 and P65 and test your predicate in an appropriate way. Note: This is a difficult problem. Don't give up too early!

Which layout do you like most?

Example in Haskell:
```haskell
> layout tree65
Branch ('n',(5,1)) (Branch ('k',(3,2)) (Branch ('c',(2,3)) ...
```


##Problem 67A
A string representation of binary trees

Somebody represents binary trees as strings of the following type:

a(b(d,e),c(,f(g,)))
a) Write a Prolog predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term). Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, combine the two predicates in a single predicate tree_string/2 which can be used in both directions.

Example in Haskell:
```haskell
Main> stringToTree "x(y,a(,b))" >>= print
Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
Main> let t = cbtFromList ['a'..'z'] in stringToTree (treeToString t) >>= print . (== t)
True
```


##Problem 68
Preorder and inorder sequences of binary trees. We consider binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67.

a) Write predicates preorder/2 and inorder/2 that construct the preorder and inorder sequence of a given binary tree, respectively. The results should be atoms, e.g. 'abdecfg' for the preorder sequence of the example in problem P67.

b) Can you use preorder/2 from problem part a) in the reverse direction; i.e. given a preorder sequence, construct a corresponding tree? If not, make the necessary arrangements.

c) If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given, then the tree is determined unambiguously. Write a predicate pre_in_tree/3 that does the job.

Example in Haskell:
```haskell
Main> let { Just t = stringToTree "a(b(d,e),c(,f(g,)))" ;
            po = treeToPreorder t ;
            io = treeToInorder t } in preInTree po io >>= print
Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))
```


##Problem 69
Dotstring representation of binary trees.

We consider again binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67. Such a tree can be represented by the preorder sequence of its nodes in which dots (.) are inserted where an empty subtree (nil) is encountered during the tree traversal. For example, the tree shown in problem P67 is represented as 'abd..e..c.fg...'. First, try to establish a syntax (BNF or syntax diagrams) and then write a predicate tree_dotstring/2 which does the conversion in both directions. Use difference lists.

Example in Haskell:
```haskell
> fst (ds2tree example)
Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))
 
> tree2ds (Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty))
"xy..z0..."
```
</p></details>
<details><summary>**Questions 70B to 73: Multiway trees**</summary><p>
A multiway tree is composed of a root element and a (possibly empty) set of successors which are multiway trees themselves. A multiway tree is never empty. The set of successor trees is sometimes called a forest.

p70.gif

##Problem 70B
(*) Check whether a given term represents a multiway tree.

In Prolog or Lisp, one writes a predicate to check this.

In Haskell, we define multiway trees as a datatype, as in the module Data.Tree:

```haskell
data Tree a = Node a [Tree a] deriving (Eq, Show)
```
Some example trees:
```haskell
tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]
```
The last is the tree illustrated above.

As in problem 54A, all members of this type are multiway trees; there is no use for a predicate to test them.

##Problem 70C
(*) Count the nodes of a multiway tree.

Example in Haskell:
```haskell
Tree> nnodes tree2
2
```

##Problem 70
(**) Tree construction from a node string.

We suppose that the nodes of a multiway tree contain single characters. In the depth-first order sequence of its nodes, a special character ^ has been inserted whenever, during the tree traversal, the move is a backtrack to the previous level.

By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^^

p70.gif

Define the syntax of the string and write a predicate tree(String,Tree) to construct the Tree when the String is given. Make your predicate work in both directions.

Example in Haskell:
```haskell
Tree> stringToTree "afg^^c^bd^e^^^"
Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]
 
Tree> treeToString (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]])
"afg^^c^bd^e^^^"
```

##Problem 71
(*) Determine the internal path length of a tree.

We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree. By this definition, tree5 has an internal path length of 9.

Example in Haskell:
```haskell
Tree> ipl tree5
9
Tree> ipl tree4
2
```


##Problem 72
(*) Construct the bottom-up order sequence of the tree nodes.

Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence of the nodes of the multiway tree Tree.

Example in Haskell:
```haskell
Tree> bottom_up tree5
"gfcdeba"
```


##Problem 73
(**) Lisp-like tree representation.

There is a particular notation for multiway trees in Lisp. Lisp is a prominent functional programming language, which is used primarily for artificial intelligence problems. As such it is one of the main competitors of Prolog. In Lisp almost everything is a list, just as in Prolog everything is a term.

The following pictures show how multiway tree structures are represented in Lisp.

p73.png

Note that in the "lispy" notation a node with successors (children) in the tree is always the first element in a list, followed by its children. The "lispy" representation of a multiway tree is a sequence of atoms and parentheses '(' and ')', which we shall collectively call "tokens". We can represent this sequence of tokens as a Prolog list; e.g. the lispy expression (a (b c)) could be represented as the Prolog list ['(', a, '(', b, c, ')', ')']. Write a predicate tree_ltl(T,LTL) which constructs the "lispy token list" LTL if the tree is given as term T in the usual Prolog notation.

(The Prolog example given is incorrect.)

Example in Haskell:
```haskell
Tree> display lisp tree1
"a"
Tree> display lisp tree2
"(a b)"
Tree> display lisp tree3
"(a (b c))"
Tree> display lisp tree4
"(b d e)"
Tree> display lisp tree5
"(a (f g) c (b d e))"
```
As a second, even more interesting exercise try to rewrite tree_ltl/2 in a way that the inverse conversion is also possible.
</p></details>
<details><summary>**Questions 80 to 89: Graphs**</summary><p>
A graph is defined as a set of nodes and a set of edges, where each edge is a pair of nodes.

graph1.gif

There are several ways to represent graphs in Prolog. One method is to represent each edge separately as one clause (fact). In this form, the graph depicted below is represented as the following predicate:

edge(h,g).
edge(k,f).
edge(f,b).
...
We call this edge-clause form. Obviously, isolated nodes cannot be represented. Another method is to represent the whole graph as one data object. According to the definition of the graph as a pair of two sets (nodes and edges), we may use the following Prolog term to represent the example graph:

graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])
We call this graph-term form. Note, that the lists are kept sorted, they are really sets, without duplicated elements. Each edge appears only once in the edge list; i.e. an edge from a node x to another node y is represented as e(x,y), the term e(y,x) is not present. The graph-term form is our default representation. In SWI-Prolog there are predefined predicates to work with sets.

A third representation method is to associate with each node the set of nodes that are adjacent to that node. We call this the adjacency-list form. In our example:

[n(b,[c,f]), n(c,[b,f]), n(d,[]), n(f,[b,c,k]), ...]
The representations we introduced so far are Prolog terms and therefore well suited for automated processing, but their syntax is not very user-friendly. Typing the terms by hand is cumbersome and error-prone. We can define a more compact and "human-friendly" notation as follows: A graph is represented by a list of atoms and terms of the type X-Y (i.e. functor '-' and arity 2). The atoms stand for isolated nodes, the X-Y terms describe edges. If an X appears as an endpoint of an edge, it is automatically defined as a node. Our example could be written as:

[b-c, f-c, g-h, d, f-b, k-f, h-g]
We call this the human-friendly form. As the example shows, the list does not have to be sorted and may even contain the same edge multiple times. Notice the isolated node d. (Actually, isolated nodes do not even have to be atoms in the Prolog sense, they can be compound terms, as in d(3.75,blue) instead of d in the example).

graph2.gif

When the edges are directed we call them arcs. These are represented by ordered pairs. Such a graph is called directed graph. To represent a directed graph, the forms discussed above are slightly modified. The example graph above is represented as follows:

###Arc-clause form

arc(s,u).
arc(u,r).
...
###Graph-term form

digraph([r,s,t,u,v],[a(s,r),a(s,u),a(u,r),a(u,s),a(v,u)])
Adjacency-list form

[n(r,[]),n(s,[r,u]),n(t,[]),n(u,[r]),n(v,[u])]
Note that the adjacency-list does not have the information on whether it is a graph or a digraph.

###Human-friendly form

[s > r, t, u > r, s > u, u > s, v > u] 
Finally, graphs and digraphs may have additional information attached to nodes and edges (arcs). For the nodes, this is no problem, as we can easily replace the single character identifiers with arbitrary compound terms, such as city('London',4711). On the other hand, for edges we have to extend our notation. Graphs with additional information attached to edges are called labelled graphs.

graph3.gif

###Arc-clause form

arc(m,q,7).
arc(p,q,9).
arc(p,m,5).
###Graph-term form

digraph([k,m,p,q],[a(m,p,7),a(p,m,5),a(p,q,9)])
###Adjacency-list form

[n(k,[]),n(m,[q/7]),n(p,[m/5,q/9]),n(q,[])]
Notice how the edge information has been packed into a term with functor '/' and arity 2, together with the corresponding node.

###Human-friendly form

[p>q/9, m>q/7, k, p>m/5]
The notation for labelled graphs can also be used for so-called multi-graphs, where more than one edge (or arc) are allowed between two given nodes.

##Problem 80
(***) Conversions

Write predicates to convert between the different graph representations. With these predicates, all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form. The reason this problem is rated (***) is not because it's particularly difficult, but because it's a lot of work to deal with all the special cases.

Example in Haskell:
```haskell
graphToAdj Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]
```

##Problem 81
(**) Path from one node to another one

Write a function that, given two nodes a and b in a graph, returns all the acyclic paths from a to b.

Example in Haskell:
```haskell
paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[[1,2,3,4],[1,3,4]]
paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[]
```

##Problem 82
(*) Cycle from a given node

Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a given node A in the graph G. The predicate should return all cycles via backtracking.

Example in Haskell:
```haskell
graph> cycle 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[[2,3,4,2]]
graph> cycle 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[]
```

##Problem 83
(**) Construct all spanning trees

Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all spanning trees of a given graph. With this predicate, find out how many spanning trees there are for the graph depicted to the left. The data of this example graph can be found in the file p83.dat. When you have a correct solution for the s_tree/2 predicate, use it to define two other useful predicates: is_tree(Graph) and is_connected(Graph). Both are five-minutes tasks!

Example in Haskell:
```haskell
length $ spantree k4
16
```

##Problem 84
(**) Construct the minimal spanning tree

Write a predicate ms_tree(Graph,Tree,Sum) to construct the minimal spanning tree of a given labelled graph. Hint: Use the algorithm of Prim. A small modification of the solution of P83 does the trick. The data of the example graph to the right can be found in the file p84.dat.

Example in Haskell:
```haskell
prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
[(1,2,12),(1,3,34),(2,4,55),(2,5,32)]
```


##Problem 85
(**) Graph isomorphism

Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection f: N1 -> N2 such that for any nodes X,Y of N1, X and Y are adjacent if and only if f(X) and f(Y) are adjacent.

Write a predicate that determines whether two graphs are isomorphic. Hint: Use an open-ended list to represent the function f.

Example in Haskell:
```haskell
graphG1 = [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
iso graphG1 graphH1
True
```

##Problem 86
(**) Node degree and graph coloration

a) Write a predicate degree(Graph,Node,Deg) that determines the degree of a given node.

b) Write a predicate that generates a list of all nodes of a graph sorted according to decreasing degree.

c) Use Welch-Powell's algorithm to paint the nodes of a graph in such a way that adjacent nodes have different colors.

Example in Haskell:
```haskell
kcolor ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]
[('a',1),('b',2),('c',1),('d',2),('e',3),('f',2),('g',1),('h',3),('i',3),('j',2)]
```

##Problem 87
(**) Depth-first order graph traversal (alternative solution)

Write a predicate that generates a depth-first order graph traversal sequence. The starting point should be specified, and the output should be a list of nodes that are reachable from this starting point (in depth-first order).

Example in Haskell:
```haskell
depthfirst ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]) 1
[1,2,3,4,5]
```

##Problem 88
(**) Connected components (alternative solution)

Write a predicate that splits a graph into its connected components.

Example in Haskell:
```haskell
connectedcomponents ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])
[[1,2,3,4,5][6,7]]
```

##Problem 89
(**) Bipartite graphs

Write a predicate that finds out whether a given graph is bipartite.

Example in Haskell:
```haskell
bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)])
True
bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)])
False

#Miscellaneous problems
##Problem 90
(**) Eight queens problem

This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.

Hint: Represent the positions of the queens as a list of numbers 1..N. Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.

Example in Haskell:
```haskell
> length (queens 8)
92
> head (queens 8)
[1,5,8,6,3,7,2,4]
```
</p></details>
<details><summary>**Questions 90 to 94: Miscellaneous problems**</summary><p>
##Problem 91
(**) Knight's tour

Another famous problem is this one: How can a knight jump on an NxN chessboard in such a way that it visits every square exactly once? A set of solutions is given on the The_Knights_Tour page.

Hints: Represent the squares by pairs of their coordinates of the form X/Y, where both X and Y are integers between 1 and N. (Note that '/' is just a convenient functor, not division!) Define the relation jump(N,X/Y,U/V) to express the fact that a knight can jump from X/Y to U/V on a NxN chessboard. And finally, represent the solution of our problem as a list of N*N knight positions (the knight's tour).

There are two variants of this problem:

find a tour ending at a particular square
find a circular tour, ending a knight's jump from the start (clearly it doesn't matter where you start, so choose (1,1))
Example in Haskell:
```haskell
Knights> head $ knightsTo 8 (1,1)
[(2,7),(3,5),(5,6),(4,8),(3,6),(4,4),(6,5),(4,6),
(5,4),(7,5),(6,3),(5,5),(4,3),(2,4),(1,6),(2,8),
(4,7),(6,8),(8,7),(6,6),(4,5),(6,4),(5,2),(7,1),
(8,3),(6,2),(8,1),(7,3),(8,5),(7,7),(5,8),(3,7),
(1,8),(2,6),(3,4),(1,5),(2,3),(3,1),(1,2),(3,3),
(1,4),(2,2),(4,1),(5,3),(7,4),(8,2),(6,1),(4,2),
(2,1),(1,3),(2,5),(1,7),(3,8),(5,7),(7,8),(8,6),
(6,7),(8,8),(7,6),(8,4),(7,2),(5,1),(3,2),(1,1)]
Knights> head $ closedKnights 8
[(1,1),(3,2),(1,3),(2,1),(3,3),(5,4),(6,6),(4,5),
(2,6),(1,8),(3,7),(5,8),(4,6),(2,5),(4,4),(5,6),
(6,4),(8,5),(7,7),(6,5),(5,3),(6,1),(4,2),(6,3),
(8,2),(7,4),(5,5),(3,4),(1,5),(2,7),(4,8),(3,6),
(1,7),(3,8),(5,7),(7,8),(8,6),(6,7),(8,8),(7,6),
(8,4),(7,2),(5,1),(4,3),(3,5),(1,4),(2,2),(4,1),
(6,2),(8,1),(7,3),(5,2),(7,1),(8,3),(7,5),(8,7),
(6,8),(4,7),(2,8),(1,6),(2,4),(1,2),(3,1),(2,3)]
```


##Problem 92
(***) Von Koch's conjecture

Several years ago I met a mathematician who was intrigued by a problem for which he didn't know a solution. His name was Von Koch, and I don't know whether the problem has been solved since.

p92a.gif

Anyway the puzzle goes like this: Given a tree with N nodes (and hence N-1 edges). Find a way to enumerate the nodes from 1 to N and, accordingly, the edges from 1 to N-1 in such a way, that for each edge K the difference of its node numbers equals to K. The conjecture is that this is always possible.

For small trees the problem is easy to solve by hand. However, for larger trees, and 14 is already very large, it is extremely difficult to find a solution. And remember, we don't know for sure whether there is always a solution!

Write a predicate that calculates a numbering scheme for a given tree. What is the solution for the larger tree pictured below?

p92b.gif

Example in Haskell:
```haskell
> head $ vonKoch [(1,6),(2,6),(3,6),(4,6),(5,6),(5,7),(5,8),(8,9),(5,10),(10,11),(11,12),(11,13),(13,14)]
[6,7,8,9,3,4,10,11,5,12,2,13,14,1]
```


##Problem 93
(***) An arithmetic puzzle

Given a list of integer numbers, find a correct way of inserting arithmetic signs (operators) such that the result is a correct equation. Example: With the list of numbers [2,3,5,7,11] we can form the equations 2-3+5+7 = 11 or 2 = (3*5+7)/11 (and ten others!).

Division should be interpreted as operating on rationals, and division by zero should be avoided.

Example in Haskell:
```haskell
P93> mapM_ putStrLn $ puzzle [2,3,5,7,11]
2 = 3-(5+7-11)
2 = 3-5-(7-11)
2 = 3-(5+7)+11
2 = 3-5-7+11
2 = (3*5+7)/11
2*(3-5) = 7-11
2-(3-(5+7)) = 11
2-(3-5-7) = 11
2-(3-5)+7 = 11
2-3+5+7 = 11
```
The other two solutions alluded to in the problem description are dropped by the Haskell solution as trivial variants:
```haskell
2 = 3-(5+(7-11))
2-3+(5+7) = 11
```


##Problem 94
(***) Generate K-regular simple graphs with N nodes

In a K-regular graph all nodes have a degree of K; i.e. the number of edges incident in each node is K. How many (non-isomorphic!) 3-regular graphs with 6 nodes are there?

Sample results

Example in Haskell:
```haskell
length $ regular 6 3
2
```
</p></details>
<details><summary>**Questions 95 to 99: Miscellaneous problems II**</summary><p>
##Problem 95
(**) English number words

On financial documents, like cheques, numbers must sometimes be written in full words. Example: 175 must be written as one-seven-five. Write a predicate full-words/1 to print (non-negative) integer numbers in full words.

Example in Haskell:
```haskell
> fullWords 175
one-seven-five
```


##Problem 96
(**) Syntax checker

In a certain programming language (Ada) identifiers are defined by the syntax diagram below.

p96.gif

Transform the syntax diagram into a system of syntax diagrams which do not contain loops; i.e. which are purely recursive. Using these modified diagrams, write a predicate identifier/1 that can check whether or not a given string is a legal identifier.

Example in Haskell:
```haskell
> identifier "this-is-a-long-identifier"
True
> identifier "this-ends-in-"
False
> identifier "two--hyphens" 
False
```


##Problem 97
(**) Sudoku

Sudoku puzzles go like this:

       Problem statement                 Solution

        .  .  4 | 8  .  . | .  1  7	     9  3  4 | 8  2  5 | 6  1  7	     
                |         |                          |         |
        6  7  . | 9  .  . | .  .  .	     6  7  2 | 9  1  4 | 8  5  3
                |         |                          |         |
        5  .  8 | .  3  . | .  .  4          5  1  8 | 6  3  7 | 9  2  4
        --------+---------+--------          --------+---------+--------
        3  .  . | 7  4  . | 1  .  .          3  2  5 | 7  4  8 | 1  6  9
                |         |                          |         |
        .  6  9 | .  .  . | 7  8  .          4  6  9 | 1  5  3 | 7  8  2
                |         |                          |         |
        .  .  1 | .  6  9 | .  .  5          7  8  1 | 2  6  9 | 4  3  5
        --------+---------+--------          --------+---------+--------
        1  .  . | .  8  . | 3  .  6	     1  9  7 | 5  8  2 | 3  4  6
                |         |                          |         |
        .  .  . | .  .  6 | .  9  1	     8  5  3 | 4  7  6 | 2  9  1
                |         |                          |         |
        2  4  . | .  .  1 | 5  .  .          2  4  6 | 3  9  1 | 5  7  8
Every spot in the puzzle belongs to a (horizontal) row and a (vertical) column, as well as to one single 3x3 square (which we call "square" for short). At the beginning, some of the spots carry a single-digit number between 1 and 9. The problem is to fill the missing spots with digits in such a way that every number between 1 and 9 appears exactly once in each row, in each column, and in each square.


##Problem 98
(***) Nonograms

Around 1994, a certain kind of puzzle was very popular in England. The "Sunday Telegraph" newspaper wrote: "Nonograms are puzzles from Japan and are currently published each week only in The Sunday Telegraph. Simply use your logic and skill to complete the grid and reveal a picture or diagram." As a Prolog programmer, you are in a better situation: you can have your computer do the work! Just write a little program ;-).

The puzzle goes like this: Essentially, each row and column of a rectangular bitmap is annotated with the respective lengths of its distinct strings of occupied cells. The person who solves the puzzle must complete the bitmap given only these lengths.

             Problem statement:          Solution:
             |_|_|_|_|_|_|_|_| 3         |_|X|X|X|_|_|_|_| 3           
             |_|_|_|_|_|_|_|_| 2 1       |X|X|_|X|_|_|_|_| 2 1         
             |_|_|_|_|_|_|_|_| 3 2       |_|X|X|X|_|_|X|X| 3 2         
             |_|_|_|_|_|_|_|_| 2 2       |_|_|X|X|_|_|X|X| 2 2         
             |_|_|_|_|_|_|_|_| 6         |_|_|X|X|X|X|X|X| 6           
             |_|_|_|_|_|_|_|_| 1 5       |X|_|X|X|X|X|X|_| 1 5         
             |_|_|_|_|_|_|_|_| 6         |X|X|X|X|X|X|_|_| 6           
             |_|_|_|_|_|_|_|_| 1         |_|_|_|_|X|_|_|_| 1           
             |_|_|_|_|_|_|_|_| 2         |_|_|_|X|X|_|_|_| 2           
              1 3 1 7 5 3 4 3             1 3 1 7 5 3 4 3              
              2 1 5 1                     2 1 5 1                      
      
For the example above, the problem can be stated as the two lists [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] and [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]] which give the "solid" lengths of the rows and columns, top-to-bottom and left-to-right, respectively. Published puzzles are larger than this example, e.g. 25 x 20, and apparently always have unique ```.

Example in Haskell:
```haskell
Nonogram> putStr $ nonogram [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
|_|X|X|X|_|_|_|_| 3
|X|X|_|X|_|_|_|_| 2 1
|_|X|X|X|_|_|X|X| 3 2
|_|_|X|X|_|_|X|X| 2 2
|_|_|X|X|X|X|X|X| 6
|X|_|X|X|X|X|X|_| 1 5
|X|X|X|X|X|X|_|_| 6
|_|_|_|_|X|_|_|_| 1
|_|_|_|X|X|_|_|_| 2
 1 3 1 7 5 3 4 3
 2 1 5 1
```


##Problem 99
(***) Crossword puzzle

Given an empty (or almost empty) framework of a crossword puzzle and a set of words. The problem is to place the words into the framework.

p99.gif

The particular crossword puzzle is specified in a text file which first lists the words (one word per line) in an arbitrary order. Then, after an empty line, the crossword framework is defined. In this framework specification, an empty character location is represented by a dot (.). In order to make the solution easier, character locations can also contain predefined character values. The puzzle above is defined in the file p7_09a.dat, other examples are p7_09b.dat and p7_09d.dat. There is also an example of a puzzle (p7_09c.dat) which does not have a solution.

Words are strings (character lists) of at least two characters. A horizontal or vertical sequence of character places in the crossword puzzle framework is called a site. Our problem is to find a compatible way of placing words onto sites.

Hints: (1) The problem is not easy. You will need some time to thoroughly understand it. So, don't give up too early! And remember that the objective is a clean solution, not just a quick-and-dirty hack!

(2) Reading the data file is a tricky problem for which a solution is provided in the file p7_09-readfile.pl. See the predicate read_lines/2.

(3) For efficiency reasons it is important, at least for larger puzzles, to sort the words and the sites in a particular order. For this part of the problem, the solution of P28 may be very helpful.

Example in Haskell:
```haskell
ALPHA
ARES
POPPY

  .
  .
.....
  . .
  . .
    .
> solve $ readCrossword "ALPHA\nARES\nPOPPY\n\n  .  \n  .  \n.....\n  . .\n  . .\n    .\n"
 
[[((3,1),'A'),((3,2),'L'),((3,3),'P'),((3,4),'H'),((3,5),'A'),((1,3),'P'),((2,3)
,'O'),((3,3),'P'),((4,3),'P'),((5,3),'Y'),((3,5),'A'),((4,5),'R'),((5,5),'E'),((
6,5),'S')]]
```
</p></details>

(Though the problems number from 1 to 99, there are some gaps and some additions marked with letters. There are actually only 88 problems.)
