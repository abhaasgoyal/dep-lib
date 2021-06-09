# dep-lib

## Intro

(Taken from project description)

Code that takes as input the immediate dependencies for various libraries (that is, which libraries import which other libraries), and produces as output the full set of dependencies for those libraries. For example:

### Input File

``` text
A depends on B C
B depends on C E
C depends on G
D depends on A F
E depends on F
F depends on H
```

### Output File

``` text
A depends on B C E F G H
B depends on C E F G H
C depends on G
D depends on A B C E F G H
E depends on F H
F depends on H
```
## Usage
To build, use `stack` (preferably) or `cabal`

Using stack

1. Building
``` sh
stack build
```
2. Executing

``` sh
stack exec dep-lib-exe <input-file>
```

3. Testing

``` sh
stack test
```


## Additional Notes
1. Outputs each line in the same order as in the input. For eg - If B is the parent dependency in first line and A in second line, it wil print in the same order - B and then A 
2. If the same parent dependency is stated in multiple lines, it will consider the dependency list as union of child dependencies and print the same calculated overall dependencies in different lines. For eg-

### Input
``` text
X depends on Y
X depends on R
Y depends on Z
```

### Output
``` text
X depends on R Y Z
X depends on R Y Z
Y depends on Z
```
3. The dependency list for each parent **within** a line is presented in a sorted order (since graph is represented by a dependency set). From the above example - Note that R, Y and Z are in sorted order.
