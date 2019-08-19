## knight-shortest-path
Simple CLI tool which accepts in `STDIN` a list of chess positions and return in `STDOUT` the shortest sequence of moves a knight can make.

### Interactive use
```bash
λ ~/knight-shortest-path/ stack runhaskell knight.hs 8 8                        
A1 C2 B5 F2 A1 H8
A1 C2 A3 B5 C3 D1 F2 D3 C1 B3 A1 B3 D4 F5 H6 F7 H8
A1
INVALID
```

### Configuring the board size
 - If one parameter is provided, it will be used as both width and height for the board
 - If two parameters are provided, they will be used respectively as width and height for the board
 - Letters from A-Z are used for the x coordinate and they stack like a number in base 26 (0->A, 25->Z, 26->BA, 27->BB, etc)

### Generate test data
A short function `generateTestData` is provided to generate all the possible problems at a certain board size.

Test results for a 8x8 board are provided in the `test` file.

### Algorithm
The algorithm is `A*` and uses euclidean distance as a heuristic.

This gives priority to the cells more likely to get closer to a solution and improves the speed considerably.

If the heuristic function is nooped (`heuristic n = 0`), the algorithm turns into `BFS`.

Compared to `BFS`, time is halved when using `A*` on the test file - and the solutions are still the shortest paths:
```bash
λ ~/knight-shortest-path/ time stack runhaskell knight.hs 8 8 < test > astar-solution 
stack runhaskell knight.hs 8 8 < test > astar-solution  1.66s user 0.23s system 102% cpu 1.833 total
λ ~/knight-shortest-path/ time stack runhaskell knight.hs 8 8 < test > bfs-solution
stack runhaskell knight.hs 8 8 < test > bfs-solution  3.78s user 0.20s system 101% cpu 3.935 total
λ ~/knight-shortest-path/ diff <(awk '{print length+1}' astar-solution) <(awk '{print length+1}' bfs-solution)
```

Using `BFS` the program couldn't traverse a 10000x10000 cells board after 131s (and I decided to kill it).

The same problem was solved in 1.54s using `A*`:
```bash
λ ~/knight-shortest-path/ time echo "A1 OUP10000" | stack runhaskell knight.hs 10000 10000 > /dev/null
echo "A1 OUP10000"  0.00s user 0.00s system 56% cpu 0.002 total
stack runhaskell knight.hs 10000 10000 > /dev/null  1.54s user 0.15s system 102% cpu 1.654 total
λ ~/knight-shortest-path/ time echo "A1 OUP10000" | stack runhaskell knight.hs 10000 10000 > /dev/null
^C
echo "A1 OUP10000"  0.00s user 0.00s system 49% cpu 0.002 total
stack runhaskell knight.hs 10000 10000 > /dev/null  131.03s user 0.65s system 99% cpu 2:11.95 total
```

### TODO (aka possible improvements I will probably never touch again)
 - Return all the shortest paths?
