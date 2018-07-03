# nurikabe-hs
A graphical implementation of the Nurikabe puzzle with Haskell and gtk2hs.

I built this to convince myself I could write a graphical app in Haskell. And it works! But it's extremely ugly. 
To do: figure out how to use CSS with gtk2hs?

The repo contains the Linux executable nurikabe. To run it, use the syntax
```
./nurikabe testGrid.grid
```
where the grid file contains a description of a puzzle grid as a rectangular nested list, with zeroes as cells with no clue. That is,
`[[0,0,1],[1,0,0],[0,0,0]]` gives rise to the 3 x 3 puzzle grid

```
-------
| | |1|
-------
|1| | |
-------
| | | |
-------
```
For details about the Nurikabe puzzle format, check out for instance https://www.gmpuzzles.com/blog/nurikabe-rules-and-info/.
