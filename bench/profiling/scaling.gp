set terminal svg size 760,520 font 'sans,12'
set output 'bench/profiling/scaling.svg'
set title "sqlocaml per-operation cost vs table size (plaintext, log-log)\nslope = 1 means O(n) per op"
set xlabel "table rows"
set ylabel "ms per operation"
set logscale xy
set key top left
set grid
set xrange [800:5000]
# reference O(n) line through the insert 1k point (slope 1 on log-log)
f(x) = 9.72 * (x/1000.0)
plot \
  'bench/profiling/scaling.dat' using 1:2 with linespoints lw 2 pt 7 title 'point lookup (WHERE pk=?)', \
  '' using 1:3 with linespoints lw 2 pt 5 title 'full scan / aggregate', \
  '' using 1:4 with linespoints lw 2 pt 9 title 'insert (per row)', \
  f(x) with lines dt 2 lc rgb 'gray40' title 'ideal O(n) slope'
