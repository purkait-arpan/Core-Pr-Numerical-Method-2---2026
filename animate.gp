# set term qt
set terminal gif animate delay 5
set output "heat_animation_1D_Rod.gif"
set xlabel "Grid Point"
set ylabel "Temperature"
set yrange [0:120]
set xrange [0:100]
set grid
set title "1D Heat Conduction (FTCS Scheme) on 1D Rod"

L = 1.0
c = 0.5
dx = L / 100.0
dt = c * dx**2
t = 0
do for [i=1:9999] {
    
    filename = sprintf("run%04d.dat", i)
    plot filename u 1:2 w l lw 2 title sprintf("Numerical Soln After Step %d", i), \
    'run0001.dat' u 1:2 w p pt 21 title "Initial Temp Dist"
    
    pause 0.01
    
    t = i * dt
}
