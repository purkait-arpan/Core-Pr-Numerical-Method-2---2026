! Finite Difference method - problem 1: Laplace Equation in 2D
program laplace
real	:: grid(10,10)
call initialize_grid(grid)
call solve_laplace(grid)
call save_grid(grid)
call plot_grid(grid)
end program laplace

subroutine initialize_grid(grid)
real, intent(inout) :: grid(10,10)
integer:: i,j
grid = 0
do j = 4,7
do i = 4,7
grid(i,j) = 100.0
enddo
enddo
end subroutine initialize_grid

subroutine solve_laplace(grid)
real, intent(inout) :: grid(10,10)
integer :: i, j, k, l, m
real :: dummy_grid(10,10)
1 dummy_grid(:,:) = grid(:,:)
do i = 2,9
do j = 2,9
grid(i,j) =(grid(i+1,j) + grid(i-1,j) + grid(i,j+1) + grid(i,j-1))*0.25
do k = 4,7
do l = 4,7
grid(k,l) = 100
enddo
enddo
enddo
enddo
if (all(abs(grid - dummy_grid) > 0.01)) goto 1
end subroutine solve_laplace

subroutine save_grid(grid)
real, intent(inout) :: grid(10,10)
integer :: i
open(45,file="100points.dat",status="unknown")
write(*,*) "Solution saved at - 100points.dat"
do i = 1, 10
write(45,*) grid(i,:)
enddo
end subroutine

subroutine plot_grid(grid)
call system("gnuplot -persist 100grid_problem.gnu")
end subroutine plot_grid
