! Heat Conduction Home work Problem 1
! Assignment Date: 20 Fab 2026
program HeatCondn
real :: grid(101)
integer :: step = 9999
real :: c = 0.5
call initialize_grid(grid)
call runSteps(step,grid,c)
end program HeatCondn

subroutine initialize_grid(grid)
real, intent(inout) :: grid(101)
grid = 30.0
grid(101) = 100.0
grid(1)  = 100.0
write(*,*) "Grid Initialized ..."
write(*,1) grid
1 format(10f8.1)
end subroutine initialize_grid

subroutine printGrid(grid)
real, intent(inout) :: grid(101)
write(*,1) grid
1 format(10f8.1)
end subroutine printGrid

subroutine FTCS(grid,c)
real, intent(inout) :: grid(101)
real, intent(in) :: c
integer :: i
real :: dummy_grid(101)
do i = 2,100
dummy_grid(i) = grid(i) + c * (grid(i+1) + grid(i-1) - (2 * grid(i)))
enddo
do i = 2,100
grid(i) = dummy_grid(i)
enddo
end subroutine FTCS

subroutine runSteps(step,grid,c)
integer, intent(in) :: step
real, intent(inout) :: grid(101)
real, intent(in) :: c
integer :: i, j
character(len=20) :: filename
do i = 1, step
call FTCS(grid,c)
write(filename,'("run",I4.4,".dat")') i
open(20,file=filename,status="replace")
do j = 1, 101
write(20,*) j-1, grid(j)
enddo
 close(20)
!call printGrid(Grid)
!call sleep(1)
enddo
end subroutine runSteps
