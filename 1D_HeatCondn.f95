! Heat Conduction Problem
! Date: 20 Fab 2026
program HeatCondn
real :: grid(10)
integer :: step = 20
real :: c = 0.5
call initialize_grid(grid)
call runSteps(step,grid,c)
end program HeatCondn

subroutine initialize_grid(grid)
real, intent(inout) :: grid(10)
grid = 30.0
grid(10) = 100.0
grid(1) = 100.0
write(*,*) "Grid Initialized ..."
write(*,1) grid
1 format(10f8.1)
end subroutine initialize_grid

subroutine printGrid(grid)
real, intent(inout) :: grid(10)
write(*,1) grid
1 format(10f8.1)
end subroutine printGrid

subroutine FTCS(grid,c)
real, intent(inout) :: grid(10)
real, intent(in) :: c
integer :: i
real :: dummy_grid(10)
do i = 2,9
dummy_grid(i) = grid(i) + c * (grid(i+1) + grid(i-1) - (2 * grid(i)))
enddo
do i = 2,9
grid(i) = dummy_grid(i)
enddo
end subroutine FTCS

subroutine runSteps(step,grid,c)
integer, intent(in) :: step
real, intent(inout) :: grid(10)
real, intent(in) :: c
do i = 1, step
call FTCS(grid,c)
call printGrid(Grid)
!call sleep(1)
enddo
end subroutine runSteps
