! Solution of Simultaneous Linear Equation
! by using Gauss Elemination method and Gauss Jordon Method
! Mar 6, 2025 - Arpan Purkait
program main
real :: Am(3,4), sol(3)
call readAugMat(Am)
call printAugMat(Am)
!call GaussElemination_solver(Am,sol)
call GaussJorden_solver(Am,sol)
call printAugMat(Am)
write(*,*) "Solution is:", sol(:)
end program main

subroutine readAugMat(Am)
real, intent(inout) :: Am(3,4)
open(1,file="LinearEqns.txt",status="unknown")
do i = 1,3
read(1,*) Am(i,:)
enddo
end subroutine readAugMat

subroutine printAugMat(Am)
real, intent(inout) :: Am(3,4)
write(*,*) "Printing ..."
do i = 1,3
write(*,*) Am(i,:)
enddo
end subroutine printAugMat

subroutine GaussJorden_solver(Am,sol)
real, intent(inout) :: Am(3,4),sol(3)
integer :: n = 3
do j = 1,n
do i = 1,n
if (j /= i) Am(i,:) = Am(i,:) - ((Am(i,j) / Am(j,j)) * Am(j,:))
enddo
enddo
do i = 1,n
Am(i,:) = Am(i,:)/Am(i,i)
enddo
sol(:) = Am(:,4)
endsubroutine GaussJorden_solver

subroutine GaussElemination_solver(Am,sol)
real, intent(inout) :: Am(3,4), sol(3)
integer :: n = 3
do j = 1,n-1
do i = j+1,n
Am(i,:) = Am(i,:) - ((Am(i,j) / Am(j,j)) * Am(j,:))
enddo
enddo
do i = 1,n
Am(i,:) = Am(i,:)/Am(i,i)
enddo
sol(3) = Am(3,4)
sol(2) = Am(2,4) - (Am(2,3) * sol(3))
sol(1) = Am(1,4) - (Am(1,3) * sol(3)) - (Am(1,2) * sol(2))
end subroutine GaussElemination_solver


