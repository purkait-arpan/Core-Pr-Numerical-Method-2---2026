! Determinant Finding of nxn matrix using Guess Elemination Method
! 6-Feb-2026
program detFind
implicit none
integer :: i, j, n
real :: det
real, allocatable :: A(:,:)
! Dimention of marix
write(*,*) "Entre Dimention of marix:"
read(*,*) n
allocate(A(n,n))
! Reading Matrix
do i = 1,n
read(*,*) (A(i,j), j = 1,n)
enddo
! Writing Matrix
write(*,*) "Initial Form of Matrix:"
do i = 1,n
write(*,*) "|",A(i,:),"|"
enddo
! Algorithm
do j = 1,n-1
do i = j+1,n
A(i,:) = A(i,:) - ((A(i,j) / A(j,j)) * A(j,:))
enddo
enddo
! After operation the Matrix
write(*,*) "After First operation the Matrix:"
do i = 1,n
write(*,*) "|",A(i,:),"|"
enddo
! The determinant finding
det = 1 ! Initialization 
do i = 1,n
det = det * A(i,i)
enddo
write(*,*) "The determinant is:", det
end program detFind






