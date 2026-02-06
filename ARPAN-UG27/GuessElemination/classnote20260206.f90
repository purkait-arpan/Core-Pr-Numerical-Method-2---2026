! Determinant Finding of nxn matrix using Guess Elemination Method
! 6-Feb-2026
program detFind
implicit none
integer :: i, j
real :: det
real :: A(3,3)
! Reading Matrix
!do i = 1,3
!do j = 1,3
!read(*,*) A(i,j)
!enddo 
!enddo
A(1,:) = [1,1,1]
A(2,:) = [2,3,1]
A(3,:) = [1,2,3]
! Writing Matrix
write(*,*) "Initial Form of Matrix:"
do i = 1,3
write(*,*) "|",A(i,:),"|"
enddo
! Operations - 1 
A(1,:) = A(1,:)
A(2,:) = A(2,:) - ((A(2,1) / A(1,1)) * A(1,:))
A(3,:) = A(3,:) - ((A(3,1) / A(1,1)) * A(1,:))
! After First operation the Matrix
write(*,*) "After First operation the Matrix:"
do i = 1,3
write(*,*) "|",A(i,:),"|"
enddo
! Operation - 2
A(1,:) = A(1,:)
A(2,:) = A(2,:)
A(3,:) = A(3,:) - ((A(3,2) / A(2,2)) * A(2,:))
! After Second operation the Matrix
write(*,*) "After Second operation the Matrix:"
do i = 1,3
write(*,*) "|",A(i,:),"|"
enddo
! The determinant finding
det = 1 ! Initialization 
do i = 1,3
det = det * A(i,i)
enddo
write(*,*) "The determinant is:", det
end program detFind






