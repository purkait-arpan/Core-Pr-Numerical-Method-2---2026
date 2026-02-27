program GaussJorden
real :: A(3,3), inv_A(3,3), X(3,3), dummy(3,3)
call matrix_initializer(A)
call matrix_printer(A)
dummy = A
!call GaussJorden_diagonalizer(A)
call GaussJorden_inverse(A,inv_A)
call matrix_printer(inv_A)
X = matmul(dummy,inv_A)
call matrix_printer(X)
end program GaussJorden

subroutine matrix_initializer(A)
real, intent(inout) :: A(3,3)
A(1,1) = 1; A(1,2) = 1; A(1,3) = 1
A(2,1) = 2; A(2,2) = 3; A(2,3) = 1
A(3,1) = 1; A(3,2) = 2; A(3,3) = 3
endsubroutine matrix_initializer

subroutine IdentityMatrix_initializer(A)
real, intent(inout) :: A(3,3)
A = 0.0
do i = 1,3
A(i,i) = 1.0
enddo
endsubroutine IdentityMatrix_initializer

subroutine matrix_printer(A)
real, intent(inout) :: A(3,3)
write(*,*) "Printing Matrix ..."
do i = 1,3
write(*,*) (A(i,j), j=1,3)
enddo
endsubroutine matrix_printer

subroutine GaussJorden_diagonalizer(A)
real, intent(inout) :: A(3,3)
integer :: n = 3
do j = 1,n
do i = 1,n
if (j /= i) A(i,:) = A(i,:) - ((A(i,j) / A(j,j)) * A(j,:))
enddo
enddo
endsubroutine GaussJorden_diagonalizer

subroutine GaussJorden_inverse(A,inv_A)
real, intent(inout) :: A(3,3), inv_A(3,3)
integer :: n = 3
real :: dummy_A(3,3)
call IdentityMatrix_initializer(inv_A)
do j = 1,n
do i = 1,n
dummy_A = A
if (j /= i) then
A(i,:) = A(i,:) - ((A(i,j) / A(j,j)) * A(j,:))
inv_A(i,:) = inv_A(i,:) - ((dummy_A(i,j) / dummy_A(j,j)) * inv_A(j,:))
endif
enddo
enddo
do i = 1,n
inv_A(i,:) = inv_A(i,:)/A(i,i)
enddo
endsubroutine GaussJorden_inverse
