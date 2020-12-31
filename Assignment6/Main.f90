Program main
integer                            :: ii
real :: m(4,3), n(3,3), c(4,3)
!---------------------------
open(111, file='M.dat', status='old')
do ii = 1,4
  read(111, *) m(ii,1), m(ii,2), m(ii,3)
enddo
close(111)
!---------------------------
open(123, file='N.dat', status='old')
do ii = 1,3
  read(123, *) n(ii,1), n(ii,2), n(ii,3)
enddo
close(123)
!---------------------------
call Matrix_multip(m,n,c)
do ii = 1,4
  write(*,*) "Line ", ii, " : ", c(ii,:)
enddo
!-------------------------
open(100, file='MN.dat', status='replace')
do ii=1,4
  write(100, '(f8.1)') c(ii,1),c(ii,2),c(ii,3)
enddo 
close(100)
EndProgram main
