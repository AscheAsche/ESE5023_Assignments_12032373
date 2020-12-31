subroutine Matrix_multip(m,n,c)
integer :: i,j,k
real(4), intent(in) :: m(4,3), n(3,3)
real(4), intent(out) :: c(4,3)
do i=1,4
        do j=1,4
                c(i,j)=0
                do k=1,4
                        c(i,j)=c(i,j)+m(i,k)*n(k,j)
                enddo
        enddo
enddo

end subroutine Matrix_multip
