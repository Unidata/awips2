      subroutine minv(ab,n,ldab,scratch,det,tol,m,mode)
      real ab(ldab,n+m)
      real scratch(2*n)
      call ludcmp(ab,n,ldab,scratch,det)
      do j=1,m
        call lubksb(ab,n,ldab,scratch,ab(1,n+j))
      enddo
      end
