      Real Function Rang2D(A,mnx,nx,ny,lo,hi)
 
C*  This function reports the range of values in a 2D data grid, missing data
C*  flags not being included.
 
C*  Inputs:
C*   A(mnx,*)           Real*4       Input array.
C*   mnx                Int*4        First dimension of input array.
C*   nx,ny              Int*4        Size of input data grid.
 
C*  Outputs:
C*   lo                 Real*4       Arithmetically smallest non-flagged value.
C*   hi                 Real*4       Arithmetically largest non-flagged value.
C*   Rang2D             Real*4       Range of values (hi minus lo).
 
      Implicit None
 
      Integer*4 mnx,nx,ny,i,j
 
      Real*4 A(mnx,*),Flg,Flag,lo,hi
 
      Data Flg,Flag/1e36,1e37/
 
      hi=-Flag
      lo=Flag
 
      Do 10 j=1,ny
      Do 10 i=1,nx
      If (A(i,j).gt.Flg) Goto 10
      hi=amax1(hi,A(i,j))
      lo=amin1(lo,A(i,j))
10    Continue
      Rang2d=hi-lo
 
      Return
      End
