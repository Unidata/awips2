 
      Subroutine MatSln(A,V,P,S,mn,n,status)
 
C*  This program solves a system of linear equations using a matrix
C*  technique, specifically Gaussian elimination with partial pivoting.
 
C*  Inputs:
C*   A(0:mn,0:mn)   Real*4   Coefficient matrix (column,row).
C*   V(0:mn)        Real*4   y-value vector.
C*   P(0:mn)        Int*4    Workspace for partial pivoting.
C*   mn             Int*4    Size of arrays (from zero).
C*   n              Int*4    Size of system (from zero).
 
C*  Outputs:
C*   S(0:mn)        Real*4   Solution vector.
C*   status         Int*4    If logically false, matrix was ill conditioned
C*                           and system was not solved.
 
      Implicit None
 
      Integer*4 mn,n,r,c,re,ce,status,rp,rmax,P(0:mn),dummy
      Logical*1 ifill
c      Integer*4 i,j
 
      Real*4 A(0:mn,0:mn),V(0:mn),S(0:mn),
     -       abmax,f,vx,xx,xxx,xxxx
 
      status=0
 
c  Seed pivoting array, zero solution vector.
      Do 3 r=0,n
      S(r)=0.0
3     P(r)=r
      rmax=0
      ifill=.false.

c      Do 527 i=0,mn
c      Do 527 j=0,mn
c527   If ((i.gt.n .or. j.gt.n) .and. A(i,j).ne.0.0) 
c    -        Print *,'Outlier has non-zero',i,j,A(i,j)
 
c  Begin gaussian elimination, loop for row to use for eliminating a column in
c  those below.
      Do 20 r=0,n-1
 
c  Perform partial pivioting.
      abmax=-1.
      Do 5 rp=r,n
      If (abs(A(r,P(rp))).gt.abmax) Then
          abmax=abs(A(r,P(rp)))
          rmax=rp
         End If
5     Continue
      If (abmax.eq.0.0) Return
      dummy=P(rmax)
      P(rmax)=P(r)
      P(r)=dummy
 
c      Print *,'after pivot',r+1
c      Do 222 i=0,n
c222   Print 234,(A(j,P(i)),j=0,n),V(P(i))
c234   Format (' ',<n+1>(G10.3,' '),'~~',G10.3)
c      Print *,' '

c  Eliminate rth element in remaining rows.
      abmax=-1.
      Do 10 re=r+1,n
      f=A(r,P(re))/A(r,P(r))
      V(P(re))=V(P(re))-f*V(P(r))
      A(r,P(re))=0.
      Do 10 ce=r+1,n
      If (ce.eq.r+1) Then
          xx=A(ce,P(re))
          xxx=-(f*A(ce,P(r)))
          xxxx=xxx+xx
          If (abs(xxxx).gt.abmax) Then
              abmax=abs(xxxx)
              xx=amax1(abs(xx),abs(xxx))/10000.
              ifill=.false.
              If (abmax.lt.xx) ifill=.true.
            End If
          A(ce,P(re))=xxxx
         Else
          A(ce,P(re))=A(ce,P(re))-f*A(ce,P(r))
        End If
10    Continue
 
c      Print *,'after elimination',r+1
c      Do 333 i=0,n
c333   Print 234,(A(j,P(i)),j=0,n),V(P(i))
c      Print *,' '

      If (ifill) Return

20    Continue
 
c  Backwards solution.
      Do 40 r=n,0,-1
      vx=V(P(r))
      Do 30 c=r+1,n
30    vx=vx-S(c)*A(c,P(r))
      If (A(r,P(r)).eq.0.0) Return
40    S(r)=vx/A(r,P(r))
 
      status=1
      Return
      End
