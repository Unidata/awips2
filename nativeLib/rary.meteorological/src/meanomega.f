
      Subroutine MeanOmega(P1,U1,V1,P2,U2,V2,Dx,Dy,dt,work,
     &                     Omega,mnx,nx,ny)

C  calculates the mean adiabatic omega on a theta surface over a period
C  dt seconds.  P im mb, U and V in m/s, Dx and Dy in m.
C  Omega output in mb/s.

      Implicit None

      Integer*4 i,j,mnx,nx,ny

      Real*4    P1(mnx,ny),U1(mnx,ny),V1(mnx,ny),
     &          P2(mnx,ny),U2(mnx,ny),V2(mnx,ny),
     &          Dx(mnx,ny),Dy(mnx,ny),work(mnx,ny),Omega(mnx,ny),dt,
     &          Flg,Flag

      Data      Flg,Flag/99998.,1e37/

      Call G2Gkinematics(U1,V1,P1,Dx,Dy, mnx,ny, nx,ny,
     &                   5,work)
      Call G2Gkinematics(U2,V2,P2,Dx,Dy, mnx,ny, nx,ny,
     &                   5,Omega)
      Do 10 j=1,ny
      Do 10 i=1,nx
      If (Omega(i,j).gt.Flg) Goto 10
      If (work(i,j).gt.Flg .or. P1(i,j).gt.Flg .or. P2(i,j).gt.Flg) Then
          Omega(i,j)=Flag
      Else
          Omega(i,j)=(P2(i,j)-P1(i,j))/dt-(Omega(i,j)+work(i,j))/2
      End If
10    Continue

      Return
      End 
