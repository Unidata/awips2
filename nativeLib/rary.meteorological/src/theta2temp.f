
      Subroutine Theta2Temp(P,aflgp,Theta,aflgth,T,mnx,nx,ny)

      Implicit None

      Integer*4 aflgp,aflgth,mnx,nx,ny,i,j

      Real*4    P(mnx,ny),Theta(mnx,ny),T(mnx,ny),
     &          p0,R_cp,Flag,Flg

      Data      p0,R_cp,Flag,Flg/1000.0,0.286,1e37,99998.0/

      If (aflgp.ne.0) Then
          If (aflgth.ne.0) Then
              Do 10 j=1,ny
              Do 10 i=1,nx
              If (P(i,j).lt.Flg .and. Theta(i,j).lt.Flg) Then
                  T(i,j)=Theta(i,j)*(P(i,j)/p0)**R_cp
              Else
                  T(i,j)=Flag
              End If
10            Continue
          Else
              Do 20 j=1,ny
              Do 20 i=1,nx
              If (P(i,j).lt.Flg .and. Theta(1,1).lt.Flg) Then
                  T(i,j)=Theta(1,1)*(P(i,j)/p0)**R_cp
              Else
                  T(i,j)=Flag
              End If
20            Continue
          End If
      Else
          If (aflgth.ne.0) Then
              Do 30 j=1,ny
              Do 30 i=1,nx
              If (P(1,1).lt.Flg .and. Theta(i,j).lt.Flg) Then
                  T(i,j)=Theta(i,j)*(P(1,1)/p0)**R_cp
              Else
                  T(i,j)=Flag
              End If
30            Continue
          Else
              Do 40 j=1,ny
              Do 40 i=1,nx
              If (P(1,1).lt.Flg .and. Theta(1,1).lt.Flg) Then
                  T(i,j)=Theta(1,1)*(P(1,1)/p0)**R_cp
              Else
                  T(i,j)=Flag
              End If
40            Continue
          End If
      End If

      Return
      End
