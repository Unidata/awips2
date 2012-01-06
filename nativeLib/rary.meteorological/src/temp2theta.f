
      Subroutine Temp2Theta(P,aflgp,T,aflgt,Theta,mnx,nx,ny)

      Implicit None

      Integer*4 aflgp,aflgt,mnx,nx,ny,i,j

      Real*4    P(mnx,ny),Theta(mnx,ny),T(mnx,ny),
     &          p0,R_cp,Flag,Flg,q

      Data      p0,R_cp,Flag,Flg/1000.0,0.286,1e37,99998.0/

      If (aflgp.ne.0) Then
          If (aflgt.ne.0) Then
              Do 10 j=1,ny
              Do 10 i=1,nx
              If (P(i,j).lt.Flg .and. T(i,j).lt.Flg) Then
                  Theta(i,j)=T(i,j)*(p0/P(i,j))**R_cp
              Else
                  Theta(i,j)=Flag
              End If
10            Continue
          Else
              q=T(1,1)
              Do 20 j=1,ny
              Do 20 i=1,nx
              If (P(i,j).lt.Flg) Then
                  Theta(i,j)=q*(p0/P(i,j))**R_cp
              Else
                  Theta(i,j)=Flag
              End If
20            Continue
          End If
      Else
          If (aflgt.ne.0) Then
              q=(p0/P(1,1))**R_cp
              Do 30 j=1,ny
              Do 30 i=1,nx
              If (P(1,1).lt.Flg .and. T(i,j).lt.Flg) Then
                  Theta(i,j)=T(i,j)*q
              Else
                  Theta(i,j)=Flag
              End If
30            Continue
          Else
              q=T(1,1)*(p0/P(1,1))**R_cp
              Do 40 j=1,ny
              Do 40 i=1,nx
40            Theta(i,j)=q
          End If
      End If

      Return
      End
