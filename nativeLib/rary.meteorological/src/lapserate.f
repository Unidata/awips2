
      Subroutine LapseRate(Tlo,PZlo,Thi,PZhi,vc,mnx,nx,ny,Lapse)

      Implicit None

      Integer*4 height,theta,scalarz,pressure,scalarp
      Parameter (height=1,theta=2,scalarz=3,pressure=4,scalarp=5)

      Integer*4 vc,mnx,nx,ny,i,j

      Real*4    Tlo(mnx,ny),PZlo(mnx,ny),Thi(mnx,ny),PZhi(mnx,ny),
     &          Flag,Flg,Lapse(mnx,ny),c1,c2

      Data      Flag,Flg/1e37,99998.0/

      If (vc.eq.height) Then  ! height
          c2=-1
          Do 10 j=1,ny
          Do 10 i=1,nx
          If (Tlo(i,j).lt.Flg .and. PZlo(i,j).lt.Flg .and.
     &        Thi(i,j).lt.Flg .and. PZhi(i,j).lt.Flg .and.
     &        PZlo(i,j).lt.PZhi(i,j)) Then
              Lapse(i,j)=c2*(Thi(i,j)-Tlo(i,j))/(PZhi(i,j)-PZlo(i,j))
          Else
              Lapse(i,j)=Flag
          End If
10        Continue
      Else If (vc.eq.theta) Then  ! T=scalar theta, PZ=pressure
          c1=0.009766
          c2=0.034167*alog(Thi(1,1)/Tlo(1,1))
          Do 20 j=1,ny
          Do 20 i=1,nx
          If (PZlo(i,j).lt.Flg .and. PZhi(i,j).lt.Flg .and.
     &        PZlo(i,j).gt.PZhi(i,j)) Then
              Lapse(i,j)=c1+c2/alog(PZhi(i,j)/PZlo(i,j))
          Else
              Lapse(i,j)=Flag
          End If
20        Continue
      Else If (vc.eq.scalarz) Then ! scalar Z
          c2=-1
          Do 30 j=1,ny
          Do 30 i=1,nx
          If (Tlo(i,j).lt.Flg .and. Thi(i,j).lt.Flg) Then
              Lapse(i,j)=c2*(Thi(i,j)-Tlo(i,j))/(PZhi(1,1)-PZlo(1,1))
          Else
              Lapse(i,j)=Flag
          End If
30        Continue
      Else If (vc.eq.pressure) Then ! PZ=pressure
          c2=0.034167
          Do 40 j=1,ny
          Do 40 i=1,nx
          If (Tlo(i,j).lt.Flg .and. PZlo(i,j).lt.Flg .and.
     &        Thi(i,j).lt.Flg .and. PZhi(i,j).lt.Flg .and.
     &        PZlo(i,j).gt.PZhi(i,j)) Then
              Lapse(i,j)=c2*alog(Thi(i,j)/Tlo(i,j))/
     &                      alog(PZhi(i,j)/PZlo(i,j))
          Else
              Lapse(i,j)=Flag
          End If
40        Continue
      Else If (vc.eq.scalarp) Then ! PZ(1,1)=scalar pressure
          c2=0.034167
          Do 50 j=1,ny
          Do 50 i=1,nx
          If (Tlo(i,j).lt.Flg .and. Thi(i,j).lt.Flg) Then
              Lapse(i,j)=c2*alog(Thi(i,j)/Tlo(i,j))/
     &                      alog(PZhi(1,1)/PZlo(1,1))
          Else
              Lapse(i,j)=Flag
          End If
50        Continue
      End If

      Return
      End
