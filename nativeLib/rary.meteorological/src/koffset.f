
      Subroutine Koffset(Temp,mni,ni,nj,k0)

      Implicit None

      Integer*4 mni,ni,nj,i,j
      Real*4    Flag,Flg,Temp(mni,nj),k0
      Data      Flag,Flg/1e37,99998./

      Do 1 j=1,nj
      Do 1 i=1,ni
1     If (Temp(i,j).lt.Flg) Goto 2
      k0=Flag
      Return

2     If (Temp(i,j).lt.100.0) Then
          k0=273.15
      Else
          k0=0.0
      End If

      Return
      End
