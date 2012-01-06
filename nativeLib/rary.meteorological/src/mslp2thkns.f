
      Subroutine Mslp2Thkns(Mslp,Hgt,Thkns,mni,ni,nj)

C  This routine estimates 1000 to 500 mb thickness from 500 height
C  and mean sea level pressure.

      Implicit None

      Integer*4 mni,ni,nj,i,j

      Real*4    Mslp(mni,nj),Hgt(mni,nj),Thkns(mni,nj),
     &          a,b,c,Flag,Flg

      Data      a,b,c,Flag,Flg/
     &          0.4599042,3.262312,0.1902672,1e37,99998./

      Do 10 j=1,nj
      Do 10 i=1,ni
      If (Mslp(i,j).gt.Flg .or. Hgt(i,j).gt.Flg) Then
          Thkns(i,j)=Flag
      Else
          Thkns(i,j)=Hgt(i,j)*a/(Mslp(i,j)**c-b)
      End If
10    Continue

      Return
      End
