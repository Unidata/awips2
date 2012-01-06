 
      Subroutine DgeoComps(Z,F,
     -                     SpaX,SpaY,mnx,mny,nx,ny,
     -                     dugdx,dugdy,dvgdx,dvgdy)

C  Z(mnx,mny)       Real   Input height field.
C  F(mnx,mny)       Real   Array of coriolis parameter.
C  SpaX(mnx,mny)    Real   Grid spacing in the X direction.
C  SpaY(mnx,mny)    Real   Grid spacing in the Y direction.
C  mnx,mny          Int    Dimensions of input arrays.
C  nx,ny            Int    Dimensions of input grids.
C  dugdx(mnx,mny)   Real   d/dx of u component of geostrophic wind.
C  dugdy(mnx,mny)   Real   d/dy of u component of geostrophic wind.
C  dvgdx(mnx,mny)   Real   d/dx of v component of geostrophic wind.
C  dvgdy(mnx,mny)   Real   d/dy of v component of geostrophic wind.
 
 
      Implicit None
 
      Integer*4 mnx,mny,nx,ny,im,ip,jm,jp,i,j
 
      Real*4    dugdx(mnx,mny),dugdy(mnx,mny),
     -          dvgdx(mnx,mny),dvgdy(mnx,mny),
     -          F(mnx,mny),SpaX(mnx,mny),SpaY(mnx,mny),
     -          Z(mnx,mny),
     -          g,qqq,www

      Real*4 Flag,Flg
c      Common/FlagFlg/Flag,Flg
      Data Flag,Flg/1e37,99998.0/
      Data g/9.806/

c  Check validity of input parameters.
      If (nx.lt.3 .or. nx.gt.mnx .or. ny.lt.3 .or. ny.gt.mny) Return

c  Flag out boundaries.
      Do 21 i=1,nx
21    dugdx(i,1)=Flag
       Do 22 j=2,ny-1
       dugdx(1,j)=Flag
22     dugdx(nx,j)=Flag
        Do 23 i=1,nx
23      dugdx(i,ny)=Flag
      Do 31 i=1,nx
31    dugdy(i,1)=Flag
       Do 32 j=2,ny-1
       dugdy(1,j)=Flag
32     dugdy(nx,j)=Flag
        Do 33 i=1,nx
33      dugdy(i,ny)=Flag
      Do 41 i=1,nx
41    dvgdx(i,1)=Flag
       Do 42 j=2,ny-1
       dvgdx(1,j)=Flag
42     dvgdx(nx,j)=Flag
        Do 43 i=1,nx
43      dvgdx(i,ny)=Flag
      Do 51 i=1,nx
51    dvgdy(i,1)=Flag
       Do 52 j=2,ny-1
       dvgdy(1,j)=Flag
52     dvgdy(nx,j)=Flag
        Do 55 i=1,nx
55      dvgdy(i,ny)=Flag

c  Calculate components.
3000  jm=1
      j=2
      Do 3091 jp=3,ny
      im=1
      i=2
      Do 3090 ip=3,nx
      If (Z(im,jm).gt.Flg .or. Z(im,jp).gt.Flg .or.
     -    Z(ip,jm).gt.Flg .or. Z(ip,jp).gt.Flg .or.
     -    Z(i,jm).gt.Flg .or. Z(i,jp).gt.Flg .or.
     -    Z(im,j).gt.Flg .or. Z(ip,j).gt.Flg .or.
     -    Z(i,j).gt.Flg) Then
          dugdx(i,j)=Flag
          dugdy(i,j)=Flag
          dvgdx(i,j)=Flag
          dvgdy(i,j)=Flag
      Else
          qqq=g/F(i,j)
          www=Z(i,j)+Z(i,j)
          dvgdy(i,j)=qqq*(Z(ip,jp)-Z(ip,jm)-Z(im,jp)+Z(im,jm))/
     -               (4*SpaX(i,j)*SpaY(i,j))
          dugdx(i,j)=-dvgdy(i,j)
          dugdy(i,j)=qqq*(www-Z(i,jp)-Z(i,jm))/(SpaY(i,j)*SpaY(i,j))
          dvgdx(i,j)=qqq*(Z(ip,j)+Z(im,j)-www)/(SpaX(i,j)*SpaX(i,j))
      End If
      im=i
      i=ip
3090  Continue
      jm=j
      j=jp
3091  Continue

      Return
      End
