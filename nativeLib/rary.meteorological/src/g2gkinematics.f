
      Subroutine G2GKinematics(Udx,Vdy,
     -                         Q,SpaX,SpaY,mnx,mny,nx,ny,
     -                         choice,Scalar)

C  Udx(mnx,mny)     Real   U component or d/dx.
C  Vdy(mnx,mny)     Real   V component or d/dy.
C  Q(mnx,mny)       Real   Array of parameter to advect, etc. (see notes)
C  SpaX(mnx,mny)    Real   Grid spacing in the X direction.
C  SpaY(mnx,mny)    Real   Grid spacing in the Y direction.
C  mnx,mny          Int    Dimensions of input arrays.
C  nx,ny            Int    Dimensions of input grids.
C  choice           Int    Controls type of kinematics parameter to calculate.
C  Scalar(mnx,mny)  Real   Scalar field output (or input to get components).

C  choice=1, vorticity;
C  choice=2, divergence, ignore Q;
C  choice=3, vorticity advection;
C  choice=4, divergence of Q;
C  choice=5, advection of Q.
c  choice=6, laplacian of Q, ignore U and V.
c  choice=7, Scalar is input, ignore Q, work back to d/dx in U and d/dy in V.
c  choice=8, Scalar is height, Q is coriolis, work back to Ug and Vg.
c  choice=9, Total deformation into scalar.
c  choice=10, Deformation components, Scalar is x comp and Q is y comp.
c  choice=11, Scalar is height, Q is coriolis, geo def in Udx.
c  choice=12, Scalar is height, Q is coriolis, work back to geo def vectors.
c  choice=13, Scalar is height, Q is coriolis, input wind components
c             changed to Uag and Vag.
c  choice=14, Scalar is input, U and V is ridge/trough vector.
c  choice=15, Scalar is input, U and V is ridge vector.
c  choice=16, Scalar is input, U and V is trough vector.
c  choice=17, U and V is input, Scalar is vector continuity.

C Note for options one and three:
C  To work with relative vorticity, fill first level of array Q with zeroes.
C  For absolute vorticity, fill first level of Q with coriolis parameter
C  values.

      Implicit None

      Integer*4 mnx,mny,nx,ny,choice,im,ip,jm,jp,i,j,ii,jj

      Real*4    Udx(mnx,mny),Vdy(mnx,mny),
     -          Q(mnx,mny),SpaX(mnx,mny),SpaY(mnx,mny),
     -          Scalar(mnx,mny),
     -          g,qqq,www,dsh,dst,r1,r2,r3,r4,ttt,sss

      Real*4 Flag,Flg
c      Common/FlagFlg/Flag,Flg
      Data Flag,Flg/1e37,1e36/
      Data g/9.806/

c  Check validity of input parameters.
      If (choice.lt.1 .or. choice.gt.17) Then
          Write (*,*) 'Invalid choice ',choice,' in g2gkinematics.'
          Return
      EndIf
      If (nx.lt.3 .or. nx.gt.mnx .or. ny.lt.3 .or. ny.gt.mny) Then
          Write (*,*) 'Invalid dimensions ',choice,' in g2gkinematics.'
          Return
      EndIf

c  Flag out boundaries.
      If (choice.le.6 .or. choice.eq.9 .or.
     &    choice.eq.10 .or. choice.eq.17) Then
          Do 11 i=1,nx
11        Scalar(i,1)=Flag
           Do 12 j=2,ny-1
           Scalar(1,j)=Flag
12         Scalar(nx,j)=Flag
            Do 13 i=1,nx
13          Scalar(i,ny)=Flag
      Else
          Do 21 i=1,nx
21        Udx(i,1)=Flag
           Do 22 j=2,ny-1
           Udx(1,j)=Flag
22         Udx(nx,j)=Flag
            Do 23 i=1,nx
23          Udx(i,ny)=Flag
          Do 31 i=1,nx
31        Vdy(i,1)=Flag
           Do 32 j=2,ny-1
           Vdy(1,j)=Flag
32         Vdy(nx,j)=Flag
            Do 33 i=1,nx
33          Vdy(i,ny)=Flag
      End If

c  Branch to loop for choice of operation.
      Goto (1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,
     &      11000,12000,13000,14000,14000,14000,17000) choice

c  Vorticity.
1000  jm=1
      j=2
      Do 1091 jp=3,ny
      im=1
      i=2
      Do 1090 ip=3,nx
      If (Udx(i,jm).gt.Flg .or. Udx(i,jp).gt.Flg .or.
     -    Vdy(im,j).gt.Flg .or. Vdy(ip,j).gt.Flg) Then
          Scalar(i,j)=Flag
      Else
          Scalar(i,j)=( (Vdy(ip,j)-Vdy(im,j))/SpaX(i,j)+
     -                  (Udx(i,jm)-Udx(i,jp))/SpaY(i,j) )/2
     -                  + Q(i,j)
      End If
      im=i
      i=ip
1090  Continue
      jm=j
      j=jp
1091  Continue
      Return

c  Divergence.
2000  jm=1
      j=2
      Do 2091 jp=3,ny
      im=1
      i=2
      Do 2090 ip=3,nx
      If (Vdy(i,jm).gt.Flg .or. Vdy(i,jp).gt.Flg .or.
     -    Udx(im,j).gt.Flg .or. Udx(ip,j).gt.Flg) Then
          Scalar(i,j)=Flag
      Else
          Scalar(i,j)=((Udx(ip,j)-Udx(im,j))/SpaX(i,j)+
     -                 (Vdy(i,jp)-Vdy(i,jm))/SpaY(i,j))/2
      End If
      im=i
      i=ip
2090  Continue
      jm=j
      j=jp
2091  Continue
      Return

c  Vorticity advection.
3000  jm=1
      j=2
      Do 3091 jp=3,ny
      im=1
      i=2
      Do 3090 ip=3,nx
      If (Udx(im,jm).gt.Flg .or. Udx(im,jp).gt.Flg .or.
     -    Udx(ip,jm).gt.Flg .or. Udx(ip,jp).gt.Flg .or.
     -    Vdy(im,jm).gt.Flg .or. Vdy(im,jp).gt.Flg .or.
     -    Vdy(ip,jm).gt.Flg .or. Vdy(ip,jp).gt.Flg .or.
     -    Vdy(im,j).gt.Flg .or. Vdy(ip,j).gt.Flg .or.
     -    Udx(i,jm).gt.Flg .or. Udx(i,jp).gt.Flg .or.
     -    Udx(i,j).gt.Flg .or. Vdy(i,j).gt.Flg) Then
          Scalar(i,j)=Flag
      Else
          Scalar(i,j)=( Udx(i,j)*
     -            ( (Udx(ip,jp)+Udx(im,jm)-Udx(im,jp)-Udx(ip,jm))/4
     -              -Vdy(ip,j)-Vdy(im,j)+Vdy(i,j)+Vdy(i,j) )
     -                - Vdy(i,j)*
     -            ( (Vdy(ip,jp)+Vdy(im,jm)-Vdy(im,jp)-Vdy(ip,jm))/4
     -              -Udx(i,jp)-Udx(i,jm)+Udx(i,j)+Udx(i,j) )
     -                  ) /(SpaX(i,j)*SpaY(i,j))
     -             +    (Udx(i,j)*(Q(im,j)-Q(ip,j))/SpaX(i,j)+
     -                   Vdy(i,j)*(Q(i,jm)-Q(i,jp))/SpaY(i,j))/2
      End If
      im=i
      i=ip
3090  Continue
      jm=j
      j=jp
3091  Continue
      Return

c  Divergence of Q.
4000  jm=1
      j=2
      Do 4091 jp=3,ny
      im=1
      i=2
      Do 4090 ip=3,nx
      If (Vdy(i,jm).gt.Flg .or. Vdy(i,jp).gt.Flg .or.
     -    Udx(im,j).gt.Flg .or. Udx(ip,j).gt.Flg .or.
     -    Q(i,jm).gt.Flg .or. Q(i,jp).gt.Flg .or.
     -    Q(im,j).gt.Flg .or. Q(ip,j).gt.Flg) Then
          Scalar(i,j)=Flag
      Else
          Scalar(i,j)=((Q(ip,j)*Udx(ip,j)-Q(im,j)*Udx(im,j))
     -                    /SpaX(i,j)+
     -                 (Q(i,jp)*Vdy(i,jp)-Q(i,jm)*Vdy(i,jm))
     -                    /SpaY(i,j))/2
      End If
      im=i
      i=ip
4090  Continue
      jm=j
      j=jp
4091  Continue
      Return

c  Advection of Q.
5000  jm=1
      j=2
      Do 5091 jp=3,ny
      im=1
      i=2
      Do 5090 ip=3,nx
      If (Q(im,j).gt.Flg .or. Q(ip,j).gt.Flg .or.
     -    Q(i,jm).gt.Flg .or. Q(i,jp).gt.Flg .or.
     -    Udx(i,j).gt.Flg .or. Vdy(i,j).gt.Flg) Then
          Scalar(i,j)=Flag
      Else
          Scalar(i,j)=(Udx(i,j)*(Q(im,j)-Q(ip,j))/SpaX(i,j)+
     -                 Vdy(i,j)*(Q(i,jm)-Q(i,jp))/SpaY(i,j))/2
      End If

      im=i
      i=ip
5090  Continue
      jm=j
      j=jp
5091  Continue
      Return

c  Laplacian of Q.
6000  jm=1
      j=2
      Do 6091 jp=3,ny
      im=1
      i=2
      Do 6090 ip=3,nx
      If (Q(im,j).gt.Flg .or. Q(ip,j).gt.Flg .or. Q(i,j).gt.Flg .or.
     -    Q(i,jm).gt.Flg .or. Q(i,jp).gt.Flg) Then
          Scalar(i,j)=Flag
      Else
          qqq=Q(i,j)+Q(i,j)
          Scalar(i,j)=(Q(im,j)+Q(ip,j)-qqq)/(SpaX(i,j)*SpaX(i,j)) +
     -                (Q(i,jm)+Q(i,jp)-qqq)/(SpaY(i,j)*SpaY(i,j))
      End If
      im=i
      i=ip
6090  Continue
      jm=j
      j=jp
6091  Continue
      Return

c  d/dy and d/dx of scalar.
7000  jm=1
      j=2
      Do 7091 jp=3,ny
      im=1
      i=2
      Do 7090 ip=3,nx
      If (Scalar(im,j).gt.Flg .or. Scalar(ip,j).gt.Flg .or.
     -    Scalar(i,jm).gt.Flg .or. Scalar(i,jp).gt.Flg) Then
          Udx(i,j)=Flag
          Vdy(i,j)=Flag
      Else
          Udx(i,j)=(Scalar(ip,j)-Scalar(im,j))/(2*SpaX(i,j))
          Vdy(i,j)=(Scalar(i,jp)-Scalar(i,jm))/(2*SpaY(i,j))
      End If
      im=i
      i=ip
7090  Continue
      jm=j
      j=jp
7091  Continue
      Return

c  Ug and Vg where scalar contains height.
8000  jm=1
      j=2
      Do 8091 jp=3,ny
      im=1
      i=2
      Do 8090 ip=3,nx
      If (Scalar(im,j).gt.Flg .or. Scalar(ip,j).gt.Flg .or.
     -    Scalar(i,jm).gt.Flg .or. Scalar(i,jp).gt.Flg) Then
          Udx(i,j)=Flag
          Vdy(i,j)=Flag
      Else
          Udx(i,j)=g*(Scalar(i,jm)-Scalar(i,jp))/(2*SpaY(i,j)*Q(i,j))
          Vdy(i,j)=g*(Scalar(ip,j)-Scalar(im,j))/(2*SpaX(i,j)*Q(i,j))
      End If
      im=i
      i=ip
8090  Continue
      jm=j
      j=jp
8091  Continue
      Return

c  Deformation.
9000  jm=1
      j=2
      Do 9091 jp=3,ny
      im=1
      i=2
      Do 9090 ip=3,nx
      If (Vdy(i,jm).gt.Flg .or. Vdy(i,jp).gt.Flg .or.
     -    Udx(i,jm).gt.Flg .or. Udx(i,jp).gt.Flg .or.
     -    Vdy(im,j).gt.Flg .or. Vdy(ip,j).gt.Flg .or.
     -    Udx(im,j).gt.Flg .or. Udx(ip,j).gt.Flg) Then
          Scalar(i,j)=Flag
          Q(i,j)=Flag
      Else
          qqq=0.5/SpaX(i,j)
          www=0.5/SpaY(i,j)
          dst=(Udx(ip,j)-Udx(im,j))*qqq+(Vdy(i,jm)-Vdy(i,jp))*www
          dsh=(Udx(i,jp)-Udx(i,jm))*qqq+(Vdy(ip,j)-Vdy(im,j))*www
          Scalar(i,j)=sqrt(dst*dst+dsh*dsh)
      End If
      im=i
      i=ip
9090  Continue
      jm=j
      j=jp
9091  Continue
      Return

c  Deformation components.
10000 jm=1
      j=2
      Do 10091 jp=3,ny
      im=1
      i=2
      Do 10090 ip=3,nx
      If (Vdy(i,jm).gt.Flg .or. Vdy(i,jp).gt.Flg .or.
     -    Udx(i,jm).gt.Flg .or. Udx(i,jp).gt.Flg .or.
     -    Vdy(im,j).gt.Flg .or. Vdy(ip,j).gt.Flg .or.
     -    Udx(im,j).gt.Flg .or. Udx(ip,j).gt.Flg) Then
          Scalar(i,j)=Flag
          Q(i,j)=Flag
      Else
          qqq=0.5/SpaX(i,j)
          www=0.5/SpaY(i,j)
          dst=(Udx(ip,j)-Udx(im,j))*qqq+(Vdy(i,jm)-Vdy(i,jp))*www
          dsh=(Udx(i,jp)-Udx(i,jm))*qqq+(Vdy(ip,j)-Vdy(im,j))*www
          qqq=dst*dst+dsh*dsh
          www=sqrt(qqq)
          Scalar(i,j)=sqrt((qqq+www*dst)/2)
          If (Scalar(i,j).ne.0.0) Then
              Q(i,j)=www*dsh/(2*Scalar(i,j))
          Else
              Q(i,j)=www
          End If
      End If
      im=i
      i=ip
10090 Continue
      jm=j
      j=jp
10091 Continue
      Return

c  Geostrophic deformation.
11000 jm=1
      j=2
      Do 11091 jp=3,ny
      im=1
      i=2
      Do 11090 ip=3,nx
      If (Scalar(im,jm).gt.Flg .or. Scalar(im,j).gt.Flg .or.
     -    Scalar(im,jp).gt.Flg .or. Scalar(i,jm).gt.Flg .or.
     -    Scalar(i,j).gt.Flg .or. Scalar(i,jp).gt.Flg .or.
     -    Scalar(ip,jm).gt.Flg .or. Scalar(ip,j).gt.Flg .or.
     -    Scalar(ip,jp).gt.Flg) Then
          Udx(i,j)=Flag
          Vdy(i,j)=Flag
      Else
          dst=(Scalar(ip,jm)+Scalar(ip,jm)-
     -         Scalar(ip,jp)-Scalar(im,jm))/(2*SpaX(i,j)*SpaY(i,j))
          www=Scalar(i,j)+Scalar(i,j)
          dsh=( (Scalar(i,jp)+Scalar(i,jm)-www)/(SpaY(i,j)*SpaY(i,j))
     -        - (Scalar(ip,j)+Scalar(im,j)-www)/(SpaX(i,j)*SpaX(i,j)) )
          Udx(i,j)=g*sqrt(dst*dst+dsh*dsh)/Q(i,j)
      End If
      im=i
      i=ip
11090 Continue
      jm=j
      j=jp
11091 Continue
      Return

c  Geostrophic deformation components.
12000 jm=1
      j=2
      Do 12091 jp=3,ny
      im=1
      i=2
      Do 12090 ip=3,nx
      If (Scalar(im,jm).gt.Flg .or. Scalar(im,j).gt.Flg .or.
     -    Scalar(im,jp).gt.Flg .or. Scalar(i,jm).gt.Flg .or.
     -    Scalar(i,j).gt.Flg .or. Scalar(i,jp).gt.Flg .or.
     -    Scalar(ip,jm).gt.Flg .or. Scalar(ip,j).gt.Flg .or.
     -    Scalar(ip,jp).gt.Flg) Then
          Udx(i,j)=Flag
          Vdy(i,j)=Flag
      Else
          dst=g*(Scalar(ip,jm)+Scalar(ip,jm)-
     -           Scalar(ip,jp)-Scalar(im,jm))/
     -           (2*Q(i,j)*SpaX(i,j)*SpaY(i,j))
          www=Scalar(i,j)+Scalar(i,j)
          dsh=(g/Q(i,j))*
     -      ( (Scalar(i,jp)+Scalar(i,jm)-www)/(SpaY(i,j)*SpaY(i,j))
     -       - (Scalar(ip,j)+Scalar(im,j)-www)/(SpaX(i,j)*SpaX(i,j)) )
          qqq=dst*dst+dsh*dsh
          www=sqrt(qqq)
          Udx(i,j)=sqrt((qqq+www*dst)/2)
          If (Udx(i,j).ne.0.0) Then
              Vdy(i,j)=www*dsh/(2*Scalar(i,j))
          Else
              Vdy(i,j)=www
          End If
      End If
      im=i
      i=ip
12090 Continue
      jm=j
      j=jp
12091 Continue
      Return

c  Uag and Vag where scalar contains height.
13000 jm=1
      j=2
      Do 13091 jp=3,ny
      im=1
      i=2
      Do 13090 ip=3,nx
      If (Scalar(im,j).gt.Flg .or. Scalar(ip,j).gt.Flg .or.
     -    Scalar(i,jm).gt.Flg .or. Scalar(i,jp).gt.Flg) Then
          Udx(i,j)=Flag
          Vdy(i,j)=Flag
      Else
          If (Udx(i,j).lt.Flg) Udx(i,j)=Udx(i,j)-
     -        g*(Scalar(i,jm)-Scalar(i,jp))/(2*SpaY(i,j)*Q(i,j))
          If (Vdy(i,j).lt.Flg) Vdy(i,j)=Vdy(i,j)-
     -        g*(Scalar(ip,j)-Scalar(im,j))/(2*SpaX(i,j)*Q(i,j))
      End If
      im=i
      i=ip
13090 Continue
      jm=j
      j=jp
13091 Continue
      Return

c  Ridge/trough vector.
14000 Continue
      jm=1
      j=2
      Do 14091 jp=3,ny
      im=1
      i=2
      Do 14090 ip=3,nx
      If (Scalar(i,jm).gt.Flg .or. Scalar(i,jp).gt.Flg .or.
     -    Scalar(im,jm).gt.Flg .or. Scalar(ip,jp).gt.Flg .or.
     -    Scalar(im,jp).gt.Flg .or. Scalar(im,jp).gt.Flg .or.
     -    Scalar(im,j).gt.Flg .or. Scalar(ip,j).gt.Flg .or.
     -    Scalar(i,j).gt.Flg) Then
          Udx(i,j)=Flag
          Vdy(i,j)=Flag
      Else
          ttt=Scalar(im,jp)+Scalar(i,jp)+Scalar(ip,jp)+
     &        Scalar(im,j)-2*Scalar(i,j)+Scalar(ip,j)+
     &        Scalar(im,jm)+Scalar(i,jm)+Scalar(ip,jm)
          www=ttt+          Scalar(i,jp)+
     &        Scalar(im,j)-4*Scalar(i,j)+Scalar(ip,j)+
     &                      Scalar(i,jm)
          r1 = ttt-3*(Scalar(im,j)+Scalar(ip,j))
          r2 = www-3*(Scalar(im,jm)+Scalar(ip,jp))
          r3 = ttt-3*(Scalar(i,jm)+Scalar(i,jp))
          r4 = www-3*(Scalar(im,jp)+Scalar(ip,jm))
          dst = r1*r1-r3*r3
          dsh = (r2*r2-r4*r4)*0.707
          sss = 1
          If (abs(dst).gt.abs(dsh)) Then
              If (dst.gt.0 .and. r1.gt.0 .or.
     &            dst.lt.0 .and. r3.gt.0) sss = -1
          Else
              If (dsh.gt.0 .and. r2.gt.0 .or.
     &            dsh.lt.0 .and. r4.gt.0) sss = -1
          End If
          If (choice.eq.16) sss=-sss
          If (choice.gt.14 .and. sss.lt.0) Then
              Vdy(i,j)=0
              Udx(i,j)=0
              Goto 14089
          End If
          ttt=1/(SpaX(i,j)*SpaY(i,j))
          If (dst.lt.0) Then
              dst = -(sqrt(-dst)*ttt)
          Else
              dst =   sqrt( dst)*ttt
          End If
          If (dsh.lt.0) Then
              dsh = -(sqrt(-dsh)*ttt)
          Else
              dsh =   sqrt( dsh)*ttt
          End If
          qqq=dst*dst+dsh*dsh
          www=sqrt(qqq)
          ttt=sqrt((qqq+www*dst)/2)
          Udx(i,j)=sss*ttt
          If (ttt.ne.0.0) Then
              Vdy(i,j)=sss*www*dsh/(2*ttt)
          Else
              Vdy(i,j)=sss*www
          End If
      End If
14089 Continue
      im=i
      i=ip
14090 Continue
      jm=j
      j=jp
14091 Continue
      Return

c  Vector continuity.
17000 Continue
      jm=1
      j=2
      Do 17091 jp=3,ny
      im=1
      i=2
      Do 17090 ip=3,nx
      Scalar(i,j)=Flag
      If (Udx(i,j).gt.Flg .or. Vdy(i,j).gt.Flg) Goto 17089
      Do 17085 jj=jm,jp
      Do 17085 ii=im,ip
      If (ii.eq.i .and. jj.eq.j) Goto 17085
      If (Udx(ii,jj).gt.Flg .or. Vdy(ii,jj).gt.Flg) Goto 17085
      www = abs(Udx(i,j)*Udx(ii,jj)+Vdy(i,j)*Vdy(ii,jj))
      If (Scalar(i,j).gt.Flg .or. www.gt.Scalar(i,j)) Scalar(i,j)=www
17085 Continue
      If (Scalar(i,j).lt.Flg) Scalar(i,j)=sqrt(Scalar(i,j))
17089 Continue
      im=i
      i=ip
17090 Continue
      jm=j
      j=jp
17091 Continue

      End
