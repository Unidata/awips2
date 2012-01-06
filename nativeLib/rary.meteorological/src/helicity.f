      Subroutine calchelicity(HW,PW,UW,VW,NW,elev,ztop,ghx,ghy,
     *           diravg,spdavg,stmdir,stmspdkts,helicity,SRHel)
      IMPLICIT NONE

C Statement of purpose.
C ---------------------
C This subroutine calculates the storm relative helicity from
C the surfce to 3000 meters above ground level.  Code lifted
C from the original FORTRAN code routine 'generate.for'.
C
C History.
C --------
C D. Perry       27 Feb 97    'Original' version.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------
C HW          Real Array    Wind level heights (m asl).
C PW          Real Array    Wind level pressures (mb).
C UW          Real Array    Wind level u-components (m/s).
C VW          Real Array    Wind level v-components (m/s).
C NW          Integer       Number of wind levels passed.
C ELEV        Real          Station elevation (m agl).
C ztop        Real          Top of desired helicity layer (m agl).
C diravg      Real          0-6km avg wind direction (deg).
C spdavg      Real          0-6km avg wind speed (m/s).
C
C On output:
C stmdir      Real          0-6km Storm Motion Dir (30 deg to right of diravg)
C stmspdkts   Real          0-6km Storm Motion Spd (75% of spdavg)
C SRHel       Real          Storm Relative Helicity (m2/s2).
C ----------

C---- Input arguments.

      Integer NW
      Real*4 elev,ztop,HW(NW),PW(NW),UW(NW),VW(NW),diravg,spdavg

C---- Output arguments.

      Real*4 SRHel,helicity,ghx,ghy,stmdir,stmspdkts,stmspdms

C---- Internal variables.
	
      Real*4 Utop,Vtop
      Real*4 Ustorm,Vstorm
      Real*4 wgt1,wgt2
      Integer topidx,i

C---- Subroutine constants.

      Real*4 FLAG,M2KTS
      PARAMETER (FLAG=99999.,M2KTS=1.94254)

C---- Initialize output mean wind value to FLAG.

      helicity=FLAG
      SRHel=FLAG

C---- Calculate helicity.
      Do 1004 topidx=2,NW-1
1004      If (HW(topidx)-elev.ge.ztop) Goto 1005
1005  If (HW(topidx)-elev.lt.ztop) Goto 9999
      topidx=topidx-1
      wgt2=(ztop+elev-HW(topidx))/(HW(topidx+1)-HW(topidx))
      wgt1=1.0-wgt2
      Utop=UW(topidx)*wgt1+UW(topidx+1)*wgt2
      Vtop=VW(topidx)*wgt1+VW(topidx+1)*wgt2
      helicity=0.0
      Do 1010 i=1,topidx-1
1010      helicity=helicity-UW(i)*VW(i+1)+UW(i+1)*VW(i)
      helicity=helicity-UW(topidx)*Vtop+Utop*VW(topidx)
      ghx=Vtop-VW(1)
      ghy=-Utop+UW(1)

C---- Calculate default storm wind using previously calculated
C     0-6 km density-weighted average wind.
      stmdir= diravg + 30.0
      if (stmdir.gt.360.) stmdir = stmdir-360.0
C     use storm spd (kts) for display output
C     use storm spd (m/s) for helicity calculation
      stmspdkts = (spdavg*M2KTS)*0.75
      stmspdms = spdavg*0.75
      call uvcomp(stmdir,stmspdms,Ustorm,Vstorm,1)
     
      SRHel=helicity+ghx*Ustorm+ghy*Vstorm
9999  return
      End
