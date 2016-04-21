C MEMBER GETSACCO
C  (from old member FCEX1)
C
      SUBROUTINE GETSACCO(OPNAME, SAC, SCO)
C.......................................
C
C     Obtains the sacramento parameters and carryover for the modelid 
C     and operation name.  SAC and SCO are returned arrays.
C
C     IMPORTANT: NUMOP IS SET TO 1 WITHIN THIS SUBROUTINE
C     THIS SUBROUTINE ONLY WORKS, CURRENTLY, FOR THE SACRAMENTO MODEL
C
C.......................................
C
C     WRITTEN BY Hank Herr - HRL    2/13/01
cew   modified by edwin welles to use the FSMCO1 common 3/30/01
C
C.......................................
C
C     Pick up the P and C arrays.
      INCLUDE 'common/fp'
      INCLUDE 'common/fc'

      common/sacco1/ opernames(10), UZTWC_1(10),UZFWC_1(10),LZTWC_1(10),
     1                LZFSC_1(10),LZFPC_1(10),ADIMC_1(10), fgco_1(10)
    
C
C     Passed in:
      DIMENSION OPNAME(2)
C      
C     Used within the routine:
      DIMENSION SAC(7)
      DIMENSION SCO(7)
      character*8 opernames, c_opername
      INTEGER   CINDEX, PINDEX, PPOS, NUMOP, MINPOS, CFOUND, VALUE
      INTEGER   PLPOS, XTRACO, PLFGPOS, TEMP1, ii
      real UZTWC_1,UZFWC_1,LZTWC_1,LZFSC_1,LZFPC_1,ADIMC_1,fgco_1
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Mods/RCS/getsacco.f,v $
     . $',                                                             '
     .$Id: getsacco.f,v 1.2 2001/06/14 18:37:37 dws Exp $
     . $' /
C    ===================================================================
C
C      
C     I only run this routine for the sacramento model...
      NUMOP = 1
C
c   search the C Array for the location in the P array.
c   Could do this differently, but not now.
c
C     Loop through the C array...      
      CINDEX = 1
      MINPOS = 1
      CFOUND = 0
      DO WHILE (CINDEX.LE.MC.AND.CINDEX.GE.MINPOS.AND.CFOUND.EQ.0)
C
C       If the number used for the operation in the C array is the same
C       as that pased in as NUMOP, then...
C
        VALUE = C(CINDEX)
        IF (VALUE .EQ. NUMOP) THEN
C
C         If operation name from the C array (position 2 and 3) is the
C         same as that pased in as OPNAME, then...
          IF(C(CINDEX+2).EQ.OPNAME(1).AND.C(CINDEX+3).EQ.OPNAME(2)) THEN
C
C           Get the position in the P array.
            PPOS = C(CINDEX+4)
C                  
C           Force the while loop to finish by setting CFOUND = 1
            CFOUND = 1
C
C         Otherwise, go to the next piece of the C array
          ELSE
            CINDEX = C(CINDEX+1)
          ENDIF
C             
C       Otherwise, go to the next piece of the C array
        ELSE
          CINDEX = C(CINDEX+1)
        ENDIF
C          
C     END OF THE DO LOOP
      END DO
C
C     NOTE: At this point, CINDEX is still pointing to the point in the
C       C array where the carryover for this operation is stored.
C
C     If CFOUND == 0, then I found nothing.  Set the arrays to missing.
      IF (CFOUND.EQ.0) THEN
        SAC(1) = -999
        SAC(2) = -999
        SAC(3) = -999
        SAC(4) = -999
        SAC(5) = -999
        SAC(6) = -999
        return
      ENDIF
C
C     PPOS already points to the second part of the P array.
      PINDEX = PPOS
C
C     Get the position in the PL array of the data
      PLPOS = P(PINDEX + 19)
C
C     Get the SAC-SMA parameters -- use PINDEX as the base, remembering
C     to subtract 1, to account for the fact that PINDEX points to the
C     1 position in the array, and not zero.
      SAC(1) = P(PINDEX - 1 + PLPOS + 2)
      SAC(2) = P(PINDEX - 1 + PLPOS + 3)
      SAC(3) = P(PINDEX - 1 + PLPOS + 10)
      SAC(4) = P(PINDEX - 1 + PLPOS + 11)
      SAC(5) = P(PINDEX - 1 + PLPOS + 12) 
C     
C     The maximum for ADIMC is the sum of UZTWM and LZTWM
      SAC(6) = SAC(1) + SAC(3) 
C
C     The maximum for the frozen ground number is fixed at 100
      SAC(7) = -100
C
cew  determine the carryover to use by matching the model names
      write(c_opername,163) (opname(jj),jj=1,2)
      do 161 ii=1,10
       if(opernames(ii) .eq. c_opername)then

         sco(1)=uztwc_1(ii)
         sco(2)=uzfwc_1(ii)
         sco(3)=lztwc_1(ii)
         sco(4)=lzfsc_1(ii)
         sco(5)=lzfpc_1(ii)
         sco(6)=adimc_1(ii)
         sco(7)=fgco_1(ii)
           goto 162
       endif
        
 161  continue
 162  continue
 163  format(2a4)         

C      
C     Return and end.
C     Debug print statements:
c      WRITE (*,20) NUMOP,OPNAME
20    FORMAT (' IN GETSACCO : NUMOP=',I3,' NAME=',2A4)
c     WRITE (*,30) SAC(1),SAC(2),SAC(3),SAC(4),SAC(5),SAC(6),SAC(7)
30    FORMAT(' IN GETSACCO : UZTWM=',F6.2,' UZFWM=',F6.2,' LZTWM=',F6.2,
     $       ' LZFSM=',F6.2,' LZFPM=',F6.2,' ADIMM=',F6.2,' FGCM=',F8.2)
c      WRITE (*,40) SCO(1),SCO(2),SCO(3),SCO(4),SCO(5),SCO(6),SCO(7)
40    FORMAT(' IN GETSACCO : UZTWC=',F6.2,' UZFWC=',F6.2,' LZTWC=',F6.2,
     $       ' LZFSC=',F6.2,' LZFPC=',F6.2,' ADIMC=',F6.2,' FGCO=',F8.2)
      RETURN
C      
      END    

          
