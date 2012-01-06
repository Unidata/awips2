C MODULE VLOC
C-----------------------------------------------------------------------
C
C  THIS ROUTINE COMPUTES SEVERAL PARAMETERS BASED ON LOCATION FOR THE 
C  COMPUTATION OF SOLAR RADIATION FROM SKY COVER OR PERCENT SUNSHINE
C
C  INPUTS:
C    - STATION LATITUDE IN DEGREES FROM THE PARM FILE
C
C  OUTPUTS:
C    - CONSTANTS USED IN ESTIMATING SOLAR RADIATION
C
      SUBROUTINE VLOC (JSLOT,PEPARM,JPRMMX,DDATA,LRY)
C
      INTEGER*2 DDATA(LRY)
      DIMENSION PEPARM(JPRMMX)
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mape/RCS/vloc.f,v $
     . $',                                                             '
     .$Id: vloc.f,v 1.2 1999/07/06 16:16:46 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPTRCE.GT.0) WRITE(IOPDBG,*) 'ENTER VLOC'
C
      IDEBUG=IPBUG('VLOC')
C
      IF (IDEBUG.GT.0) WRITE(IOPDBG,10) JSLOT,JPRMMX
10    FORMAT (' IN VLOC: JSLOT=',I4,' JPRMMX=',I5)
C
      IPNTR=(JSLOT-1)*58
C
C  FIND STATION LATITUDE
      LATPTR=IPNTR+10
C
C  GET POINTER TO STORE LOCATION PARAMETERS.
      LOCPTR=IPNTR+49
      M=LOCPTR
      N=LOCPTR+5
      RLAT=PEPARM(LATPTR)
      A=RLAT-25.
      B=RLAT-44.
C
C  DEFINE EXP1
      PEPARM(LOCPTR)=.7575-.0018*A
      LOCPTR=LOCPTR+1
C
C  DEFINE EXP2
      PEPARM(LOCPTR)=.7250+.00288*B
      LOCPTR=LOCPTR+1
C
C  DEFINE LAT1
      PEPARM(LOCPTR)=2.139+.0423*A
      LOCPTR=LOCPTR+1
C
C  DEFINE LAT2
      PEPARM(LOCPTR)=30.0-0.667*A
      LOCPTR=LOCPTR+1
C
C  DEFINE LAT3
      PEPARM(LOCPTR)=2.9-0.0629*B
      LOCPTR=LOCPTR+1
C
C  DEFINE LAT4
      PEPARM(LOCPTR)=18.0+0.833*B
C
      IF (IDEBUG.GT.0) WRITE (IOPDBG,20) A,B,M,N,(PEPARM(J),J=M,N)
20    FORMAT (' IN VLOC: '
     * ' A=',F8.3,' B=',F8.3,
     * ' M=',I4,' N=',I4 /
     * ' EXP1=',F8.3,' EXP2=',F8.3,
     * ' LAT1=',F8.3,' LAT2=',F8.2,' LAT3=',F8.3,' LAT4 ',F8.3)
C
      IF (IPTRCE.GT.0) WRITE(IOPDBG,*) 'EXIT VLOC'
C
      RETURN
C
      END
