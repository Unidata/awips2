C MODULE PUC32
C-----------------------------------------------------------------------
C
      SUBROUTINE PUC32 (PO)
C
C  THIS ROUTINE PUNCHES CARDS WHICH CAN BE READ IN THE PIN32 ROUTINE
C  TO DEFINE OR RE-DEFINE A SEGMENT OR SEGMENTS FOR THE FFG OPERATION.
C
C  PARAMETRIC DATA IS THAT WHICH IS CURRENTLY IN THE P ARRAY.  CARRYOVER
C  DATA IS NOT USED IN THIS OPERATION.
C
C***********************************************************************
C  ROUTINE INITIALLY WRITTEN BY - JANICE LEWIS - HRL - 9/1991
C  ADDED MINIMUM AND MAXIMUM THRESHOLD RUNOFF VALUES - TIM SWEENEY - HRL
C     1/1995
C
      CHARACTER*4 FFGTYP
      CHARACTER*8 RTNNAM
      CHARACTER*8 FFGAID,BASNID,OPNARR,OPTYRR,OPNASN,OPTYSN
      CHARACTER*20 FFGDSC

      DIMENSION PO(*)
      PARAMETER (LARRY=100)
      DIMENSION PARRY(LARRY)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc32.f,v $
     . $',                                                             '
     .$Id: puc32.f,v 1.3 2003/12/16 15:28:20 scv Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='PUC32'
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER ',RTNNAM
C
      IBUG=IFBUG('FFG')
C
      CALL UMEMOV (PO(2),FFGAID,2)
      CALL UMEMOV (PO(4),FFGDSC,5)
      CALL UMEMOV (PO(9),OPTYRR,2)
      CALL UMEMOV (PO(11),OPNARR,2)
      CALL UMEMOV (PO(13),OPTYSN,2)
      CALL UMEMOV (PO(15),OPNASN,2)
      CALL UMEMOV (PO(17),BASNID,2)
C
      IDUR = PO(19)
C
      LMMRO = PO(24)
      IF (LMMRO.GT.0) THEN
         ROMIN = PO(LMMRO)
         ROMAX = PO(LMMRO+1)
         ELSE
            ROMIN = 0.0
            ROMAX = 0.0
         ENDIF
C
      WRITE (IPU,100) FFGAID,FFGDSC,IDUR,ROMIN,ROMAX
  100 FORMAT (A,1X,A,5X,I1,2F5.2)
      WRITE (IPU,110) BASNID,OPTYRR,OPNARR,OPTYSN,OPNASN
  110 FORMAT (A,1X,4(A,2X))
C
C  CHECK IF FFG PARAMETER RECORD EXISTS
      FFGTYP='FFG'
      IREC=0
      CALL RPPREC (FFGAID,FFGTYP,IREC,LARRY,PARRY,NFILL,IRECNX,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL PSTRDC (ISTAT,FFGTYP,FFGAID,IREC,LARRY,NFILL)
         ENDIF
C
      RETURN
C
      END
