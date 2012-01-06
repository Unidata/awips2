C MODULE XFMAPC
C-----------------------------------------------------------------------
C
      SUBROUTINE XFMAPC (COLST,MAXCO,MAXCON,IERR)
C
C  *********************************************************************
C  XFMAPC READS THE COMPUTATIONAL ORDER LIST FROM THE PREPROCESSOR
C  PARAMETRIC DATA BASE INTO ARRAY COLST AND RETURNS THE NUMBER OF
C  IDENTIFIERS IN THE LIST
C  *********************************************************************
C
C  IMPORTANT VARIABLES INCLUDE:
C
C      COLST = LIST OF FUTURE MAP IDENTIFIERS IN COMPUTATIONAL ORDER
C      MAXCON = THE NUMBER OF IDENTIFIERS IN COLIST
C
      CHARACTER*8 PARMTP
      CHARACTER*8 PARMID,OLDOPN
      DIMENSION COLST(1)
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fmap/RCS/xfmapc.f,v $
     . $',                                                             '
     .$Id: xfmapc.f,v 1.3 2000/03/14 12:15:36 page Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=-1
      CALL FSTWHR ('XFMAPC  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF(IPTRCE.GT.0) WRITE(IOPDBG,*) 'ENTER XFMAPC'
      IBUG=IPBUG('XFC ')
C
C  READ THE COMPUTATIONAL ORDER INFORMATION
      PARMID=' '
      PARMTP='FMPO'
      IPTR=0
      CALL RPPREC(PARMID,PARMTP,IPTR,MAXCO,COLST,NFILL,IPTRNX,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL PSTRDC (ISTAT,PARMTP,PARMID,IPTR,MAXCO,NFILL)
         IERR=1
         GO TO 99
         ENDIF
C
      IF (IBUG.EQ.1) CALL PDUMPA (NFILL,COLST,PARMTP,PARMID,1)
C
      MAXCON=COLST(4)
      IF (IBUG.GT.0) WRITE (IOPDBG,*) 'MAXCON=',MAXCON
C
      K=5
      IEND=2*MAXCON
      DO 30 I=1,IEND
         COLST(I)=COLST(K)
         K=K+1
   30    CONTINUE
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
      IF (IPTRCE.GT.0) WRITE (IOPDBG,*) 'EXIT XFMAPC'
C
 99   RETURN
      END
