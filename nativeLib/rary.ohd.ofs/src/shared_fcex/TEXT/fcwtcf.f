C MEMBER FCWTCF
C  (from old member FCFCWTCF)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 08/10/95.15:28:44 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE FCWTCF (IR,IW1,IW2,X,IFWX)
C
C  ROUTINE TO WRITE A RECORD TO THE CARRYOVER FILE.
C
C  ARGUMENT LIST:
C     IR - RECORD NUMBER ON KFCRY TO BE WRITTEN TO
C    IW1 - FIRST WORD POSITION IN RECORD IR TO BE REPLACED
C    IW2 - FINAL WORD POSITION IN RECORD IR TO BE REPLACED
C      X - ARRAY HOLDING VALUES TO BE WRITTEN
C   IFWX - FORCED WRITE INDICATOR
C          1=FORCE WRITE RECORD TO FILE
C          0=NO FORCE WRITE
C
      INCLUDE 'common/where'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fccgd'
      INCLUDE 'common/errdat'
      INCLUDE 'common/fdbug'
C
      DIMENSION X(1),OLDNAM(2),Y(100)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fcwtcf.f,v $
     . $',                                                             '
     .$Id: fcwtcf.f,v 1.4 1999/01/19 21:46:16 page Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=0
      CALL UMEMOV (OPNAME,OLDNAM,2)
      CALL UMEMOV ('FCWTCF  ',OPNAME,2)
C
      IBUG=0
      IF (IFBUG('COTR').EQ.1) IBUG=1
      IF (IFBUG('COBG').EQ.1) IBUG=2
C
      IF (ITRACE.GT.0) WRITE (IODBUG,10)
10    FORMAT (' *** ENTER FCWTCF')
C
      IF (IBUG.GT.0) WRITE (IODBUG,*)
     *   ' IR=',IR1,
     *   ' IW1=',IW1,
     *   ' IW2=',IW2,
     *   ' '
C
      IFW=IFWX
      IF (IFW.NE.1.AND.IFW.NE.0) THEN
         WRITE (IPR,30) IFW
30       FORMAT ('0**WARNING** THE VALUE OF THE FORCE WRITE ',
     *      'INDICATOR (',I2,') IS INVALID AND WILL BE SET TO 0.')
         IFW=0
         ENDIF
C
C  CHECK VALUES OF WORD POSITIONS
      IF (IW1.GT.0.AND.IW2.LE.NWR) GO TO 50
         NWR1=NWR+1
         WRITE (IPR,40) IW1,IW2,NWR1
40    FORMAT ('0**ERROR** ONE OR BOTH OF THE REQUESTED WORD POSITIONS ',
     *   'ARE INVALID. ',
     *   'IW1 (',I3,') SHOULD BE GREATER THAN ZERO. ',
     *   'IW2 (',I3,') SHOULD BE LESS THAN ',I3,'.')
         CALL ERROR
         GO TO 70
C
C  READ CARRYOVER
50    CALL FCRDCF (IR,1,NWR,Y,IERR)
      IF (IERR.GT.0) THEN
         WRITE (IPR,55)
55    FORMAT ('0**ERROR** IN FCWTCF - READING CARRYOVER.')
         CALL ERROR
         GO TO 70
         ENDIF
C
C  MOVE VALUES TO ARRAY
      DO 60 I=IW1,IW2
         Y(I)=X(I-IW1+1)
60       CONTINUE
C
C  WRITE TO CARRYOVER FILE
       IF (IFW.EQ.0) THEN     
          CALL UWRITT (KFCRY,IR,Y,IERR)
          IF (IERR.GT.0) THEN
             WRITE (IPR,65) IR,KFCRY
65    FORMAT ('0**ERROR** IN FCWTCF - WRITING RECORD ',I6,
     *   ' TO UNIT ',I2,'.')
             CALL ERROR
             GO TO 70 
             ENDIF
          ENDIF
       IF (IFW.EQ.1) THEN
          CALL UWRITT (KFCRY,IR,Y,IERR)
          IF (IERR.GT.0) THEN
             WRITE (IPR,65) IR,KFCRY
             CALL ERROR
             GO TO 70 
             ENDIF
          ENDIF
C
70    IF (ITRACE.GT.0) WRITE (IODBUG,80)
80    FORMAT (' *** EXIT FCWTCF')
C
      CALL UMEMOV (OLDNAM,OPNAME,2)
C
      RETURN
C
      END
