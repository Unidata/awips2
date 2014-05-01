C MEMBER HSETAR
C  (from old member HCLSEGID)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/25/95.15:10:12 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HSETAR
C
C  THIS ROUTINE SETS THE VALUES IN THE ARGUMENT ARRAYS FROM THE OPTION
C  RECORD
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hseg1'
      INCLUDE 'hclcommon/hgargm'
      INCLUDE 'hclcommon/hargmn'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hsetar.f,v $
     . $',                                                             '
     .$Id: hsetar.f,v 1.1 1995/09/17 18:43:07 dws Exp $
     . $' /
C    ===================================================================
C
C
      CALL HINCNX
      IF (ISTAT.NE.0) GO TO 100
      NUMARS=IOPTRC(NXOPT)
      IF (NUMARS.EQ.0) GO TO 100
C
C LOOP ON NUMBER OF ARGUMENTS, IF NEED NEW RECORD, GET IT
C
      DO 90 I=1,NUMARS
C     MAKE SURE COMPLETE ARGUMENT IS HERE OR ELSE READ ANOTHER RECORD
         CALL HINCNX
         IF (ISTAT.NE.0) GO TO 100
         IAP=IOPTRC(NXOPT)
         CALL HINCNX
         IF (ISTAT.NE.0) GO TO 100
         LEN=IOPTRC(NXOPT)
         NMOV1=LEN
         NMOV2=0
         IF (LEN+NXOPT.LE.LRECOP) GO TO 10
            NMOV1=LRECOP-NXOPT
            NMOV2=LEN-NMOV1
10       IF (NMOV1.EQ.0) GO TO 30
         IF (IAP.LT.0) GO TO 20
C        MOVE LOCAL ARGUMENT (POSSIBLY ONLY FIRST PART)
            CALL UMEMOV (IOPTRC(NXOPT+1),IARG(IAP),NMOV1)
            GO TO 30
20       CALL UMEMOV (IOPTRC(NXOPT+1),IGARG(-IAP),NMOV1)
C     CHECK IF MORE IN THE NEXT RECORD
30       IF (NMOV2.EQ.0) GO TO 50
            NXOPT=LRECOP
            LEN=NMOV2-1
         CALL HINCNX
         IF (ISTAT.NE.0) GO TO 100
         IF (IAP.LT.0) GO TO 40
C        MOVE SECOND PART TO LOCAL
            CALL UMEMOV (IOPTRC(1),IARG(IAP+NMOV1),NMOV2)
            GO TO 50
C     GLOBAL ARG
40       CALL UMEMOV (IOPTRC(1),IGARG(-IAP+NMOV1),NMOV2)
C     UPDATE POINTERS
50       NXOPT=NXOPT+LEN
C     CHECK FOR DATE WITH STAR THAT NEED TO HAVE TODAY ADDED
         IF (NMOV1+NMOV2.NE.7) GO TO 70
            IF (IAP.LT.0) GO TO 60
               CALL HSETDY (IARG(IAP))
               GO TO 70
C     CHECK IF GLOBAL DATE
60       CALL HSETDY (IGARG(-IAP))
70       IF (IHCLDB.GT.1) WRITE (LPD,80) IAP,NXOPT,NMOV1,NMOV2
80    FORMAT (' IN HSETAR - IAP=',I4,' NXOPT=',I4,' NMOV1&2=',2I4)
90       CONTINUE
C
100   IF (IHCLDB.GT.1) WRITE (LPD,110) NUMARS
110   FORMAT (' IN HSETAR - FINISHED SETTING ',I4,' ARGS')
C
      RETURN
C
      END
