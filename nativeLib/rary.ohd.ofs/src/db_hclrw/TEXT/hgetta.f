C MEMBER HGETTA
C  (from old member HCLSEGID)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/25/95.15:10:12 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HGETTA
C
C
C  THIS ROUTINE READS THE TEMPORARY FILE TO RESTORE TECHNIQUE
C  AND ARGUMENT VALUES TO THE DEFAULT SETTINGS AS THEY WERE AT THE
C  BEGINNING OF THE COMPUTE COMMAND.  THEN HIDCHK CAN MAKE ANY
C  CHANGES THAT WERE SPECIFIED BY THE OPTIONS.
C
C
      DIMENSION ISVRAY(2,80)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/htechn'
      INCLUDE 'hclcommon/hgtech'
      INCLUDE 'hclcommon/hargmn'
      INCLUDE 'hclcommon/hgargm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hgetta.f,v $
     . $',                                                             '
     .$Id: hgetta.f,v 1.1 1995/09/17 18:42:22 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C READ THE SAVED FILE
C
      REWIND KTMPSV
      READ (KTMPSV) ISVRAY
C
C SET TECHNIQUE VALUES
C
      NT=ISVRAY(1,1)
      NA=ISVRAY(2,1)
      IF (NT.EQ.0) GO TO 100
C
C LOOP FOR EACH TECH
C    K IS COUNTER OF TECHNIQUES, I IS INDEX IN ARRAY(RESET WHEN NEW
C      RECORD IS READ
C
      K=2
      I=2
10    IT=ISVRAY(1,I)
      IF (IT.GT.0) ITECH(IT)=ISVRAY(2,I)
      IF (IT.LT.0) IGTECH(-IT)=ISVRAY(2,I)
          IF (IHCLDB.GT.1) WRITE (LPD,20) (ISVRAY(J,I),J=1,2)
20    FORMAT (' IN HGETTA - TECH ',I5,' SET TO',I5)
      IF (K.EQ.NT) GO TO 30
      I=I+1
      K=K+1
      IF (I.LE.80) GO TO 10
C
C READ ANOTHER RECORD
C
      READ (KTMPSV) ISVRAY
      I=1
      GO TO 10
C
C NOW DO ARGUMENT VALUES
C NA IS LAST USED 2 WORD PAIR IN ISVRAY
C
30    IF (NA.EQ.0) GO TO 100
      I=I+1
      K=K+1
40    IF (I.LE.80) GO TO 50
      READ (KTMPSV) ISVRAY
      I=1
50    IA=ISVRAY(1,I)
      N=ISVRAY(2,I)
C
C NN IS NUMBER OF PAIRS FOR THIS SET OF ARGUMENTS
C ALL ARGS FOR 1 TECHNIQUE ARE DONE IN 1 FELL SWOOP
C
      NN=(N+1)/2
      NNI=NN
C
C SEE IF NEED A NEW RECORD
C
      N1=0
      IF (I.EQ.80) GO TO 70
      IF (I+NN.LE.80) GO TO 80
C
C NEED TO MOVE SOME AND THEN GET ANOTHER RECORD
C
      N1=(80-I)*2
      N=N-N1
      IF (IA.GT.0) CALL UMEMOV (ISVRAY(1,I+1),IARG(IA),N1)
      IF (IA.LT.0) CALL UMEMOV (ISVRAY(1,I+1),IGARG(-IA),N1)
C
      M=I+1
      MM=I+(N1+1)/2
      IF (IHCLDB.GT.1) WRITE (LPD,60) I,IA,N1,M,MM,
     *   ((ISVRAY(L,J),L=1,2),J=M,MM)
60    FORMAT (' IN HGETTA - MOVING END OF RECORD, I=',I5,', IA=',I5,
     * ', N1=',I5,', M=',I5,', MM=',I5,', SET TO'/
     * (9X,20(1X,I5)))
C
70    READ (KTMPSV) ISVRAY
      I=0
      NNI=(N+1)/2
C
C SET APPROPRIATE ARGUMENT ARRAY
C
80    M=I+1
      IF (IA.GT.0) CALL UMEMOV (ISVRAY(1,M),IARG(IA+N1),N)
      IF (IA.LT.0) CALL UMEMOV (ISVRAY(1,M),IGARG(-IA+N1),N)
C
      I=M+NNI
      K=K+NN+1
      MM=I-1
      IF (IHCLDB.GT.1) WRITE (LPD,90) K,IA,N,N1,M,MM,
     *   ((ISVRAY(L,J),L=1,2),J=M,MM)
90    FORMAT (' IN HGETTA - K=',I4,' IA&N=',2I5,', N1=',I5,', M=',I5,
     *  ', MM=',I5,', SET TO'/(9X,20(1X,I5)))
      IF (K.LT.NA) GO TO 40
C
100   RETURN
C
      END
