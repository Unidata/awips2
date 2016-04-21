C MODULE FFRDRL
C-----------------------------------------------------------------------
C
      SUBROUTINE FFRDRL (X,N,IBUG,IER,LCD7)
C.......................................................................
C
C     THIS ROUTINE READS REAL VALUES IN FREE FORMAT WITH THE
C     FOLLOWING RULES:
C
C        1. REAL VALUES MUST BE SEPARATED BY AT LEAST ONE BLANK
C        2. IF AN 'X' IS ENCOUNTERED DECODING IS HALTED ON THE
C           CURRENT CARD AND THE NEXT CARD IS READ AS A CONTINUATION
C        3. NO CHARACTERS EXCEPT DIGITS (0-9), MINUS SIGN (-),
C           DECIMAL POINT (.), AND 'X' ARE ALLOWED
C.......................................................................
C
C   ROUTINE ORIGINALLY WRITTEN BY
C           GEORGE F. SMITH - HRL   OCTOBER 1979   VERSION 1
C.......................................................................
C
C   VARIABLES IN ARGUMENT LIST
C
C     1. X    - A REAL ARRAY IN WHICH THE VALUES READ WILL BE STORED
C     2. N    - THE NUMBER OF VALUES TO BE READ
C     3. IBUG - INDICATOR IF DEBUG INFORMATION TO BE OUTPUT:
C                 0 = NO DEBUG INFORMATION WRITTEN
C                 1 = DEBUG INFORMATION WRITTEN
C     4. IER  - ERROR INDICATOR:
C                 0 = NO ERROR ENCOUNTERED
C                 1 = ERROR ENCOUNTERED
C.......................................................................
C
C   NOTE:  INFORMATION TO THE RIGHT OF AN 'X' IS NOT DECODED
C          AND MAY CONTAIN DESCRIPTIVE INFORMATION ABOUT THE
C          DATA BEING READ.
C          ONLY THE FIRST 72 COLUMNS OF ANY CARD ARE READ -
C          INFORMATION BEYOND COLUMN 72 WILL BE IGNORED.
C.......................................................................
C
      INCLUDE 'common/ionum'
C
      DIMENSION X(N)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_util/RCS/ffrdrl.f,v $
     . $',                                                             '
     .$Id: ffrdrl.f,v 1.2 2000/03/13 20:47:08 page Exp $
     . $' /
C    ===================================================================
C
C
      IER=0
C
      CALL RDVCRL (NX,X,IBUG,IER,LCD7)
      IF (IER.EQ.1) GO TO 20
C
      IF (NX.NE.N) THEN
         WRITE (IPR,10) N,NX
10    FORMAT ('0**ERROR** ',I5,' VALUES WERE EXPECTED. ',
     *   I5,' WERE READ.')
         CALL ERROR
         IER=1
	 ENDIF
C
20    RETURN
C
      END
