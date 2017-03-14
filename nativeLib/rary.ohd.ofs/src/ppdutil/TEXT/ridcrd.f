C MEMBER RIDCRD
C  (from old member PDRIDCRD)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/18/94.07:06:24 BY $WC20SV
C
       SUBROUTINE RIDCRD (ISTAFL,IDRAY,NUMID,LASTCD,ISTAT)
C
C          ROUTINE:  RIDCRD
C
C             VERSION:  1.0.0
C
C                DATE:  5-18-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE PARSES A CARD LOOKING FOR STATION ID'S.  IT
C    SEARCHES FOR A CONTINUATION FIELD(&), AND CONTINUES TO READ CARDS
C    UNTIL A STOP CARD IS ENCOUNTERED.  IT KEEPS TRACK OF THE NUMBER
C    OF ID'S READ, AND SETS A FLAG WHEN THE BUFFER IS FULL AND THE
C    ROUTINE IS TO BE CALLED AGAIN.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C        ISTAFL    I     I     1      STATION FLAG
C                                     0=8-CHAR ID
C                                     1=NUMERIC ID
C        IDRAY     A8    O    NUMID   ARRAY CONTAINING ID'S
C        NUMID     I     O     1      # OF ID'S READ FROM CARDS
C        LASTCD    I     O     1      READ FLAG
C                                      0=LAST CARD READ
C                                      1=CALL ROUTINE AGAIN
C        ISTAT     I     O     1      STATUS INDICATOR
C                                      0=OK,NORMAL RETURN
C                                      1=NO MORE CARDS(MORE EXPECTED)
C                                      2=DAIOIT READ ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
      INCLUDE 'udatas'
      INCLUDE 'udebug'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IDRAY(2,1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil/RCS/ridcrd.f,v $
     . $',                                                             '
     .$Id: ridcrd.f,v 1.1 1995/09/17 19:09:49 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA ICONT/4H&   /
C
C***********************************************************************
C
C
      ISTAT=0
C
      IF (IPDTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER RIDCRD')
      LASTCD=0
      NUMID=0
C
C  BLANK ID ARRAY
C
      CALL UMEMST (IBLNK,IDRAY,40)
C
      J=1
C
C  READ CARD
      CALL RPCARD (IBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 80
C
C  PRINT CARD
      CALL WPCARD (IBUF)
C
C  FIND FIELD ON CARD
      CALL UFREE (1,72)
      IF (ISTAT.NE.0) GO TO 80
C
C  GET ID'S
      DO 40 I=1,NFIELD
         IF (ISTAFL.EQ.1) GO TO 20
            NUM=IFSTOP(I)-IFSTRT(I)+1
            IF (NUM.GT.8) NUM=8
            CALL UPACK1 (IBUF(IFSTRT(I)),IDRAY(1,J),NUM)
            GO TO 30
C  ID'S ARE NUMERIC
20       IF (IFTYPE(I).NE.1) GO TO 50
         CALL UNUMIC (IBUF,IFSTRT(I),IFSTOP(I),IDRAY(1,J))
30       IF (IDRAY(1,J).EQ.ICONT) GO TO 70
         NUMID=J
         J=J+1
         IF (J.EQ.40) GO TO 100
40       CONTINUE
      GO TO 100
C
50    WRITE (LPE,60) I
60    FORMAT (' **ERROR** FIELD ',I2,' IS NOT AN INTEGER. ',
     *   'CARD IGNORED.')
      GO TO 100
C
70    LASTCD=1
      GO TO 100
80    CONTINUE
C
      WRITE (LPE,90)
90    FORMAT (' **ERROR** IN RIDCRD. READ ERROR.')
      ISTAT=1
C
100   IF (IPDTR.GT.0) WRITE (IOGDB,110)
110   FORMAT (' *** EXIT RIDCRD')
C
      RETURN
C
      END
