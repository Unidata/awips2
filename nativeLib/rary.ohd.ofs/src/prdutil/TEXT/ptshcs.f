C MEMBER PTSHCS
C  (from old member PRDPRINT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/12/95.10:54:24 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PTSHCS (IPAGE,IFORM,IKEY)
C
C  ROUTINE TO PRINT TABLE HEADERS
C
      INCLUDE 'uio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/ptshcs.f,v $
     . $',                                                             '
     .$Id: ptshcs.f,v 1.1 1995/09/17 19:16:55 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IKEY.EQ.1) GO TO 10
C
C  PRINTING HEADERS
      IF (IPAGE.EQ.1) CALL UPAGE (LP)
      NLINEL=7
      CALL ULINEL (LP,NLINEL,IRETRN)
      IF (IRETRN.EQ.1) CALL UPAGE (LP)
      IF (IFORM.EQ.0) THEN
         CALL ULINE (LP,5)
         WRITE (LP,30)
         GO TO 20
         ENDIF
      CALL ULINE (LP,5)
      WRITE (LP,40)
      GO TO 20
C
C  PRINTING DATA
10    IF (IPAGE.EQ.1) CALL UPAGE (LP)
      IF (IPAGE.EQ.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,50)
         ENDIF
C
20    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT ('0  TIME',29X,'VALUES   MAX     ACTUAL',4X,'DATE OF',7X,
     *     'DATE OF',41X,'NUMBER' /
     *   2X,'SERIES   DATA',15X,'TIME  /TIME',4X,'DATA',5X,'DATA',
     *      2(4X,'FIRST DATA'),39X,'OF HRS' /
     *   4X,'ID',5X,'TYPE  TYPE   UNIT',2X,'STEP',3X,'STEP',
     *      2(3X,'VALUES'),4X,'REGULAR ',7X,'FUTURE',6X,
     *      'LAT',4X,'LONG',7X,'DESCRIPTION',6X,'OF QPF' /
     *   1X,8('-'),2X,4('-'),2X,
     *      5('-'),2X,4('-'),2X,4('-'),2X,6('-'),2X,6('-'),3X,
     *      6('-'),2(2X,12('-')),2X,5('-'),2X,6('-'),2X,20('-'),1X,
     *      6('-'))
40    FORMAT ('0  TIME',33X,'VALUES  MAXIMUM  ACTUAL',4X,'DATE OF' /
     *   2X,'SERIES   DATA',19X,'TIME  /TIME',4X,'DATA',5X,'DATA',
     *      4X,'FIRST DATA',44X,'FUTURE' /
     *   4X,'ID',5X,'TYPE  CONTENTS  UNITS',2X,
     *      'STEP',3X,'STEP',2(3X,'VALUES'),5X,'VALUE',7X,'LAT',4X,
     *      'LONG',7X,'DESCRIPTION',9X,'TS ID  TYPE' /
     *   1X,8('-'),2X,4('-'),2X,
     *      8('-'),2X,5('-'),2X,4('-'),2X,6('-'),2X,7('-'),2X,6('-'),2X,
     *      12('-'),2X,5('-'),2X,6('-'),2X,20('-'),2X,8('-'),1X,4('-'))
50    FORMAT (' ')
C
      END
