C MEMBER UNAMCP
C----------------------------------------------------------------------
C
       SUBROUTINE UNAMCP (STRNG1,STRNG2,ISTAT)
C
C  ROUTINE COMPARES TWO STRINGS.
C
C   ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       STRNG1     A8    I     1     FIRST STRING
C       STRNG2     A8    I     1     SECOND STRING
C       ISTAT      I     O     1     STATUS INDICATOR
C                                       0=SAME
C                                       1=NOT SAME
C
C
      CHARACTER*8 STRNG1,STRNG2
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/unamcp.f,v $
     . $',                                                             '
     .$Id: unamcp.f,v 1.1 1995/09/17 19:02:19 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      ISTAT=1
C
      IF (STRNG1.EQ.STRNG2) ISTAT=0
C
      IF (ICMTRC.GT.0) WRITE (ICMPRU,10) STRNG1,STRNG2,ISTAT
10    FORMAT (' *** EXIT UNAMCP : STRNG1=',A,
     *       ' STRNG2=',A,' ISTAT=',I2)
C
      RETURN
C
      END
