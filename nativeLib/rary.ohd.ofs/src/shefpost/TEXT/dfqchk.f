C MODULE DFQCHK
C***********************************************************************
C   THIS SUBROUTINE FILTERS THE VALUE-QUALITY CODE AND CHANGES THE VALUE
C    TO MISSING IF THE QUALITY CODE IS CERTAIN VALUES.
C   THIS ROUTINE WAS ADDED IN DEC 1999 (CALLED FROM DFPOST).
C
C      NAME    TYPE   I/O     DIM      DESCRIPTION
C
C      ITYPE    I      I       1       HOLDS 4-CHAR DATA TYPE
C                                      (NOT CURRENTLY USED IN THIS VER)
C      DPR      D      O       1       DATA VALUE THAT MAY BE SET TO
C                                      MISSING
C      QCODE    I      I       1       HOLDS 4-CHAR QUALITY CODE (LAST
C                                      THREE CHARS ARE BLANK)
C***********************************************************************
      SUBROUTINE DFQCHK(ITYPE,DPR,QCODE)

      INTEGER            ITYPE, QCODE
      INTEGER            LETR, LETF
      DOUBLE PRECISION   DPR,MISSG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpost/RCS/dfqchk.f,v $
     . $',                                                             '
     .$Id: dfqchk.f,v 1.1 2000/03/14 14:28:55 page Exp $
     . $' /
C    ===================================================================
C

CC    Debug code is commented out with 'CC'
CC    double precision   dprsav

      DATA    LETR,LETF,MISSG / 4hR   , 4hF   , -999.0 /

CC      dprsav = dpr

        IF ( QCODE.EQ.LETR .OR. QCODE.EQ.LETF ) DPR = MISSG

CC      write(77,'(A4,2F14.3,''  q: '',A4)') ITYPE,dprsav,DPR,QCODE

      RETURN
      END
