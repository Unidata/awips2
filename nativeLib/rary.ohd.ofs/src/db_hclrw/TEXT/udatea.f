C MEMBER UDATEA
C  (from old member UDATEA)
C
C     DESC - THIS SUBROUTINE DECODES A DATE FIELD AND RETURNS
C     DESC - THE JULIAN DAY AND INTERNAL HOUR, AND JULIAN HOUR
C     DESC - FOR THAT DATE.
C     DESC - THIS MODULE ALSO CONTAINS SUBROUTINE UDATEC.
C.......................................................................
C
C       SUBROUTINE ORIGINALLY WRITTEN BY
C          GEORGE F. SMITH - HRL - APRIL 1982
C        MODIFIED TO ALLOW NHRADD TO RANGE FROM -24 TO 24
C          GEORGE F. SMITH - HRL - JUNE 1984
C      MODIFIED SO UDATEA CALLS SUBROUTINE UDATEC INSTEAD OF
C       HAVING UDATEC AS AN ENTRY POINT
C        GEORGE F. SMITH - HRL - JUNE 1988
C.......................................................................
C
C      UDATEA IS PASSED A STRING CONTAINING THE DATE THROUGH
C        THE ARGUMENT LIST.
C.......................................................................
C
C     ARGUMENT LIST
C
C     NCHAR  - THE NUMBER OF CHARACTERS IN THE DATE FIELD
C     DATE   - ARRAY CONTAINING THE DATE
C     IPACK  - INDICATOR SPECIFYING WHETHER THE DATE ARRAY IS
C              PACKED OR UNPACKED
C               =0, UNPACKED (1 CHARACTER PER WORD)
C               =1, PACKED (4 CHARACTERS PER WORD)
C     NHRADD - THE NUMBER OF HOURS TO ADD TO THE INTERNAL
C              HOUR AND JULIAN HOUR IF NO HOUR IS DECODED
C     NHSWCH - SWITCH SPECIFYING IF INTERNAL HOUR SHOULD BE IN
C              00-23 OR 01-24 HOUR RANGE
C               =0, 00 TO 23
C               =1, 01 TO 24
C     IPRINT - SWITCH SPECIFYING WHETHER OR NOT TO PRINT
C              A WARNING MESSAGE IF FIELD DECODED IS NOT
C              A VALID DATE
C               =0, DO NOT PRINT MESSAGE
C               NE 0, PRINT MESSAGE AND CALL WARN
C     JULDA  - JULIAN DAY RETURNED
C     INTHR  - INTERNAL HOUR RETURNED
C     JULHR  - JULIAN HOUR RETURNED
C     ISTAT  - STATUS FLAG
C               =0, OK
C               =1, INVALID DATE FIELD
C               =2, ERROR IN ARGUMENT INPUT
C                   ONE OF IPACK, ISF, IEF, NHRADD OR NHSWCH NOT
C                   IN VALID RANGE
C.......................................................................
C.......................................................................
C
      SUBROUTINE UDATEA(NCHAR,DATE,IPACK,NHRADD,NHSWCH,IPRINT,JULDA,
     1  INTHR,JULHR,ISTAT)
C.......................................................................
C.......................................................................
C
      COMMON/UFREEI/ NFIELD,IFTYPE(40),IFCNT(40),IFSTRT(40),IFSTOP(40),
     *               IBUF(80),KOUNT
      COMMON/IONUM/IN,IPR,IPU
      COMMON/MDFLTD/INHOUR
C
      DIMENSION DATE(1),IDARAY(7)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/udatea.f,v $
     . $',                                                             '
     .$Id: udatea.f,v 1.1 1995/09/17 18:43:21 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C
C     SET STATUS TO ZERO
C
      ISTAT=0
C.......................................................................
C
C     FOR SUBROUTINE UDATEA - FIRST COPY ARRAY DATE INTO IBUF IN
C     COMMON BLOCK /UFREEI/
C
C     FIRST CHECK IPACK SWITCH TO SEE IF DATE MUST BE UNPACKED
C
      IF(IPACK.EQ.0.OR.IPACK.EQ.1)GO TO 10
C
C     ERROR IN IPACK - PRINT WARNING AND ASSUME DATE IS UNPACKED
C
      WRITE(IPR,600)IPACK
  600 FORMAT(1H0,10X,'**WARNING** IN UDATEA - VALUE OR ARGUMENT IPACK ',
     1 'MUST BE 0 OR 1'/11X,'VALUE ENTERED IS ',I11,', 0 IS ASSUMED.')
      CALL WARN
C
      ISTAT=2
      JPACK=0
      GO TO 11
C
   10 JPACK=IPACK
C
   11 LNIBUF=80
C
      IF(NCHAR.LE.LNIBUF)GO TO 15
C
C     TOO MANY CHARACTERS IN DATE ARRAY
C
      GO TO 25
C
   15 IF(JPACK.EQ.1)GO TO 20
C
C     DATA IS UNPACKED SO JUST COPY INTO IBUF
C
      CALL UMEMOV(DATE,IBUF,NCHAR)
C
      GO TO 30
C
C     DATA IS PACKED SO UNPACK INTO IBUF
C     FIRST MUST COMPUTE NUMBER OF WORDS OF PACKED DATA IN DATE
C
   20 NWORDS=(NCHAR-1)/4 + 1
C
      CALL UNPAKS(DATE,IBUF,NWORDS,LNIBUF,IER)
C
      IF(IER.EQ.0)GO TO 30
C
C     NOT ENOUGH ROOM IN IBUF TO HOLD DATE
C
   25 WRITE(IPR,601)NCHAR
  601 FORMAT(1H0,10X,'**WARNING** DATE FIELD IS TOO LONG - ',
     1 'LENGTH IS ',I3,' CHARACTERS.')
      CALL WARN
      ISTAT=1
C
      JULDA=0
      INTHR=0
      JULHR=0
      GO TO 999
C
C     DATE IS NOW IN UNPACKED FORM IN IBUF
C     SET VALUES FOR START AND END OF DATE IN IBUF
C
   30 ISFT=1
      IEFT=NCHAR
C
      CALL UDATEC(ISFT,IEFT,NHRADD,NHSWCH,IPRINT,JULDA,INTHR,JULHR,
     1            ISTAT)
C
 999  RETURN
      END
