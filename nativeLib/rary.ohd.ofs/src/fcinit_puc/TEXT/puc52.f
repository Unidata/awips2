C MEMBER PUC52
C-----------------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE PUC52 (P,C)

C     THIS IS THE CARD PUNCH ROUTINE FOR SSARR SUMMING POINT

C     THIS ROUTINE ORIGINALLY WRITTEN BY 
C        RAY FUKUNAGA - NWRFC  JULY 1995      

C    POSITION      CONTENTS OF P ARRAY
C      1           VERSION NUMBER OF OPERATION
C      2-19        DESCRIPTION - TITLE
C     20           # OF INPUT TIME SERIES TO SUM

C     21-22        BEGIN INTERVAL OUTPUT TIME SERIES IDENTIFIER
C     23           BEGIN INTERVAL OUTPUT TIME SERIES DATA TYPE CODE
C     24           BEGIN INTERVAL OUTPUT TIME SERIES TIME INTEVAL
C
C     25-26        END INTERVAL OUTPUT TIME SERIES IDENTIFIER
C     27           END INTERVAL OUTPUT TIME SERIES DATA TYPE CODE
C     28           END INTERVAL OUTPUT TIME SERIES TIME INTERVAL

C     FOR EACH INPUT TIME SERIES TO BE SUMMED
C     29-30        INPUT TIME SERIES IDENTIFIER
C     31           INPUT TIME SERIES DATA TYPE CODE
C     32           INPUT TIME SERIES TIME INTERVAL
C     33           CARRYOVER FLAG
C                  = 'CARY', FROM CARRYOVER ARRAY
C                  = 'FLAT', SET EQUAL TO SECOND ELEMENT
C                  = '    ', SET EQUAL TO ZERO
C                  = 'VALU', READ IN FROM INPUT

C     THEREFORE THE NUMBER OF ELEMENTS REQUIRED IN THE P ARRAY IS
C        28 +
C         5 * NUMBER OF INPUT TIME SERIES TO BE SUMMED

C     POSITION     CONTENTS OF C ARRAY
C      1+      INITIAL BEGIN INCREMENT INFLOW FOR EACH INPUT TIME SERIES
C              IF INPUT TIME SERIES IS AN END INCREMENT TIME SERIES,
C                 VALUE SET TO ZERO
C              ELSE, IF INPUT TS IS A BEGIN INCREMENT TIME SERIES,
C                 VALUE IS SET DEPENDING ON THE CARRYOVER FLAG


      DIMENSION P(*),C(*)
      INTEGER         PINT1,PINT2,PINT3
      REAL            PREAL(19)
      CHARACTER*4     PCHAR(19)
      EQUIVALENCE    (PREAL(1),PCHAR(1))

C     COMMON BLOCKS

      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/PUDFLT/IPDFLT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc52.f,v $
     . $',                                                             '
     .$Id: puc52.f,v 1.3 2003/06/17 18:18:28 hsu Exp $
     . $' /
C    ===================================================================
C

C SAMPLE INPUT DECK
C
C TEST SUMMING OF 3 TIME SERIES (1 BEGIN, 1 END, 1 SINGLE)
C BRN9 SQIB BRN9 SQIE 6 6 3
C WEII2    SQIB BLNK 6 0
C WEII2    SQIE BLNK 6 0
C BRN8     SQIN VALU 6 3000   

      CALL FPRBUG ('PUC52   ',1,52,IBUG)

        DO 23 II=2,19
   23   PREAL(II) = P(II)
      WRITE(IPU,500) (PCHAR(II),II=2,19)
 500  FORMAT(18A4)

        PREAL(1) = P(21)
        PREAL(2) = P(22)
        PREAL(3) = P(23)
           PINT1 = P(24)
        PREAL(4) = P(25)
        PREAL(5) = P(26)
        PREAL(6) = P(27)
           PINT2 = P(28)
           PINT3 = P(20)
      WRITE(IPU,501) (PCHAR(II),II=1,6),PINT1,PINT2,PINT3
 501  FORMAT(2(2A,1X,A),3I6)    

      DO 100 I=1,NINT(P(20))
         IS = 5*I+ 24  
            PREAL(1) = P(IS)
            PREAL(2) = P(IS+1)
            PREAL(3) = P(IS+2)
               PINT1 = P(IS+3)
            PREAL(4) = P(IS+4)
      WRITE(IPU,502) (PCHAR(II),II=1,4),PINT1,C(I)
  502 FORMAT(2A,1X,A,1X,A,I6,F10.2)
C     WRITE(IPU,502) (P(J),J=IS,IS+2),P(IS+4),P(IS+3),C(I)
C 502 FORMAT(2A,1X,A,1X,A,1X,A,I6,F10.2)
  100 CONTINUE

      IF (ITRACE.GE.1) WRITE(IODBUG,90)
 90   FORMAT('PUC52:  EXITED:')

      RETURN
      END
