C***********************************************************************
C                                                                      *
C         MEMBER URHCTG
C                                                                      *
C***********************************************************************
       SUBROUTINE URHCTG(ISTAT)
C***********************************************************************
C                                                                      *
C          SUBROUTINE:  URHCTG                                         *
C                                                                      *
C             VERSION:  1.0.0                                          *
C                                                                      *
C                DATE:  7-7-82
C                                                                      *
C              AUTHOR:  JONATHAN D . GERSHAN
C                       DATA SCIENCES INC                              *
C                       8555 16TH ST, SILVER SPRING, MD 587-3700       *
C***********************************************************************
C                                                                      *
C          DESCRIPTION:                                                *
C                                                                      *
C    ROUTINE TO READ THE FIRST RECORD FROM THE HCL DEFINITION FILES    *
C    INTO COMMON HCNTRL                                                *
C                                                                      *
C***********************************************************************
C                                                                      *
C          ARGUMENT LIST:                                              *
C                                                                      *
C         NAME    TYPE  I/O   DIM   DESCRIPTION                        *
C                                                                      *
C                                                                      *
C       ISTAT       I    O     1      STATUS INDICATOR                 *
C                                      0 = NORMAL                      *
C                                      OTHER = DAIO ERROR              *
C                                                                      *
C***********************************************************************
C                                                                      *
C          COMMON:                                                     *
C                                                                      *
      INCLUDE 'udsi'
      INCLUDE 'uio'
      INCLUDE 'hclcommon/hcntrl'
      INCLUDE 'hclcommon/hcuuno'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urhctg.f,v $
     . $',                                                             '
     .$Id: urhctg.f,v 1.1 1995/09/17 19:17:42 dws Exp $
     . $' /
C    ===================================================================
C
C                                                                      *
C***********************************************************************
C                                                                      *
C          DIMENSION AND TYPE DECLARATIONS:                            *
C                                                                      *
C                                                                      *
C***********************************************************************
C                                                                      *
C          DATA:                                                       *
C                                                                      *
C                                                                      *
C***********************************************************************
C
C
      CALL UREADT(KGLFUI,1,HCNTL(1,2),ISTAT)
      IF(ISTAT.EQ.0) GO TO 999
C
C     DAIO ERROR
C
 900  CONTINUE
      WRITE(LPE,909)
 909  FORMAT(' **ERROR** IN URHCTG. DAIO ERROR')
C
C     DEBUG AND RETURN
C
 999  CONTINUE
      IF(NOBUG.EQ.3) WRITE(LPD,1000) ISTAT
 1000 FORMAT(' SUBROUTINE URHCTG EXECUTED - STATUS = ',I2)
      RETURN
      END
