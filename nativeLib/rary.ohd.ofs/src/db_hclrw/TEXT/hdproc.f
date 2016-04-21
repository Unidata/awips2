C MODULE HDPROC
C-----------------------------------------------------------------------
C
C  ROUTINE TO DEFINE LOCAL AND GLOBAL HCL PROCEDURES.
C
C  ARGUMENT LIST:
C
C     NAME     TYPE   I/O   DIM   DESCRIPTION
C     ------   ----   ---   ---   ------------
C     IGL        I     I     1    GLOBAL LOCAL FLAG
C     ISTAT      I     O     1    STATUS:
C                                   0=OK
C                                   OTHER=NOT DEFINED
C
      SUBROUTINE HDPROC (IGL,ISTAT)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdproc.f,v $
     . $',                                                             '
     .$Id: hdproc.f,v 1.2 2001/06/13 13:37:03 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET FLAG TO INDICATE A DEFINITION
      IREPFL=0
C
      CALL HDRPR (IREPFL,IGL,ISTAT)
C
      RETURN
C
      END
