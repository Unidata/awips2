C MODULE HREPPR
C-----------------------------------------------------------------------
C
C  ROUTINE TO REDEFINE LOCAL AND GLOBAL HCL PROCEDURES.
C
C  ARGUMENT LIST:
C
C     NAME     TYPE   I/O   DIM   DESCRIPTION
C     ------   ----   ---   ---   ------------
C     ISTAT      I     O     1    STATUS:
C                                   0=OK
C                                   OTHER=NOT DEFINED
C
      SUBROUTINE HREPPR (ISTAT)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hreppr.f,v $
     . $',                                                             '
     .$Id: hreppr.f,v 1.2 2001/06/13 13:37:57 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET FLAG TO INDICATE A REDEFINITION
      IREPFL=1
C
C  IGL IS SET BY HDRPR FOR REDEFINES
      CALL HDRPR (IREPFL,IGL,ISTAT)
C
      RETURN
C
      END
