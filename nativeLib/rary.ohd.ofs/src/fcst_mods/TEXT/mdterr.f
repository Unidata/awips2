C MODULE MDTERR
C
      SUBROUTINE MDTERR(I)
C
C  THIS ROUTINE WRITES A MESSAGE IF AN INVALID DATE FIELD IS FOUND.
C
      INCLUDE 'ufreex'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fpwarn'
C
      DIMENSION OLDOPN(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mods/RCS/mdterr.f,v $
     . $',                                                             '
     .$Id: mdterr.f,v 1.2 1998/07/02 20:45:11 page Exp $
     . $' /
C    ===================================================================
C
C
      CALL FSTWHR('MDTERR  ',0,OLDOPN,IOLDOP)
C
      IF (MODWRN.EQ.1) THEN
         WRITE(IPR,600) I,ICDBUF(1:72)
  600 FORMAT('0**WARNING** AN INVALID DATE WAS FOUND IN FIELD ',I2,
     1 ' OF THE FOLLOWING MOD CARD:' / 
     2  13X,A)
         CALL WARN
         ENDIF
C         
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C      
      RETURN
C      
      END
