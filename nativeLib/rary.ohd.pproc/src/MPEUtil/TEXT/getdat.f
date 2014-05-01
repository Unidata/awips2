#include "pragma.inc"

      SUBROUTINE GETDAT(DTTM, len_dttm)
C
C   THIS SUBROUTINE GENERATES THE Z DATE/TIME IN THE FORM MMDDYYYYHHMM as a char
C
C   CALLING SUBROUTINE: main_siipp, main_ofsde
C   SUBROUTINES CALLED: datimg,FFI2A
C
c   len_dttm = length of dttm variable
c
      CHARACTER*2 CEM,CED,CEH,CEN
      CHARACTER*4 CEY4
      CHARACTER*12 DTTM
      DIMENSION ITIME(6)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/awips/whfs/dev/HP/precip_proc/source/pproc_util/src/RCS/getdat.f,v $
     . $',                                                             '
     .$Id: getdat.f,v 1.6 2001/11/05 18:44:12 pst Exp $
     . $' /
C    ===================================================================
C
C
      CALL datimg(ITIME)
      IEY4=ITIME(1) + 1900
      IEM=ITIME(3)
      IED=ITIME(4)
      IEH=ITIME(5)
      IMN=ITIME(6)
C
      CALL ffi2a(CED,1,2,1,IED)
      CALL ffi2a(CEY4,1,4,1,IEY4)
      CALL ffi2a(CEM,1,2,1,IEM)
      CALL ffi2a(CEH,1,2,1,IEH)
      CALL ffi2a(CEN,1,2,1,IMN)
C
      IF(CEM(1:1).EQ.' ') CEM(1:1)='0'
      IF(CED(1:1).EQ.' ') CED(1:1)='0'
      IF(CEH(1:1).EQ.' ') CEH(1:1)='0'
      IF(CEN(1:1).EQ.' ') CEN(1:1)='0'
      DTTM=CEM//CED//CEY4//CEH//CEN
      len_dttm = index(dttm,' ') - 1
      if(len_dttm.eq.-1) len_dttm=12
C
      RETURN
      END
