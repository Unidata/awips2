      SUBROUTINE DURDEC(IDURC,IDURAT,IHR,intlppp,intuppp,ISTAT)
C
C   THIS SUBROUTINE DECODES THE DURATION CODE FOR PP AND RRS DATA.
C   IF DURATION CODE = 5004, THEN 24 HOUR DURATION IS ASSUMED IF
C    OBS TIME IS WITHIN WINDOW.
C   IF DURATION CODE = 2001 AND OBS TIME IS WITHIN WINDOW AROUND 12Z,
C     THEN OBS TIME IS SET TO 12Z, ELSE OBS TIME IS UNCHANGED.
c   upper and lower bounds of window are defined by intlppp,intuppp
c     parameters.
C
C   CALLING SUBROUTINES:
c   ofsde:  processDischFut, processHeightFut, processRRSObs
c   siipp:  calpp
C
C   IDURC = shef DURATION CODE FIELD (input) 
C   IDURAT = DURATION IN WHOLE HOURS (output)
c   IHR = hour of observation (z time)
c   intlppp = number of hours before 12z for window
c   intuppp = number of hours after 12z for window
C
      integer*2 idurc
      CHARACTER CDUR*4
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/awips/whfs/dev/HP/precip_proc/source/pproc_util/src/RCS/durdec.f,v $
     . $',                                                             '
     .$Id: durdec.f,v 1.1 2001/10/11 19:11:11 pst Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
c
c   duration = 0 -- instantaneous
c
      if(idurc.eq.0) then
         idurat=0
         return
      end if
C
      WRITE(CDUR,'(I4)') IDURC
      READ(CDUR,'(I1,I1,I2)') ICTYPE,IDUM,INUM
C
c   duration in units of hours
c
      IF(ICTYPE.EQ.1) THEN
         IDURAT=INUM
c
c   duration in units of days
c
      ELSE IF(ICTYPE.EQ.2) THEN
         IDURAT=24*INUM
c
         if(ihr.ge.12) then
            IW=IHR-12
            int=intuppp
         else
            IW=12-IHR
            int=intlppp
         end if
c
	 IF(IW.LE.int) IHR=12
c
c   duration = partial daily total
c
      ELSE IF(ICTYPE.EQ.5) THEN
         if(ihr.ge.12) then
            IW=IHR-12
            int=intuppp
         else
            IW=12-IHR
            int=intlppp
         end if
c
         IF(IW.LE.int) THEN
            IDURAT=24
	    IHR=12
         ELSE
            IDURAT=IHR-12
            IF(IDURAT.LT.0) IDURAT=IHR+12
         END IF
c
c   duration not like any above
c   for example, 3001 -- monthly   0015 -- 15 minute
c
      ELSE
         ISTAT=1
      END IF
      RETURN
      END
