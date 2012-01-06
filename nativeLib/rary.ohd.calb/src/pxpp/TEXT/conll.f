      SUBROUTINE CONLL(N,XLAT,XLONG)
C     CONVERTS LATITUDE AND LONGITUDE FROM MINUTES AND
C        SECONDS TO DECIMAL FORMAT
c
c specify the maximum number of stations allowed
c
      parameter (nsx=500)
      DIMENSION XLAT(nsx),XLONG(nsx)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/pxpp/RCS/conll.f,v $
     . $',                                                             '
     .$Id: conll.f,v 1.3 2002/05/15 17:29:26 dws Exp $
     . $' /
C    ===================================================================
C
         ILAT=XLAT(N)+0.01
         RLAT=ILAT
         YMNSEC=XLAT(N)-ILAT
         YMIN=YMNSEC*100
         IMIN=YMIN+0.01
         RMIN=IMIN
         YSEC=YMIN-IMIN
         ISEC=YSEC*100+0.01
         RSEC=ISEC
         XLAT(N)=RLAT+(RMIN/60)+(RSEC/3600)
         ILONG=XLONG(N)+0.01
         RLONG=ILONG
         XMNSEC=XLONG(N)-ILONG
         XMIN=XMNSEC*100
         IMIN=XMIN+0.01
         RMIN=IMIN
         XSEC=XMIN-IMIN
         ISEC=XSEC*100+0.01
         RSEC=ISEC
         XLONG(N)=RLONG+(RMIN/60)+(RSEC/3600)
      RETURN
      END
