        SUBROUTINE DM_GETGNAV ( iflno, fhdnam, mxword, rheadr, nword,
     +                        iret )
C************************************************************************
C* DM_GETGNAV                                                           *
C*                                                                      *
C* This subroutine reads a real file header from a DM file.  The        *
C* length of the file header must be less than MXWORD.                  *
C*                                                                      *
C* DM_GETGNAV  ( IFLNO, FHDNAM, MXWORD, RHEADR, NWORD, IRET )           *
C*                                                                      *
C* Input parameters:                                                    *
C*      IFLNO           INTEGER         File number                     *
C*      FHDNAM          CHAR*4          File header name                *
C*      MXWORD          INTEGER         Maximum words to return         *
C*                                                                      *
C* Output parameters:                                                   *
C*      RHEADR (NWORD)  REAL            File header                     *
C*      NWORD           INTEGER         Header length                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                       -4 = file not open             *
C*                                       -6 = write error               *
C*                                       -7 = read error                *
C*                                       -8 = file header undefined     *
C*                                      -18 = file header too long      *
C*                                      -21 = incorrect data type       *
C*                                      -29 = invalid file hdr name     *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov	02/10	Created					*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'dmcmn.cmn'
        INCLUDE         'dbcmn.cmn'
C
        CHARACTER*(*)   fhdnam
        REAL            rheadr (*)

        CHARACTER  navstr*256, gridnav(10)*60, proj*3,
     +  amodel*30,anevent*30,nvtime*20
C------------------------------------------------------------------------
        nword = 0
        sign = 1.
        dangl1 = 90.

        IF ( dbread ) THEN
           IF ( INDEX(dbdatasrc,'grid') .gt. 0 .and. 
     +          INDEX(fhdnam, 'NAVB') .gt. 0 ) THEN
              CALL ST_NULL ( dbmodel, amodel,   lstr, ier )
c                 CALL ST_NULL ( evtname, evtname,   lstr, ier )
              CALL DB_GETEVTNAME ( anevent, ier )
              CALL ST_NULL ( anevent, anevent,   lstr, ier )
              CALL DB_GETNAVTIME ( nvtime,ier )
               CALL ST_NULL ( nvtime,nvtime, lstr, ier )
              CALL DB_GETGNAV (amodel,anevent,nvtime, navstr,
     +                         lnavstr,iret)
c              print *, "after DB_GETGNAV ", navstr, lnavstr
              IF ( iret .ne. 0 .or. lnavstr .eq. 0 ) THEN
                 iret = -7
                 RETURN
              END IF


           ELSE IF ( INDEX(dbdatasrc,'grid') .gt. 0 .and. 
     +          INDEX(fhdnam, 'ANLB') .gt. 0 ) THEN
C
C*      Set analysis block.
C
C
c        CALL GR_MBAN  ( deltan, deltax, deltay, grltln, eltln, dltln,
c     +                  anlblk, ier )

c              CALL GR_MBAN  ( 4., 0., 0., grltln, eltln, 
c     +                        dltln, anlblk, ier )

              nword = 128
              DO ii = 1, nword
                 rheadr (ii) = 0.0
              END DO
              RETURN
           END IF
        END IF
        nexp = 10
        CALL ST_CLSL (navstr(:lnavstr), ';', ' ', nexp,
     +                gridnav, nprm, ier)
C
C*      Map projection type.
C*
        proj = gridnav(1)
        CALL ST_NUMB(gridnav(2), kx, ier)
        CALL ST_NUMB(gridnav(3), ky, ier)
        dimx = kx
        dimy = ky
C       
C*      Get items specific to each projection.
C
        IF ( proj .eq. 'LCC' .or. proj .eq. 'STR' ) THEN
C
C*          Get lat/lon of lower left point.
C
            CALL ST_NUMB(gridnav(4), lat1, ier)
            rlat1 = ( lat1 / 10000. ) * DTR
C
            CALL ST_NUMB(gridnav(5), lon1, ier)
            rlon1 = ( lon1 / 10000. ) * DTR
C
C*          std - lat at which cone or cylinder intersects earth.
C
            CALL ST_NUMB(gridnav(6), latin, ier)
            std = latin / 10000.
C
C*          Lov - orientation of the grid (center longitude).
C
            CALL ST_NUMB(gridnav(8), lov, ier)
            clon = lov / 10000.
            CALL PRNLON ( 1, clon, ier )
            cntlon = clon * DTR
C
C*          tdx and tdy direction grid increments.
C
            CALL ST_CRNM(gridnav(9), tdx, ier)
            CALL ST_CRNM(gridnav(10), tdy, ier)
            tdx = tdx / 10000.
            tdy = tdy / 10000.
            tdx = tdx * 1000.
            tdy = tdy * 1000.
C
C*          Projection center flag (NP=0, SP=1).
C*     //TODO - find out about the ipole!!!
C
            ipole = 0
            IF ( ipole .eq. 1 ) THEN
                sign   = - 1.
                dangl1 = - 90.
            END IF
        ELSE IF ( proj .eq. 'CED' ) THEN
            dlat1 = lat1 / 10000.
            dlon1 = lon1 / 10000.
C
C*          Get lat/lon of lower left point.
C
            CALL ST_NUMB(gridnav(6), lat1, ier)
            dlat1 = lat1 / 10000. 
C
            CALL ST_NUMB(gridnav(5), lon1, ier)
            dlon1 = lon1 / 10000.
C*
C*          Get lat/lon of upper right point.
C
            CALL ST_NUMB(gridnav(4), lat2, ier)
            dlat2 = lat2 / 10000.
C
            CALL ST_NUMB(gridnav(7), lon2, ier)
            dlon2 = lon2 / 10000.
            dangl1 = 0.
            dangl2 = 0.
            dangl3 = 0.
        ELSE IF ( proj .eq. 'MER' ) THEN   
C
C*          Get lat/lon of lower left point.
C
            CALL ST_NUMB(gridnav(4), lat1, ier)
            rlat1 = ( lat1 / 10000. ) * DTR
C
            CALL ST_NUMB(gridnav(5), lon1, ier)
            rlon1 = ( lon1 / 10000. ) * DTR
C
C*          std - lat at which cone or cylinder intersects earth.
C
            CALL ST_NUMB(gridnav(6), latin, ier)
            std = latin / 10000.
C
C
            CALL ST_NUMB(gridnav(7), lat2, ier)
            rlat2 = ( lat2 / 10000. ) * DTR
            CALL ST_NUMB(gridnav(8), lon2, ier)
            rlon2 = ( lon2 / 10000. ) * DTR
        END IF
C
C*      ***************************
C*      *** Polar Stereographic ***
C*      ***************************
C
        IF ( proj .eq. 'STR' ) THEN
C
C*          Linear coordinates of lower left point. (1+sin 90) is
C*          excluded.
C
            x1 =   RADIUS * TAN ( PI4TH - sign * rlat1 / 2. ) *
     +             SIN ( rlon1 - cntlon )
            y1 = (-RADIUS) * TAN ( PI4TH - sign * rlat1 / 2. ) *
     +             COS ( rlon1 - cntlon ) * sign
C
C*          Compute grid spacing on the pole plane. (1+sin 90) is
C*          also excluded.
C
            dx = tdx / ( 1. + SIN ( PI / 3. ) )
            dy = tdy / ( 1. + SIN ( PI / 3. ) )
C
C*          Compute the linear coordinates of the upper right point.
C*          Assumes left to right, top to bottom image scanning.
C
            x2 = x1 + ( kx - 1 ) * dx
            y2 = y1 + ( ky - 1 ) * dy * sign
C
C*          Set lat/lon of upper right point and projection angles.
C
            dlat2  = sign * ( HALFPI - 2. * ATAN2 ( SQRT ( ( x2 * x2 )
     +               + ( y2 * y2 ) ), RADIUS ) ) * RTD
            y2     = (-sign) * y2
            dlon2  = ( cntlon + ATAN2 ( x2, y2 ) ) * RTD
            CALL PRNLON ( 1, dlon2, ier )
C
C*          Set lat/lon of lower left point.
C
            dlat1 = lat1 / 10000.
            dlon1 = lon1 / 10000.
C
C*          Set projection angles. Angle1 is set to be 90 or -90.
C
            dangl2 = clon
            dangl3 = 0.
C
        ELSE IF ( proj .eq. 'LCC' ) THEN
C
C*          *************************
C*          *** Lambert Conformal ***
C*          *************************
C
C*          Compute the constant of the tangent cone.
C
            psi    = HALFPI - ( ABS ( std ) * DTR )
            ccone  = COS ( psi )
            rearth = RADIUS / ccone
C
C*          Compute the linear coordinate for the lower left grid point.
C*          The auxiliary function is excluded.
C
            x1 =   rearth * ( TAN ( ( HALFPI - sign * rlat1 ) / 2. ) **
     +             ccone ) * SIN ( ccone * ( rlon1 - cntlon ) )
            y1 = (-rearth) * ( TAN ( ( HALFPI - sign * rlat1 ) / 2. ) **
     +             ccone ) * COS ( ccone * ( rlon1 - cntlon ) ) * sign
C
C*          Recompute grid spacing. Alpha is the constant term in the
C*          map scale factor.
C
            alpha = SIN ( psi ) / ( TAN ( psi / 2. ) ** ccone )
            dx    = tdx / alpha
            dy    = tdy / alpha
C
C*          Compute the linear coordinates for the upper right grid
C*          point.  Assumes left to right, top to bottom image scanning.
C
            x2 = x1 + ( kx - 1 ) * dx
            y2 = y1 + ( ky - 1 ) * dy * sign
C
C*          Compute the lat/lon coordinates of the upper right point.
C
            rlat2 = sign * ( HALFPI - 2. * ATAN ( ( SQRT ( ( x2 * x2 ) +
     +              ( y2 * y2 ) ) / rearth ) ** ( 1. / ccone ) ) )
            dlat2 = rlat2 * RTD
            y2    = (-sign) * y2
            rlon2 = cntlon + ATAN2 ( x2, y2 ) * ( 1. / ccone )
C
            dlon2 = rlon2 * RTD
            CALL PRNLON ( 1, dlon2, ier )
C
C*          Set the lat/lon coordinates of the lower left point.
C
            dlat1 = lat1 / 10000.
            dlon1 = lon1 / 10000.
C
C*          Set projection angles.
C
            dangl1 = std
            dangl2 = clon
            dangl3 = dangl1
C
        ELSE IF ( proj .eq. 'MER' ) THEN
C
C*          **************** 
C*          *** Mercator *** 
C*          **************** 
C
C*          Compute lat/lon of lower left and upper right points. 
C*          Make sure that the longitudes are between -180 and +180.
C
            IF ( rlat1 .gt. rlat2 ) THEN
                temp  = rlat1 * RTD
                dlat1 = rlat2 * RTD
                dlat2 = temp
C
                temp  = rlon1 * RTD
                dlon1 = rlon2 * RTD
                dlon2 = temp
            ELSE
                dlat1 = rlat1 * RTD
                dlat2 = rlat2 * RTD
                dlon1 = rlon1 * RTD
                dlon2 = rlon2 * RTD
            END IF

            CALL PRNLON ( 1, dlon1, ier )
            CALL PRNLON ( 1, dlon2, ier )
C
C*          Set projection angles.
C
            dangl1 = 0.
            dangl2 = 0.
            dangl3 = 0.
        END IF
C
C*      Set navigation block.
C
        CALL GR_MNAV  ( proj, kx, ky, dlat1, dlon1, dlat2,
     +                  dlon2, dangl1, dangl2, dangl3, .true.,
     +                  rheadr, ier )

        nword = 256

C*
        RETURN
        END
