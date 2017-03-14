C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE SETPLAT
C-----------------------------------------------------------------------
C  Routine to set platform information.
C-----------------------------------------------------------------------
C  notes: (1) This subroutine reads the platform from the defaults
C             file and sets a common block variable.
C         (2) This routine should be called from the main program as
C             soon as possible so that platform-dependent data can
C             take effect.
C-----------------------------------------------------------------------
C variables:
C
C ierr          .... error status variable (1 if error, 0 if not)
C opersys       .... platform
C apps_var       .... apps_vared variable name in defaults file
C-----------------------------------------------------------------------

      subroutine setplat ( ierr )

      include 'common/fdbug'
      include 'common/where'
      include 'common/sionum'
      include 'common/platform'

      character*128  apps_var,apps_val
      integer  lapps_var,lapps_val,ierr
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/gen/RCS/setplat.f,v $
     . $',                                                             '
     .$Id: setplat.f,v 1.5 2002/02/11 13:29:49 michaelo Exp $
     . $' /
C    ===================================================================
C

      call umemov ('SETPLAT ',opname,2)

      if (itrace .ge. 1) write(iodbug,*) 'ENTER SETPLAT'
      
      ierr = 0

C  get the platform from apps_default (maximum 8 characters)
      opersys = ' '
      apps_var  = 'nwsrfs_platform '
      lapps_var = lenstr(apps_var)
      apps_val = ' '
      call get_apps_defaults ( apps_var,lapps_var,apps_val,lapps_val )
      if ( lapps_val.le.0 .or. lapps_val.gt.8 ) then
         write (istderr,10) apps_var(1:lenstr(apps_var))
10    format('**ERROR** apps_default ',a,' not specified.')
         ierr = 1
         else
            opersys(1:8) = apps_val(1:lapps_val)
         endif

      if (itrace .ge. 1) write(iodbug,*) 'EXIT SETPLAT'

      return

      end
