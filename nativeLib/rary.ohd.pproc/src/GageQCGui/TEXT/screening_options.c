#include <Xm/Xm.h>

#include "gageqc_gui.h"
#include "gageqc_types.h"
#include "map_library.h"

void screening_options ( Widget w, 
		         XtPointer client_data,
			 XtPointer call_data )
{
   extern int pcpn_day ;
   extern struct pdata pdata [ ];
   extern struct tdata tdata [ ];

   int num_temp_stations;
   int num_precip_stations;

   struct station * precip_stations = NULL;
   struct station * temp_stations = NULL;

   precip_stations = get_precip_station_list ( & num_precip_stations );
   temp_stations = get_temperature_station_list ( & num_temp_stations );
   

   if((int)client_data==0)
   {
             pdata[pcpn_day].stddev=5.0;
   }
   else if((int)client_data==1)
   {
             pdata[pcpn_day].stddev=3.0;
   }
   else if((int)client_data==2)
   {
             pdata[pcpn_day].stddev=1.0;
   }

   if((int)client_data==0)
   {
             tdata[pcpn_day].stddev=15.0;
   }
   else if((int)client_data==1)
   {
             tdata[pcpn_day].stddev=10.0;
   }
   else if((int)client_data==2)
   {
             tdata[pcpn_day].stddev=5.0;
   }

   mSetCursor ( M_WATCH );

   estimate_daily_stations(pcpn_day, precip_stations, num_precip_stations);
   estimate_partial_stations(pcpn_day, precip_stations, num_precip_stations);
   quality_control_stations(pcpn_day, precip_stations, num_precip_stations);
   check_consistency(pcpn_day, precip_stations, num_precip_stations);
   restore_bad_values(pcpn_day, precip_stations, num_precip_stations);

   estimate_daily_tstations(pcpn_day, temp_stations, num_temp_stations);
   quality_control_tstations(pcpn_day, temp_stations, num_temp_stations);
   restore_bad_tvalues(pcpn_day, temp_stations, num_temp_stations);

   mSetCursor ( M_NORMAL );

   send_expose ( );
}
