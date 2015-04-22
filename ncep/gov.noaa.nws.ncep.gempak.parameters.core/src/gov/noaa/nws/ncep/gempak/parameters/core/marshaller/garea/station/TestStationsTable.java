package gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.station;

import java.util.List;

public class TestStationsTable {

	/**
	 * @param args
	 */
    public static void main(String args[])  throws Exception{
    	
    	StationDataReader my_sfstns_data_reader = new StationDataReader("res/sfstns.xml");

        List<Station> list = my_sfstns_data_reader.getStationData();
        for(Station itm : list){
            System.out.println(" Station Id    	   = " + itm.getStid() +
            		            " Station Number    = " + itm.getStnnum()+
            		            " Station Name 	   = " + itm.getStnname()+
            		            " State ID   = " + itm.getState()+
            		            " Country ID = " + itm.getCountry()+
            		            " Latitude   = " + itm.getLatitude()+
            		            " Longitude  = " + itm.getLongitude()+
            		            " Station Elevation = " + itm.getElevation() +
            		            " Priority = " + itm.getPriority() 
                               );
       }
    } 

}
