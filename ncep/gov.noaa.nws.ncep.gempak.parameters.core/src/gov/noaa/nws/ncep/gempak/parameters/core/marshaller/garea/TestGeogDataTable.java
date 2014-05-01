package gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea;

import java.util.List;

public class TestGeogDataTable {

	/**
	 * @param args
	 */
    public static void main(String args[])  throws Exception{
    	
    	GeographicalDataReader my_geog_data_reader = new GeographicalDataReader("res/geog.xml");

        List<GeographicalData> list = my_geog_data_reader.getGeographicalData();
        for(GeographicalData itm : list){
            System.out.println("Geographical Area Code = "  + itm.getGeogCode() + 
            	"\n Geographical Area Descriptor = "        + itm.getGeogAreaName() + 	
                "\n Center Latitude  = "                    + itm.getCenterLat() + 
                "\n Center Longitude = "                    + itm.getCenterLon() +
                "\n Lower Left  Latitude  = "               + itm.getLowerLeftLat() +
                "\n Lower Left  Longitude = "               + itm.getLowerLeftLon() +
                "\n Upper Right Latitude  = "               + itm.getUpperRightLat() +
                "\n Upper Right Longitude = "               + itm.getUpperRightLon() +  
                "\n Map Projection String = "               + itm.getMapProjectionString()
            );
       }
    } 

}
