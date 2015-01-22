package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

public class TestGeoMagStationsTable {

	/**
	 * @param args
	 */
    public static void main(String args[])  throws Exception{
    	
    	
    	GeoMagStationTableReaderWriter gmstr = new GeoMagStationTableReaderWriter("/awips2/edex/data/utility/edex_static/base/ncep/geomagstns/GeoMagStations.xml");
        List<GeoMagStation> list = gmstr.readGeoMagStationList();//gml.getGeoMagStationList();
        System.out.println(" Station count: " + list.size());
        for(GeoMagStation itm : list){
        	System.out.println(" *******************************************************"); 
            System.out.println(" Station Code  = " + itm.getStationCode() +
            		            " Provider   = " + itm.getProvider()+
            		            " Source(s)  = " + Arrays.asList(itm.getSource())+
            		            " Location   =  " + itm.getLocation().getLatitude()+ " " + itm.getLocation().getLongitude()+
            		            " Data Order = " + itm.getDataOrder()+
            		            " Process KQDC   = " + itm.getProcessKQDC()+
            		            " KpStation  = " + itm.getKpStation()+            		            
            		            " \n Raw Data Format   :");
            		            
            if (itm.getRawDataFormat().getHeaderFormat()!=null) {
            	System.out.println(" Header Pattern:" + itm.getRawDataFormat().getHeaderFormat().getPattern());
            	
            	if (itm.getRawDataFormat().getHeaderFormat().getGroup() != null) {
	            	List<Group> groups = Arrays.asList(itm.getRawDataFormat().getHeaderFormat().getGroup());
	            	            	
	            	System.out.println(" Groups :");
	            	
	            	for (Group group: groups) {
	            		System.out.println(" ID: " + group.getId() + " Name:" + group.getName());
	            	}
            	}
            	
            }
            if (itm.getRawDataFormat().getDataFormat()!=null) {
            	System.out.println(" Data Pattern:" + itm.getRawDataFormat().getDataFormat().getPattern() + " Conversion Required: " + itm.getRawDataFormat().getDataFormat().getConversionRequired());
            	if (itm.getRawDataFormat().getDataFormat().getGroup() != null) {
	            	List<Group> groups = Arrays.asList(itm.getRawDataFormat().getDataFormat().getGroup());
	            	            	
	            	System.out.println(" Groups :");
	            	
	            	for (Group group: groups) {
	            		System.out.println(" ID: " + group.getId() + " Name:" + group.getName());
	            	}    
            	}
            }
            System.out.println(" *******************************************************");
       }
        
    } 

}
