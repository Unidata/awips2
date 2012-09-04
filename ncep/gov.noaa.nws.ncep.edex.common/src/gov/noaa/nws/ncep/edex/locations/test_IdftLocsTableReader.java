/**
 * This function tests the Idft Point Location Table Reader, IdftLocsTableReader
 * by printing out all the elements in the XML file.  It also gets the first and
 * last element from the list and prints them out 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12May2009  	98    	   F. J. Yen   Initial Creation
 *                       
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 */
package gov.noaa.nws.ncep.edex.locations;

import java.util.List;

public class test_IdftLocsTableReader {
    
    public static void main(String args[]) throws Exception{
    	String idftLocsXmlName = "../build.edex/esb/data/utility/edex_static/base/ncep/stns/idftLoc.xml";
    	IdftLocsTableReader myloc = new IdftLocsTableReader (idftLocsXmlName);
        List<IdftPoint> list = myloc.getIdftLocsTable();
        for(IdftPoint itm : list){
            System.out.println(
            	"  Stid = "     + itm.getStid()      + 
            	"  Stnnum= "    + itm.getStnnum()    + 	
                "  Stnname = "  + itm.getStnname()   +
                "  Latitude = " + itm.getLatitude()  +
                "  Longitude =" + itm.getLongitude() +
                "  Elevation =" + itm.getElevation() +
                "  Priortiy ="  + itm.getPriority() );
        }
        // Get the first and last elements of the list and print them along with the list size
        System.out.println(" Stid(0)=" + list.get(0).stid
        		+ " Stnnum(0) = " + list.get(0).stnnum 
        		+ " Stnname(0) = " + list.get(0).stnname
        		+ " Latitude(0) = " + list.get(0).latitude
        		+ " Longitude(0) = " + list.get(0).longitude
        		+ "\n Stid(206) = " + list.get(206).stid
        		+ " Stnnum(206) = " + list.get(206).stnnum
        		+ " Stnname(206) = " + list.get(206).stnname
        		+ " Latitude(206) = " + list.get(206).latitude
        		+ " Longitude(206) = " + list.get(206).longitude
        		+ "\n size = " + list.size());
    } 
}        
