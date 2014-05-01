package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

/**
 *<pre>
 *
 *  
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 04/01/2013   975        sgurung      Initial Creation 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
public class GeoMagStationTableReader {
private final String PACKAGE = "gov.noaa.nws.ncep.common.dataplugin.geomag.table";
	
	private String xmlFile = null;
		
    public GeoMagStationTableReader(String file) {
    	xmlFile = file;
	}

	public List<GeoMagStation> getStationList() throws JAXBException{
           	
        JAXBContext context = JAXBContext.newInstance(
            PACKAGE);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        GeoMagStationList sfstnlist = null;
        
        File file = new File(xmlFile);
        
		try {
			if (file.exists()) {
				sfstnlist = (GeoMagStationList)unmarshaller.unmarshal(
				    new FileReader(xmlFile));
				List<GeoMagStation> listOfItems = sfstnlist.getGeoMagStationList();			
			    return  listOfItems;
			}
		} catch (FileNotFoundException e) {

			e.printStackTrace();
		}
       
		return null;
        
         
    }    
}
