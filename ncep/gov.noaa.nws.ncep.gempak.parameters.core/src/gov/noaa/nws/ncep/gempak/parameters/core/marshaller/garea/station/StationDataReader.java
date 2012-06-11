package gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.station;

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
 * 01-Oct-2009    171       Archana     Initial Creation 
 * </pre>
 * 
 * @author Archana
 * @version 1
 */
public class StationDataReader {
private final String PACKAGE = "gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.station";
	
	// Default table location
	private String xmlFile = "res/sfstns.xml";
	
	public StationDataReader() {
    	
	}
	
    public StationDataReader(String file) {
    	xmlFile = file;
	}

	public List<Station> getStationData() throws JAXBException{
           	
        JAXBContext context = JAXBContext.newInstance(
            PACKAGE);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        StationList sfstnlist = null;
        
		try {

			sfstnlist = (StationList)unmarshaller.unmarshal(
			    new FileReader(xmlFile));
			List<Station> listOfItems = sfstnlist.getStation();			
		    return  listOfItems;
		} catch (FileNotFoundException e) {

			e.printStackTrace();
		}
       
		return null;
        
         
    }    
}
