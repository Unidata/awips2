package gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea;

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
 * 29-Sep-2009    171       Archana     Initial Creation 
 * </pre>
 * 
 * @author Archana
 * @version 1
 */
public class GeographicalDataReader {
private final String PACKAGE = "gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea";
	
	// Default table location
	private String xmlFile = "res/geog.xml";
	
	public GeographicalDataReader() {
    	
	}
	
    public GeographicalDataReader(String file) {
    	xmlFile = file;
	}

	public List<GeographicalData> getGeographicalData() throws JAXBException{
           	
        JAXBContext context = JAXBContext.newInstance(
            PACKAGE);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        GeogCodeList gclist = null;
        
		try {

			gclist = (GeogCodeList)unmarshaller.unmarshal(
			    new FileReader(xmlFile));
			List<GeographicalData> listOfItems = gclist.getGeographicalData();			
		    return  listOfItems;
		} catch (FileNotFoundException e) {

			e.printStackTrace();
		}
       
		return null;
        
         
    }    
}
