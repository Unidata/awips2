package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
/*
 * The KStationCoeffTable Reader.
 * 
 * <pre>
 * SOFTWARE HISTORY
 *                   
 * ate          Ticket#     Engineer   Description
 * -----------  ----------  ---------- --------------------------
 * 05/14/2013   #989        qzhou      Initial Creation
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */
public class KStationCoeffTableReader {
	
	private String xmlFile = null;
		
    public KStationCoeffTableReader(String file) {
    	xmlFile = file;
	}

	public List<KStationCoefficient> getStationList() throws JAXBException{
		  	
        JAXBContext context = JAXBContext.newInstance(KStationCoefficients.class); 
            //PACKAGE);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        KStationCoefficients sfstnlist = null;
        
        File file = new File(xmlFile);
        
		try {
			if (file.exists()) {
				sfstnlist = (KStationCoefficients)unmarshaller.unmarshal(
				    new FileReader(xmlFile));
				List<KStationCoefficient> listOfItems = sfstnlist.getKStationCoefficients();	
				
			    return  listOfItems;
			}
		} catch (FileNotFoundException e) {

			e.printStackTrace();
		}
       
		return null;
        
         
    }    
}


