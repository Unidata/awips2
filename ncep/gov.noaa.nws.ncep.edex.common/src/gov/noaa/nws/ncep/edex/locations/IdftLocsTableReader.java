/**
 * This function reads the Idft Point Location Table from idftLoc.xml
 * and unmarshall it.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14May2009  	98    	   F. J. Yen   Initial Creation
 *                       
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 */
package gov.noaa.nws.ncep.edex.locations;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

public class IdftLocsTableReader {
	
	
	private final String PACKAGE = "gov.noaa.nws.ncep.edex.locations";
	
	private String xmlFilename = null;
	
    public IdftLocsTableReader(String file) {
    	/*
    	 * file is the full name including the path for the
    	 * idft point location xml file, idftLoc.xml
    	 */
    	
    	xmlFilename = file;
	}

	public List<IdftPoint> getIdftLocsTable() throws JAXBException{

	    File xmlFile = new File(xmlFilename);
	    
        JAXBContext context = JAXBContext.newInstance(
           PACKAGE);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        IdftLocs loc = null;
        
		try {
			loc = (IdftLocs)unmarshaller.unmarshal(
			    new FileReader(xmlFile));
			List<IdftPoint> listOfItems = loc.getIdftPoint();			
			return  listOfItems;
		    
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			
		} catch (NullPointerException e2) {
			e2.printStackTrace();		
		}	
		
		return null;
              
    }    
}