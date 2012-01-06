package gov.noaa.nws.ncep.viz.ui.locator.resource;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

/**
 * This function reads locator settings from locator_tbl.xml table.
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/2008  	22    		M. Li      Initial Creation
 * 11/25/2009    138        G. Hull    add writeLocatorTable (from lost to10 change)
 *                       
 * </pre>
 * 
 * @author M. Li
 * @version 1
 */
public class LocatorTableReader {
    
	private final String PACKAGE = "gov.noaa.nws.ncep.viz.ui.locator.resource";
	
	// Default table location
	private String xmlFile = "res/locator_tbl.xml";
	
	public LocatorTableReader() {
    	
	}
	
    public LocatorTableReader(String file) {
    	xmlFile = file;
	}

	public List<Locator> getLocatorTable() throws JAXBException{
           	
        JAXBContext context = JAXBContext.newInstance(
            PACKAGE);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        Locators loc = null;
        
		try {
			loc = (Locators)unmarshaller.unmarshal(
			    new FileReader(xmlFile));
			List<Locator> listOfItems = loc.getLocator();			
		    return  listOfItems;
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
       
		return null;
	}
	
	public void writeLocatorTable( List<Locator> locatorsList ) {
		try {
			JAXBContext context = JAXBContext.newInstance(PACKAGE);

			Marshaller marshaller = context.createMarshaller();
			marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT,
					Boolean.TRUE );

			Locators locs = new Locators();
			locs.locator = locatorsList;
			marshaller.marshal( locs, new FileWriter( xmlFile ) );
		} catch (JAXBException e) {
			System.out.println( "JAXB Error marshalling Locators Table : " + e.getMessage() );
		} catch (FileNotFoundException e) {
			System.out.println( "JAXB Error marshalling Locators Table : " + e.getMessage() );
		} catch (IOException e) {
			System.out.println( "JAXB Error marshalling Locators Table : " + e.getMessage() );
		}
}

}