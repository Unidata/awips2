package gov.noaa.nws.ncep.viz.tools.cursor;


import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

/**
 * This function reads MISC attribute settings from wcp_tbl.xml table.
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/2009  	96    		M. Li      Initial Creation
 *                       
 * </pre>
 * 
 * @author M. Li
 * @version 1
 */
public class CursorReferenceTableReader {
    
	private final String PACKAGE = "gov.noaa.nws.ncep.viz.tools.cursor";
	
	// Default table location
	private String xmlFile = "res/cursorset_tbl.xml";
	
	public CursorReferenceTableReader() {
    	
	}
	
    public CursorReferenceTableReader(String file) {
    	xmlFile = file;
	}

	public List<CursorReference> getTable() throws JAXBException{
           	
        JAXBContext context = JAXBContext.newInstance(
            PACKAGE);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        CursorReferences miscs = null;
        
		try {
			miscs = (CursorReferences)unmarshaller.unmarshal(
			    new FileReader(xmlFile));
			List<CursorReference> listOfItems = miscs.getCursorReference();			
		    return  listOfItems;
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
       
		return null;
        
         
    }    
}