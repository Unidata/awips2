package gov.noaa.nws.ncep.viz.cloudHeight.soundings;


import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

/**
 * This function reads soundingModel settings from SoundingModels.xml table.
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/18/09  	115   		Greg Hull      Initial Creation
 *
 * </pre>
 * 
 * @author G. Hull
 * @version 1
 */
public class SoundingModelReader {
    
	private final String PACKAGE = "gov.noaa.nws.ncep.viz.cloudHeight.soundings";
	
	private String xmlFile = null;//"cloudHeight/SoundingModels.xml";
	
	public SoundingModelReader() {
	}
	
    public SoundingModelReader(String file) {
    	xmlFile = file;
	}

	public List<SoundingModel> getSoundingModels() throws JAXBException{
		JAXBContext context = JAXBContext.newInstance( PACKAGE);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        SoundingModels sndMdls = null;
        
		try {
			sndMdls = (SoundingModels)unmarshaller.unmarshal(
			    new FileReader(xmlFile));
			List<SoundingModel> sndingMdlList = sndMdls.getSoundingModel();
		    return  sndingMdlList;
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
    }
}