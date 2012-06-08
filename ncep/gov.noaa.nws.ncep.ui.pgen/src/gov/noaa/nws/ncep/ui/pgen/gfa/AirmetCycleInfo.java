/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.AirmetCycleInfo
 * 
 * July 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;

import java.io.File;
import java.util.Calendar;
import java.util.List;

import org.dom4j.Document;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;

/**
 * Helper file to read  cycle configuration.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/10		#223		M.Laryukhin	Initial creation
 * 
 * </pre>
 * 
 * @author M.Laryukhin
 * @version 1
 */
public class AirmetCycleInfo {

	private static Document airmetCycleDoc;

	public static final String AIRMET_ELEMENT_XPATH = "/airmetcycle/element";

	/**
	 * Getter for the document.
	 * 
	 * @return
	 */
	public static Document getDocument() {
		if (airmetCycleDoc == null) {
			readAirmetCycle();
		}
		return airmetCycleDoc;
	}

	/**
	 * Read the airmetcycle.xml file
	 */
	private static void readAirmetCycle() {

		try {
			File airmetCycleFile = PgenStaticDataProvider.getProvider().getStaticFile( 
					PgenStaticDataProvider.getProvider().getPgenLocalizationRoot()+ "airmetcycle.xml" ); 	
  	    
			SAXReader reader = new SAXReader();
			airmetCycleDoc = reader.read(airmetCycleFile.getAbsoluteFile() );
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/*
	 * This method is just to suppress warning
	 */
	@SuppressWarnings("unchecked")
	public static List<Node> selectNodes(String xPath) {
		return (List<Node>) AirmetCycleInfo.getDocument().selectNodes(xPath);
	}

	public static String getIssueTime() {
		int cycleH = PgenCycleTool.getCycleHour();
		int cycleD = PgenCycleTool.getCycleDay();

		String xPath = AIRMET_ELEMENT_XPATH + "[@cycle='" + PgenCycleTool.pad(cycleH) + "']";
		
		List<Node> elements = selectNodes(xPath);
		
		String ret;
		
		if(elements.isEmpty()) {
			xPath = AIRMET_ELEMENT_XPATH + "[@timezone='DEFAULT']";
			elements = selectNodes(xPath);
			String d = elements.get(0).valueOf("@delayMin");
			int delayMin = Integer.parseInt(d);
			Calendar cal = Calendar.getInstance();
			cal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cycleD, cycleH, 0, 0);
			
			cal.add(Calendar.MINUTE, -delayMin);
			
			ret = PgenCycleTool.pad(cal.get(Calendar.HOUR_OF_DAY))
					+ PgenCycleTool.pad(cal.get(Calendar.MINUTE));
		} else {
			ret = elements.get(0).valueOf("@issue");
		}
		
		return ret;
	}
	
	public static Calendar getUntilTime() {
		int cycleH = PgenCycleTool.getCycleHour();
		int cycleD = PgenCycleTool.getCycleDay();

		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.DAY_OF_MONTH, cycleD);
		cal.set(Calendar.HOUR_OF_DAY, cycleH);
		cal.set(Calendar.MINUTE, 0);
		cal.set(Calendar.SECOND, 0);
		cal.add(Calendar.HOUR_OF_DAY, 6);
		
		return cal;
	}

}
