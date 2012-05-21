/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.GfaInfo
 * 
 * July 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;

import java.awt.Color;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.dom4j.Document;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;
import org.eclipse.swt.graphics.RGB;

/**
 * Helper class to read the GFA configuration.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/10		#223		M.Laryukhin	Initial creation
 * 02/12		#662		J. Wu		Adjust forecast hour with color/line width
 * 02/12		#672		J. Wu		Add FA area specific state list
 * 
 * </pre>
 * 
 * @author M.Laryukhin
 * @version 1
 */
public class GfaInfo {

	// gui settings 
	private static Document doc;

	public static final String HAZARD_XPATH = "/root/hazard";
	public static final String FCSTHR_XPATH = "/root/fcstHr";
	public static final String TAG_XPATH = "/root/tag";
	public static final String DESK_XPATH = "/root/desk";
	public static final String ISSUE_TYPE_XPATH = "/root/issueType";
	public static final String GFA_OTLKGEN_RATIO_XPATH = "/root/gfaOtlkgenRatio";
	
	public static final String AIRMET_ELEMENT_XPATH = "/airmetcycle/element";
	
	public static final String GFA_SNAPSHOT = "snapshot";
	public static final String GFA_SMEAR = "smear";
	public static final String GFA_OUTLOOK = "outlook";

	public static final int GFA_SMEAR_LINEWIDTH = 3;
	public static final int GFA_OUTLOOK_LINEWIDTH = 4;
	public static final int GFA_OTHER_LINEWIDTH = 2;
	

	/**
	 * Hazard type vs color array (index means the same as the position
	 * in fcstHr combo box)
	 */
	private static HashMap<String, RGB[]> rgbMap;

	/**
	 * Pairs like ("red", new RGB(255, 0, 0)) read from gfa.xml
	 */
	private static HashMap<String, RGB> definedColors;
	
	/**
	 * Hazard categories
	 */
	private static HashMap<String, HazardCategory> hazardCategories;
	
	public enum HazardCategory{
		SIERRA, TANGO, ZULU, NONE;
	}

	/**
	 * State orders in each FA area.
	 */
	private static HashMap<String, ArrayList<String> > stateOrderByArea;
	
	/**
	 * Getter for the document. 
	 * 
	 * @return
	 */
	public static Document getDocument() {
		if (doc == null) {
			readOptions();
		}
		return doc;
	}

	/**
	 * Read the menu configuration from gfa.xml file
	 */
	private static void readOptions() {
		File gfainfoFile = PgenStaticDataProvider.getProvider().getStaticFile( 
				PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + "gfa.xml");   	    

		try {
			SAXReader reader = new SAXReader();
			doc = reader.read(gfainfoFile.getAbsoluteFile());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/*
	 * This method is just to suppress warning
	 */
	@SuppressWarnings("unchecked")
	public static List<Node> selectNodes(String xPath) {
		return (List<Node>) GfaInfo.getDocument().selectNodes(xPath);
	}

	/**
	 * Creates an rgb map for hazard types and returns the corresponding color.
	 * 
	 * @param hazard
	 * @return
	 */
	public static RGB getRGB(String hazard, int fcstHrIndex) {
		if (rgbMap == null) {
			loadColors();
		}

		if (fcstHrIndex < 0 || fcstHrIndex >= rgbMap.get(hazard).length) {
			// take last as default if parameter is out of range (fcstHr=Other)
			fcstHrIndex = rgbMap.get(hazard).length-1;
		}

		return rgbMap.get(hazard)[fcstHrIndex];
	}
	
	/**
	 * Load colors.
	 */
	private static void loadColors() {
		List<Node> colorNodes = selectNodes("/root/color/value");

		definedColors = new HashMap<String, RGB>();
		for (Node n : colorNodes) {
			int r = Integer.parseInt(n.valueOf("@r"));
			int g = Integer.parseInt(n.valueOf("@g"));
			int b = Integer.parseInt(n.valueOf("@b"));
			// for example, <"red", (255,0,0)>
			definedColors.put(n.valueOf("@name"), new RGB(r, g, b));
		}

		List<Node> hazardNodes = selectNodes(HAZARD_XPATH);
		List<Node> fcstHrNodes = selectNodes(FCSTHR_XPATH);

		// hazard type vs. color array (index means the same as the position
		// in fcstHr combo box)
		rgbMap = new HashMap<String, RGB[]>();

		for (Node n : hazardNodes) {
			RGB[] colors = new RGB[fcstHrNodes.size()];
			int i = 0;
			for (Node f : fcstHrNodes) {
				String type = f.valueOf("@type"); // for example, "smear"
				String colorStr = n.valueOf("@" + type); // for example,
				// "blue"
				colors[i++] = definedColors.get(colorStr);
			}
			rgbMap.put(n.valueOf("@name"), colors);
		}
	}

	/**
	 * Returns the default colors for the hazard and forecast hour pair. 
	 * 
	 * @param hazard
	 * @param fcstHr
	 * @return
	 */
	public static Color[] getDefaultColors(String hazard, String fcstHr) {
		if (definedColors == null) {
			loadColors();
		}
		
		String xPath = HAZARD_XPATH + "[@name='" + hazard + "']";
		List<Node> hazardNodes = selectNodes(xPath);
		
		xPath = FCSTHR_XPATH + "[@name='" + fcstHr + "']";
		List<Node> fcsthrNodes = selectNodes(xPath);
				
		if (fcsthrNodes.size() != 1) {
			try{
				if(fcstHr.indexOf("-") == -1) { // snapshot
					xPath = FCSTHR_XPATH + "[@name='0 Z']";
				} else {
					String second= fcstHr.split("-")[1];
					String hour = second.split(":")[0];
					if(Integer.parseInt(hour) <=6){ // smear
						xPath = FCSTHR_XPATH + "[@name='0-6']";
					} else { // outlook
						xPath = FCSTHR_XPATH + "[@name='6-9']";
					}
				}
			} catch (Exception e){
				xPath = FCSTHR_XPATH + "[@name='Other']";
			}
			fcsthrNodes = selectNodes(xPath);
		}
		
		String gfaType = GFA_SNAPSHOT;
		if ( fcsthrNodes.size() != 1 ) {  //hard-coded
			if ( fcstHr.indexOf("-") >= 0 ) {
				String second= fcstHr.split("-")[1];
				String hour = second.split(":")[0];
				if(Integer.parseInt(hour) <=6) {
					gfaType = GFA_SMEAR;
				} else {
					gfaType = GFA_OUTLOOK;
				}
			}			
		}
		else {
			gfaType = fcsthrNodes.get(0).valueOf("@type"); // from table
		}
		
		if (hazardNodes.size() != 1 ) {
			throw new IllegalArgumentException("Please check hazard name");
		}
		
		String colorStr = hazardNodes.get(0).valueOf("@" + gfaType );
		
		RGB rgb = definedColors.get(colorStr);
		Color color = new Color(rgb.red, rgb.green, rgb.blue);
		return new Color[]{color, color};
	}
	
	/**
	 * Returns the default line width for the forecast hour. 
	 * 
	 * @param fcstHr
	 * @return
	 */
	public static int getLineWidth(String fcstHr) {
		String xPath = FCSTHR_XPATH + "[@name='" + fcstHr + "']";
		List<Node> fcsthrNodes = selectNodes(xPath);
		
		int lineWidth = GFA_OTHER_LINEWIDTH;
		if ( fcsthrNodes.size() > 0 ) {
			lineWidth = Integer.parseInt( fcsthrNodes.get(0).valueOf("@linewidth") );
		}
		else {
			if ( fcstHr.indexOf("-") >= 0 ) {
				String second= fcstHr.split("-")[1];
				String hour = second.split(":")[0];
				if( Integer.parseInt(hour) <= 6 ) {
					lineWidth = GFA_SMEAR_LINEWIDTH;
				} else {
					lineWidth = GFA_OUTLOOK_LINEWIDTH;
				} 
			}			
			
		}

		return lineWidth;
	}
	
	public static boolean isFormat(String hazard){
		if (definedColors == null) {
			loadColors();
		}
		
		String xPath = HAZARD_XPATH + "[@name='" + hazard + "']";
		List<Node> hazardNodes = selectNodes(xPath);
		if (hazardNodes.size() != 1) {
			throw new IllegalArgumentException("Please check hazard name");
		}
		String format = hazardNodes.get(0).valueOf("@format");
		return !"false".equals(format);
	}

	private static HashMap<String, HazardCategory> getHazardCategories() {
		if(hazardCategories == null) {
			hazardCategories = new HashMap<String, HazardCategory>();
			List<Node> nodes = selectNodes(HAZARD_XPATH);
			for(Node n: nodes){
				String key = n.valueOf("@name");
				String category = n.valueOf("@category");
				HazardCategory cat = HazardCategory.valueOf(HazardCategory.class, category);
				if(cat == null) cat = HazardCategory.NONE;
				hazardCategories.put(key, cat);
			}
		}
		return hazardCategories;
	}
	
	public static HazardCategory getHazardCategory(String hazard){
		return getHazardCategories().get(hazard);
	}
	
	public static double getGfaOtlkgenRatio(){
		List<Node> nodes = selectNodes(GFA_OTLKGEN_RATIO_XPATH);
		Node n = nodes.get(0);
		String rationStr = n.getStringValue();
		return Double.parseDouble(rationStr);
	}

    /**
     * Return the fixed ordered state list in each FA area.
     */
	public static HashMap<String, ArrayList<String> > getStateOrderByArea() {
		
		if ( stateOrderByArea == null ) {
			stateOrderByArea = new HashMap<String, ArrayList<String> >();
			ArrayList<String>  bos = new ArrayList<String>();
			String[] bosStr = new String[]{"ME","NH","VT","MA","RI","CT","NY",
                                           "LO","PA","NJ","OH","LE","WV","MD",
                                           "DC", "DE", "VA", "CSTL WTRS"};
			for ( String st : bosStr ) {
				bos.add( st );
			}
			stateOrderByArea.put( "BOS", bos);
			
			ArrayList<String>  mia = new ArrayList<String>();
			String[] miaStr = new String[]{"NC", "SC", "GA", "FL" , "CSTL WTRS"};
			for ( String st : miaStr ) {
				mia.add( st );
			}
			stateOrderByArea.put( "MIA", mia );

			ArrayList<String>  chi = new ArrayList<String>();
			String[] chiStr = new String[]{"ND", "SD", "NE", "KS", "MN", "IA", 
                                           "MO", "WI", "LM", "LS", "MI", "LH", 
                                           "IL", "IN", "KY"};
			for ( String st : chiStr ) {
				chi.add( st );
			}
			stateOrderByArea.put( "CHI", chi );

			ArrayList<String>  dfw = new ArrayList<String>();
			String[] dfwStr = new String[]{"OK", "TX", "AR", "TN", "LA", "MS", 
                                           "AL", "CSTL WTRS"};
			for ( String st : dfwStr ) {
				dfw.add( st );
			}
			stateOrderByArea.put( "DFW", dfw );
			
			ArrayList<String>  slc = new ArrayList<String>();
			String[] slcStr = new String[]{"ID", "MT", "WY", "NV", "UT", "CO", 
                                           "AZ", "NM"} ;
			for ( String st : slcStr ) {
				slc.add( st );
			}
			stateOrderByArea.put( "SLC", slc );
			
			ArrayList<String>  sfo = new ArrayList<String>();
			String[] sfoStr = new String[]{"WA", "OR", "CA", "CSTL WTRS"};
			for ( String st : sfoStr ) {
				sfo.add( st );
			}
			stateOrderByArea.put( "SFO", mia );
			
		}
		
		return stateOrderByArea;
	}
	
}
