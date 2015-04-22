/*
 * LinePatternManager
 * 
 * Date created: 17 MARCH 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.display.ArrowHead.ArrowHeadType;
import gov.noaa.nws.ncep.ui.pgen.display.PatternSegment.PatternType;

import java.io.File;
import java.io.PrintWriter;
import java.util.HashMap;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

/**
 * This class maintains a HashMap of defined Line Patterns that can be applied to
 * a multi-point line path.<P>
 * This class is implemented as a singleton, and the predefined Line Patterns are
 * constructed when the instance is created.  After the internal patterns are loaded, others
 * can be read in from an XML file if it exists.  
 * 
 * Users can get a reference to this object using the static method getInstance().  
 * @author sgilbert
 *
 */
public class LinePatternManager {

	/**
	 * The singleton instance;
	 */
	private static LinePatternManager instance=null;;
	
	/**
	 * A map of the available Line Patterns with a descriptive String as the key. 
	 */
	private HashMap<String,LinePattern> patternMap;
	
	/**
	 * constructor used by the getInstance method.
	 */
	protected LinePatternManager() {
		
		patternMap = new HashMap<String,LinePattern>();
		initialize();
		
	}
	
	/**
	 * Static method used to request the instance of the LinePatternList object.
	 * @return reference to this object
	 */
	public static synchronized LinePatternManager getInstance() {
		
		if ( instance == null ) {
			instance = new LinePatternManager();
		}
		return instance;	
				
	}
	
	/**
	 * Initialize the HashMap holding the Line Patterns
	 */
	private void initialize() {
	
		loadInternal();
		File patterns = PgenStaticDataProvider.getProvider().getStaticFile( 
				PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + "linePatterns.xml");
		

		if ( patterns != null && patterns.exists() ) {
			loadPatternsFromFile( patterns.getAbsolutePath() );
		}		
	}
	
	/**
	 * Gets the LinePattern associated with the requested id String
	 * @param key the name/description of the LinePattern desired.
	 * @return The requested LinePattern
	 * @throws If requested Pattern cannot be found.
	 */
	public LinePattern getLinePattern(String key) throws LinePatternException {
		
		LinePattern pattern = patternMap.get(key);
		if ( pattern == null ) throw new LinePatternException("Could not find line pattern: " + key);
		return pattern;
	}

	/**
	 * Gets a list of all currently available LinePatterns
	 * @return An array of Line Pattern names
	 */
	public String[] getPatternNames() {
		
		String[] names = new String[patternMap.size()];
		
		int i=0;
		for ( LinePattern lp : patternMap.values() ) {
			names[i++] = lp.getName();
		}
		return names;
		
	}
	
	/**
	 * Gets a list of IDs used to identify and request a specific LinePattern
	 * @return An array of Pattern ids
	 */
	public String[] getPatternIds() {
		
		String[] ids = new String[patternMap.size()];
		
		int i=0;
		for ( String str : patternMap.keySet() ) {
			ids[i++] = str;
		}
		return ids;
		
	}
	
	/**
	 * Constructs the HashMap holding all these internally defined LinePatterns.
	 * <P>
	 */
	private void loadInternal() {
		
		LinePattern lp;
		
		lp = new LinePattern("Solid Line",false,null);
		patternMap.put("LINE_SOLID", lp);
		
		lp = new LinePattern("Line with open arrow head",true,ArrowHeadType.OPEN);
		patternMap.put("POINTED_ARROW", lp);
		
		lp = new LinePattern("Line with closed arrow head",true,ArrowHeadType.FILLED);
		patternMap.put("FILLED_ARROW", lp);
		
		lp = new LinePattern("Dotted Line", false, null);
		lp.addSegment(new PatternSegment(0,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.CIRCLE_FILLED,0,8,0,false));
		patternMap.put("LINE_DASHED_2", lp);

		lp = new LinePattern("Short Dashed", false, null);
		lp.addSegment(new PatternSegment(2,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(3,PatternType.BLANK,0,0,0,false));
		patternMap.put("LINE_DASHED_3", lp);

		lp = new LinePattern("Medium Dashed", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		patternMap.put("LINE_DASHED_4", lp);

		lp = new LinePattern("Long Dash Short Dash", false, null);
		lp.addSegment(new PatternSegment(6,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(3,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(3,PatternType.BLANK,0,0,0,false));
		patternMap.put("LINE_DASHED_5", lp);

		lp = new LinePattern("Long Dashed", false, null);
		lp.addSegment(new PatternSegment(5,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		patternMap.put("LINE_DASHED_6", lp);

		lp = new LinePattern("Long Dash Three Short Dashes", false, null);
		lp.addSegment(new PatternSegment(8,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(3,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(3,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(3,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(3,PatternType.BLANK,0,0,0,false));
		patternMap.put("LINE_DASHED_7", lp);
		
		lp = new LinePattern("Long Dash Dot", false, null);
		lp.addSegment(new PatternSegment(8,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.CIRCLE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.BLANK,0,0,0,false));
		patternMap.put("LINE_DASHED_8", lp);

		lp = new LinePattern("Medium Dash Dot Dot Dot", false, null);
		lp.addSegment(new PatternSegment(6,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.CIRCLE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.CIRCLE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.CIRCLE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.BLANK,0,0,0,false));
		patternMap.put("LINE_DASHED_9", lp);

		lp = new LinePattern("Long Dash Dot Dot", false, null);
		lp.addSegment(new PatternSegment(15,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.CIRCLE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.CIRCLE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(0,PatternType.BLANK,0,0,0,false));
		patternMap.put("LINE_DASHED_10", lp);

		lp = new LinePattern("Dashed Line with open arrow head", true, ArrowHeadType.OPEN);
		lp.addSegment(new PatternSegment(8,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(6,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.LINE,0,0,0,false));
		patternMap.put("DASHED_ARROW", lp);

		lp = new LinePattern("Dashed Line with filled arrow head", true, ArrowHeadType.FILLED);
		lp.addSegment(new PatternSegment(8,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(6,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.LINE,0,0,0,false));
		patternMap.put("DASHED_ARROW_FILLED", lp);

		lp = new LinePattern("Ball-and-Chain", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.CIRCLE,0,8,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		patternMap.put("BALL_CHAIN", lp);

		lp = new LinePattern("ZigZag", false, null);
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE,0,2,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE,0,2,0,true));
		patternMap.put("ZIGZAG", lp);
		
		lp = new LinePattern("Scallop", false, null);
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE,0,8,0,false));
		patternMap.put("SCALLOPED", lp);
		
		lp = new LinePattern("Alternating Angled Ticks", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.ARC_90_DEGREE,0,1,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.ARC_90_DEGREE,0,1,0,true));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		patternMap.put("ANGLED_TICKS_ALT", lp);
		
		lp = new LinePattern("Filled Circle", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.CIRCLE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		patternMap.put("FILLED_CIRCLES", lp);
		
		lp = new LinePattern("Line-Caret-Line", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(6,PatternType.ARC_270_DEGREE_WITH_LINE,0,2,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		patternMap.put("LINE_WITH_CARETS", lp);

		lp = new LinePattern("Line-Caret-Line with spaces", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(6,PatternType.ARC_270_DEGREE,0,2,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		patternMap.put("LINE_CARET_LINE", lp);

		lp = new LinePattern("Sine Curve", false, null);
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE,0,8,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE,0,8,0,true));
		patternMap.put("SINE_CURVE", lp);
		
		lp = new LinePattern("Stationary Front at the surface", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,8,0,true));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,1,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,1,2,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,1,0,0,false));
		patternMap.put("STATIONARY_FRONT", lp);
		
		lp = new LinePattern("Stationary Front Frontogenesis", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,8,0,true));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,1,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,1,2,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,1,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		patternMap.put("STATIONARY_FRONT_FORM", lp);
		
		lp = new LinePattern("Stationary Front Frontolysis", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,8,0,true));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.LINE,1,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,1,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,1,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,1,2,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,1,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,1,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.LINE,1,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		patternMap.put("STATIONARY_FRONT_DISS", lp);
		
		lp = new LinePattern("Warm Front at the surface", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		patternMap.put("WARM_FRONT", lp);

		lp = new LinePattern("Warm Front Frontogenesis", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		patternMap.put("WARM_FRONT_FORM", lp);

		lp = new LinePattern("Warm Front Frontolysis", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(16,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		patternMap.put("WARM_FRONT_DISS", lp);

		lp = new LinePattern("Cold Front at the surface", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,2,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		patternMap.put("COLD_FRONT", lp);

		lp = new LinePattern("Cold Front Frontogenesis", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,2,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		patternMap.put("COLD_FRONT_FORM", lp);

		lp = new LinePattern("Cold Front Frontolysis", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,2,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(16,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		patternMap.put("COLD_FRONT_DISS", lp);

		lp = new LinePattern("Occluded Front at the surface", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,2,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		patternMap.put("OCCLUDED_FRONT", lp);
		
		lp = new LinePattern("Occluded Front Frontogenesis", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,2,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		patternMap.put("OCCLUDED_FRONT_FORM", lp);
		
		lp = new LinePattern("Occluded Front Frontolysis", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,2,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(16,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(16,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		patternMap.put("OCCLUDED_FRONT_DISS", lp);

		lp = new LinePattern("TROF", false, null);
		lp.addSegment(new PatternSegment(12,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.BLANK,0,0,0,false));
		patternMap.put("TROF", lp);
		
		lp = new LinePattern("Tropical TROF", false, null);
		patternMap.put("TROPICAL_TROF", lp);
		
		lp = new LinePattern("Dry-Line", false, null);
		lp.addSegment(new PatternSegment(8,PatternType.ARC_180_DEGREE_CLOSED,0,8,0,false));
		patternMap.put("DRY_LINE", lp);
		
		lp = new LinePattern("Instability (Squall) Line", false, null);
		lp.addSegment(new PatternSegment(8,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.CIRCLE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.CIRCLE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.LINE,0,0,0,false));
		patternMap.put("INSTABILITY", lp);
		
		lp = new LinePattern("Box-Circle", false, null);
		lp.addSegment(new PatternSegment(8,PatternType.BOX,0,0,2,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.CIRCLE,0,8,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		patternMap.put("BOX_CIRCLE", lp);

		lp = new LinePattern("Filled Box-Open Box with filled arrow head", true, ArrowHeadType.FILLED);
		lp.addSegment(new PatternSegment(4,PatternType.BOX_FILLED,0,0,2,false));
		lp.addSegment(new PatternSegment(4,PatternType.BOX,0,0,2,false));
		lp.addSegment(new PatternSegment(4,PatternType.BOX_FILLED,0,0,2,false));
		patternMap.put("FILL_OPEN_BOX", lp);

		lp = new LinePattern("Line-X-Line", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.X_PATTERN,0,0,2,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		patternMap.put("LINE_X_LINE", lp);

		lp = new LinePattern("Line-2Xs-Line", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.X_PATTERN,0,0,2,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.X_PATTERN,0,0,2,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		patternMap.put("LINE_XX_LINE", lp);

		lp = new LinePattern("Filled Circle-X", false, null);
		lp.addSegment(new PatternSegment(4,PatternType.CIRCLE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.X_PATTERN,0,0,2,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		patternMap.put("FILL_CIRCLE_X", lp);
		
		lp = new LinePattern("Box-X", false, null);
		lp.addSegment(new PatternSegment(8,PatternType.BOX,0,0,2,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.X_PATTERN,0,0,2,false));
		lp.addSegment(new PatternSegment(2,PatternType.BLANK,0,0,0,false));
		patternMap.put("BOX_X", lp);

		lp = new LinePattern("Line-Circle-Line with filled arrow head", true, ArrowHeadType.FILLED);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.CIRCLE,0,8,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		patternMap.put("LINE_CIRCLE_ARROW", lp);

		lp = new LinePattern("Line-Filled-Circle-Line with filled arrow head", true, ArrowHeadType.FILLED);
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.CIRCLE_FILLED,0,8,0,false));
		lp.addSegment(new PatternSegment(4,PatternType.LINE,0,0,0,false));
		patternMap.put("LINE_FILLED_CIRCLE_ARROW", lp);
		
		lp = new LinePattern("Double Line", false, null);
		lp.addSegment(new PatternSegment(1,PatternType.DOUBLE_LINE,0,0,2,false));
		patternMap.put("DOUBLE_LINE", lp);
		
		lp = new LinePattern("Z-Line", false, null);
		lp.addSegment(new PatternSegment(8,PatternType.Z_PATTERN,0,0,2,false));
		patternMap.put("ZZZ_LINE", lp);
		
		lp = new LinePattern("Tick Mark", false, null);
		lp.addSegment(new PatternSegment(8,PatternType.TICK,0,0,2,false));
		patternMap.put("TICK_MARKS", lp);
		
		lp = new LinePattern("Streamline-like", false, null);
		lp.addSegment(new PatternSegment(12,PatternType.LINE,0,0,0,false));
		lp.addSegment(new PatternSegment(8,PatternType.ARROW_HEAD,0,2,0,false));
		patternMap.put("STREAM_LINE", lp);
		
	}
	
	/**
	 * Uses JAXB to marshal this list of LinePatterns to an XML file.
	 * @param filename output XML filename
	 */
	public void savePatternsToFile(String filename) {
		
		PrintWriter writer = null;

		/*
		 * Create a list of line patterns from the HashMap
		 */
	    LinePatternList patternList = new LinePatternList(patternMap);
		
		try {
			// RTS utility: SerializationUtil.jaxbMarshalToXmlFile(lpm, filename);
			/*
			 * Set JAXB Marshaling context and properties
			 */
			JAXBContext context = JAXBContext.newInstance(LinePatternList.class,LinePatternMapEntry.class);
			Marshaller msh = context.createMarshaller();
			msh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);

			/*
			 * create output file
			 */
			File fileOut = new File(filename);
			writer = new PrintWriter(fileOut);

			/*
			 * Marshal list of line patterns to XML file
			 */
			msh.marshal(patternList, writer);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		finally {
			if ( writer != null ) writer.close();
		}
	}
	

	/**
	 * Uses JAXB to unmarshal a list of LinePatterns from an XML file,
	 * and load them into the internal HashMap. 
	 * @param filename input XML filename
	 */
	public void loadPatternsFromFile(String filename) {
		
		File fileIn = null;
		
		try {
			//RTS utility: instance = (LinePatternManager) SerializationUtil.jaxbUnmarshalFromXmlFile(filename);
			JAXBContext context = JAXBContext.newInstance(LinePatternList.class);
			Unmarshaller msh = context.createUnmarshaller();
			
			/*
			 * unmarshal list of line patterns
			 */
			fileIn = new File(filename);
			LinePatternList lpl = (LinePatternList) msh.unmarshal(fileIn);
			
			// Add each LinePattern to hashMap
			for (LinePatternMapEntry entry : lpl.getPatternList() ) {
			  patternMap.put(entry.getPatternId(), entry.getPattern());
			}

		}
		catch (Exception e) {
			e.printStackTrace();
		}

	}
	
}
