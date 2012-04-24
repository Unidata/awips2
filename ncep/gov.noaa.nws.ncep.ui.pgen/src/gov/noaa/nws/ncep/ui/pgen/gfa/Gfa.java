/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.Gfa
 * 
 * July 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;
import gov.noaa.nws.ncep.viz.common.SnapUtil;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

import org.apache.log4j.Logger;
import org.dom4j.Node;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Polygon;


/**
 * Class for GFA element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/2010		#223		M.Laryukhin	Initial Creation.
 * 05/2010		#256		B. Yin		Added getForecastHours to make filter work
 * 02/2011					J. Wu		Made deep copy of GFA
 * 04/2011		#?			B. Yin		Re-factor IAttribute
 * 05/2011					J. Wu		Added reduce-able flags.
 * 01/2012					J. Wu		Avoid null pointer in copy().
 * 02/12        #597        S. Gurung   Moved snap functionalities to SnapUtil from SigmetInfo. 
 * 
 * </pre>
 * 
 * @author mlaryukhin
 */
@ElementOperations( { Operation.COPY_MOVE, Operation.DELETE_PART, Operation.DELETE_POINT,
		Operation.ADD_POINT, Operation.MODIFY, Operation.GFA_FROM } )
public class Gfa extends Line implements IGfa, Comparable<Gfa> {

	/** Logger */
	private final static Logger logger = Logger.getLogger(Gfa.class);

	private String hazard = "";
	private String fcstHr = "";
	private String tag = "";
	private String desk = "";
	private String issueType = "";
	private int cycleDay = -1;
	private int cycleHour = -1;
	private String type = "";
	private String area = "";
	private String beginning = "";
	private String ending = "";
	private String states = "";

	private Coordinate			gfaTextCoordinate;
	// other gui configurable values, for example Coverage for CLD, Top/Bottom for ICE etc
	private HashMap<String, String> values = new HashMap<String, String>();
	
	/** The list of points which should be ignored when snapping*/
	private ArrayList<Coordinate> notToBeSnapped = new ArrayList<Coordinate>();
	
	private final static int MIN_WIDTH_GFA_TEXT = 8;
	
	public static final String GR = "GR";
	public static final String FREQUENCY = "Frequency";
	public static final String CATEGORY = "Category";
	public static final String FZL_RANGE = "FZL RANGE";
	public static final String LEVEL = "Level";
	public static final String INTENSITY = "Intensity";
	public static final String SPEED = "Speed";
	public static final String DUE_TO = "DUE TO";
	public static final String LYR = "LYR";
	public static final String COVERAGE = "Coverage";
	public static final String BOTTOM = "Bottom";
	public static final String TOP = "Top";
	public static final String TOP_BOTTOM = "Top/Bottom";
	public static final String FZL_TOP_BOTTOM = "FZL Top/Bottom";
	public static final String CONTOUR = "Contour";
	
	public static final String ENDG_HR = "ENDG_HR";
	public static final String DVLPG_HR = "DVLPG_HR";
	public static final String SNAPSHOT_TYPE = "SNAPSHOT_TYPE";
	public static final String ISSUE_TIME = "ISSUE_TIME";
	public static final String UNTIL_TIME = "UNTIL_TIME";

	public static final String FROM = "FROM";
	public static final String BOUNDED_BY = "BOUNDED BY";
	
	private static final String REDUCE_FLAGS = "REDUCE_FLAGS";

	// areas
	public static final String MIA = "MIA";
	public static final String DFW = "DFW";
	public static final String SFO = "SFO";
	public static final String BOS = "BOS";
	public static final String CHI = "CHI";
	public static final String SLC = "SLC";

	/**
	 * Named attributes on this object, to be used as a holder of intermediate parameters. 
	 */
	private HashMap<String, Object> attributes = new HashMap<String, Object>();

	/**
	 * public constructor
	 */
	public Gfa() {
		super();
		
		logger.trace("Gfa created");
	}

	public Gfa(IAttribute attr, ArrayList<Coordinate> points) {
		super();
		if (attr == null) {
			throw new IllegalArgumentException("null IAttribute argument");
		}
		this.update(attr);

		setLinePoints(points);
		
		logger.trace("Gfa created");
	}

	public Gfa(Coordinate[] range, Color[] colors, float lineWidth, double sizeScale,
			boolean closed, boolean filled, List<Coordinate> linePoints, Coordinate textCoordinate,
			int smoothFactor, FillPattern fillPattern, String pgenCategory, String pgenType,
			String hazard, String fcstHr, String tag, String desk, String issueType, int cycleDay, int cycleHour,
			String type, String area, String beginning, String ending, String states) {
		super(range, colors, lineWidth, sizeScale, closed, filled, linePoints, smoothFactor,
				fillPattern, pgenCategory, pgenType);
		setGfaTextCoordinate(textCoordinate);
		setGfaHazard(hazard);
		setGfaFcstHr(fcstHr);
		setGfaTag(tag);
		setGfaDesk(desk);
		setGfaIssueType(issueType);
		setGfaCycleDay(cycleDay);
		setGfaCycleHour(cycleHour);
		setGfaType(type);
		setGfaArea(area);
		setGfaBeginning(beginning);
		setGfaEnding(ending);
		setGfaStates(states);
	}
	
	/**
	 * This constructor is to be used when other parameters should be defaulted.  
	 * 
	 * @param filled
	 * @param linePoints
	 * @param hazard
	 * @param fcstHr
	 * @param tag
	 * @param desk
	 * @param issueType
	 * @param type
	 */
	public Gfa( ArrayList<Coordinate> linePoints, String hazard, 
			String fcstHr, String tag, String desk, String issueType, String type, HashMap<String, String> values) {
		super();
		// parameters
		setPoints(linePoints);
		setGfaHazard(hazard);
		setGfaFcstHr(fcstHr);
		setGfaTag(tag);
		setGfaDesk(desk);
		setGfaIssueType(issueType);
		setGfaType(type);

		// default values
		setPgenCategory("MET");
		setPgenType("GFA");
		setFillPattern(FillPattern.SOLID);
		setRange(null);
		setFilled(false);
		setColors(GfaInfo.getDefaultColors(hazard, fcstHr));
		setClosed(true);
		setLineWidth(GfaInfo.getLineWidth(fcstHr));
		setSizeScale(1.0);
		setSmoothFactor(0);
		setGfaCycleDay(PgenCycleTool.getCycleDay());
		setGfaCycleHour(PgenCycleTool.getCycleHour());
		setGfaTextCoordinate(getCentroid());
		setGfaValues(values);
	}

	@Override
	/**
	 * make a deep copy of the Gfa
	 * 
	 * Strings, Colors, Coordinates, Maps, Lists, ... should be copied instead
	 * of using the references.
	 * 
	 */
	public Gfa copy() {

		Gfa gfa = new Gfa();
        gfa.update( this );
        
		gfa.setPgenCategory( new String(this.getPgenCategory() ) );
		gfa.setPgenType( new String(this.getPgenType() ) );
		gfa.setParent(this.getParent());

		gfa.setClosed(this.isClosedLine());
		gfa.setFilled(this.isFilled());
		
		Color[] colorCopy = new Color[this.getColors().length];
		for (int i=0; i<this.getColors().length; i++) {
			colorCopy[i] = new Color(this.getColors()[i].getRed(),
					                 this.getColors()[i].getGreen(),
					                 this.getColors()[i].getBlue() );
		}
		gfa.setColors( colorCopy );
		
		gfa.setLineWidth(this.getLineWidth());
		gfa.setSizeScale(this.getSizeScale());
		gfa.setSmoothFactor(this.getSmoothFactor());
		
		gfa.setFillPattern(this.getFillPattern());
		
		ArrayList<Coordinate> ptsCopy = new ArrayList<Coordinate>();
		for (int i=0; i < this.getPoints().size(); i++) {
			ptsCopy.add(new Coordinate(this.getPoints().get(i)));
		}
		gfa.setPoints( ptsCopy );
		
		gfa.setGfaHazard( new String( nvl(this.getGfaHazard()) ) );
		gfa.setGfaTag( new String( nvl(this.getGfaTag()) ) );
		gfa.setGfaFcstHr( new String( nvl(this.getGfaFcstHr())) );
		gfa.setGfaDesk( new String( nvl(this.getGfaDesk()) ) );
		gfa.setGfaIssueType( new String( nvl(this.getGfaIssueType()) ) );
		gfa.setGfaType( new String( nvl(this.getGfaType()) ) );
		gfa.setGfaArea( new String( nvl(this.getGfaArea()) ) );
		gfa.setGfaBeginning( new String( nvl(this.getGfaBeginning()) ) );
		gfa.setGfaEnding( new String( nvl(this.getGfaEnding()) ) );
		gfa.setGfaStates( new String( nvl(this.getGfaStates()) ) );
		
		gfa.setGfaCycleDay(this.getGfaCycleDay());
		gfa.setGfaCycleHour(this.getGfaCycleHour());
		
		HashMap<String, String> gfaValuesCopy = new HashMap<String, String>();
		for ( String str : this.getGfaValues().keySet() ) {
			gfaValuesCopy.put( new String(str) , new String( nvl(this.getGfaValues().get( str ) ) ) );
		}
		gfa.setGfaValues( gfaValuesCopy );

		gfa.setGfaTextCoordinate( new Coordinate(this.getGfaTextCoordinate() ) );
		
		ArrayList<Coordinate> notToBeSnappedCopy = new ArrayList<Coordinate>();
		for ( Coordinate pt : this.getNotToBeSnapped() ) {
			notToBeSnappedCopy.add( new Coordinate( pt ) );
		}
		gfa.setNotToBeSnapped( notToBeSnappedCopy );

		boolean[] reduceFlags = this.getReduceFlags();
		boolean[] reduceFlagsCopy = new boolean[ reduceFlags.length ];
		for ( int ii = 0; ii < reduceFlags.length; ii++ ) {
			reduceFlagsCopy[ ii ] = reduceFlags[ ii ];
		}
		gfa.setReduceFlags( reduceFlagsCopy );
		
		//More AIRMET/OUTLOOK attributes
/*		gfa.setGfaValue( Gfa.GR, new String( nvl( this.getGfaValue( Gfa.GR ) ) ) );
		gfa.setGfaValue( Gfa.FREQUENCY, new String( nvl( this.getGfaValue( Gfa.FREQUENCY ) ) ) );
		gfa.setGfaValue( Gfa.CATEGORY, new String( nvl( this.getGfaValue( Gfa.CATEGORY ) ) ) );
		gfa.setGfaValue( Gfa.FZL_RANGE, new String( nvl( this.getGfaValue( Gfa.FZL_RANGE ) ) ) );
		gfa.setGfaValue( Gfa.LEVEL, new String( nvl( this.getGfaValue( Gfa.LEVEL ) ) ) );
		gfa.setGfaValue( Gfa.INTENSITY, new String( nvl( this.getGfaValue( Gfa.INTENSITY ) ) ) );
		gfa.setGfaValue( Gfa.SPEED, new String( nvl( this.getGfaValue( Gfa.SPEED ) ) ) );
		gfa.setGfaValue( Gfa.DUE_TO, new String( nvl( this.getGfaValue( Gfa.DUE_TO ) ) ) );
		gfa.setGfaValue( Gfa.LYR, new String( nvl( this.getGfaValue( Gfa.LYR ) ) ) );
		gfa.setGfaValue( Gfa.COVERAGE, new String( nvl( this.getGfaValue( Gfa.COVERAGE ) ) ) );
		gfa.setGfaValue( Gfa.TOP, new String( nvl( this.getGfaValue( Gfa.TOP ) ) ) );
		gfa.setGfaValue( Gfa.BOTTOM, new String( nvl( this.getGfaValue( Gfa.BOTTOM ) ) ) );				
		gfa.setGfaValue( Gfa.FZL_TOP_BOTTOM, new String( nvl( this.getGfaValue( Gfa.FZL_TOP_BOTTOM ) ) ) );
		gfa.setGfaValue( Gfa.CONTOUR, new String( nvl( this.getGfaValue( Gfa.CONTOUR ) ) ) );
		
		gfa.setGfaValue( Gfa.ENDG_HR, new String( nvl( this.getGfaValue( Gfa.ENDG_HR ) ) ) );
		gfa.setGfaValue( Gfa.DVLPG_HR, new String( nvl( this.getGfaValue( Gfa.DVLPG_HR ) ) ) );		
		gfa.setGfaValue( Gfa.SNAPSHOT_TYPE, new String( nvl( this.getGfaValue( Gfa.SNAPSHOT_TYPE ) ) ) );
*/		
		Calendar cal = this.getAttribute( Gfa.ISSUE_TIME, Calendar.class );
		
		if ( cal != null ) {
			Calendar  issueTime = Calendar.getInstance(); 
		    issueTime.setTimeInMillis( cal.getTimeInMillis() );		
		    gfa.addAttribute( Gfa.ISSUE_TIME, issueTime );
		}
		
		cal = this.getAttribute( Gfa.UNTIL_TIME, Calendar.class );
		if ( cal != null ) {
		    Calendar  untilTime = Calendar.getInstance(); 
		    untilTime.setTimeInMillis( cal.getTimeInMillis() );		
		    gfa.addAttribute( Gfa.UNTIL_TIME, untilTime );
		}
		
		if( "ICE".equals( this.getGfaHazard() ) ){
			gfa.setGfaValue( "Type", new String( nvl( this.getGfaValue( "Type" ) ) ) );
		}		
        
		GfaWording w = this.getAttribute( GfaRules.WORDING, GfaWording.class );		
		if ( w != null) {
			GfaWording wds = new GfaWording();           
			wds.condsContg = new String( nvl( w.getCondsContg() ) );
			wds.fromCondsDvlpg = new String( nvl( w.getFromCondsDvlpg() ) );
			wds.fromCondsEndg = new String( nvl( w.getFromCondsEndg() ) );
			wds.genOlk = new String( nvl( w.getGenOlk() ) );
			wds.otlkCondsDvlpg = new String( nvl( w.getOtlkCondsDvlpg() ) );
			wds.otlkCondsEndg = new String( nvl( w.getOtlkCondsEndg() ) );
			gfa.addAttribute( GfaRules.WORDING, wds );
		}
		
		return gfa;
	}


	@Override
	public void update(IAttribute iattr) {
		
		if ( iattr instanceof IGfa ){
			super.update(iattr);

			IGfa attr = (IGfa) iattr;
			setSizeScale(1.0);
			setGfaHazard(attr.getGfaHazard());
			setGfaTag(attr.getGfaTag());
			setGfaFcstHr(attr.getGfaFcstHr());
			setGfaDesk(attr.getGfaDesk());
			setGfaIssueType(attr.getGfaIssueType());
			setGfaType(attr.getGfaType());
			setGfaValues(attr.getGfaValues());
			setGfaArea(attr.getGfaArea());
			setGfaBeginning(attr.getGfaBeginning());
			setGfaEnding(attr.getGfaEnding());
			setGfaStates(attr.getGfaStates());
			setGfaCycleDay(attr.getGfaCycleDay());
			setGfaCycleHour(attr.getGfaCycleHour());
			setClosed(!"Open".equalsIgnoreCase(values.get(Gfa.CONTOUR)));
		}
	}

	@Override
	public String getPatternName() {
		if("FZLVL".equals(hazard)) {
			if("SFC".equals(getGfaValue(Gfa.LEVEL))){
				setSizeScale(0.3);
				return "ZIGZAG";
			} else {
				setSizeScale(1.0);
				return "LINE_DASHED_3";
			}
		}
		return "LINE_SOLID";
	}
	
	public String getSymbolType() {
		if ("TURB".equals(hazard) 
				|| "TURB-HI".equals(hazard) 
				|| "TURB-LO".equals(hazard)) {
			return "TURBULENCE_4";
		} else if ("ICE".equals(hazard)) {
			return "ICING_05";
		} else if ("SFC_WND".equals(hazard) && "30KT".equalsIgnoreCase(getGfaValue(Gfa.SPEED))){
			return "SFC_WND_30K";
		} else if ("SFC_WND".equals(hazard) && "20KT".equalsIgnoreCase(getGfaValue(Gfa.SPEED))){
			return "SFC_WND_20K";
		} else if("MT_OBSC".equals(hazard)){
			return "MT_OBSC";
		}
		// do not display any symbol
		return null;
	}

	public String[] getString() {
		ArrayList<String> list = new ArrayList<String>();
		if (!"NRML".equals(issueType)) {
			list.add(issueType);
		}
		String f = "";
		if(isSnapshot()){
			try{
				Calendar cal = Calendar.getInstance();
				cal.set(Calendar.DAY_OF_MONTH, cycleDay);
				cal.set(Calendar.HOUR_OF_DAY, cycleHour);
				cal.set(Calendar.MINUTE, 0);
				int h;
				if(fcstHr.contains(":")) {
					String[] s = fcstHr.split(":");
					h = Integer.parseInt(s[0].trim());
					int m = Integer.parseInt(s[1].trim());
					cal.add(Calendar.MINUTE, m);
				} else {
					h = Integer.parseInt(fcstHr);
				}
				cal.add(Calendar.HOUR, h);
				h = cal.get(Calendar.HOUR_OF_DAY);
				String value = h < 10 ? "0" + h : "" + h;
				int m = cal.get(Calendar.MINUTE);
				f = fcstHr + " " + value + ":" + (m < 10 ? "0" + m : "" + m);
			} catch(Exception ignore){
				f = fcstHr; 
			}
		} else {
			f = fcstHr; 
		}
		f += " " + getGfaTag() + desk;
		list.add(f);
		if("CLD".equals(hazard) && getGfaValue(Gfa.COVERAGE) != null && !getGfaValue(Gfa.COVERAGE).isEmpty()) {
			list.add(getGfaValue(Gfa.COVERAGE));
		}
		
		if("MTW".equals(hazard)) {
			list.add(getGfaValue(Gfa.INTENSITY));
		} else if("TS".equals(hazard)) {
			if(getGfaValue(Gfa.CATEGORY) != null && !getGfaValue(Gfa.CATEGORY).isEmpty()) list.add(getGfaValue(Gfa.CATEGORY));
			if(getGfaValue(Gfa.FREQUENCY) != null && !getGfaValue(Gfa.FREQUENCY).isEmpty()) list.add(getGfaValue(Gfa.FREQUENCY));
		}

		String typeToDisplay = hazard;
		if(type != null && !type.isEmpty()) typeToDisplay += " " + type;
		if("TS".equals(hazard) && "true".equals(getGfaValue("GR"))) typeToDisplay += " " + "GR";
		for(Node n: getDisplayTextNodes()){
			typeToDisplay = typeToDisplay.replaceAll(n.valueOf("@originalText"), 
					n.valueOf("@displayAs").replace(",,", "\n"));
		}

		if("FZLVL".equals(hazard)) {
			typeToDisplay = typeToDisplay.replaceAll("FZLVL", "0°:" + getGfaValue(Gfa.LEVEL));
		}
		
		for (String s: justify(f.length(), typeToDisplay)){
			list.add(s.trim().replace("ICONHERE", ""));
		}
		
		if("CLD".equals(hazard)) {
			list.add(getGfaValue(Gfa.BOTTOM));
		} else if ("ICE".equals(hazard) 
				|| "TURB".equals(hazard) 
				|| "TURB-HI".equals(hazard) 
				|| "TURB-LO".equals(hazard) 
				|| "MTW".equals(hazard) 
				|| "M_FZLVL".equals(hazard)
				|| "TS".equals(hazard) 
				|| "CLD_TOPS".equals(hazard)){
			if(getGfaTop() != null && !getGfaTop().isEmpty()) list.add(getGfaTop());
			if(getGfaBottom() != null && !getGfaBottom().isEmpty()) {
				String line = getGfaBottom();
				if("FZL".equals(line) && getGfaValue(Gfa.FZL_TOP_BOTTOM) != null) {
					line = getGfaValue(Gfa.FZL_TOP_BOTTOM);
				}
				list.add(line);
			}
		} else if ("SFC_WND".equals(hazard)){
			list.add("");
		}
		String[] a = new String[list.size()];
		return list.toArray(a);
	}

	@SuppressWarnings("unchecked")
	private List<Node> getDisplayTextNodes() {
		String xPath = "/root/displayText/value[@hazard='" + hazard + "']|/root/displayText/value[@hazard='']";
		List<Node> displayTextNodes = GfaInfo.getDocument().selectNodes(xPath);
		return displayTextNodes;
	}

	/**
	 * Splits the string using space and slash as delimiters with the specified max line width. 
	 * 
	 * @param width
	 * @param st
	 * @return
	 */
	public static String[] justify(int width, String str) {
		width = (width < MIN_WIDTH_GFA_TEXT) ? MIN_WIDTH_GFA_TEXT : width; 
		StringBuffer buf = new StringBuffer(str);
		int lastDelimeter = -1; 
		int lineStart = 0;
		int i = 0;

		while (i < buf.length()) {
			if (buf.charAt(i) == ' ' || buf.charAt(i) == '/') lastDelimeter = i + 1;
			if (buf.charAt(i) == '\n') {
				lastDelimeter = -1;
				lineStart = i + 1;
			}
			if (i > lineStart + width - 1) {
				if (lastDelimeter != -1) {
					buf.insert(lastDelimeter, '\n');
					lineStart = lastDelimeter + 1;
					lastDelimeter = -1;
				} else {
					buf.insert(i, '\n');
					lineStart = i + 1;
				}
			}
			i++;
		}
		return buf.toString().split("\n");
	}

	public String getGfaHazard() {
		return hazard;
	}

	public void setGfaHazard(String hazard) {
		this.hazard = hazard;
	}

	public String getGfaFcstHr() {
		return fcstHr;
	}

	@Override
	public String getForecastHours(){
		return getGfaFcstHr();
	}
	
	public void setGfaFcstHr(String fcstHr) {
		this.fcstHr = (fcstHr == null || fcstHr.isEmpty()) ? "0": fcstHr;
	}

	public String getGfaTag() {
		return tag;
	}

	public void setGfaTag(String tag) {
		if(tag != null) tag = tag.replace("*", "");
		this.tag = tag;
	}

	public String getGfaDesk() {
		return desk;
	}

	public void setGfaDesk(String desk) {
		this.desk = desk;
	}

	public String getGfaIssueType() {
		return issueType;
	}

	public void setGfaIssueType(String issueType) {
		this.issueType = issueType;
	}

	public Coordinate getGfaTextCoordinate() {
		return gfaTextCoordinate;
	}

	public void setGfaTextCoordinate(Coordinate gfaTextCoordinate) {
		this.gfaTextCoordinate = gfaTextCoordinate;
	}

	public int getGfaCycleDay() {
		return cycleDay;
	}

	public int getGfaCycleHour() {
		return cycleHour;
	}
	
	public void setGfaCycleDay(int day) {
		this.cycleDay = day;
	}
	
	public void setGfaCycleHour(int hour) {
		this.cycleHour = hour;
	}

	public String getGfaType() {
		return type;
	}

	public void setGfaType(String type) {
		this.type = type;
	}
	
	public String getGfaArea() {
		return area;
	}

	public void setGfaArea(String area) {
		this.area = area;
	}
	
	public String getGfaBeginning() {
		return beginning;
	}

	public void setGfaBeginning(String beginning) {
		this.beginning = beginning;
	}

	public String getGfaEnding() {
		return ending;
	}

	public void setGfaEnding(String ending) {
		this.ending = ending;
	}
	
	public String getGfaStates() {
		return states;
	}

	public void setGfaStates(String states) {
		this.states = states;
	}

	public String getGfaValue(String key) {
		return values.get(key);
	}
	
	public String getGfaTop() {
		return getGfaValue(Gfa.TOP);
	}

	public String getGfaBottom() {
		return getGfaValue(Gfa.BOTTOM);
	}

	public void setGfaValue(String key, String value) {
		if (value == null) {
			// do nothing
			return;
		}
		values.put(key, value);
	}
	
	public HashMap<String, String> getGfaValues(){
		HashMap<String, String> copy = new HashMap<String, String> ();
		if(values == null) return copy;
		
		for(String key: values.keySet()){
			copy.put(key, values.get(key));
		}
		return copy;
	}
	
	public void setGfaValues(HashMap<String, String> values) {
		this.values = values; 
	}
	
	public String valuesToString(){
		StringBuilder sb = new StringBuilder(200);
		if (values == null || values.isEmpty()) return null;
		for (String key: values.keySet()){
			sb.append("(").append(key).append(",");
			sb.append(values.get(key).toString());
			sb.append(")");
		}
		return sb.toString();
	}

	public boolean isSnapshot() {
		return fcstHr != null && fcstHr.indexOf("-") == -1;
	}
	
	public boolean isAirmet() {
		if(isSnapshot()) return false;
		String[] s = nvl(getGfaFcstHr()).split("-");
		int[] hm = getHourMinInt(s[1]);
		return hm[0] <= 6;
	}

	public boolean isOutlook() {
		if(isSnapshot()) return false;
		String[] s = nvl(getGfaFcstHr()).split("-");
		int[] hm0 = getHourMinInt(s[0]);
		int[] hm1 = getHourMinInt(s[1]);
		return hm0[0] >= 6 && hm1[0]>6;
	}

	public boolean isFormat() {
		boolean isFormat = GfaInfo.isFormat(getGfaHazard());
		if("SFC_WND".equals(getGfaHazard())) 
			// smear only 30KT
			isFormat &= "30KT".equalsIgnoreCase(getGfaValue(Gfa.SPEED));
		return isFormat;
	}

	/**
	 * @return the string
	 */
	public String toString() {
		StringBuilder	result = new StringBuilder( getClass().getSimpleName());

        result.append("Category:\t" + pgenCategory + "\n");        
        result.append("Type:\t" + pgenType + "\n");        
        result.append("hazard:\t" + hazard + "\t\n"); 
        result.append("fcstHr:\t" + fcstHr + "\t\n"); 
        result.append("tag:\t" + tag + "\t\n"); 
        result.append("desk:\t" + desk + "\t\n"); 
        result.append("issueType:\t" + issueType + "\t\n"); 
        result.append("type:\t" + type + "\t\n");

        result.append("Color:\t" + colors[0] + "\n");
        result.append("LineWidth:\t" + lineWidth + "\n");
        result.append("SizeScale:\t" + sizeScale + "\n");
        result.append("Closed:\t" + closed + "\n");
        result.append("Filled:\t" + filled + "\n");
        result.append("SmoothFactor:\t" + smoothFactor + "\n");
        result.append("FillPattern:\t" + fillPattern + "\n");
        
        result.append("gfaTextCoordinate:\t" + "\t\n");
        
        result.append("Non-null attributes:\t" + "\t\n"); 
        for(String key: attributes.keySet()){
        	Object o = attributes.get(key);
        	if(o != null) {
        		result.append("\t"+ key);
        		if(!(o instanceof Gfa) && !(o instanceof Collection)) {
        			result.append("\t" + o.toString());
        		}
        		result.append("\n");
        	}
        }
    	
        result.append("Location:\t" + "\t\n"); 
        for ( Coordinate point : linePoints )	{
        	result.append("\t" + point.x + "\t" + point.y + "\n");
        }
     	 	       		
		return result.toString();
	}

	/**
	 * Add a point which should be ignored when snapping.
	 * 
	 * @param coordinate
	 */
	public void addNotToBeSnapped(Coordinate c) {
		for(Coordinate p: getPoints()) {
			if (GfaRules.compareCoordinates(c, p)) {
				notToBeSnapped.add(c);
				return;
			}
		}
		// else ignore
	}
	
	/**
	 * Add a list of points which should be ignored when snapping.
	 * 
	 * @param list
	 */
	public void addNotToBeSnapped(Coordinate[] array) {
		if (array == null) return;
		for(Coordinate c: array) {
			addNotToBeSnapped(c);
		}
	}


	/**
	 * Clear the list of not_to_be_snapped points.
	 */
	public void clearNotToBeSnapped() {
		notToBeSnapped.clear();
	}
	
	/**
	 * Getter notToBeSnapped.
	 * 
	 * @return
	 */
	public ArrayList<Coordinate> getNotToBeSnapped() {
		return notToBeSnapped;
	}

	/**
	 * Setter for notToBeSnapped
	 * 
	 * @param notToBeSnapped
	 */
	public void setNotToBeSnapped(ArrayList<Coordinate> notToBeSnapped) {
		this.notToBeSnapped = notToBeSnapped;
	}

	public void snap(){
		if(isSnapshot()) return; // snapshots are not to be snapped 
		ArrayList<Coordinate> points = getPoints();
		for(Coordinate p: points) {
			if(canBeSnapped(p)) {
				List<Coordinate> tempList = new ArrayList<Coordinate>();
				tempList.add(p);
				tempList = SnapUtil.getSnapWithStation(tempList,SnapUtil.VOR_STATION_LIST,10,16, false);
				Coordinate c = tempList.get(0);
				p.setCoordinate(c); // update the coordinate
			}
		}
	}

	public boolean canBeSnapped(Coordinate point) {
		for(Coordinate no: notToBeSnapped) {
			if (GfaRules.compareCoordinates(no, point)) {
				// close within PRECISION, not to be snapped
				return false;
			}
		}
		return true;
	}

	public Object getAttribute(String attrName){
		return getAttribute(attrName, Object.class);
	}
	
	@SuppressWarnings("unchecked")
	public<T> T getAttribute(String attrName, Class T){
		return (T)attributes.get(attrName);
	}
	
	public void addAttribute(String attrName, Object value){
		attributes.put(attrName, value);
	}
	
	public void removeAttribute(String attrName){
		attributes.remove(attrName);
	}

	public void clearAttributes() {
		attributes.clear();
	}
	
	
	/**
	 * Get the reduce-able flags for each point. 
	 * By default, all points are reduce-able.
	 * @return flags
	 */
	public boolean[] getReduceFlags() {
		
		boolean[] flags = (boolean[])attributes.get( REDUCE_FLAGS );
		
		if ( flags == null ) {
			flags = new boolean[ this.getLinePoints().length ];
            for ( int ii = 0; ii < this.getLinePoints().length; ii++ ) {
            	flags[ ii ] = true;
            }
		}
		
		return flags;
		
	}
	
	
	/**
	 * Set the reduce-able flags for each point. 
	 * @param reduceFlgs
	 */
	public void setReduceFlags( boolean[] reduceFlgs ) {
		attributes.put( REDUCE_FLAGS,  reduceFlgs );
	}

	
	
	/**
	 * Converts "5:45" type of strings into an int array {5, 45}, "9" into {9, 0}, etc.
	 * 
	 * @param hourMinStr
	 * @return int array length 2
	 */
	public static int[] getHourMinInt(String hourMinStr){
		if (nvl(hourMinStr).isEmpty()) {
			return null;
		}
		String [] s = hourMinStr.split(":");
		int[] hm = new int[2];
		hm[0] = Integer.parseInt(s[0]);
		if(s.length > 1) {
			hm[1] = Integer.parseInt(s[1]);
		}
		return hm;
	}

	public static String nvl(String value) {
		return value == null ? "" : value;
	}

	@Override
	public int compareTo(Gfa g1) {
		if(this == g1) return 0;
		else return 1;
	}	

	/**
	 * Convenient function to convert GFA points into a JTS Polygon
	 * 
	 * @return Polygon
	 */
	public Polygon toPolygon() {		
		return GfaClip.getInstance().gfaToPolygon( this );
	}
	
}
