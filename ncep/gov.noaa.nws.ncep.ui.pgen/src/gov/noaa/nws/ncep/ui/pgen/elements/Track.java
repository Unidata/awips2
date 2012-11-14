/*
 * Track
 * 
 * Date created: 12 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;
import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.ui.pgen.display.ITrack;
import gov.noaa.nws.ncep.ui.pgen.display.TrackPoint;

import java.awt.Color;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class to represent a track element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09					M. Gao   	Initial Creation.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 08/11 		?			B. Yin		Fixed the no half-hour label bug.
 * 02/12        TTR456      Q.Zhou      Added speed knot, mph. Added combos and roundTo indices for speed & dir .
 *                                       Modified setSpeed
 * 06/12        #777        Q.Zhou      Modified DEFAULT_EXTRA_POINT_NUMBER.                                      
 * </pre>
 * 
 * @author	M. Gao
 * @version	0.0.1
 */
@ElementOperations ( {Operation.COPY_MOVE, Operation.EXTRAPOLATE} )
public class Track extends Line implements ITrack {
//public class Track extends MultiPointElement implements ITrack {

//	private final static org.apache.log4j.Logger log = 
//		org.apache.log4j.Logger.getLogger(Track.class);

	
	private final static float DEFAULT_FONT_SIZE = 14.0f; 
	private final static float DEFAULT_LINE_WIDTH = 0.1f; 
	private final static int DEFAULT_EXTRA_POINT_NUMBER = 5; 
//	private final static int DEFAULT_INTERVAL_HOUR = 1; 
	private final static String INTERVAL_TIME_FORMAT_PATTERN = "HH:mm"; 
	public void setExtrapMarker(String extrapMarker) {
		this.extrapMarker = extrapMarker;
	}

	public void setExtrapLinePattern(String extrapLinePattern) {
		this.extrapLinePattern = extrapLinePattern;
	}

	private final static String INITIAL_MARKER = "FILLED_DIAMOND"; 
	private final static String EXTRAP_MARKER = "FILLED_TRIANGLE"; 
	private final static String INITIAL_LINE_PATTERN = "LINE_SOLID"; 
	private final static String EXTRAP_LINE_PATTERN = "LINE_SOLID"; 
	/*
	 * This value of DEFAULT_LINE_PATTERN is a temporary solution
	 * Eventually a decision needs to be done for what a value 
	 * should be used
	 */
	public final static String TRACK_PGEN_CATEGORY = "Track"; 
	public final static String TRACK_INFO_DLG_CATEGORY_NAME = "TRACK_EXTRA_POINTS_INFO"; 
	public final static String TRACK_PGEN_TYPE = "STORM_TRACK"; 
	
	private Color initialColor; 
	private Color extrapColor; 
	private String initialMarker; 
	private String initialLinePattern; 
	private String extrapMarker; 
	private String extrapLinePattern; 

	private TrackPoint[] initTrackPoints; 
	private TrackPoint[] extrapPoints; 
	private String fontName; 
	private float fontSize; 
	private FontStyle fontStyle; 

	private float lineWidth; 
	private int extraDrawingPointNumber; 

	private boolean setTimeButtonSelected; 

	private Calendar firstTimeCalendar; 
	private Calendar secondTimeCalendar; 
	private String intervalTimeString; 
	private int intervalComboSelectedIndex; 

	private int fontSizeComboSelectedIndex; 
	private int fontNameComboSelectedIndex; 
	private int fontStyleComboSelectedIndex; 
	
	private int elapsedHourForExtraPoint;
	private int elapsedMinuteForExtraPoint; 
	
	private ExtraPointTimeDisplayOption extraPointTimeDisplayOption; 
	private String skipFactorTextString; 
	private boolean[] extraPointTimeTextDisplayIndicator; 
	
	// direction, speed and unit are not saved to a file	
	private double directionForExtraPoints; 
	private boolean roundDirBtnSelected;
	private int roundDirComboSelectedIndex;
	
	private double speed;   // speed in meter/millisecond 
	private double speedInKnotOverHour; 
	private double speedInKilometerOverHour;
	private double speedInMileOverHour;
	private boolean roundBtnSelected;
	private int unitComboSelectedIndex;
	private int roundComboSelectedIndex;
	
	public Track() {}
	
	public Track(ArrayList<Coordinate> _locations, Calendar _firstTimeCalendar, 
			Calendar _secondTimeCalendar) {
		initializeInitTrackPoints(_locations, 
				_firstTimeCalendar, _secondTimeCalendar); 
	}

	
	public void initializeInitTrackPoints(ArrayList<Coordinate> locations, 
			Calendar firstTimeCalendar, Calendar secondTimeCalendar) {
		initializeInitTrackPoints(locations); 
		initializeInitFirstTimeCalendar(firstTimeCalendar); 
		initializeInitSecondTimeCalendar(secondTimeCalendar); 
	}
	
	public void initializeTrackByTrackAttrDlgAndLocationList(ITrack trackAttrDlgObject, ArrayList<Coordinate> locations) {
		initializeInitTrackPoints(locations); 
		initializeInitFirstTimeCalendar(trackAttrDlgObject.getFirstTimeCalendar()); 
		initializeInitSecondTimeCalendar(trackAttrDlgObject.getSecondTimeCalendar()); 
		
		this.setInitialMarker(""); 
		this.setSetTimeButtonSelected(trackAttrDlgObject.isSetTimeButtonSelected()); 
		this.setExtraDrawingPointNumber(trackAttrDlgObject.getExtraDrawingPointNumber()); 
		
		this.setIntervalTimeString(getIntervalTimeTextStringValue(trackAttrDlgObject)); 
		
		this.setFontName(trackAttrDlgObject.getFontName()); 
		this.setFontStyle(trackAttrDlgObject.getStyle()); 
		this.setFontSize(trackAttrDlgObject.getFontSize()); 
		this.setInitialColor(trackAttrDlgObject.getInitialColor()); 
		this.setExtrapColor(trackAttrDlgObject.getExtrapColor()); 
		this.setExtraPointTimeDisplayOption(trackAttrDlgObject.getExtraPointTimeDisplayOption()); 
		this.setSkipFactorTextString(trackAttrDlgObject.getSkipFactorText()); 
		this.setFontNameComboSelectedIndex(trackAttrDlgObject.getFontNameComboSelectedIndex()); 
		this.setFontSizeComboSelectedIndex(trackAttrDlgObject.getFontSizeComboSelectedIndex()); 
		this.setFontStyleComboSelectedIndex(trackAttrDlgObject.getFontStyleComboSelectedIndex()); 
		this.setUnitComboSelectedIndex(trackAttrDlgObject.getUnitComboSelectedIndex()); 
		this.setRoundComboSelectedIndex(trackAttrDlgObject.getRoundComboSelectedIndex()); 
		if (trackAttrDlgObject.getRoundComboSelectedIndex() >0)
			this.setRoundBtnSelected(true);
		else 
			this.setRoundBtnSelected(false);
		
		this.setRoundDirComboSelectedIndex(trackAttrDlgObject.getRoundDirComboSelectedIndex()); 
		if (trackAttrDlgObject.getRoundDirComboSelectedIndex() >0)
			this.setRoundDirBtnSelected(true);
		else 
			this.setRoundDirBtnSelected(false);
		
		//This method can be called only after all of the above settings are completed!
		this.calculateExtrapTrackPoints(); 
		/**
		 * For now, the type is the name of a LinePattern defined in
		 *       "gov.noaa.nws.ncep.ui.pgen.display.LinePatternList"	
		 * However, there is no type has been decided for Track element 
		 * yet. Thus, it is now hard coded.        
		 */
//		this.setType("Solid Lines"); 
		this.setPgenCategory(Track.TRACK_PGEN_CATEGORY); 
		this.setPgenType(Track.TRACK_PGEN_TYPE); 
	}
	
	public void initializeInitTrackPoints(ArrayList<Coordinate> locations) {
		TrackPoint[] initTrackPointArray = new TrackPoint[locations.size()]; 
		int arrayIndex = 0; 
//		log.info("inside Track.initializeInitTrackPoints, points Number="+locations.size()); 
		for(Coordinate currentCoordinate : locations) {
			TrackPoint eachTrackPoint = new TrackPoint(currentCoordinate, null); 
			initTrackPointArray[arrayIndex++] = eachTrackPoint; 
		}
		setInitTrackPoints(initTrackPointArray); 
	}
	
	public void initializeInitFirstTimeCalendar(Calendar firstCalendar) {
		setFirstTimeCalendar(firstCalendar); 
		if(getInitTrackPoints() != null) {
			int initTrackPointLength = getInitTrackPoints().length; 
			if(initTrackPointLength >= 2) {
				getInitTrackPoints()[initTrackPointLength - 2].setTime(firstCalendar); 
			}
		}
	}

	public void initializeInitSecondTimeCalendar(Calendar secondCalendar) {
		setSecondTimeCalendar(secondCalendar); 
		if(getInitTrackPoints() != null) {
			int initTrackPointLength = getInitTrackPoints().length; 
			if(initTrackPointLength >= 2) {
				getInitTrackPoints()[initTrackPointLength - 1].setTime(secondCalendar); 
			}
		}
	}

	public void calculateExtrapTrackPoints() {
		extrapPoints = calculateExtrapTrackPoints(getInitialPoints(), getFirstTimeCalendar(), 
				getSecondTimeCalendar(), getExtraDrawingPointNumber(), 
				getElapsedHourForExtraPoint(), getElapsedMinuteForExtraPoint(), 
				getExtraPointTimeDisplayOption(), getSkipFactorTextString()); 
		
		/*
		 * it is important to override the variable "ArrayList<Coordinate> linePoints" with 
		 * initTrackPoints and extrapPoints arrays
		 */
		setLinePointsValue(getInitialPoints(), getExtrapPoints()); 
		
		/*
		 * a linePattern needs to be set for PgenSelectingTool to pick up
		 * the correct dialogue window to popup 
		 */
		setPgenCategory(Track.TRACK_PGEN_CATEGORY); 
		setPgenType(Track.TRACK_PGEN_TYPE); 
	}
	
	public void setLinePointsValue(TrackPoint[] initTrackPoints, TrackPoint[] extrapPoints) {
		int listSize = 1; 
		if(initTrackPoints != null)
			listSize += initTrackPoints.length; 
		if(extrapPoints != null)
			listSize += extrapPoints.length; 
		ArrayList<Coordinate> coordinatePointList = new ArrayList<Coordinate>(listSize);
		addArrayToArrayList(coordinatePointList, initTrackPoints); 
		addArrayToArrayList(coordinatePointList, extrapPoints); 
		
		/*
		 * Now use the inherited method setLinePoints to set the linPoints value
		 */
		setLinePoints(coordinatePointList); 
	}

	/**
	 * Creates a copy of this object.  This is a deep copy and new objects are
	 * created so that we are not just copying references of objects
	 */
//	@Override
//	public DrawableElement copy() {
	public Track copy() {

		/*
		 * create a new Track object and initially set its attributes to this one's
		 */
		Track newTrack = new Track();
		Calendar newFirstTimeCalendar = Calendar.getInstance(); 
		newFirstTimeCalendar.setTimeInMillis(getFirstTimeCalendar().getTimeInMillis()); 
		newTrack.setFirstTimeCalendar(newFirstTimeCalendar); 
//		newTrack.setFirstTimeCalendar(getFirstTimeCalendar()); 
		
		Calendar newSecondTimeCalendar = Calendar.getInstance(); 
		newSecondTimeCalendar.setTimeInMillis(getSecondTimeCalendar().getTimeInMillis()); 
		newTrack.setSecondTimeCalendar(newSecondTimeCalendar); 
//		newTrack.setSecondTimeCalendar(getSecondTimeCalendar()); 

		TrackPoint[] newInitTrackPoints = new TrackPoint[getInitTrackPoints().length]; 
//		log.info("######, before enter the loop for TrackPoint.clone, newInitTrackPoints.length="+newInitTrackPoints.length); 
		for(int i=0; i<newInitTrackPoints.length; i++) {
//			log.info("######, inside enter the loop for TrackPoint.clone, index of the TrackPoint.clone="+i); 
//			if(getInitTrackPoints()[i].getLocation() == null)
//				log.info("#####, getInitTrackPoints()["+i+"].getLocation() is NULL"); 
//			else 
//				log.info("#####, getInitTrackPoints()["+i+"].getLocation() is NOTNOTNOT NULL"); 
				
			newInitTrackPoints[i] = TrackPoint.clone(getInitTrackPoints()[i].getLocation(), getInitTrackPoints()[i].getTime()); 
		}
		newTrack.setInitTrackPoints(newInitTrackPoints); 
//		newTrack.setInitTrackPoints(getInitTrackPoints()); 
		
		// not sure if extrap points shoud be simply copied over or calculated by initial points 
//		TrackPoint[] newExtrapTrackPoints = new TrackPoint[getExtrapPoints().length]; 
//		for(int i=0; i<newExtrapTrackPoints.length; i++) {
//			if(getExtrapPoints()[i].getLocation() == null || getExtrapPoints()[i].getTime() == null)
//				throw new PGenRuntimeException("Method: Track.copy(), both location and time can not be NULL for any of extrap points"); 
//			newExtrapTrackPoints[i] = TrackPoint.clone(getExtrapPoints()[i].getLocation(), getExtrapPoints()[i].getTime()); 
//		}
		
		newTrack.setFontStyle(getFontStyle()); 
		newTrack.setFontSize(getFontSize()); 
		newTrack.setFontName(new String(getFontName())); 
		newTrack.setExtraPointTimeDisplayOption(getExtraPointTimeDisplayOption()); 
		
		boolean[] newExtraPointTimeTextDisplayIndicator = new boolean[getExtraPointTimeTextDisplayIndicator().length]; 
		for(int i=0; i<getExtraPointTimeTextDisplayIndicator().length; i++) {
			newExtraPointTimeTextDisplayIndicator[i] = getExtraPointTimeTextDisplayIndicator()[i]; 
		}
		newTrack.setExtraPointTimeTextDisplayIndicator(newExtraPointTimeTextDisplayIndicator); 
//		newTrack.setExtraPointTimeTextDisplayIndicator(getExtraPointTimeTextDisplayIndicator()); 
		
		newTrack.setSkipFactorTextString(new String(getSkipFactorTextString())); 
		newTrack.setInitialColor(new Color(getInitialColor().getRed(), getInitialColor().getGreen(), 
				getInitialColor().getBlue())); 
		newTrack.setExtrapColor(new Color(getExtrapColor().getRed(), getExtrapColor().getGreen(), 
				getExtrapColor().getBlue())); 
		
		newTrack.setIntervalTimeString(new String(getIntervalTimeString())); 
		
		/*
		 * Under some scenarios, e.g. marshall/unmarshall procees, the ExtraDrawingPointNumber 
		 * is not serialized. Thus, first try to use the length of ExtrapPoints array
		 */
		if(getExtrapPoints() != null)
			newTrack.setExtraDrawingPointNumber(getExtrapPoints().length); 
		else
			newTrack.setExtraDrawingPointNumber(getExtraDrawingPointNumber()); 
//		newTrack.setExtraDrawingPointNumberText(getExtraDrawingPointNumberText()); 
		newTrack.setExtrapLinePattern(new String(getExtrapLinePattern())); 
		newTrack.setExtrapMarker(new String(getExtrapMarker())); 
//		log.info("#######, inside track.copy(), getInitialLinePattern()="+getInitialLinePattern()); 
		newTrack.setInitialLinePattern(new String(getInitialLinePattern())); 
		newTrack.setInitialMarker(new String(getInitialMarker())); 
//		newTrack.setType("Solid Lines"); 
		if(getPgenCategory() != null)
			newTrack.setPgenCategory(new String(getPgenCategory())); 
		if(getPgenType() != null)
			newTrack.setPgenType(new String(getPgenType())); 
		
		newTrack.calculateExtrapTrackPoints(); 
		
		/*
		 * Now copy some important attributes that make a Line or multi-points element can be displayed 
		 */
		newTrack.setClosed(isClosedLine() );
		newTrack.setFilled(isFilled() );
		/*
		 * colors value in Track will only be used in dragging initial points. Thus, 
		 * here the color of the initial points is used 
		 */
		Color[] initColors = new Color[1]; 
		initColors[0] = getInitialColor(); 
		newTrack.setColors(initColors);
		newTrack.setLineWidth(getLineWidth());	    
		newTrack.setSizeScale(getSizeScale());
//		newTrack.setSmoothFactor(getSmoothFactor());       
		newTrack.setSmoothFactor(2);       
		newTrack.setFillPattern(getFillPattern());	    

		newTrack.setParent(this.getParent());
		
//		log.info("@@@###$$$%%%!!!, the last line of track.copy()"); 
		return newTrack; 
	}
	
	public void update(ITrack trackAttrDlg) {
//		log.info("############, track.update(...) is executed!!!!"); 
		initializeInitFirstTimeCalendar(trackAttrDlg.getFirstTimeCalendar()); 
		initializeInitSecondTimeCalendar(trackAttrDlg.getSecondTimeCalendar()); 
		
		setInitialMarker(INITIAL_MARKER); 
		setSetTimeButtonSelected(trackAttrDlg.isSetTimeButtonSelected()); 
		setExtraDrawingPointNumber(trackAttrDlg.getExtraDrawingPointNumber()); 
		setIntervalTimeString(trackAttrDlg.getIntervalTimeString()); 
		setFontName(trackAttrDlg.getFontName()); 
		setFontStyle(trackAttrDlg.getStyle()); 
		setFontSize(trackAttrDlg.getFontSize()); 
		setInitialColor(trackAttrDlg.getInitialColor()); 
		setExtrapColor(trackAttrDlg.getExtrapColor()); 
		setExtraPointTimeDisplayOption(trackAttrDlg.getExtraPointTimeDisplayOption()); 
		setSkipFactorTextString(trackAttrDlg.getSkipFactorText()); 
		setFontNameComboSelectedIndex(trackAttrDlg.getFontNameComboSelectedIndex()); 
		setFontSizeComboSelectedIndex(trackAttrDlg.getFontSizeComboSelectedIndex()); 
		setFontStyleComboSelectedIndex(trackAttrDlg.getFontStyleComboSelectedIndex()); 
		setUnitComboSelectedIndex(trackAttrDlg.getUnitComboSelectedIndex()); 
		setRoundComboSelectedIndex(trackAttrDlg.getRoundComboSelectedIndex());
		if (trackAttrDlg.getRoundComboSelectedIndex() >0)
			this.setRoundBtnSelected(true);
		else 
			this.setRoundBtnSelected(false);
		
		setRoundDirComboSelectedIndex(trackAttrDlg.getRoundDirComboSelectedIndex());
		if (trackAttrDlg.getRoundDirComboSelectedIndex() >0)
			this.setRoundDirBtnSelected(true);
		else 
			this.setRoundDirBtnSelected(false);
		
		//This method can be called only after all of the above settings are completed!
		calculateExtrapTrackPoints(); 
		/**
		 * For now, the type is the name of a LinePattern defined in
		 *       "gov.noaa.nws.ncep.ui.pgen.display.LinePatternList"	
		 * However, there is no type has been decided for Track element 
		 * yet. Thus, it is now hard coded.        
		 */
//		setType("Solid Lines"); 
		setPgenType(Track.TRACK_PGEN_TYPE); 
		setPgenCategory(Track.TRACK_PGEN_CATEGORY); 
	}
	
	public void addPoint(int index, Coordinate point) {
		ArrayList<Coordinate> allLinePoints = getPoints(); 
		if(allLinePoints == null)
			allLinePoints = new ArrayList<Coordinate>(); 
		
		if(allLinePoints.size() <= index)
			allLinePoints.add(point); 
		else 
			allLinePoints.add(index, point);
		
		int initialPointSize= getInitialPoints().length; 
		int extrapPointSize = getExtrapPoints().length; 
		
		if(index < initialPointSize) {
			TrackPoint modifiedTrackPoint = getInitialPoints()[index]; 
			modifiedTrackPoint.setLocation(point); 
		} else {
			int extrapindex = index - initialPointSize; 
			if(extrapindex >= 0 && extrapindex < extrapPointSize) {
				TrackPoint modifiedTrackPoint = getExtrapPoints()[extrapindex];
				modifiedTrackPoint.setLocation(point); 
			}
		}
//		linePoints.add(index, point);
	}
	
	public void removePoint(int index ) {
		ArrayList<Coordinate> allLinePoints = getPoints(); 
		if(allLinePoints == null)
			return; 
//		if(allLinePoints.size() > index && getInitialPoints().length > index) {
		if(allLinePoints.size() > index) {
			allLinePoints.remove(index);
		}
		
//		linePoints.remove( index );
		
	}

	public void setPoints( ArrayList<Coordinate> pts ){
		if(pts == null)
			return; 
		/*
		 * call MultiPointElement.setPoint(...)
		 */
		super.setPoints(pts); 
		/*
		 * Now initialize initPoint and ExtrapPoint arrays 
		 */
		int allPointsSize = pts.size(); 
		
		int initTrackPointSize = 0; 
		if(getInitTrackPoints() != null)
			initTrackPointSize = getInitTrackPoints().length; 
		int index = 0; 
		
		/*
		 * reset coordinate values for initial track points
		 */
		while(index < initTrackPointSize && index < allPointsSize) {
			TrackPoint currentTrackPoint = getInitTrackPoints()[index]; 
			if(currentTrackPoint != null)
				currentTrackPoint.setLocation(pts.get(index)); 
			index++; 
		}
		
		
		
		
		int extrapTrackPointSize = 0; 
		if(getExtrapPoints() != null)
			extrapTrackPointSize = getExtrapPoints().length; 
		int extrapIndex = 0; 
		while(extrapIndex < extrapTrackPointSize && 
				index < allPointsSize) {
			TrackPoint currentTrackPoint = getExtrapPoints()[extrapIndex]; 
			if(currentTrackPoint != null)
				currentTrackPoint.setLocation(pts.get(index)); 
			index++; 
			extrapIndex++; 
		}
		
		
//		this.linePoints = pts;
		
	}

	/*
	 * All private help methods start here
	 */
	private String getIntervalTimeTextStringValue(ITrack trackAttrDlg) {
		
		return trackAttrDlg.getIntervalTimeString(); 
	}
	
	private void addArrayToArrayList(ArrayList<Coordinate> coordinatePointList, TrackPoint[] trackPoints) {
		if(trackPoints == null)
			return; 
		for(TrackPoint trackPoint : trackPoints) {
			coordinatePointList.add(trackPoint.getLocation()); 
		}
	}

	private TrackPoint[] calculateExtrapTrackPoints(TrackPoint[] initialPoints, 
			Calendar initPointBeforeLastInitPointTimeCal, 
			Calendar lastInitPointTimeCal, 
			int extraDrawingPointNumber, 
			int elapsedHourForExtraPointValue, 
			int elapsedMinuteForExtraPointValue, 
			ExtraPointTimeDisplayOption extraPointTimeDisplayOption, 
			String skipFactorTextString) {
		if(initialPoints == null || initialPoints.length < 2) {
//			log.error("Method:calculateExtrapTrackPoints, the input initialPoints is null or initialPoints.length less than 2"); 
			return null; 
		}
		
		int arrayLength = initialPoints.length; 
		Coordinate initPointBeforeLastInitPointCoordinate = initialPoints[arrayLength - 2].getLocation(); 
		Coordinate lastInitPointCoordinate = initialPoints[arrayLength - 1].getLocation(); 
		if(!isCoordinateValid(initPointBeforeLastInitPointCoordinate) || 
				!isCoordinateValid(lastInitPointCoordinate) || 
				initPointBeforeLastInitPointTimeCal == null || 
				lastInitPointTimeCal == null) {
//			log.error("Method: calculateExtrapTrackPoints, find 4 possible invalid input: startPointCoordinate or "+
//					"destPointCoordinate is invalid. firstTimeCal or secondTimeCal is null"); 
			return null; 
		}
		
		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
		gc.setStartingGeographicPoint(initPointBeforeLastInitPointCoordinate.x, 
				initPointBeforeLastInitPointCoordinate.y); 
		gc.setDestinationGeographicPoint(lastInitPointCoordinate.x, 
				lastInitPointCoordinate.y); 
		double direction = gc.getAzimuth(); 
		setDirectionForExtraPoints(direction); 
		
		double distanceInMeter = gc.getOrthodromicDistance(); 
		long timeDifference = getTimeDifferenceInMillisecond(initPointBeforeLastInitPointTimeCal, 
				lastInitPointTimeCal); 
		double speed = distanceInMeter / (double)timeDifference; 
		setSpeed(speed); 

		//Calculate the first extra point time Calendar
		Calendar firstExtraPointTimeCal = getTimeElapsedCalendarForFirstExtraPoint(lastInitPointTimeCal, 
				elapsedHourForExtraPointValue, 
				elapsedMinuteForExtraPointValue);

		//calculate the distance between the last Initial point and the first extra point
		double distanceBetweenLastInitPointAndFirstExtraPoint = calculateDistanceBetweenLastInitPointAndFirstExtraPoint(speed, 
				lastInitPointTimeCal, firstExtraPointTimeCal); 
		
		//calculate the distance among extra points
		double distanceForExtraPoint = calculateDistanceForExtraPoints(speed, 
				firstExtraPointTimeCal, elapsedHourForExtraPointValue,
				elapsedMinuteForExtraPointValue); 
		
		TrackPoint[] extrapTrackPointArray = calculateExtrapTrackPoints(gc, 
				lastInitPointCoordinate, direction, 
				distanceBetweenLastInitPointAndFirstExtraPoint, 
				distanceForExtraPoint, 
				extraDrawingPointNumber, lastInitPointTimeCal, 
				firstExtraPointTimeCal, 
				elapsedHourForExtraPointValue, 
				elapsedMinuteForExtraPointValue); 
		disableSomeTimeTagsDisplayBasedOnExtraPointTimeDisplayOption(extrapTrackPointArray, 
				extraPointTimeDisplayOption, skipFactorTextString); 
		
		return extrapTrackPointArray; 
	}

	private void disableSomeTimeTagsDisplayBasedOnExtraPointTimeDisplayOption(TrackPoint[] extrapTrackPointArray, 
			ExtraPointTimeDisplayOption extraPointTimeDisplayOption, String skipFactorTextString) {
		if(extrapTrackPointArray == null)
			return; 
		boolean[] extraPointTimeTagFlagArray = new boolean[extrapTrackPointArray.length]; 
		initializeBooleanArray(extraPointTimeTagFlagArray, true); 
		
		if(extraPointTimeDisplayOption == ITrack.ExtraPointTimeDisplayOption.SKIP_FACTOR) {
			removeTimeTagsBasedOnSkipFactor(extrapTrackPointArray, skipFactorTextString, extraPointTimeTagFlagArray); 
		} else if(extraPointTimeDisplayOption == ITrack.ExtraPointTimeDisplayOption.SHOW_FIRST_LAST) { 
			removeTimeTagsBasedOnShowLastFirstOnly(extrapTrackPointArray, extraPointTimeTagFlagArray); 
		} else if(extraPointTimeDisplayOption == ITrack.ExtraPointTimeDisplayOption.ON_ONE_HOUR) { 
			removeTimeTagsBasedOnHourMinuteValue(extrapTrackPointArray, true, extraPointTimeTagFlagArray);
		} else if(extraPointTimeDisplayOption == ITrack.ExtraPointTimeDisplayOption.ON_HALF_HOUR) { 
			removeTimeTagsBasedOnHourMinuteValue(extrapTrackPointArray, false, extraPointTimeTagFlagArray);
		}
		else { 
			removeTimeTagsBasedOnHourMinuteValue(extrapTrackPointArray, false, extraPointTimeTagFlagArray); 
		}
		
		/*
		 * set the updated boolean array back to the indicator, 
		 * in displayElementFactory, this boolean array will be 
		 * used to indicate if a time text should be skipped
		 */
		setExtraPointTimeTextDisplayIndicator(extraPointTimeTagFlagArray); 
	}

	private void removeTimeTagsBasedOnShowLastFirstOnly(TrackPoint[] extrapTrackPointArray, boolean[] timeTagIndicatorArray) {
		if(extrapTrackPointArray.length < 3)
			return; 
		for(int i=1; i<(extrapTrackPointArray.length - 1); i++) {
			timeTagIndicatorArray[i] = false; 
		}
	}

	private void removeTimeTagsBasedOnSkipFactor(TrackPoint[] extrapTrackPointArray, 
			String skipFactorTextString, boolean[] timeTagIndicatorArray) {
		if(skipFactorTextString == null)
			return; 
		/*
		 * always show the time tags of the first and last extra points
		 */
		if(extrapTrackPointArray.length < 3)
			return; 
		
		int skipFactorIntValue = 0; 
		try {
			skipFactorIntValue = Integer.parseInt(skipFactorTextString); 
		} catch(NumberFormatException nfe) {
//			log.error("The input of skipFactorTextString is invalid, skipFactorTextString="+
//					skipFactorTextString); 
		}
		
		/*
		 * 1. make sure skipFactorIntValue is a valid value
		 * 2. since the last and first extra  points are always 
		 * 	  displayed, thus do skipFactorIntValue > (extrapTrackPointArray.length - 2)
		 *    check  
		 */
		if(!(skipFactorIntValue > 0) || skipFactorIntValue > (extrapTrackPointArray.length - 2))
			return; 
		
		for(int i=1; i<=skipFactorIntValue; i++) {
			timeTagIndicatorArray[i] = false; 
		}
	}
	
	private void removeTimeTagsBasedOnHourMinuteValue(TrackPoint[] extrapTrackPointArray, 
			boolean isExactHourDisplayed, boolean[] timeTagIndicatorArray) {  
		for(int i=0; i<(extrapTrackPointArray.length); i++) {
			TrackPoint targetTrackPoint = extrapTrackPointArray[i]; 
			Calendar targetPointTimeCal = targetTrackPoint.getTime(); 
				
			if(!isTimeTagDisplayable(targetPointTimeCal, isExactHourDisplayed)) {
				/*
				 * mark the flag array index for the time needs to be hided 
				 */
				timeTagIndicatorArray[i] = false; 
			}
		}
	}

	private void initializeBooleanArray(boolean[] booleanArray, boolean initValue) {
		if(booleanArray == null)
			return; 
		for(int i=0; i<booleanArray.length; i++)
			booleanArray[i] = initValue; 
	}
	
	private boolean isTimeTagDisplayable(Calendar targetPointTimeCal, boolean isExactHourDisplayed) {
		boolean isDisplayable = false; 
		if(targetPointTimeCal == null)
			return isDisplayable; 
		
		int minuteIntValue = targetPointTimeCal.get(Calendar.MINUTE); 
		if(isExactHourDisplayed) {
			if(minuteIntValue == 0) 
				isDisplayable = true; 
		} else {
//			int hourIntValue = targetPointTimeCal.get(Calendar.HOUR_OF_DAY); 
			if( minuteIntValue == 0 || minuteIntValue == 30)
				isDisplayable = true; 
		}
		return isDisplayable; 
	}
	
	private double calculateDistanceBetweenLastInitPointAndFirstExtraPoint(double speed, 
			Calendar lastInitPointCal, Calendar firstExtraPointCal) {   
		long timeDifferenceBetweenExtraPoints = getTimeDifferenceInMillisecond(lastInitPointCal, 
				firstExtraPointCal); 
		double distanceBetweenLastInitPointAndFirstExtraPoint = speed * timeDifferenceBetweenExtraPoints; 
		return distanceBetweenLastInitPointAndFirstExtraPoint; 
	}
	
	private double calculateDistanceForExtraPoints(double speed, 
			Calendar firstExtraPointCal, int elapsedHourForExtraPoint, 
			int elapsedMinuteForExtraPoint) {
		Calendar nextExtraPointTimeCal = getTimeElapsedCalendar(firstExtraPointCal, 
				elapsedHourForExtraPoint, 
				elapsedMinuteForExtraPoint);
		long timeDifferenceBetweenExtraPoints = getTimeDifferenceInMillisecond(firstExtraPointCal, 
				nextExtraPointTimeCal); 
		double distanceForExtraPoint = speed * timeDifferenceBetweenExtraPoints; 
		return distanceForExtraPoint; 
	}
	
	private TrackPoint[] calculateExtrapTrackPoints(GeodeticCalculator gc, 
			Coordinate lastInitPointCoordinate, double direction, 
			double distanceBetweenLastInitPointAndFirstExtraPoint, 
			double distanceBetweenExtraPoint, int extraPointNumber, 
			Calendar lastInitPointTimeCal, Calendar firstExtraPointTimeCal, 
			int elapsedHourForExtraPoint, 
			int elapsedMinuteForExtraPoint) {
		double startLongitude = lastInitPointCoordinate.x; 
		double startLatitude = lastInitPointCoordinate.y; 
		TrackPoint[] trackPointArray = new TrackPoint[extraPointNumber]; 

		/*
		 * Add the first extra point
		 */
		java.awt.geom.Point2D firstExtraPointPoint2dValue = getNextPoint2DValue(gc, startLongitude, startLatitude, 
				direction, distanceBetweenLastInitPointAndFirstExtraPoint);
		startLongitude = firstExtraPointPoint2dValue.getX();
		startLatitude = firstExtraPointPoint2dValue.getY();
		Coordinate firstExtraPointCoordinate = new Coordinate(startLongitude, startLatitude); 
		TrackPoint firstExtraTrackPoint = new TrackPoint(firstExtraPointCoordinate, firstExtraPointTimeCal); 
		trackPointArray[0] = firstExtraTrackPoint; 
		
		
		Calendar newPointCal = firstExtraPointTimeCal; 
		for(int i = 1; i < (extraPointNumber); i++) {
			java.awt.geom.Point2D pt = getNextPoint2DValue(gc, startLongitude, startLatitude, 
					direction, distanceBetweenExtraPoint);
			startLongitude = pt.getX();
			startLatitude = pt.getY();
			Coordinate nextCoordinate = new Coordinate(startLongitude, startLatitude); 
			newPointCal = getTimeElapsedCalendar(newPointCal, elapsedHourForExtraPoint,
					elapsedMinuteForExtraPoint); 
			TrackPoint eachTrackPoint = new TrackPoint(nextCoordinate, newPointCal); 
			trackPointArray[i] = eachTrackPoint; 
		}
		return trackPointArray; 
	}
	
	private java.awt.geom.Point2D getNextPoint2DValue(GeodeticCalculator gc, 
			double startingPointLongitude, double startIngPointLatitude, 
			double direction, double distanceBetweenTwoPoints) {
		gc.setStartingGeographicPoint(startingPointLongitude, startIngPointLatitude);
		gc.setDirection(direction, distanceBetweenTwoPoints);  
		java.awt.geom.Point2D pt = gc.getDestinationGeographicPoint();
		return pt; 
	}
	
	private long getTimeDifferenceInMillisecond(Calendar startTimeCal, Calendar endTimeCal) {
		long startTimeInMillisecond = startTimeCal.getTimeInMillis(); 
		long endTimeInMillisecond = endTimeCal.getTimeInMillis(); 
		
		long timeDiffInMillisecond = endTimeInMillisecond - startTimeInMillisecond; 
		return timeDiffInMillisecond; 
	}
	
	private boolean isCoordinateValid(Coordinate coordinate) {
		if(coordinate.x > 180.0 || coordinate.x < -180.0 || coordinate.y > 90.0 ||
				coordinate.y < -90.0)
			return false; 
		return true; 
	}
	
	private Calendar getIntervalCalendarByParsingString(String dateString, 
			String formatStringPattern, Calendar secondTimeCal) {
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat(formatStringPattern); 
		Calendar cal = null; 
		int elapsedHour = 1;
		int elapsedMinute = 0; 
		if(dateString != null) {
			try {
				Date date = simpleDateFormat.parse(dateString); 
				cal = Calendar.getInstance(); 
				cal.setTime(date); 
				elapsedHour = cal.get(Calendar.HOUR_OF_DAY); 
				elapsedMinute = cal.get(Calendar.MINUTE); 
			} catch(ParseException pe) {
//				log.error("The input of dateString is invalid, parse fails, dateString="+dateString); 
				elapsedHour = 1; 
			}
		}
		setElapsedHourForExtraPoint(elapsedHour); 
		setElapsedMinuteForExtraPoint(elapsedMinute); 
		cal = getTimeElapsedCalendar(secondTimeCal, elapsedHour, elapsedMinute); 
		return cal; 
	}
	
	private Calendar getTimeElapsedCalendarForFirstExtraPoint(Calendar startCalendar, int elapsedHour, 
			int elapsedMinute) {
		Calendar nextCal = Calendar.getInstance(); 
		nextCal.setTimeInMillis(startCalendar.getTimeInMillis()); 
		/*
		 * played with the nmap2 APP, get confused with the logic behind, right now, calculate 
		 * the first extra point as the logic described below:  
		 * 1. if elapsedHour > 1 hour, set the first extra point time as the next exact hour
		 * 2. if elapsedHour == 0 and last init. point MINUTE value >= elapsedMinute, 
		 * 		set the first time extra point as the next exact hour
		 * 3. if elapsedHour == 0 and last init. point MINUTE value < elapsedMinute, 
		 * 		set the first time extra point as the exact hour of the last init. point + elapsedMinute
		 * In the future, this logic may be changed once the real logic is figured. Michael Gao comment. 
		 */
		if(elapsedHour >= 1) {
			nextCal.set(Calendar.MINUTE, 0); 
			nextCal.add(Calendar.HOUR_OF_DAY, 1); 
		} else {
			int currentMinute = nextCal.get(Calendar.MINUTE); 
			if(currentMinute >= elapsedMinute) {
				nextCal.set(Calendar.MINUTE, 0); 
				nextCal.add(Calendar.HOUR_OF_DAY, 1); 
			} else {
				nextCal.set(Calendar.MINUTE, elapsedMinute); 
			}
		}
		return nextCal; 
	}
	
	private Calendar getTimeElapsedCalendar(Calendar startCalendar, int elapsedHour, 
			int elapsedMinute) {
		Calendar nextCal = Calendar.getInstance(); 
		nextCal.setTimeInMillis(startCalendar.getTimeInMillis()); 
		nextCal.add(Calendar.HOUR_OF_DAY, elapsedHour); 
		nextCal.add(Calendar.MINUTE, elapsedMinute); 
		return nextCal; 
	}
	
	
	/*
	 * all override setters and getters of the instance variables go here 
	 */
	@Override
	public Color getExtrapColor() {
		return this.extrapColor;
	}

	@Override
	public String getExtrapLinePattern() {
		return EXTRAP_LINE_PATTERN;
	}

	@Override
	public String getExtrapMarker() {
		return EXTRAP_MARKER;
	}

	public TrackPoint[] getInitTrackPoints() {
		return initTrackPoints;
	}

	public void setInitTrackPoints(TrackPoint[] initTrackPoints) {
		this.initTrackPoints = initTrackPoints;
	}

	public void setExtrapPoints(TrackPoint[] extrapPoints) {
		this.extrapPoints = extrapPoints;
	}

	@Override
	public TrackPoint[] getExtrapPoints() {
		return extrapPoints;
	}

	@Override
	public Color getInitialColor() {
		return this.initialColor;
	}

	@Override
	public String getInitialLinePattern() {
		return INITIAL_LINE_PATTERN;
	}

	@Override
	public String getInitialMarker() {
		return INITIAL_MARKER;
	}

	@Override
	public TrackPoint[] getInitialPoints() {
		return initTrackPoints;
	}

	@Override
	public String getFontName() {
		return this.fontName;
	}
	
	public void setFontName(String _fontName) {
		this.fontName = _fontName; 
	}
	
	@Override
	public float getFontSize() {
		if(this.fontSize <= 0.0)
			return DEFAULT_FONT_SIZE; 
		return this.fontSize; 
	}
	
	public void setFontSize(float _fontSize) {
		this.fontSize = _fontSize; 
	}
	
	public FontStyle getFontStyle() {
		return fontStyle;
	}

	public void setFontStyle(FontStyle fontStyle) {
		this.fontStyle = fontStyle;
	}
	
	@Override
	public float getLineWidth() {
		if(this.lineWidth <= 0.0)
			return DEFAULT_LINE_WIDTH; 
		return this.lineWidth; 
	}
	
	/*
	 * all non-override setters and getters of the instance variables go here 
	 */
	public boolean isSetTimeButtonSelected() {
		return setTimeButtonSelected;
	}

	public void setSetTimeButtonSelected(boolean setTimeButtonSelected) {
		this.setTimeButtonSelected = setTimeButtonSelected;
	}

	public int getFontSizeComboSelectedIndex() {
		return fontSizeComboSelectedIndex;
	}

	public void setFontSizeComboSelectedIndex(int fontSizeComboSelectedIndex) {
		this.fontSizeComboSelectedIndex = fontSizeComboSelectedIndex;
	}

	public int getFontNameComboSelectedIndex() {
		return fontNameComboSelectedIndex;
	}

	public void setFontNameComboSelectedIndex(int fontNameComboSelectedIndex) {
		this.fontNameComboSelectedIndex = fontNameComboSelectedIndex;
	}

	public int getFontStyleComboSelectedIndex() {
		return fontStyleComboSelectedIndex;
	}

	public void setFontStyleComboSelectedIndex(int fontStyleComboSelectedIndex) {
		this.fontStyleComboSelectedIndex = fontStyleComboSelectedIndex;
	}

	public int getIntervalComboSelectedIndex() {
		return intervalComboSelectedIndex;
	}

	public void setIntervalComboSelectedIndex(int intervalComboSelectedIndex) {
		this.intervalComboSelectedIndex = intervalComboSelectedIndex;
	}

	public int getUnitComboSelectedIndex() {
		return unitComboSelectedIndex;
	}

	public void setUnitComboSelectedIndex(int unitComboSelectedIndex) {
		this.unitComboSelectedIndex = unitComboSelectedIndex;
	}

	public int getRoundComboSelectedIndex() {
		return roundComboSelectedIndex;
	}

	public void setRoundComboSelectedIndex(int roundComboSelectedIndex) {
		this.roundComboSelectedIndex = roundComboSelectedIndex;
	}

	public boolean getRoundBtnSelected() {
		return roundBtnSelected;
	}

	public void setRoundBtnSelected(boolean roundBtnSelected) {
		this.roundBtnSelected = roundBtnSelected;
	}

	public int getRoundDirComboSelectedIndex() {
		return roundDirComboSelectedIndex;
	}

	public void setRoundDirComboSelectedIndex(int roundDirComboSelectedIndex) {
		this.roundDirComboSelectedIndex = roundDirComboSelectedIndex;
	}

	public boolean getRoundDirBtnSelected() {
		return roundDirBtnSelected;
	}

	public void setRoundDirBtnSelected(boolean roundDirBtnSelected) {
		this.roundDirBtnSelected = roundDirBtnSelected;
	}

	public double getDirectionForExtraPoints() {
		return directionForExtraPoints;
	}

	public void setDirectionForExtraPoints(double directionForExtraPoints) {
		this.directionForExtraPoints = directionForExtraPoints;
	}

	public double getSpeed() {
		return speed;
	}

	public void setSpeed(double speed) {
		this.speed = speed;
		/*
		 * The original speed is meters / millisecond, now it needs
		 * to be converted to following units
		 */
		this.speedInKnotOverHour = speed * 1944; 
		this.speedInKilometerOverHour = speed * 3600;		
		this.speedInMileOverHour = speed * 2237;
	}
	
	public double getSpeedInKnotOverHour() {
		return speedInKnotOverHour;
	}
	public double getSpeedInKilometerOverHour() {
		return speedInKilometerOverHour;
	}
	public double getSpeedInMileOverHour() {
		return speedInMileOverHour;
	}
	

	public ExtraPointTimeDisplayOption getExtraPointTimeDisplayOption() {
		return extraPointTimeDisplayOption;
	}

	public void setExtraPointTimeDisplayOption(ExtraPointTimeDisplayOption extraPointTimeDisplayOption) {
		this.extraPointTimeDisplayOption = extraPointTimeDisplayOption;
	}

	public String getSkipFactorTextString() {
		return skipFactorTextString;
	}

	public void setSkipFactorTextString(String skipFactorTextString) {
		this.skipFactorTextString = skipFactorTextString;
	}

	public boolean[] getExtraPointTimeTextDisplayIndicator() {
		return extraPointTimeTextDisplayIndicator;
	}

	public void setExtraPointTimeTextDisplayIndicator(
			boolean[] extraPointTimeTextDisplayIndicator) {
		this.extraPointTimeTextDisplayIndicator = extraPointTimeTextDisplayIndicator;
	}

	public void setInitialColor(Color initialColor) {
		this.initialColor = initialColor;
	}

	public void setExtrapColor(Color extrapColor) {
		this.extrapColor = extrapColor;
	}

	public void setInitialMarker(String initialMarker) {
		this.initialMarker = initialMarker;
	}

	public void setInitialLinePattern(String initialLinePattern) {
		this.initialLinePattern = initialLinePattern;
	}

	public void setLineWidth(float lineWidth) {
		this.lineWidth = lineWidth;
	}

	public int getExtraDrawingPointNumber() {
		if(!(this.extraDrawingPointNumber > 0))
			return DEFAULT_EXTRA_POINT_NUMBER; 
		return this.extraDrawingPointNumber;
	}

	public void setExtraDrawingPointNumber(int extraDrawingPointNumber) {
		this.extraDrawingPointNumber = extraDrawingPointNumber;
	}

	public Calendar getFirstTimeCalendar() {
		return firstTimeCalendar;
	}

	public void setFirstTimeCalendar(Calendar firstTimeCalendar) {
		this.firstTimeCalendar = firstTimeCalendar;
	}

	public Calendar getSecondTimeCalendar() {
		return secondTimeCalendar;
	}

	public void setSecondTimeCalendar(Calendar secondTimeCalendar) {
		this.secondTimeCalendar = secondTimeCalendar;
	}

	public String getIntervalTimeString() {
		return intervalTimeString;
	}

	public void setIntervalTimeString(String _intervalTimeString) {
		/*
		 * intervalTimeString == null is allowed
		 */
		this.intervalTimeString = _intervalTimeString;

		Calendar intervalTimeCal = getIntervalCalendarByParsingString(_intervalTimeString, 
				INTERVAL_TIME_FORMAT_PATTERN, getSecondTimeCalendar());
		//setIntervalTimeCalendar(intervalTimeCal); 
	}

	public int getElapsedHourForExtraPoint() {
		return elapsedHourForExtraPoint;
	}

	private void setElapsedHourForExtraPoint(int elapsedHourForExtraPoint) {
		this.elapsedHourForExtraPoint = elapsedHourForExtraPoint;
	}

	public int getElapsedMinuteForExtraPoint() {
		return elapsedMinuteForExtraPoint;
	}

	private void setElapsedMinuteForExtraPoint(int elapsedMinuteForExtraPoint) {
		this.elapsedMinuteForExtraPoint = elapsedMinuteForExtraPoint;
	}

	@Override
	public String getSkipFactorText() {
		return skipFactorTextString;
	}

	@Override
	public FontStyle getStyle() {
		// TODO Auto-generated method stub
		return null;
	}

}
