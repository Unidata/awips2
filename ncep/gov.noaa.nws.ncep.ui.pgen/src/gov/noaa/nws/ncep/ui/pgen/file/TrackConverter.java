/*
 * gov.noaa.nws.ncep.ui.pgen.file.ProductConverter
 * 
 * Date created: 17 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.file;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.ArrayList;
import java.awt.Color;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.elements.Track;
import gov.noaa.nws.ncep.ui.pgen.display.ITrack;
import gov.noaa.nws.ncep.ui.pgen.display.TrackPoint;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;

/**
 * Define a ProductConverter Class - some methods to convert the products between XML format 
 * and the actual in-memory PGEN products. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/2012		TTR456			Q. Zhou   	Added speed, dir. Default to kts, no round
 * </pre>
 * */
public class TrackConverter {

	private final static org.apache.log4j.Logger log = 
		org.apache.log4j.Logger.getLogger(TrackConverter.class);
	
	/*
	 *  Convert a XML file DrawableElement object to a list of PGEN in-memory 
	 *  DrawableElement objects
	 */	
    public static List<Track> getTrackElementListByTrackBeanList ( 
		               List<gov.noaa.nws.ncep.ui.pgen.file.Track> trackBeanList) {
    	List<Track> trackElementList = new ArrayList<Track>(trackBeanList.size()); 
    	
    	for(gov.noaa.nws.ncep.ui.pgen.file.Track trackBean : trackBeanList) {
    		Track trackElement = new Track(); 
    		
    		trackElement.setInitialColor(getColorByColorTypeBean(trackBean.initialColor, true)); 
    		trackElement.setExtrapColor(getColorByColorTypeBean(trackBean.extrapColor, false)); 
    		
    		trackElement.setInitTrackPoints(getTrackPointElementListByTrackPointBeanList(trackBean.getInitialPoints()).toArray(new TrackPoint[trackBean.getInitialPoints().size()])); 
    		trackElement.setExtrapPoints(getTrackPointElementListByTrackPointBeanList(trackBean.getExtrapPoints()).toArray(new TrackPoint[trackBean.getExtrapPoints().size()])); 
    		
    		/*
    		 * Now initialize track's firstTimeCalendar and secondTimeCalendar using initTrackPoints
    		 */
    		trackElement.setFirstTimeCalendar(getFirstOrSecondTimeCalendarByTrackBean(trackBean, true)); 
    		trackElement.setSecondTimeCalendar(getFirstOrSecondTimeCalendarByTrackBean(trackBean, false)); 
    		
    		/*
    		 * Important note: the following two method calls are necessary.
    		 * 1. combine init and extrap points to allow PgenSource to go over every point for drawing
    		 * 2. setLinePattern is from the parent class of Track, the name is kind of misleading
    		 *    It is hard coded now, for future, a new field is needed to the XSD file. 
    		 */
    		trackElement.setLinePointsValue(trackElement.getInitialPoints(), trackElement.getExtrapPoints()); 
    		
    		trackElement.setExtraPointTimeTextDisplayIndicator(getBooleanArrayByBooleanList(trackBean.extraPointTimeTextDisplayIndicator)); 
    		
    		trackElement.setInitialLinePattern(trackBean.getInitialLinePattern()); 
    		trackElement.setExtrapLinePattern(trackBean.getExtrapLinePattern()); 
    		trackElement.setInitialMarker(trackBean.getInitialMarker()); 
    		trackElement.setExtrapMarker(trackBean.getExtrapMarker()); 
    		trackElement.setFontName(trackBean.getFontName()); 
    		if(trackBean.getLineWidth() != null)
    			trackElement.setLineWidth(trackBean.getLineWidth().floatValue()); 
    		else 
    			trackElement.setLineWidth((float)1.0);  //set a 1.0 as the default value
    		if(trackBean.getFontSize() != null)
    			trackElement.setFontSize(trackBean.getFontSize().floatValue()); 
    		else 
    			trackElement.setFontSize((float)2.0);  //set a 2.0 as the default value
    		
    		if(trackBean.getPgenCategory() == null)
    			trackElement.setPgenCategory(Track.TRACK_PGEN_CATEGORY); 
    		else 
    			trackElement.setPgenCategory(trackBean.getPgenCategory()); 
    		
    		if(trackBean.getPgenType() == null)
    			trackElement.setPgenType(Track.TRACK_PGEN_TYPE); 
    		else
    			trackElement.setPgenType(trackBean.getPgenType()); 
    		
    		/*
    		 * add speed, dir. Default to kts, no round --Quan
    		 */
    		TrackPoint[] initPts = trackElement.getInitTrackPoints();
    		int initPtsLength = initPts.length; 
    		Coordinate initPointBeforeLastInitPointCoordinate = initPts[initPtsLength - 2].getLocation(); 
    		Coordinate lastInitPointCoordinate = initPts[initPtsLength - 1].getLocation(); 
    		
    		GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
    		gc.setStartingGeographicPoint(initPointBeforeLastInitPointCoordinate.x, 
    				initPointBeforeLastInitPointCoordinate.y); 
    		gc.setDestinationGeographicPoint(lastInitPointCoordinate.x, 
    				lastInitPointCoordinate.y); 
    		double direction = gc.getAzimuth(); 
    		
    		double distanceInMeter = gc.getOrthodromicDistance(); 
    		long timeDifference = initPts[initPtsLength - 1].getTime().getTimeInMillis() -
    				initPts[initPtsLength - 2].getTime().getTimeInMillis(); 
    		double speed = distanceInMeter / (double)timeDifference; 
    		
    		trackElement.setDirectionForExtraPoints(direction);
    		trackElement.setSpeed(speed);
    		
    		/*
    		 * add something related to line drawing
    		 */
    		trackElement.setSizeScale((double) 1.0); 
    		trackElement.setSmoothFactor(0); 
    		trackElement.setClosed(false); 
    		trackElement.setFilled(false); 
    		trackElement.setFillPattern(FillPattern.FILL_PATTERN_0); 
    		
    		/*
    		 * The following attributes are necessary to fill values for the pop-up
    		 * TrackAttiDlg window from the restored line images. 
    		 */
    		boolean setTimeButtonSelectedFlag = true;   // set the default value as TRUE
    		if(trackBean.isSetTimeButtonSelected() != null)
    			setTimeButtonSelectedFlag = trackBean.isSetTimeButtonSelected().booleanValue(); 
  			trackElement.setSetTimeButtonSelected(setTimeButtonSelectedFlag); 
    		
  			int intervalComboSelectedIndexValue = 0;    //set the default value
    		if(trackBean.getIntervalComboSelectedIndex() != null)
    			intervalComboSelectedIndexValue = trackBean.getIntervalComboSelectedIndex().intValue(); 
    		trackElement.setIntervalComboSelectedIndex(intervalComboSelectedIndexValue); 
    		
    		trackElement.setIntervalTimeString(trackBean.getIntervalTimeTextString()); 
    		
    		String extraPointTimeDisplayOptionName = ITrack.ExtraPointTimeDisplayOption.SKIP_FACTOR.name();  // set the default value
    		if(trackBean.getExtraPointTimeDisplayOptionName() != null)
    			extraPointTimeDisplayOptionName = trackBean.getExtraPointTimeDisplayOptionName(); 
  			trackElement.setExtraPointTimeDisplayOption(ITrack.ExtraPointTimeDisplayOption.valueOf(extraPointTimeDisplayOptionName)); 
  			
  			String skipFactorTextString = "0";  //set the default value
  			if(trackBean.getSkipFactorTextString() != null) 
  				skipFactorTextString = trackBean.getSkipFactorTextString(); 
  			trackElement.setSkipFactorTextString(skipFactorTextString); 
  			
  			int fontNameComboSelectedIndex = 0; //set the default value
  			if(trackBean.getFontNameComboSelectedIndex() != null) 
  				fontNameComboSelectedIndex = trackBean.getFontNameComboSelectedIndex().intValue(); 
  			trackElement.setFontNameComboSelectedIndex(fontNameComboSelectedIndex); 
  			
  			int fontSizeComboSelectedIndex = 0; //set the default value
  			if(trackBean.getFontSizeComboSelectedIndex() != null) 
  				fontSizeComboSelectedIndex = trackBean.getFontSizeComboSelectedIndex().intValue(); 
  			trackElement.setFontSizeComboSelectedIndex(fontSizeComboSelectedIndex); 
  			
  			int fontStyleComboSelectedIndex = 0; //set the default value
  			if(trackBean.getFontStyleComboSelectedIndex() != null) 
  				fontStyleComboSelectedIndex = trackBean.getFontStyleComboSelectedIndex().intValue(); 
  			trackElement.setFontStyleComboSelectedIndex(fontStyleComboSelectedIndex); 
  			
    		trackElementList.add(trackElement); 
    	}
    	  	
        return trackElementList;
    }

    public static gov.noaa.nws.ncep.ui.pgen.file.Track getTrackBeanByTrackElement( 
            Track trackElement) {
    	gov.noaa.nws.ncep.ui.pgen.file.Track trackBean = new gov.noaa.nws.ncep.ui.pgen.file.Track(); 

    	trackBean.setInitialColor(getColorTypeBeanByColorElement(trackElement.getInitialColor())); 
    	trackBean.setExtrapColor(getColorTypeBeanByColorElement(trackElement.getExtrapColor())); 
    	
    	if(trackElement.getInitialPoints() != null) {
    		for(TrackPoint currentTrackPoint : trackElement.getInitialPoints()) {
    			trackBean.getInitialPoints().add(getTrackPointBeanByTrackPointElement(currentTrackPoint)); 
    		}
    	}
    	if(trackElement.getExtrapPoints() != null) {
    		for(TrackPoint currentTrackPoint : trackElement.getExtrapPoints()) {
    			trackBean.getExtrapPoints().add(getTrackPointBeanByTrackPointElement(currentTrackPoint)); 
    		}
    	}
    	 
    	trackBean.getExtraPointTimeTextDisplayIndicator().addAll(getBooleanObjectList(trackElement.getExtraPointTimeTextDisplayIndicator())); 

    	trackBean.setInitialLinePattern(trackElement.getInitialLinePattern()); 
    	trackBean.setExtrapLinePattern(trackElement.getExtrapLinePattern()); 
    	trackBean.setInitialMarker(trackElement.getInitialMarker()); 
    	trackBean.setExtrapMarker(trackElement.getExtrapMarker()); 
    	trackBean.setFontName(trackElement.getFontName()); 
    	trackBean.setFontSize(new Float(trackElement.getFontSize())); 
    	trackBean.setLineWidth(new Float(trackElement.getLineWidth())); 
    	
    	trackBean.setPgenCategory(trackElement.getPgenCategory()); 
    	trackBean.setPgenType(trackElement.getPgenType()); 
    	
    	/*
    	 * The following attributes are not necessary to save the line images 
    	 * and late to restore them back on the map. However, they are necessary 
    	 * values to pop-up and set up the correct attributes of the TrackAttrDlg 
    	 */
    	trackBean.setSetTimeButtonSelected(new Boolean(trackElement.isSetTimeButtonSelected())); 
    	trackBean.setIntervalComboSelectedIndex(new Integer(trackElement.getIntervalComboSelectedIndex())); 
    	trackBean.setIntervalTimeTextString(trackElement.getIntervalTimeString());  
    	trackBean.setExtraPointTimeDisplayOptionName(trackElement.getExtraPointTimeDisplayOption().name()); 
    	trackBean.setSkipFactorTextString(trackElement.getSkipFactorTextString()); 
    	trackBean.setFontNameComboSelectedIndex(new Integer(trackElement.getFontNameComboSelectedIndex())); 
    	trackBean.setFontSizeComboSelectedIndex(new Integer(trackElement.getFontSizeComboSelectedIndex())); 
    	trackBean.setFontStyleComboSelectedIndex(new Integer(trackElement.getFontStyleComboSelectedIndex())); 
    	
    	return trackBean; 
    }
    
    /*
     * help methods for transferring objects back and forth between class objects and JAXB beans
     */
    private static Calendar getFirstOrSecondTimeCalendarByTrackBean(gov.noaa.nws.ncep.ui.pgen.file.Track trackBean, boolean isFirstTimeCalendar) {
    	int indexOffSet = 1; 
    	if(isFirstTimeCalendar)
    		indexOffSet++; 
    	List<TrackPoint> trackPointElementList = getTrackPointElementListByTrackPointBeanList(trackBean.getInitialPoints()); 
    	if(trackPointElementList == null || trackPointElementList.size() < 2) {
    		log.error("Retrieved List<TrackPoint> trackPointElementList is NULL or the initial points are less than 2 points"); 
    		return null; 
    	}
    	int listSize = 	trackPointElementList.size(); 
    	return trackPointElementList.get(listSize - indexOffSet).getTime(); 
    }
    
    private static java.awt.Color getColorByColorTypeBean(gov.noaa.nws.ncep.ui.pgen.file.ColorType colorTypeBean, 
    		boolean isInitColor) {
    	if(colorTypeBean == null || colorTypeBean.getColor() == null) {
    		if(isInitColor)
    			return new Color(0, 0, 255); //return a default color as Blue
    		else 
    			return new Color(0, 192, 0);  //return a green color as the default color for extrapPoint
    	}
    	gov.noaa.nws.ncep.ui.pgen.file.Color colorBean = colorTypeBean.getColor(); 
    	return new Color(colorBean.getRed(), colorBean.getGreen(), colorBean.getBlue(), 
    			colorBean.getAlpha()); 
    }
    
    private static List<TrackPoint> getTrackPointElementListByTrackPointBeanList(List<gov.noaa.nws.ncep.ui.pgen.file.TrackPoint> trackPointBeanList) {
    	List<TrackPoint> trackPointElementList = new ArrayList<TrackPoint>(trackPointBeanList.size());
    	for(gov.noaa.nws.ncep.ui.pgen.file.TrackPoint trackPointBean : trackPointBeanList) {
    		java.util.Calendar currentCalendar = null; 
    		if(trackPointBean.getTime() != null) {
        		currentCalendar = trackPointBean.getTime().toGregorianCalendar();  
    		}
    		Coordinate currentCoordinate = getCoordinateByTrackPointBean(trackPointBean); 
    		TrackPoint currentTrackPointElement = new TrackPoint(currentCoordinate, currentCalendar); 
    		trackPointElementList.add(currentTrackPointElement); 
    	}
    	return trackPointElementList; 
    }
    
    private static Coordinate getCoordinateByTrackPointBean(gov.noaa.nws.ncep.ui.pgen.file.TrackPoint trackPointBean) {
    	Coordinate coordinate = new Coordinate(); 
    	if(trackPointBean.getLocation() != null) {
        	coordinate.x = trackPointBean.getLocation().getLongitude(); 
        	coordinate.y = trackPointBean.getLocation().getLatitude(); 
    	}
    	return coordinate; 
    }
    
    private static boolean[] getBooleanArrayByBooleanList(List<Boolean> booleanList) {
    	boolean[] booleanArray = new boolean[booleanList.size()]; 
    	int arrayIndex = 0; 
    	for(Boolean currentBoolean : booleanList) {
    		booleanArray[arrayIndex++] = currentBoolean.booleanValue(); 
    	}
    	return booleanArray; 
    }

    
    private static gov.noaa.nws.ncep.ui.pgen.file.ColorType getColorTypeBeanByColorElement(java.awt.Color colorElement) {
    	gov.noaa.nws.ncep.ui.pgen.file.ColorType colorTypeBean = new gov.noaa.nws.ncep.ui.pgen.file.ColorType(); 
    	gov.noaa.nws.ncep.ui.pgen.file.Color colorBean = new gov.noaa.nws.ncep.ui.pgen.file.Color(); 
    	colorBean.setAlpha(colorElement.getAlpha()); 
    	colorBean.setBlue(colorElement.getBlue()); 
    	colorBean.setRed(colorElement.getRed()); 
    	colorBean.setGreen(colorElement.getGreen()); 
    	colorTypeBean.setColor(colorBean); 
    	return colorTypeBean; 
    }
    
    private static gov.noaa.nws.ncep.ui.pgen.file.TrackPoint getTrackPointBeanByTrackPointElement(TrackPoint trackPointElement) {
    	gov.noaa.nws.ncep.ui.pgen.file.TrackPoint trackPointBean = new gov.noaa.nws.ncep.ui.pgen.file.TrackPoint(); 

    	if(trackPointElement.getTime() != null) {
        	GregorianCalendar gregorianCalendar = new GregorianCalendar(); 
        	gregorianCalendar.setTimeInMillis(trackPointElement.getTime().getTimeInMillis()); 
        	try {
            	XMLGregorianCalendar xmlGregorianCalendar = DatatypeFactory.newInstance().newXMLGregorianCalendar(gregorianCalendar);
            	trackPointBean.setTime(xmlGregorianCalendar); 
        	} catch(DatatypeConfigurationException dce) {
        		log.error("Error, instantiating XMLGregorianCalendar failed, error="+dce.getMessage()); 
        	}
    	}
    	if(trackPointElement.getLocation() != null) {
        	trackPointBean.location = new gov.noaa.nws.ncep.ui.pgen.file.TrackPoint.Location(); 
    		trackPointBean.getLocation().setLongitude(trackPointElement.getLocation().x); 
    		trackPointBean.getLocation().setLatitude(trackPointElement.getLocation().y); 
    	}
    	return trackPointBean; 
    }
    
    private static List<Boolean> getBooleanObjectList(boolean[] booleanArray) {
    	List<Boolean> booleanList = null; 
    	if(booleanArray == null)
    		booleanList = new ArrayList<Boolean>(); 
    	else 
    		booleanList = new ArrayList<Boolean>(booleanArray.length);
    	for(boolean booleanValue : booleanArray) {
    		Boolean booleanObject = new Boolean(booleanValue); 
    		booleanList.add(booleanObject); 
    	}
    	return booleanList; 
    }
    
  
}

