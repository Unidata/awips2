package gov.noaa.nws.ncep.viz.rsc.nonconvsigmet.rsc;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.geom.PixelCoordinate;
import com.vividsolutions.jts.geom.Coordinate;

import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import gov.noaa.nws.ncep.common.dataplugin.nonconvsigmet.NonConvSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.nonconvsigmet.NonConvSigmetRecord;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import com.raytheon.uf.viz.core.map.IMapDescriptor;


/**
 * NonConvSigmetResource - Display NonConvective SIGMET data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20 Jul 2009    132       B. Hebbard  Initial creation.
 * 08/18/09       147       Greg Hull   extend AbstractNatlCntrsResource
 * 08/20/09       145       Greg Hull   init() -> initResource()
 * 01 Dec 2009    migration B. Hebbard  Migrate to11d3->d6
 * 07 May 2010    n/a       B. Hebbard  Rename for clarity:  ClassType->HazardType ; Series->SeriesName
 * 30 Sep 2010    307       Greg Hull   have NonConvSigmetRscDataObject implement IRscDataObject
 * 18 Nov 2010    307       Greg Hull   newRscDataObjsList -> newRscDataObjsQueue
 * 05 Aug 2011    n/a       B. Hebbard  Preserve newRscDataObjsQueue during traversal in processCancelRequest
 * 05/23/12       785       Q. Zhou     Added getName for legend.
 * 17 Aug 2012    655       B. Hebbard  Added paintProps as parameter to IDisplayable draw
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
public class NonConvSigmetResource extends 
          AbstractNatlCntrsResource< NonConvSigmetResourceData, NCMapDescriptor> implements
        INatlCntrsResource { 

	private NonConvSigmetResourceData ncRscData;

    //  a map from an identifier string (which is unique within a single frame)
    //  to a structure for a single displayable element
    private class FrameData extends AbstractFrameData {
        HashMap<String, NonConvSigmetRscDataObject> nonConvSigmetDataMap;  

		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
        	nonConvSigmetDataMap = new HashMap<String,NonConvSigmetRscDataObject>();
		}

		public boolean updateFrameData( IRscDataObject rscDataObj ) {
//			NonConvSigmetRecord nonConvSigmetRecord = (NonConvSigmetRecord) pdo;		
//		    NonConvSigmetRscDataObject nonConvSigmetData = getNonConvSigmetData( nonConvSigmetRecord );
		    NonConvSigmetRscDataObject nonConvSigmetData = (NonConvSigmetRscDataObject) rscDataObj;

		    // sanity check; cancel and corrections should not show up here.
    		if( nonConvSigmetData != null ) {
        		//  If CANCEL, just put on separate queue for processing later
			    if( nonConvSigmetData.hazardType == HazardType.CANCEL ||
			    	nonConvSigmetData.correction ) {
			    	// sanity check 
			    }
			    else {
			    	String keyString = nonConvSigmetData.sequenceID;
			    	NonConvSigmetRscDataObject existingNonConvSigmetData = nonConvSigmetDataMap.get(keyString);

			    	//  If keyString is not in the list, or if the ref time is newer,
			    	//  then add the data to the list
			    	if( existingNonConvSigmetData == null || 
			    		nonConvSigmetData.issueTime.greaterThan( existingNonConvSigmetData.issueTime ) ) {
			    		nonConvSigmetDataMap.put(keyString, nonConvSigmetData);
			    	}
			    }
    		}

			return true;
		}
    }

    //  Structure containing displayable information for a single
    //  displayable element -- that is, a single nonconvective SIGMET
    //  area.  This corresponds to the information from the (raw)
    //  NonConvSigmetRecord record, but has been 'crunched' a bit to
    //  (1) reduce things to those needed for display, and
    //  (2) preprocess where possible so the paint() method can
    //  work directly from this data as efficiently as possible.
    protected class NonConvSigmetRscDataObject  implements IRscDataObject {
    	HazardType      hazardType;    //  see enumeration immediately below
        String          sequenceID;    //  ex:  "ROMEO 3"
        SeriesName      seriesName;    //  ex:  ROMEO (enum)
        DataTime        issueTime;     //  issue time from bulletin
        DataTime        eventTime;     //  valid time with validPeriod for start/end
        DataTime        endTime;       //  end time of individual nonConvective SIGMET
        Boolean         correction;    //  true if COR (correction) or AMD (amendment)
        int             flightLevel1;  //  altitude LOWER bound; <0 if not specified
        int             flightLevel2;  //  altitude UPPER bound; <0 if not specified
        String          intensity;     //  ex: "OCNL SEV" (not currently displayed)
        int             numPoints;     //  size of following array
        LatLonPoint[]   points;        //  lat/lon of points, ordered as in bulletin
        
		@Override
		public DataTime getDataTime() {
			return eventTime; // include start/end time in the validPeriod
		}
    }
    
    
    private enum HazardType {
    	ICE,         //  icing
    	ICGIC,       //  icing in clouds
    	ICGIP,       //  icing in precipitation
    	ICGICIP,     //  icing in clouds and in precipitation
    	TURBULENCE,  //  turbulence
    	VA,          //  volcanic ash activity
    	DU,          //  duststorm or sandstorm
    	TEST,        //  test message (not displayed)
    	CANCEL,      //  cancel previously issued SIGMET
    	OTHER,       //  unclassified (unused)
    	UNKNOWN      //  unknown (assigned by decoder)
    }
    
    private enum SeriesName {
    	//  SIGMET series identifiers used by AWC for CONUS
    	NOVEMBER, OSCAR, PAPA, QUEBEC, ROMEO, UNIFORM, VICTOR, WHISKEY, XRAY, YANKEE, UNKNOWN
    }

    private IFont font;

	//  For fast determination -- during paint -- whether user wants
	//  to draw a given hazard class and SIGMET series
	Map<HazardType, Boolean> hazardEnable = new EnumMap<HazardType, Boolean>(HazardType.class);
	Map<SeriesName, Boolean> seriesEnable = new EnumMap<SeriesName, Boolean>(SeriesName.class);

	private List<NonConvSigmetRscDataObject> cancelList;
	private List<NonConvSigmetRscDataObject> modifyList;

    /**
     * Create a NonConvective SIGMET resource.
     * 
     * @throws VizException
     */
    public NonConvSigmetResource( NonConvSigmetResourceData rscData, 
    		LoadProperties loadProperties ) throws VizException {
    	super(rscData, loadProperties);
    	ncRscData = (NonConvSigmetResourceData)resourceData;
    	cancelList = new ArrayList<NonConvSigmetRscDataObject>();
    	modifyList = new ArrayList<NonConvSigmetRscDataObject>();
    }

    protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
    	return (AbstractFrameData) new FrameData( frameTime, timeInt );
    }

    public void initResource(IGraphicsTarget grphTarget) throws VizException {
    	font = grphTarget.initializeFont("Monospace", 14, new IFont.Style[] { IFont.Style.BOLD });
    	updateEnableMaps();
    	queryRecords();
    }

    // override the default method which processes PluginDataObjects so we 
    // can process, store and time match NonConvSigmetRscDataObjects.
    @Override
	protected IRscDataObject[] processRecord( Object pdo ) {
    	if( !(pdo instanceof NonConvSigmetRecord ) ) {
    		return new IRscDataObject[]{};
    	}
    	NonConvSigmetRscDataObject ncSigmetRscDataObj = 
    		           getNonConvSigmetData( (NonConvSigmetRecord)pdo );
    	
    	// Don't time match the cancel or correction, instead we add them to lists
    	// which will be post-processed to cancel/modify the appropriate sigmets
    	//
    	if( ncSigmetRscDataObj.hazardType == HazardType.CANCEL ) {
    		cancelList.add( ncSigmetRscDataObj );
    		return new IRscDataObject[]{};
    	}
    	else if( ncSigmetRscDataObj.correction ) {
    		modifyList.add( ncSigmetRscDataObj );
    		return new IRscDataObject[]{};
    	}
    	else {
    		return new IRscDataObject[]{ ncSigmetRscDataObj };
    	}
    }
    
    
    public void paintFrame( AbstractFrameData frameData, IGraphicsTarget target, PaintProperties paintProps) throws VizException {
  		FrameData currFrameData = (FrameData)frameData;

    	//  Allocate font and calculate vertical offset parameter for lines of text
		double screenToWorldRatio = paintProps.getCanvasBounds().width
		                          / paintProps.getView().getExtent().getWidth();
		Rectangle2D charSize = target.getStringBounds(font, "N");
		double charHeight = charSize.getHeight();
		double offsetY = charHeight / screenToWorldRatio;
		
        if( currFrameData != null ) {
    		//  Loop through the (preprocessed) nonConvective SIGMET data records
    		//  (This should be fast.)
    		Collection<NonConvSigmetRscDataObject> nonConvSigmetDataValues = 
    			                       currFrameData.nonConvSigmetDataMap.values();
    		
    		for( NonConvSigmetRscDataObject nonConvSigmetData : nonConvSigmetDataValues) {

    			//  Check for invalid time range...  Now done in timeMatching
//    			if (activeFrameTime.compareTo(nonConvSigmetData.startTime) < 0 ||
//    	            activeFrameTime.compareTo(nonConvSigmetData.endTime) >= 0) continue;

    			//  ...and whether hazard class and series letter enabled by user
    			if (hazardEnable.get(nonConvSigmetData.hazardType) &&
			    	seriesEnable.get(nonConvSigmetData.seriesName)) {
					
				    //  Defaults
				    RGB color = new RGB (155, 155, 155);
				    LineStyle lineStyle = LineStyle.SOLID;
				    int lineWidth   = 2;
				    int symbolWidth = 2;
				    int symbolSize  = 2;
				    String symbolType = "ASTERISK";
				    
				    //  Set class-dependent attributes, where applicable
				    switch (nonConvSigmetData.hazardType) {
				    	case ICE:      //  for now, all ice...
				    	case ICGIC:    //  ...is just ice...
				    	case ICGIP:    //  ...but could...
				    	case ICGICIP:  //  ...draw distinctly
				    		color       = ncRscData.icingColor;
				    		lineWidth   = ncRscData.icingLineWidth;
				    		symbolWidth = ncRscData.icingSymbolWidth;
				    		symbolSize  = ncRscData.icingSymbolSize;
				    		symbolType  = "ICING_08";
				    		break;
				    	case TURBULENCE:
				    		color       = ncRscData.turbulenceColor;
				    		lineWidth   = ncRscData.turbulenceLineWidth;
				    		symbolWidth = ncRscData.turbulenceSymbolWidth;
				    		symbolSize  = ncRscData.turbulenceSymbolSize;
				    		symbolType  = "TURBULENCE_4";
				    		break;
				    	case VA:
				    		color       = ncRscData.volcanicAshColor;
				    		lineWidth   = ncRscData.volcanicAshLineWidth;
				    		symbolWidth = ncRscData.volcanicAshSymbolWidth;
				    		symbolSize  = ncRscData.volcanicAshSymbolSize;
				    		symbolType  = "PRESENT_WX_201";
				    		break;
				    	case DU:
				    		color       = ncRscData.duststormColor;
				    		lineWidth   = ncRscData.duststormLineWidth;
				    		symbolWidth = ncRscData.duststormSymbolWidth;
				    		symbolSize  = ncRscData.duststormSymbolSize;
				    		symbolType  = "PRESENT_WX_031";
				    		break;
				    	case TEST:
				    		lineStyle   = LineStyle.SHORT_DASHED;  //  per NMAP
				    	case OTHER: 
				    	case UNKNOWN:
				    	default:
				    }
			    	
				    //  Draw the polygon
				    
			    	PixelCoordinate prevLoc = null;
			    	PixelCoordinate textLocation = null;
			    	for (int i=0; i<nonConvSigmetData.numPoints; i++) {
			    		LatLonPoint currentPoint = nonConvSigmetData.points[i];
			    		if (currentPoint == null) continue;  //  gracefully skip over omitted points (say, location lookup failure)
			    		double[] latLon = { currentPoint.getLongitude(LatLonPoint.INDEGREES),
			    				            currentPoint.getLatitude(LatLonPoint.INDEGREES) }; 
			    		PixelCoordinate currLoc = new PixelCoordinate(descriptor.worldToPixel(latLon));
			    		if (prevLoc != null) {  // skip first location
			    			//  draw line/polygon segment
			    			target.drawLine(prevLoc.getX(), prevLoc.getY(), prevLoc.getZ(), 
			    					        currLoc.getX(), currLoc.getY(), currLoc.getZ(), 
			    					        color, lineWidth, lineStyle);
			    		}
			    		if (textLocation == null || textLocation.getY() < currLoc.getY()) {
			    			textLocation = currLoc;
			    		}
			    		prevLoc = currLoc;
			    	}

			    	// Draw labels and symbol

			    	if (textLocation != null) {

			    		//  Use ArrayList since count not known in advance
			    		List<String> labelList = new ArrayList<String>();
			    		
			    		if(ncRscData.nameNumberEnable) {  //  sequence ID; for example, "OSCAR 1"
			    			labelList.add(nonConvSigmetData.sequenceID);
			    		}

			    		if(ncRscData.flightLevelEnable) {  //  altitude upper bound
			    			//  Note we skip both lines if this option disabled,
			    			//  but if enabled and no value, we draw blank line
			    			if (nonConvSigmetData.flightLevel2 >= 0) {
			    			    labelList.add("    " + nonConvSigmetData.flightLevel2);
			    			}
			    			else {
			    				labelList.add("");
			    			}
			    		}
			    		
			    		if(ncRscData.symbolEnable) {  //  drawn independently of text block, but sequenced here
				    		Color[] colors = new Color[] {new Color(color.red, color.green, color.blue)};
				    		//  Compute offset center of symbol in pixel coordinates, based on
				    		//  number of text lines so far...
				    		double[] pixelSymbolLocation = {
				    				textLocation.getX(),
				    				textLocation.getY() + offsetY * (labelList.size()+0.5) };
				    		//  ...and convert back to world coordinates, as Coordinate, for Symbol()
				    		double[] worldSymbolLocation = descriptor.pixelToWorld(pixelSymbolLocation);
				    		Coordinate coordinateSymbolLocation = new Coordinate(
				    				worldSymbolLocation[0],
				    				worldSymbolLocation[1] );
				    		String category = new String("Symbol");
				    		//  Generate the symbol object...
				    		Symbol symbol = new Symbol( null,   //  unused
			    					colors, symbolWidth, symbolSize * 0.60,  // scale per NMAP
			    					false,  //  clear
			    					coordinateSymbolLocation, category, symbolType);
			    			//  ...and display it
			    			DisplayElementFactory df = new DisplayElementFactory( target, getNcMapDescriptor() );
			    			ArrayList<IDisplayable> displayEls = df.createDisplayElements( symbol , paintProps );
			    			for (IDisplayable each : displayEls) {
			    				each.draw(target, paintProps);
			    				each.dispose();
			    			}
			    		}
			    		//  Skip a line in the text block where the symbol is centered
			    		//  (even if the symbol is not being drawn)
			    		labelList.add("");
			    		
			    		if(ncRscData.flightLevelEnable) {  //  altitude lower bound
			    			//  Note we skip both lines if this option disabled,
			    			//  but if enabled and no value, we draw blank line
			    			if (nonConvSigmetData.flightLevel1 >= 0) {
			    			    labelList.add("    " + nonConvSigmetData.flightLevel1);
			    			}
			    			else {
			    				labelList.add("");
			    			}
			    		}
			    		
			    		if(ncRscData.timeEnable) {  //  ending time
			    			String endTimeS = nonConvSigmetData.endTime.toString();
			    			labelList.add(endTimeS.substring(11, 13)       // hour
			    					    + endTimeS.substring(14, 16));     // minute
			    		}

			    		//  Text block complete, now draw it...
			    		if(!labelList.isEmpty()) {
			    			target.drawStrings(font, labelList.toArray(new  String[0]),
			    					textLocation.getX(),
			    					//  Offset in following is just in case first strings are blank,
			    					//  because of the way drawStrings works
			    					textLocation.getY() + offsetY * (labelList.get(0).isEmpty() ? 2 : 0),
			    					0,
			    					TextStyle.NORMAL,
			    					new RGB[] {color, color, color, color, color, color},  //TODO:  Cleaner way?
			    					HorizontalAlignment.LEFT, 
			    					VerticalAlignment.TOP );
			    		}
			    	}
			    }
    		}
    	}    	
    }                        

    // This method is called after the NonConvSigmetRscDataObjects have been added
    // to the newRscDataObjsList queue and before these objects are timeMatched to 
    // the frames. processRecords has saved off the cancellations and corrections into the
    // modify and cancelLists. We will use this list to correct or cancel the times in the
    // newRscDataObjsList so the time matching will be correct.
    // 
    @Override
    protected boolean preProcessFrameUpdate() {
   
    	//  Process MODIFY queue, for corrections/amendments.  (Because receipt order
    	//  of bulletins isn't assured, necessary to wait until all possible 'victims'
    	//  are in place first.)  Note that this doesn't remove any data records; it
    	//  just adjusts end times.
    	for( NonConvSigmetRscDataObject modify : modifyList) {
    		//TODO:  Note that NMAP uses stronger criteria for COR/AMD than for
    		//  cancellations when selecting 'victims':  The candidate must match
    		//  not only sequence ID, but also any TWO of the following:  class,
    		//  start time, end time.  We currently use the same criteria for
    		//  matching on both modify and cancel -- sequence ID must match,
    		//  and update time must fall after issue time of original (and
    		//  before the current end time, or of course there's nothing to
    		//  do anyway).  Seems this is OK, but might want to check on the
    		//  rationale for NMAP handling.
    		processCancelRequest(modify);
    	}

    	//  Process CANCEL queue.  (Because receipt order of bulletins isn't assured,
    	//  was necessary to wait until all possible 'victims' are in place first.)
    	//  Note that this doesn't remove any data records; it just adjusts end times.
    	//  Cancellations are processed after corrections, since a single SIGMET
    	//  could have been corrected/amended shortly after issue, and then cancelled
    	//  hours later at phenomena termination; in this case we need to make sure
    	//  the cancellation time supersedes the correction time, and not the reverse.
    	for( NonConvSigmetRscDataObject cancel : cancelList ) {
    		processCancelRequest(cancel);
    	}
        	
    	return true;
    }
    
    private NonConvSigmetRscDataObject getNonConvSigmetData( NonConvSigmetRecord nonConvSigmetRecord) {

    	//  A NonConvSigmetRscDataObject object roughly corresponds to a single NonConvSigmetRecord
    	//  from the database, but it's distilled down a bit to only the stuff we'll need for
    	//  paint() later.
    	if (nonConvSigmetRecord.getSigmetId() == null) 
    		 return null;  //  bail if not worth going further

    	NonConvSigmetRscDataObject nonConvSigmetData  = new NonConvSigmetRscDataObject();
    	
    	//  Convert hazardType string to an enum, just to avoid string comparisons
    	//  during all those paint()'s.
    	try {
    		nonConvSigmetData.hazardType = HazardType.valueOf(nonConvSigmetRecord.getHazardType());
    	}
    	catch (IllegalArgumentException e) {
    		//TODO:  Signal unrecognized hazardType string
    		nonConvSigmetData.hazardType = HazardType.UNKNOWN;
    	}

    	//TODO:  REMOVE this workaround for decoder issue (DU classified as UNKNOWN) -------- 
    	if (nonConvSigmetData.hazardType == HazardType.UNKNOWN  &&
    	    nonConvSigmetRecord.getHazardType().equals("UNKNOWN") &&
    	    (nonConvSigmetRecord.getBullMessage().contains(" BLDU ") ||
    	     nonConvSigmetRecord.getBullMessage().contains(" DS ") ||
    	     nonConvSigmetRecord.getBullMessage().contains(" SS "))) {
    		nonConvSigmetData.hazardType = HazardType.DU  ;
    	}
    	//TODO:  END ------------------------------------------------------------------------ 
    		
    	nonConvSigmetData.sequenceID   = nonConvSigmetRecord.getSigmetId();
    	nonConvSigmetData.seriesName   = getSeriesName(nonConvSigmetData.sequenceID);
    	nonConvSigmetData.issueTime    = nonConvSigmetRecord.getDataTime();
    	
    	// the eventTime which includes the start/end time within the validPeriod
    	nonConvSigmetData.eventTime    
    	         = new DataTime( nonConvSigmetRecord.getStartTime(),
    	        		 new TimeRange( nonConvSigmetRecord.getStartTime(),
    	        				        nonConvSigmetRecord.getEndTime() ) );
    	nonConvSigmetData.endTime      = new DataTime( nonConvSigmetRecord.getEndTime());
    	nonConvSigmetData.correction   = nonConvSigmetRecord.getCorrectionRemarks().equalsIgnoreCase("COR") ||
    	                                 nonConvSigmetRecord.getCorrectionRemarks().equalsIgnoreCase("AMD");
    	nonConvSigmetData.flightLevel1 = nonConvSigmetRecord.getFlightLevel1();
    	nonConvSigmetData.flightLevel2 = nonConvSigmetRecord.getFlightLevel2();
    	//  If decoder provides only one flight level, it must be the top
    	//  TODO:  decoder issue here?  differs from nmap2  REMOVE following if changed
    	if (nonConvSigmetData.flightLevel1 >= 0 &&
    		nonConvSigmetData.flightLevel2 <  0) {
    		nonConvSigmetData.flightLevel2 = nonConvSigmetData.flightLevel1;
    		nonConvSigmetData.flightLevel1 = -9999;
    	}
    	nonConvSigmetData.intensity    = nonConvSigmetRecord.getHazardIntensity();								

    	//  Incorporate all info from the child location records into
        //  (nice, fast) arrays of latitude/longitude points.
    	nonConvSigmetData.numPoints = nonConvSigmetRecord.getNonConvSigmetLocation().size();    	
    	if (nonConvSigmetData.numPoints > 0) {
    		nonConvSigmetData.points = new LatLonPoint[nonConvSigmetData.numPoints];
    		for (NonConvSigmetLocation nonConvSigmetLocation : nonConvSigmetRecord.getNonConvSigmetLocation()) {
    			LatLonPoint point = new LatLonPoint(nonConvSigmetLocation.getLatitude(),
    					                            nonConvSigmetLocation.getLongitude(),
    					                            LatLonPoint.INDEGREES);
    			int index = nonConvSigmetLocation.getIndex() - 1;
    			//TODO:  (Optional) Add sanity checks for uniqueness and completeness
    			//                  of indices
    			nonConvSigmetData.points[index] = point;
    		}
    	}

        return nonConvSigmetData;
    }
    
    private SeriesName getSeriesName(String sequenceID) {
    	//  Return a discrete enum representing series found in ID string
    	for (SeriesName s : SeriesName.values()) {
    		if (sequenceID.toUpperCase().contains(s.toString())) {
    			return s;
    		}
    	}
		return SeriesName.UNKNOWN;
    }
    
    //  Look for any existing SIGMET data records and adjust the end time
    //  
    private void processCancelRequest(NonConvSigmetRscDataObject cancel) {
    	
    	for ( IRscDataObject rscDataObj : newRscDataObjsQueue ) {
    		
    		NonConvSigmetRscDataObject candidate = (NonConvSigmetRscDataObject) rscDataObj;
    		
			//  Note second issueTime rather than startTime, since an
			//  issued SIGMET could be canceled before its valid time;
			//  also strict inequality assures proper sequencing
    		if (cancel.seriesName == candidate.seriesName &&
    			cancel.sequenceID.equals(candidate.sequenceID) &&
    			cancel.issueTime.compareTo(candidate.issueTime) > 0 &&
    			cancel.issueTime.compareTo(candidate.endTime)   < 0) {

    			candidate.endTime = cancel.issueTime;;
    		}
        }	
    }
    
    @Override
    public void disposeInternal() {
    	super.disposeInternal();
//    	if( font != null ) {
//    		font.dispose();
//    	}
    }

    public void updateEnableMaps() {
    	//  EnumMaps so we can quickly determine -- during paint --
        //  whether a given hazard class and/or SIGMET series is enabled
        //  (needs to be drawn, or can be skipped)...
    	
    	hazardEnable = new EnumMap<HazardType, Boolean>(HazardType.class);
    	hazardEnable.put(HazardType.ICE,        ncRscData.icingEnable);
    	hazardEnable.put(HazardType.ICGIC,      ncRscData.icingEnable);
    	hazardEnable.put(HazardType.ICGIP,      ncRscData.icingEnable);
    	hazardEnable.put(HazardType.ICGICIP,    ncRscData.icingEnable);
    	hazardEnable.put(HazardType.TURBULENCE, ncRscData.turbulenceEnable);
    	hazardEnable.put(HazardType.VA,         ncRscData.volcanicAshEnable);
    	hazardEnable.put(HazardType.DU,         ncRscData.duststormEnable);
    	hazardEnable.put(HazardType.TEST,       true);
    	hazardEnable.put(HazardType.CANCEL,     false);
    	hazardEnable.put(HazardType.OTHER,      true);
    	hazardEnable.put(HazardType.UNKNOWN,    true);

    	seriesEnable = new EnumMap<SeriesName, Boolean>(SeriesName.class);
    	seriesEnable.put(SeriesName.NOVEMBER, ncRscData.novemberEnable);
    	seriesEnable.put(SeriesName.OSCAR,    ncRscData.oscarEnable);
    	seriesEnable.put(SeriesName.PAPA,     ncRscData.papaEnable);
    	seriesEnable.put(SeriesName.QUEBEC,   ncRscData.quebecEnable);
    	seriesEnable.put(SeriesName.ROMEO,    ncRscData.romeoEnable);
    	seriesEnable.put(SeriesName.UNIFORM,  ncRscData.uniformEnable);
    	seriesEnable.put(SeriesName.VICTOR,   ncRscData.victorEnable);
    	seriesEnable.put(SeriesName.WHISKEY,  ncRscData.whiskeyEnable);
    	seriesEnable.put(SeriesName.XRAY,     ncRscData.xrayEnable);
    	seriesEnable.put(SeriesName.YANKEE,   ncRscData.yankeeEnable);
    	seriesEnable.put(SeriesName.UNKNOWN,  true);
    }


    @Override
    public void resourceAttrsModified() {
    	//  Note:  Could do updates individually in get/setters,
    	//  but doing it centrally seems less error prone...
    	updateEnableMaps(); 
    }
    
    @Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.nonConvSigmetDataMap.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}