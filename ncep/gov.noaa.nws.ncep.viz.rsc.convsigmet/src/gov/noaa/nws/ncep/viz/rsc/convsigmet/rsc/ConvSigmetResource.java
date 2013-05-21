package gov.noaa.nws.ncep.viz.rsc.convsigmet.rsc;

import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetSection;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.geom.PixelCoordinate;
import com.raytheon.uf.viz.core.map.IMapDescriptor;

/**
 * ConvSigmetResource - Display Convective SIGMET data.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 16 Jun 2009  95         B. Hebbard  Initial creation.
 * 17 Jun 2009  115        G. Hull     Integrate with INatlCntrsResouce
 * 02 Jul 2009	134		   M. Li	   Use vors.xml
 * 10 Aug 2009             B. Hebbard  Convert to TO11 structure
 * 19 Aug 2009             B. Hebbard  Extend new AbstractNatlCntrsResource
 * 08/20/09       145      Greg Hull   init() -> initResource()
 * 21 Sep 2009             B. Hebbard  Remove VOR processing:  Use lat/lon direct from DB, now that updated decoder provides this
 * 24 Nov 2009             Greg Hull   migrate to to11d6
 * 03 Oct 2010    307      Greg Hull   modify processRecords and updateFrameData to process 
 *        						  	   ConvSigmetRscDataObjs instead of ConvSigmetRecord
 * 22 Apr 2011             B. Hebbard  Prevent label overlap where W&C or C&E Outlooks have coincident top points
 * 09 Mar 2012    728      B. Hebbard  Use postProcessFrameUpdate() to remove, for each frame, SIGMETs/Outlooks 
 *                                     if superseded by any in that frame more recently issued for the same
 *                                     region (W/C/E) (TTR 143).
 * 05/23/12       785      Q. Zhou     Added getName for legend.
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
public class ConvSigmetResource extends AbstractNatlCntrsResource<ConvSigmetResourceData, NCMapDescriptor>
                                implements INatlCntrsResource {
	
	private ConvSigmetResourceData convSigmetResourceData;

    private class FrameData extends AbstractFrameData {

    	//  A map from an identifier string (like "18C", which is unique
    	//  within a single frame) to a structure for one displayable element
    	//  (polygon, line, point, etc.) for a SIGMET or Outlook location
        HashMap<String, ConvSigmetRscDataObj> convSigmetDataMap;

		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
        	convSigmetDataMap = new HashMap<String,ConvSigmetRscDataObj>();
    	}

        public boolean updateFrameData( IRscDataObject rscDataObj) {
        	if( !(rscDataObj instanceof ConvSigmetRscDataObj) ) {
        		System.out.println("ConvSigmet.updateFrameData expecting ConvSigmetRscDataObj "
        			+ " instead of: " + rscDataObj.getClass().getName() );
        		return false;
        	}
        
        	ConvSigmetRscDataObj cSigRscData = (ConvSigmetRscDataObj) rscDataObj;

        	if( cSigRscData != null &&
        		cSigRscData.csigType != ConvSigmetType.CS &&
        		cSigRscData.csigType != ConvSigmetType.UNKNOWN) {
        		//  Note that -- unlike similar resources -- sequenceID alone is
        		//  not unique even within a single frame, since a CONVECTIVE SIGMET
        		//  and OUTLOOK can both be designated, say, "1C".  So we suffix with
        		//  the class type, to ensure uniqueness within a single frame.
        		//  TODO:  Is this good enough?  Since sequence IDs 'reset' each
        		//  frame/hour, might want to add end time string for added safety?
        		String keyString = cSigRscData.sequenceID + " " + cSigRscData.csigType;
        		ConvSigmetRscDataObj existingConvSigmetData = convSigmetDataMap.get(keyString);

        		//  If keyString is not in the list, or if the ref time is newer,
        		//  then add the data to the list
        		if( existingConvSigmetData == null || 
        			cSigRscData.issueTime.greaterThan(existingConvSigmetData.issueTime) ) {
        			
        			convSigmetDataMap.put(keyString, cSigRscData);
        		}
        	}
        	
        	return true;        	
        }
    }

    //  Structure containing displayable information for a single
    //  displayable element -- that is, a single convective SIGMET
    //  (AREA polygon, LINE line, or ISOL point) or OUTLOOK.  This
    //  corresponds to the information from the (raw) ConvSigmetSection
    //  record, but has been 'crunched' a bit to (1) reduce things to
    //  those needed for display, and (2) preprocess where possible
    //  (for example, location lookups) so the paint() method can
    //  work directly from this data as efficiently as possible.
    private class ConvSigmetRscDataObj implements IRscDataObject {
    	ConvSigmetType  csigType;    //  see enumeration immediately below
        String          sequenceID;  //  ex:  "24C"
        DataTime        issueTime;   //  issue time from bulletin
        DataTime        eventTime;   //  time range of individual convective SIGMET or outlook
        DataTime        matchTime;   //  same, but fudged so outlook 'valid' right away (per legacy)
        DataTime        endTime;     //  end time of individual convective SIGMET or outlook
        int             direction;   //  "from" direction in bulletin
        int             speed;       //  in knots
        int             distance;    //  width of LINE or diameter of ISOL (NM)
        int             flightLevel; //  as specified with FL in bulletin
        String          intensity;   //  ex:  "DVLPG"
        int             numPoints;   //  size of following array
        LatLonPoint[]   points;      //  lat/lon of points, ordered as in bulletin (may be null if invalid location)

        @Override
		public DataTime getDataTime() {
			//return eventTime;
			return matchTime;
		}
    }
    
    private enum ConvSigmetType {
    	AREA,      //  Convective SIGMET for AREA (polygon)
    	LINE,      //  Convective SIGMET for LINE of storms, w/ distance both sides (combined)
    	ISOL,      //  Convective SIGMET for ISOLated (point, w/ distance diameter)
    	OUTLOOK,   //  Not a Convective SIGMET itself, but area (polygon) where things might develop 2-6 hours hence
    	CS,        //  "NIL" Convective SIGMET; fulfilling required hourly issuance, explicitly saying there are no Convective SIGMETs for this region (W, C, or E)
    	UNKNOWN    //  Unknown type -- something wrong
    }
    
    private IFont font=null;
    
    //  ------------------------------------------------------------

    private static final double ONE_NM_RADIANS = Math.toRadians (1.0 / 60.0);

    
    /**
     * Create a Convective SIGMET resource.
     * 
     * @throws VizException
     */
    public ConvSigmetResource(ConvSigmetResourceData resourceData,
    		LoadProperties loadProperties) throws VizException {
    	super(resourceData, loadProperties);
    	convSigmetResourceData = (ConvSigmetResourceData) resourceData ;
    }

    protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
    	return (AbstractFrameData) new FrameData( frameTime, timeInt );
    }
    
    public void initResource(IGraphicsTarget grphTarget) throws VizException {
    	queryRecords();
    }

    // override to process ConvSigmetRscDataObj instead of ConvSigmetRecord since each
    // record may contain more than one 'convSigmetSection' each of which has its own
    // start and end time and must be time matched separately.
    //
    @Override
	protected IRscDataObject[] processRecord( Object pdo ) {
		if( !(pdo instanceof ConvSigmetRecord) ) {
			System.out.println( "ConvSigmet expecting ConvSigmetRecord instead of: " +
					pdo.getClass().getName() );
			return null;
		}
		
    	ConvSigmetRecord csigRec = (ConvSigmetRecord)pdo;
    	DataTime csigTime = csigRec.getDataTime();

    	Set<ConvSigmetSection> convSigmetSections = csigRec.getConvSigmetSection();
    	
    	ArrayList<ConvSigmetRscDataObj> csigRscDataObjs = new ArrayList<ConvSigmetRscDataObj>();
    	
    	for( ConvSigmetSection csigSection : convSigmetSections ) {
    		ConvSigmetRscDataObj convSigmetData = getConvSigmetData( csigTime, csigSection );

    		if( convSigmetData != null ) {
    			csigRscDataObjs.add( convSigmetData );
    		}    	
    	}
    	
    	return csigRscDataObjs.toArray( new ConvSigmetRscDataObj[0] );
	}

    
    @Override
	protected boolean postProcessFrameUpdate() {
    	//
    	
    	// for each frame...
    	
    	for (AbstractFrameData afd : frameDataMap.values()) {
    		FrameData fd = (FrameData) afd;
    		
    		// ...go through all the data time matched to this frame
    		// to determine, for every region (W, C, E), the latest issuance
    		// of SIGMETs and of Outlooks for that region
    		
    		Map<Character,DataTime> latestSigmetIssuanceTimeForRegions = new HashMap<Character,DataTime>();
    		Map<Character,DataTime> latestOutlooksIssuanceTimeForRegions = new HashMap<Character,DataTime>();
    		
    		for (ConvSigmetRscDataObj csigRDO : fd.convSigmetDataMap.values()) {
    			String sequenceID = csigRDO.sequenceID.trim().toUpperCase();
    			if (sequenceID != null && !sequenceID.isEmpty()) {
    				Character region = sequenceID.charAt(sequenceID.length()-1); // the "C" in "18C"
    				switch (csigRDO.csigType) { 
    				case AREA:
    				case ISOL:
    				case LINE:
    				case CS: // null Convective SIGMET still counts as 'issuance'
    					DataTime latestSigmetIssuanceForThisRegion =
    						latestSigmetIssuanceTimeForRegions.get(region);
    					if (latestSigmetIssuanceForThisRegion == null ||
    							csigRDO.issueTime.greaterThan(latestSigmetIssuanceForThisRegion)) {
    						latestSigmetIssuanceTimeForRegions.put(region, csigRDO.issueTime);
    					}
    					break;
    				case OUTLOOK:
    					DataTime latestOutlooksIssuanceForThisRegion =
    						latestOutlooksIssuanceTimeForRegions.get(region);
    					if (latestOutlooksIssuanceForThisRegion == null ||
    							csigRDO.issueTime.greaterThan(latestOutlooksIssuanceForThisRegion)) {
    						latestOutlooksIssuanceTimeForRegions.put(region, csigRDO.issueTime);
    					}
    					break;
    				case UNKNOWN:
    					break;
    				default:
    				}
    			}
    		}
    		
    		// Now that we've determined the latest issuances for each region --
    		// for both outlooks and actual Convective SIGMETs -- we make a second
    		// pass through the data time matched to this frame.  This time,
    		// we purge anything superseded by a later issuance.
    		
    		String[] keys = new String[1];
    		keys = fd.convSigmetDataMap.keySet().toArray(keys);
    		for (String key : keys) {
    			ConvSigmetRscDataObj csigRDO = fd.convSigmetDataMap.get(key);
    			String sequenceID = (csigRDO == null) ? null : csigRDO.sequenceID;
    			if (sequenceID != null && !sequenceID.isEmpty()) {
    				Character region = sequenceID.charAt(sequenceID.length()-1);
    				switch (csigRDO.csigType) { 
    				case AREA:
    				case ISOL:
    				case LINE:
    				case CS:
    					DataTime latestSigmetIssuanceForThisRegion =
    						latestSigmetIssuanceTimeForRegions.get(region);
    					if (latestSigmetIssuanceForThisRegion != null &&
    							latestSigmetIssuanceForThisRegion.greaterThan(csigRDO.issueTime)) {
    						fd.convSigmetDataMap.remove(key);
    					}
    					break;
    				case OUTLOOK:
    					DataTime latestOutlooksIssuanceForThisRegion =
    						latestOutlooksIssuanceTimeForRegions.get(region);
    					if (latestOutlooksIssuanceForThisRegion != null &&
    							latestOutlooksIssuanceForThisRegion.greaterThan(csigRDO.issueTime)) {
    						fd.convSigmetDataMap.remove(key);
    					}
    					break;
    				case UNKNOWN:
    					break;
    				default:
    				}
    			}
    		}
    		
    	}
    	//
		return true;
	}
    
    public void paintFrame(AbstractFrameData frameData, IGraphicsTarget target, PaintProperties paintProps) throws VizException {

    	FrameData currFrameData = (FrameData) frameData;

    	//  Allocate font and calculate vertical offset parameter for lines of text
    	font = target.initializeFont("Monospace", 14, new IFont.Style[] { IFont.Style.BOLD });
		double screenToWorldRatio = paintProps.getCanvasBounds().width
		                          / paintProps.getView().getExtent().getWidth();
		Rectangle2D charSize = target.getStringBounds(font, "N");
		double charHeight = charSize.getHeight();
		double offsetY = charHeight / screenToWorldRatio;
		
		if( paintProps == null ) {
			return;
		}

        if (currFrameData != null) {

    		//  Put per-hour options into arrays for easy lookup
			final Boolean[] enables    = {convSigmetResourceData.hour0Enable,
					                      convSigmetResourceData.hour1Enable,
					                      convSigmetResourceData.hour2Enable};
			final RGB[]     colors     = {convSigmetResourceData.hour0Color,
					                      convSigmetResourceData.hour1Color,
					                      convSigmetResourceData.hour2Color};
			final int[]     lineWidths = {convSigmetResourceData.hour0LineWidth,
					                      convSigmetResourceData.hour1LineWidth,
					                      convSigmetResourceData.hour2LineWidth};
            final Boolean[] sequenceIdEnables = {convSigmetResourceData.hour0sequenceIdEnable,
            		                             convSigmetResourceData.hour1sequenceIdEnable,
            		                             convSigmetResourceData.hour2sequenceIdEnable};

    		//  Loop through the (preprocessed) convective SIGMET data records
    		//  (This should be fast.)
    		Collection<ConvSigmetRscDataObj> convSigmetDataValues = currFrameData.convSigmetDataMap.values();
    		
    		for (ConvSigmetRscDataObj convSigmetData : convSigmetDataValues) {

    			//  Check for invalid time range
    			//  TODO:  See if this is still needed/valid...
    			//  if (activeFrameTime.compareTo(convSigmetData.startTime) < 0 ||
    	        //      activeFrameTime.compareTo(convSigmetData.endTime) >= 0) continue;
						
			    //  Just some 'safety' defaults 
				boolean enable = false;
			    RGB color = new RGB (155, 155, 155);
			    int lineWidth = 2;
			    boolean sequenceIdEnable = true;

			    //  Are we moving?  (Decoder uses negative numbers to say no.)
			    boolean inMotion = (convSigmetData.direction >= 0) && (convSigmetData.speed > 0);
			    
				for (int hour = 0; hour <= 2; hour++) {
					switch (convSigmetData.csigType) {
					    case AREA:
					    case LINE:
					    case ISOL:      //  these (may) have motion; set hourly parameters
						    enable = enables[hour] && (hour == 0 || inMotion);
						    color = colors[hour];
						    lineWidth = lineWidths[hour];
						    sequenceIdEnable = sequenceIdEnables[hour];
						    break;
					    case OUTLOOK:   //  no motion; draw only zero hour (if enabled)
						    enable = (hour == 0) && convSigmetResourceData.getOutlookEnable();
						    color = convSigmetResourceData.getOutlookColor();
						    lineWidth = convSigmetResourceData.getOutlookLineWidth();
						    sequenceIdEnable = true;  //  cannot disable sequence ID for outlooks unless outlooks disabled altogether
						    break;
					    case CS:        //  nil convective SIGMET
					    	enable = false;
						    break;
					    case UNKNOWN:   //TODO:  Sanity check error!  Unrecognized class type.
					    	enable = false;
						    break;
					    default:
					}
					if (enable) {
						PixelCoordinate prevLoc = null;
						PixelCoordinate topLocation = null;  //  text placed above (screen, not necessarily north) this won't interfere with the geometry
						double longitudeAtTopLocation = 0.0;
						for (int i=0; i<convSigmetData.numPoints; i++) {
							LatLonPoint currentPoint = convSigmetData.points[i];
							if (currentPoint == null) continue;  //  gracefully skip over omitted points (say, location lookup failure)
							if (hour > 0) {
								//  extrapolate position in future
								double headingRadians = Math.toRadians((double) (360 - (convSigmetData.direction + 180) % 360));  // TO instead of FROM; CCW instead of CW);
								double distanceRadians = hour * convSigmetData.speed * ONE_NM_RADIANS;
								currentPoint = currentPoint.positionOf(headingRadians, distanceRadians);
							}
							double[] latLon = { currentPoint.getLongitude(LatLonPoint.INDEGREES),
									            currentPoint.getLatitude(LatLonPoint.INDEGREES) }; 
							PixelCoordinate currLoc = new PixelCoordinate(descriptor.worldToPixel(latLon));
							if (prevLoc != null) {  // skip first location
								//  draw line/polygon segment
								target.drawLine(prevLoc.getX(), prevLoc.getY(), prevLoc.getZ(),
										        currLoc.getX(), currLoc.getY(), currLoc.getZ(), color, lineWidth);
							}
							else if (convSigmetData.numPoints == 1) {  //TODO:  Check for csigType ISOL instead or in addition?
								//  single point; draw marker and circle
								double delta = offsetY * 0.3;  //  tune to match NMAP
								target.drawLine(currLoc.getX()-delta, currLoc.getY(), currLoc.getZ(),
										        currLoc.getX()+delta, currLoc.getY(), currLoc.getZ(), color, lineWidth);
								target.drawLine(currLoc.getX(), currLoc.getY()-delta, currLoc.getZ(),
										        currLoc.getX(), currLoc.getY()+delta, currLoc.getZ(), color, lineWidth);
								double radius = convSigmetData.distance / 2.0;
								target.drawCircle(currLoc.getX(), currLoc.getY(), currLoc.getZ(), radius, color, lineWidth);
								topLocation = new PixelCoordinate(currLoc.getX(), currLoc.getY()-radius);  //  circle top
							}
							if (topLocation == null || topLocation.getY() > currLoc.getY()) {
								topLocation = currLoc;
								longitudeAtTopLocation = latLon[0];
							}
							prevLoc = currLoc;
						}

						// Draw labels

						if (topLocation != null) {
							
							//  Use an ArrayList since we don't know in advance how big it'll be and would like
							List<String> labelList = new ArrayList<String>();

							HorizontalAlignment horizontalAlignment = HorizontalAlignment.LEFT;
							
							if (sequenceIdEnable) {
								if (convSigmetData.csigType == ConvSigmetType.OUTLOOK) {
									// Prevent label overlap when West & Central OR Central & East outlook
									// polygons have coincident top points, by flipping some text to left
									String outlookLabel = convSigmetData.sequenceID + " OUTLOOK";
									if (convSigmetData.sequenceID.endsWith("C") && longitudeAtTopLocation > -095.0  ||    //TODO:  Tune longitude boundary?
										convSigmetData.sequenceID.endsWith("W") && longitudeAtTopLocation > -112.0	) {   //TODO:  Tune longitude boundary?
										horizontalAlignment = HorizontalAlignment.RIGHT;
										outlookLabel += "  ";
										}
									labelList.add(outlookLabel);
								}
								else {
								    labelList.add(convSigmetData.sequenceID);
								}
							}
							
							if (hour == 0 && convSigmetData.csigType != ConvSigmetType.OUTLOOK) {
								if (convSigmetResourceData.timeEnable) {
									String endTimeS = convSigmetData.endTime.toString();
									labelList.add(endTimeS.substring(8, 10) + "/"  // date
												+ endTimeS.substring(11, 13)       // hour
												+ endTimeS.substring(14, 16));     // minute
								}

								if (convSigmetResourceData.flightLevelEnable) {
									labelList.add("FL" + convSigmetData.flightLevel);
								}

								if (convSigmetResourceData.motionEnable && inMotion) {
									labelList.add(String.format("%03d",  //  leading zeroes for direction
											      convSigmetData.direction) + " " +
											      convSigmetData.speed + "kt");
								}

								if (convSigmetResourceData.intensityEnable && convSigmetData.intensity != null &&
										              !convSigmetData.intensity.isEmpty()) {
									labelList.add(convSigmetData.intensity);
								}
							}

							if (!labelList.isEmpty()) {
								target.drawStrings(font, labelList.toArray(new  String[0]),
										topLocation.getX(),
										topLocation.getY() - offsetY * (labelList.size()+0.5),
										0.0,
										TextStyle.NORMAL,
										new RGB[] {color, color, color, color, color, color},  //TODO:  Dorky!!
										horizontalAlignment,
										VerticalAlignment.TOP);
							}
						}
					}
				}
    		}
    	}
    	font.dispose();
    	font = null;
    }                        

    private ConvSigmetRscDataObj getConvSigmetData(DataTime rTime, ConvSigmetSection convSigmetSection) {

    	//  A ConvSigmetData object holds roughly the same information as a ConvSigmetSection
    	//  from the database, but it's distilled down a bit to only the stuff we'll need for
    	//  paint() later.
    	if (convSigmetSection.getSequenceID() == null) {
    		return null;  //  bail if not worth going further
    	}

    	ConvSigmetRscDataObj convSigmetData  = new ConvSigmetRscDataObj();
    	
    	//  Convert classType string to an enum, just to avoid string comparisons
    	//  during all those paint()'s.
    	try {
    		convSigmetData.csigType = ConvSigmetType.valueOf(convSigmetSection.getClassType());
    	}
    	catch (IllegalArgumentException e) {
    		//TODO:  Signal unrecognized classType string
    		convSigmetData.csigType = ConvSigmetType.UNKNOWN;
    	}
    	
    	convSigmetData.sequenceID  = convSigmetSection.getSequenceID();
    	convSigmetData.issueTime   = rTime;
    	convSigmetData.eventTime   = new DataTime( convSigmetSection.getStartTime(),
    	                                           new TimeRange( convSigmetSection.getStartTime(),
    	                                                          convSigmetSection.getEndTime() ) );
    	convSigmetData.matchTime   = new DataTime( rTime.getRefTimeAsCalendar(),
    	                                           new TimeRange( rTime.getRefTimeAsCalendar(),
    	                                                          convSigmetSection.getEndTime() ) );
    	convSigmetData.endTime     = new DataTime (convSigmetSection.getEndTime());
    	convSigmetData.direction   = convSigmetSection.getDirection();
    	convSigmetData.speed       = convSigmetSection.getSpeed();
    	convSigmetData.distance    = convSigmetSection.getDistance();
    	convSigmetData.flightLevel = convSigmetSection.getFlightLevel();
    	convSigmetData.intensity   = convSigmetSection.getIntensity();								

    	//  Child location records become arrays of latitude/longitude points.
    	convSigmetData.numPoints = convSigmetSection.getConvSigmetLocation().size();    	
    	if (convSigmetData.numPoints > 0) {
    		convSigmetData.points = new LatLonPoint[convSigmetData.numPoints];
    		for (ConvSigmetLocation convSigmetLocation : convSigmetSection.getConvSigmetLocation()) {
    	    	LatLonPoint point = new LatLonPoint (convSigmetLocation.getLatitude(), 
    	    			                             convSigmetLocation.getLongitude(),
    	    			                             LatLonPoint.INDEGREES);
    			int index = convSigmetLocation.getIndex() - 1;
    			//TODO:  Add sanity checks for uniqueness and completeness of indices
    			convSigmetData.points[index] = point;
    		}
    	}
    	
        return convSigmetData;
    }

    @Override
    public void disposeInternal() {
    	super.disposeInternal();
//    	if( font != null ) {
//    		font.dispose();
//    	}
    }

	public void resourceAttrsModified() {
		// don't need to do anything
	}
	
	@Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		if (fd == null || fd.getFrameTime() == null || fd.convSigmetDataMap.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}