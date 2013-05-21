package gov.noaa.nws.ncep.viz.rsc.hrcn.rsc;

import gov.noaa.nws.ncep.common.dataplugin.tcm.TcmPositionWinds;
import gov.noaa.nws.ncep.common.dataplugin.tcm.TcmRecord;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.Tcm;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.TcmFcst;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.TcmWindQuarters;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * HrcnResource - Display Tropical Cyclone data
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04 Oct 2011  466        B. Hebbard  Initial creation.
 * 05/23/12     785        Q. Zhou     Added getName for legend.
 * 17 Aug 2012  655        B. Hebbard  Added paintProps as parameter to IDisplayable draw
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
public class HrcnResource extends AbstractNatlCntrsResource<HrcnResourceData, NCMapDescriptor>
                                implements INatlCntrsResource {
	
	private HrcnResourceData hrcnResourceData;

    //  A map from an identifier string (which is unique within a single frame
    //  to a structure for a single displayable element
	
    private class FrameData extends AbstractFrameData {

    	protected HashMap<String, Tcm> getTcmNameToDrawableElementMap() {
			return tcmNameToDrawableElementMap;
		}

		protected void setTcmNameToDrawableElementMap(
				HashMap<String, Tcm> tcmNameToDrawableElementMap) {
			this.tcmNameToDrawableElementMap = tcmNameToDrawableElementMap;
		}

		private HashMap<String, Tcm> tcmNameToDrawableElementMap;

		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
			tcmNameToDrawableElementMap = new HashMap<String,Tcm>();
    	}

        public boolean updateFrameData(IRscDataObject rscDataObj) {
        	
        	//  Sanity I
        	if( !(rscDataObj instanceof DfltRecordRscDataObj) ) {
        		System.out.println("Hrcn.updateFrameData expecting DfltRecordRscDataObj " 
        			+ " instead of: " + rscDataObj.getClass().getName() );
        		return false;
        	}
        	
        	PluginDataObject pdo = ((DfltRecordRscDataObj)rscDataObj).getPDO();

        	//  Sanity II
        	
        	if( pdo == null ) {
        		System.out.println("Hrcn.updateFrameData expecting TcmRecord " 
        			+ " instead of: " + "null" );
        		return false;
        	}
        	
        	//  Sanity III
        	if( !(pdo instanceof TcmRecord) ) {
        		System.out.println("Hrcn.updateFrameData expecting TcmRecord " 
        			+ " instead of: " + pdo.getClass().getName() );
        		return false;
        	}
        	
        	TcmRecord tr = (TcmRecord) pdo;
        	Tcm tcm = buildTcmDrawableElement(tr);
        	
        	tcmNameToDrawableElementMap.put(tr.getStormName(), tcm);
        	
        	return true;
        }
    }
    
    private IFont font=null;
    
    //  ------------------------------------------------------------
    
    /**
     * Create a HRCN resource.
     * 
     * @throws VizException
     */
    public HrcnResource(HrcnResourceData resourceData,
    		LoadProperties loadProperties) throws VizException {
    	super(resourceData, loadProperties);
    	hrcnResourceData = (HrcnResourceData) resourceData ;
    }

    protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
    	return (AbstractFrameData) new FrameData( frameTime, timeInt );
    }
    
    public void initResource(IGraphicsTarget grphTarget) throws VizException {
    	queryRecords();
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

        	// For each storm that has a TCM issued as of the frame time...
        	
        	for (Tcm tcm : currFrameData.getTcmNameToDrawableElementMap().values()) {
        		
        		// ...display TCM PGEN object for all forecast hours.

        		DisplayElementFactory df = new DisplayElementFactory(target, getNcMapDescriptor());
        		ArrayList<IDisplayable> displayEls = df.createDisplayElements(tcm, paintProps);
        		for (IDisplayable each : displayEls) {
        			each.draw(target, paintProps);
        			each.dispose();
        		}
        	}
/*
    		//  Put per-hour options into arrays for easy lookup
			final Boolean[] enables    = {hrcnResourceData.hour0Enable,
					                      hrcnResourceData.hour1Enable,
					                      hrcnResourceData.hour2Enable};
			final RGB[]     colors     = {hrcnResourceData.hour0Color,
					                      hrcnResourceData.hour1Color,
					                      hrcnResourceData.hour2Color};
			final int[]     lineWidths = {hrcnResourceData.hour0LineWidth,
					                      hrcnResourceData.hour1LineWidth,
					                      hrcnResourceData.hour2LineWidth};
            final Boolean[] sequenceIdEnables = {hrcnResourceData.hour0sequenceIdEnable,
            		                             hrcnResourceData.hour1sequenceIdEnable,
            		                             hrcnResourceData.hour2sequenceIdEnable};

            
            
            
    		//  Loop through the (preprocessed) convective SIGMET data records
    		//  (This should be fast.)
    		Collection<HrcnRscDataObj> hrcnDataValues = currFrameData.hrcnDataMap.values();
    		
    		for (HrcnRscDataObj hrcnData : hrcnDataValues) {

    			//  Check for invalid time range
    			//  TODO:  See if this is still needed/valid...
    			//  if (activeFrameTime.compareTo(hrcnData.startTime) < 0 ||
    	        //      activeFrameTime.compareTo(hrcnData.endTime) >= 0) continue;
						
			    //  Just some 'safety' defaults 
				boolean enable = false;
			    RGB color = new RGB (155, 155, 155);
			    int lineWidth = 2;
			    boolean sequenceIdEnable = true;

			    //  Are we moving?  (Decoder uses negative numbers to say no.)
			    boolean inMotion = (hrcnData.direction >= 0) && (hrcnData.speed > 0);
			    
				for (int hour = 0; hour <= 2; hour++) {
					switch (hrcnData.classType) {
					    case AREA:
					    case LINE:
					    case ISOL:      //  these (may) have motion; set hourly parameters
						    enable = enables[hour] && (hour == 0 || inMotion);
						    color = colors[hour];
						    lineWidth = lineWidths[hour];
						    sequenceIdEnable = sequenceIdEnables[hour];
						    break;
					    case OUTLOOK:   //  no motion; draw only zero hour (if enabled)
						    enable = (hour == 0) && hrcnResourceData.getOutlookEnable();
						    color = hrcnResourceData.getOutlookColor();
						    lineWidth = hrcnResourceData.getOutlookLineWidth();
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
						for (int i=0; i<hrcnData.numPoints; i++) {
							LatLonPoint currentPoint = hrcnData.points[i];
							if (currentPoint == null) continue;  //  gracefully skip over omitted points (say, location lookup failure)
							if (hour > 0) {
								//  extrapolate position in future
								double headingRadians = Math.toRadians((double) (360 - (hrcnData.direction + 180) % 360));  // TO instead of FROM; CCW instead of CW);
								double distanceRadians = hour * hrcnData.speed * ONE_NM_RADIANS;
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
							else if (hrcnData.numPoints == 1) {  //TODO:  Check for classType ISOL instead or in addition?
								//  single point; draw marker and circle
								double delta = offsetY * 0.3;  //  tune to match NMAP
								target.drawLine(currLoc.getX()-delta, currLoc.getY(), currLoc.getZ(),
										        currLoc.getX()+delta, currLoc.getY(), currLoc.getZ(), color, lineWidth);
								target.drawLine(currLoc.getX(), currLoc.getY()-delta, currLoc.getZ(),
										        currLoc.getX(), currLoc.getY()+delta, currLoc.getZ(), color, lineWidth);
								double radius = hrcnData.distance / 2.0;
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
								if (hrcnData.classType == ClassType.OUTLOOK) {
									// Prevent label overlap when West & Central OR Central & East outlook
									// polygons have coincident top points, by flipping some text to left
									String outlookLabel = hrcnData.sequenceID + " OUTLOOK";
									if (hrcnData.sequenceID.endsWith("C") && longitudeAtTopLocation > -095.0  ||    //TODO:  Tune longitude boundary?
										hrcnData.sequenceID.endsWith("W") && longitudeAtTopLocation > -112.0	) {   //TODO:  Tune longitude boundary?
										horizontalAlignment = HorizontalAlignment.RIGHT;
										outlookLabel += "  ";
										}
									labelList.add(outlookLabel);
								}
								else {
								    labelList.add(hrcnData.sequenceID);
								}
							}
							
							if (hour == 0 && hrcnData.classType != ClassType.OUTLOOK) {
								if (hrcnResourceData.timeEnable) {
									String endTimeS = hrcnData.endTime.toString();
									labelList.add(endTimeS.substring(8, 10) + "/"  // date
												+ endTimeS.substring(11, 13)       // hour
												+ endTimeS.substring(14, 16));     // minute
								}

								if (hrcnResourceData.flightLevelEnable) {
									labelList.add("FL" + hrcnData.flightLevel);
								}

								if (hrcnResourceData.motionEnable && inMotion) {
									labelList.add(String.format("%03d",  //  leading zeroes for direction
											      hrcnData.direction) + " " +
											      hrcnData.speed + "kt");
								}

								if (hrcnResourceData.intensityEnable && hrcnData.intensity != null &&
										              !hrcnData.intensity.isEmpty()) {
									labelList.add(hrcnData.intensity);
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
    		*/
    	}
    	font.dispose();
    	font = null;
    }

    private Tcm buildTcmDrawableElement(TcmRecord tr) {
    	//  Generate a Tcm (DrawableElement) to represent all
    	//  forecast hours covered by the TcmRecord (PDO).
    	
    	Tcm t = new Tcm(
    			tr.getStormType(),
    			toInt(tr.getStormNumber()),
    			toInt(tr.getAdvisoryNumber()),
    			tr.getStormName(),
    			tr.getBasin(),
    			tr.getEyeSize() == null ? 0 : tr.getEyeSize(),  //TODO: check this out!!
    			tr.getPositionAccuracy() == null ? 0 : tr.getPositionAccuracy(),  //TODO: check this out!!
    			tr.getCorr(),
    			tr.getObsTime(),
    			tr.getCentralPressure());
    	
    	List<TcmFcst> ltf = new ArrayList<TcmFcst>();
    	for (TcmPositionWinds tpw : tr.getTcmPosWinds()) {
    		TcmFcst tf = buildTcmFcstDrawableElement(tpw);
    		ltf.add(tf);
    	}
		Collections.sort(ltf, new Comparator<TcmFcst>() {
			public int compare (TcmFcst tf1, TcmFcst tf2) {
				return tf1.getFcstHr() - tf2.getFcstHr();
			}
		});
		t.setTcmFcst(ltf);
    	
    	TcmWindQuarters twq = new TcmWindQuarters(new Coordinate(0.0, 0.0), 0, 1.0, 1.0, 1.0, 1.0);
    	t.setWaveQuatro(twq);  
    	
    	return t;
    	
    }

    private TcmFcst buildTcmFcstDrawableElement(TcmPositionWinds tpw) {
    	//  Generate a single TcmFcst (DrawableElement) to represent a given
    	//  TcmPositionWinds -- both correspond to one forecast hour.
    	
    	TcmFcst tf = new TcmFcst (
    			new Coordinate(tpw.getClon(), tpw.getClat()),
    			toInt(tpw.getFcstHour().replace("F","")),  //TODO:  Check replace/replaceAll/replaceFirst, etc...
    			buildQuatros(tpw));
    	
    	tf.setGust(tpw.getGust());
    	tf.setWindMax(tpw.getWindMax());
    	tf.setDirection(tpw.getStormDrct());
    	tf.setSpeed(tpw.getStormSped());
    	
    	return tf;
    }
    
    private double[][] buildQuatros(TcmPositionWinds tpw) {
    	//  Generate "quatros" arrays, given a TcmPositionWinds.
    	//  These are needed to feed to the constructor for TcmFcst
    	//  (the only way to get them in there!)
    	
    	final double[][] quatros = {
    			{ toDouble(tpw.getNe34k()), toDouble(tpw.getNe50k()), toDouble(tpw.getNe64k()) },
    			{ toDouble(tpw.getSe34k()), toDouble(tpw.getSe50k()), toDouble(tpw.getSe64k()) },
    			{ toDouble(tpw.getSw34k()), toDouble(tpw.getSw50k()), toDouble(tpw.getSw64k()) },
    			{ toDouble(tpw.getNw34k()), toDouble(tpw.getNw50k()), toDouble(tpw.getNw64k()) }
    	};
    	
    	return quatros;
    }
    
    //  A couple of utility conversion routines, needed because there are cases
    //  where we need numbers where the decoder hands us strings.
    //  TODO:  Consider doing this in decoder.
    //  TODO:  Are there public utilities to do this with graceful exception handling?
    
    double toDouble(String s) {
    	Double d;
    	try {
    		d = Double.parseDouble(s);
    	}
    	catch (NullPointerException e) {
    		//TODO:  Handle this more gracefully
    		d = 0.0;
    	}
    	catch (NumberFormatException e) {
    		//TODO:  Log as bogus decoder output
    		d = 0.0;
    	}
    	return d;
    }

    int toInt(String s) {
    	int i;
    	try {
    		i = Integer.parseInt(s);
    	}
    	catch (NullPointerException e) {
    		//TODO:  Handle this more gracefully
    		i = 0;
    	}
    	catch (NumberFormatException e) {
    		//TODO:  Log as bogus decoder output
    		i = 0;
    	}
    	return i;
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
		if (fd == null || fd.getFrameTime() == null || fd.tcmNameToDrawableElementMap.size() == 0) {
			return legendString + "-No Data";
		}
		return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
	}
}