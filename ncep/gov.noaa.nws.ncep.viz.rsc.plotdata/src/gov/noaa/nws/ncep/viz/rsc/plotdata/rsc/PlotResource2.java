/*****************************************************************************************
 * COPYRIGHT (c), 2006-2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.common.preferences.GraphicsAreaPreferences;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.PlotModelGenerator2;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.StaticPlotInfoPV;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.StaticPlotInfoPV.SPIEntry;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.IPreferenceStore;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.geometry.DirectPosition;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.pointdata.vadriver.VA_Advanced;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotAlertParser;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.rsc.retrieve.PointDataPlotInfoRetriever;
import com.vividsolutions.jts.geom.Coordinate;
import gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.GraphicsAreaCoordinates;

import static java.lang.System.out;
/**
 * Provides a resource that will display plot data for a given reference time.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  11/20/2006             brockwoo    Initial creation.
 *  02/17/2009             njensen     Refactored to new rsc architecture.
 *  03/17/2009      2105   jsanchez    Plot goessounding/poessounding availability.
 *  03/30/2009      2169   jsanchez    Updated initNewFrame.
 *  04/09/2009       952   jsanchez    Plot acars.   
 *  04/13/2009      2251   jsanchez    Plot profilers. 
 *  04/21/2009             chammack    Refactor to common pointData model
 *  04/28/2010     #275    ghull       Refactor raytheon's class to work with 
 *                                     AbstractNatlCntrsResource
 *  07/28/2010	   #291	   gzhang	   corrected Plot position in paintFrame()   
 *  10/04/2010     #307    ghull       PlotInfoRscDataObj wrapper for PlotInfo            
 *  03/07/2011     migration ghull     use AbstractDbPlotInfoRetriever; for now we are not 
 *                                     using the resourceChanged version of getStations.
 *  04/25/2011     n/a     bhebbard    Check for null station.distValue in run()
 *  04/27/2001	   #361    xguo        Display parameter list
 *  09/14/2011     #457    sgurung     Renamed H5 to nc
 *  09/20/2011     #459    ghull       use lat,lon as map key instead of stationId since
 *                                     the stationId is no longer uniq in all cases.
 *  10/19/2011             ghull       remove special ncuair PlotInfoRetriever and AlertParsers.                                 
 *  11/01/2011     #482    ghull       progressive disclosure fixes, rm mesowest/metar enabled. 
 *  12/05/2011             sgurung     Added method isStationMapEmpty
 *  12/07/2011     #529    bhebbard    Change "plotAll" criteria for new user "Plot All" option
 *  12/16/2011     #529    bhebbard    Suppress (for now) pre-draw check for non-timematching
 *                                     stations, due to undesirable "blinking" effect
 *  02/16/2012     #555    sgurung     Changed setPopulated() to setPopulated(true) in populateFrame().
 *  02/16/2012     #639    Q.Zhou      Changed maxDensity to 3.0(Could do 4 or 5 if needed)
 *  04/02/2012     #615    sgurung     Use modified version of PlotModelGenerator2 constructor
 *  05/18/2012     #809    sgurung     Use a separate PlotModelGenerator2 thread to create plots for stations
 *  								   within a predefined Data Area (from Preferences) but outside
 *  								   of the current display area to improve panning performance
 *  05/23/2012     785     Q. Zhou     Added getName for legend.
 *  08/22/2012     #809    sgurung     For bgGenerator thread, add stations to queue only when zoomLevel > 0.10
 *  								   (this fixes the issue of slow performance when zooming all the way in, when Data Area is set)	
 *  11/04/2012     #944    ghull       add query for Fcst Plot resources
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class PlotResource2 extends AbstractNatlCntrsResource<PlotResourceData, MapDescriptor> 
	   implements  IResourceDataChanged, IPlotModelGeneratorCaller, INatlCntrsResource {
	
	protected PlotResourceData plotRscData = null;
	
	//
	protected class PlotInfoRscDataObj implements IRscDataObject {
		private PlotInfo plotInfo;
		
		public PlotInfoRscDataObj( PlotInfo pltInfo ) {
			plotInfo = pltInfo;
		}
		
		@Override
		public DataTime getDataTime() {
			return plotInfo.dataTime;
		}		
		
		public PlotInfo getPlotInfo() {
			return plotInfo;
		}
	}
	
    private class ProgDisc extends Job {

        private boolean isRunning = false;

        private boolean newData = false;

        private List<Station> lastComputed = new ArrayList<Station>();
        
         public ProgDisc() {
            super("progressive disclosure");
            this.setSystem(true);
        }

        public void update() {
            if (isRunning == false) {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        schedule();
                    }
                });
            } else {
                newData = true;
            }
        }

        boolean cleared = false;

        public List<Station> getLastComputed() {
            if (cleared) {
                lastComputed.clear();
                cleared = false;
            }
            return lastComputed;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            isRunning = true;
            do {
                newData = false;
                FrameData frameData = (FrameData) getCurrentFrame();
                PaintProperties theseProps = new PaintProperties(lastProps);
                IExtent extent = theseProps.getView().getExtent();
               
                int displayWidth = (int) (descriptor.getMapWidth() * theseProps
                        .getZoomLevel());
                double kmPerPixel = (displayWidth / canvasWidth) / 1000.0;
                
                // currently should not happen since the generator is currently determining the plotWidth and
                // NatlCntrs doesn't use the magnification.
                if (plotWidth != actualPlotWidth * magnification) {
                    plotWidth = actualPlotWidth * magnification;
                    generator.setPlotModelSize(Math.round(plotWidth));
                    bgGenerator.setPlotModelSize(Math.round(plotWidth));
                }
                double displayHintSize = plotRscData.getPixelSizeHint() * magnification;
                
                double threshold = (displayHintSize * kmPerPixel) / density;
                
                boolean plotAll = ( density > MAX_DENSITY || threshold < distFloor  );
                
                LinkedList<Station> stationList = new LinkedList<Station>();
                LinkedList<Station> bgStationList = new LinkedList<Station>();
                List<String> toRemove = new ArrayList<String>();
                
                for (String s : frameData.stnLocList) {
                    Station station = frameData.stationMap.get(s);
                    Coordinate location = station.pixelLocation;
                    
                    if (station.info != null) {
                    	DirectPosition stnPos = getStationPosition(station);
                    
                    	if ((dataAreaEnvelope == null || stnPos == null) && !extent.contains(new double[] { location.x,location.y })) {
                        	continue;
                        }                    	
                    	else if (dataAreaEnvelope != null && stnPos != null && !dataAreaEnvelope.contains(stnPos) && !extent.contains(new double[] { location.x, location.y })) {
                        	continue;
                        }
                    	
                    	// the distValues will be null for autoUpdated stations from the PlotAlertParser.
                        // 
                        if( station.distValue == null ) {
                        	//numAutoUpdateStations++;
//                        	out.println("station.distValue = null for stn "+station.info.stationId );
                        }
                        else if( plotAll || station.distValue >= threshold ) {
                            if (extent.contains(new double[] { location.x, location.y })) {
                            	stationList.addLast(station);  
                        	}
                            else {
                            	bgStationList.addLast(station);
                            }
                        }
                    } 
                    else {  // why would this be null ??? also remove from stn map?
                        toRemove.add(station.info.stationId);
                    }
                }

                if( !toRemove.isEmpty() ) {
                	System.out.println("removing "+ toRemove.size() + " stations from list");
                }
                
//                frameData.stnLocList.removeAll(toRemove);
//                for( String rmStn : toRemove ) {
//                	frameData.stationMap.
//                }
                
//                if (runtimeProgDisc) {
//                	frameData.calculateProgDiscRuntime(stationList, threshold);
//                }
                
                List<Station> imageStations = new ArrayList<Station>();
                List<PlotInfo> newStations = new ArrayList<PlotInfo>();
                
                for (Station station : stationList) {               	
                	 if (station.plotImage == null) {
                         if (generator.isQueued(station.info) == false) {
                            newStations.add(station.info);
                         }
                     } else {
                         imageStations.add(station);
                     }
                }
                // for stations within the current display area
                generator.queueStations(newStations);
                
                if (theseProps.getZoomLevel() > 0.10) {
                newStations = new ArrayList<PlotInfo>();
                for (Station station : bgStationList) {
                    if (station.plotImage == null) {
                        if (bgGenerator.isQueued(station.info) == false) {
                           newStations.add(station.info);
                        }
                    } else {
                        imageStations.add(station);
                    }
                }

                // for stations within the data area (from Preferences) but outside of the current display area
                bgGenerator.queueStations(newStations);
                }
                
                synchronized (this) {
                    lastComputed.clear();
                    lastComputed.addAll(imageStations);
                    issueRefresh();
                }
            } while (newData);
            isRunning = false;
            return Status.OK_STATUS;
        }
    }

    private boolean imagesArrived = false;

//    private boolean runtimeProgDisc = false;

    private ProgDisc disclosureThread = null;

    private PixelExtent worldExtent;

    private PlotModelGenerator2 generator;
    
    private PlotModelGenerator2 bgGenerator;

    private VA_Advanced progDisc;

    private double actualPlotWidth;

    private double plotWidth;

    private MathTransform transform = null;

    private int canvasWidth;

    private double magnification;

    private double zoomLevel;

    private double density;
    static final float MAX_DENSITY = 3.0f;

    private PaintProperties lastProps;
    private FrameData       lastFrameData;

    private StaticPlotInfoPV spi;
    
    private double distFloor;

    private double screenToWorldRatio;

    private boolean needsUpdate = false;    
    
    private final ISchedulingRule mutexRule = new MutexRule();
    
    private MathTransform WGS84toPROJCRS = null; 
    
    private GeneralEnvelope dataAreaEnvelope = null;
    
	public class Station {
        PlotInfo info;

        IImage plotImage;

        Double distValue;

        Double origDistValue;

        Coordinate pixelLocation;

        Integer goodnessValue;

        double[] projCoords;
    }

    public class FrameData extends AbstractFrameData {
    	// map from the station Id to the station info (plotInfo and image)
        private Map<String, Station> stationMap = new HashMap<String, Station>();
        
        // list of key values in the stationMap representing the lat/lons of the Station. 
        private List<String> stnLocList = new ArrayList<String>();

//      private IWireframeShape stnLocWF=null;

        private int     uniqueueStations;
		private int     dynStations;
		  		
        protected FrameData( DataTime time, int interval ) {
			super(time, interval);
	        uniqueueStations = 0;
	        dynStations = 0;
        }

		@Override
		public boolean updateFrameData( IRscDataObject rscDataObj ) {
			if( !(rscDataObj instanceof PlotInfoRscDataObj) ) { // sanity check
				return false;
			}
			
			PlotInfo plotInfo = ((PlotInfoRscDataObj)rscDataObj).getPlotInfo();
			
			// TODO : If the dataTime doesn't get set in the data query. 
			// (Don't think this should happen anymore)
			if( plotInfo.dataTime == null ) { 
				plotInfo.dataTime = getFrameTime();
				out.println("dataTime from plotInfo is null. Setting to FrameTime");
			}
			
			String stnMapKey = getStationMapKey( plotInfo.latitude,
			                   plotInfo.longitude );
			
    		Station stn = stationMap.get( stnMapKey );// plotInfo.stationId );

    		// This can happen during an auto update or if there are multiple reports for this frame.
    		//
    		if( stn != null ) {
//        		if( stn.info.latitude != plotInfo.latitude || 
//        			stn.info.longitude != plotInfo.longitude ) {
//        			out.println("??Updating PlotInfo for station " + stn.info.stationId +
//        					" with different lat/lon???" + stn.info.latitude +"," + stn.info.longitude + "and " +
//        					plotInfo.latitude + "," + plotInfo.longitude );
//        		}
        		if( stn.info == null ) { // shouldn't happen
        			out.println("Sanity check: Found existing Station in stationMap with a null plotInfo???");
        		}
        		else {
        			//out.println(" updating station info "+stn.info.dataURI+ " with " + plotInfo.dataURI );
        			
        			if( !stn.info.stationId.equals( plotInfo.stationId ) ) {
        				out.println("2 stations "+ stn.info.stationId +" and " +
        						plotInfo.stationId + " have the same location?"	);
        			}
        			// if these are the same time, should we check which one should be used, 
        			// (or combine them?) How to determine which to use?
        			//
        			else if( stn.info.dataTime.getValidTime().getTimeInMillis() != 
        				          plotInfo.dataTime.getValidTime().getTimeInMillis() ) {
//        				out.println(" station "+ plotInfo.stationId+ " has 2 times in same frame");
//        				out.println(stn.info.dataTime+ " and " + plotInfo.dataTime );
        				
        				// determine the best time match.
        				if( timeMatch( plotInfo.dataTime ) < timeMatch( stn.info.dataTime ) ) {
        					stn.info = plotInfo;
            				out.println("Replacing Station: "+
            						stn.info.stationId+ ". at "+ stn.info.dataTime.getRefTime() 
            					   + " with "+ plotInfo.dataTime.getRefTime() );

        					if( stn.plotImage != null ) {
        	        			out.println( "    Replacing image ");
        	        			stn.plotImage.dispose();
        	        			stn.plotImage = null;
        	        		}
        				}
        			}
        		}
        		
        	} else {
        		stn = new Station();
        		
        		if( plotInfo.stationId == null ) {
        			plotInfo.stationId = plotInfo.latitude + "#" + plotInfo.longitude;
        		}
        		stn.info = plotInfo;
        		
        		calcStaticStationInfo( stn );
        	}
    		
			return true;
		}
		
		// if we are painting a frame which has not yet been loaded with data then we
		// need to request data for this frame and then
		// update the frames with 
		protected boolean populateFrame() throws VizException {	
			
	        dynStations = 0;
	        uniqueueStations = 0;
	        
	        HashMap<String, RequestConstraint> metadataMap = 
	        	new HashMap<String, RequestConstraint>(
	                plotRscData.getMetadataMap() );

	        RequestConstraint timeConstraint = new RequestConstraint();
	        
        	ResourceName rscName = getResourceData().getResourceName();
	        boolean isFcst = plotRscData.isForecastResource() &&
	        				   !plotRscData.getPluginName().equals("nctaf");
	        
	        // set the constraints for the query. 
	        // for forecast resources we need to match the cycle time and
	        // create a range constraint on the forecast hour.
	        if( isFcst ) {
	        	String[] dts = rscName.getCycleTime().toString().split(" ");
	        	String cycleTimeStr = dts[0] + " " + dts[1].substring(0, dts[1].length()-2);
	        	System.out.println("reftime constraint is:"+cycleTimeStr );

	        	timeConstraint = new RequestConstraint( cycleTimeStr );
	        	metadataMap.put("dataTime.refTime", timeConstraint );

	        	// determine the fcst hr range for the frame span
	        	long refTimeMs = rscName.getCycleTime().getRefTime().getTime();
	        	long frmStartMs = getFrameStartTime().getRefTime().getTime();
	        	long frmEndMs   = getFrameEndTime().getRefTime().getTime();

	        	long beginFcstHr = (frmStartMs - refTimeMs) / 1000;
	        	long endFcstHr   = (frmEndMs - refTimeMs) / 1000;

	        	timeConstraint = new RequestConstraint( );
	        	timeConstraint.setBetweenValueList( 
	        			new String[] { Long.toString( beginFcstHr ), 
	        					Long.toString( endFcstHr ) } );
	        	
		        timeConstraint.setConstraintType( ConstraintType.BETWEEN );
				metadataMap.put( "dataTime.fcstTime", timeConstraint );
	        }
			else {
	        String[] constraintList = { startTime.toString(), endTime.toString() };
				timeConstraint.setBetweenValueList( constraintList );
				timeConstraint.setConstraintType( RequestConstraint.ConstraintType.BETWEEN );

				metadataMap.put("dataTime", timeConstraint );
			}

	        long t0 = System.currentTimeMillis();
	        
	        List<PlotInfo> plotInfoObjs = plotRscData.getPlotInfoRetriever().getStations(
	        								 metadataMap);
	        
	        long t1 = System.currentTimeMillis();
	        out.println("InitFrame Took: " + (t1 - t0) + " To find "+ 
	        		plotInfoObjs.size() + " Stations ( entries in metadata DB.)" );

	        for( PlotInfo pltInfo : plotInfoObjs ) {	

	        	// have to add this since the dataTime is not always getting set by the 
	        	// plotInfoRetriever?
				if( pltInfo.dataTime == null ) {
					if( plotRscData.isSurfaceOnly() ) {
						pltInfo.dataTime = getFrameTime();
					}
					else { // upper air data uses the NcSoundingQuery which needs a start and stop time for 
						   // the query which needs to be correct.
						out.println("Error getting dataTime from Plot Query");
					}
				}

	        	for( IRscDataObject rscDataObj : processRecord( pltInfo )) {
	        		// sanity check: this should always be true since we constrained the
	        		// query with the start/end frame times.
	        		if( isRscDataObjInFrame( rscDataObj ) ) {
	        			updateFrameData( rscDataObj );
	        		}
	        		else {
	        			// if we don't change the plotInfoRetriever to set the dataTime, then this
	        			// really is a serious sanity check.....
	        			// This happens when the dataTime is the same as the startTime. This satisfies the query constraint
	        			// the not the time matching.
	        			if( plotRscData.isForecastResource() ) {
	        				out.println("plotInfo obj doesn't time match to the frame which queried it???");
	        			}
	        			// else this is possible when the time is equal to one of the start/end times
	        		}
	        	}
	        }

//	        out.println( "Number of dynamic stations is " + dynStations );
//	        out.println( "Number of uniq stations is " + stationMap.size() );
	        
	        if( stnLocList.size() != stationMap.keySet().size() ) {
	        	out.println("Sanity check: station lists out of sync???");
	        }
	        
	        calculateProgDisc();

	        setPopulated( true );

	        return true;
	    }
		
		// 
	    private boolean calcStaticStationInfo(Station station ) {
	        SPIEntry obsStation = null;
	        Coordinate thisLocation = null;
	        Coordinate thisPixelLocation = null;
	        if (spi != null) {
	            obsStation = spi.getSPIEntry(station.info.stationId);
	        }
	        if (obsStation != null) {
	            thisLocation = obsStation.latlon;
	            double[] thisLocationLatLon = { thisLocation.x, thisLocation.y };
	            double[] thisLocationPixel = descriptor
	                    .worldToPixel(thisLocationLatLon);
	            if (!worldExtent.contains(thisLocationPixel[0],
	                    thisLocationPixel[1])) {
	                return false;
	            }
	            thisPixelLocation = new Coordinate(thisLocationPixel[0],
	                    thisLocationPixel[1]);
	            if (obsStation.distance < distFloor) {
	                station.origDistValue = distFloor; // minDist;
	            } else {
	                station.origDistValue = obsStation.distance;
	            }
	        } 
	        else {
	            thisLocation = new Coordinate(station.info.longitude,
	                    station.info.latitude);
	            double[] thisLocationLatLon = { thisLocation.x, thisLocation.y };
	            double[] thisLocationPixel = descriptor
	                    .worldToPixel(thisLocationLatLon);
	            if (thisLocationPixel == null
	                    || !worldExtent.contains(thisLocationPixel[0],
	                            thisLocationPixel[1])) {
	                return false;
	            }
	            thisPixelLocation = new Coordinate(thisLocationPixel[0],
	                    thisLocationPixel[1]);
	            station.origDistValue = -1.0;
	            dynStations++;
	        }
	        
	        station.goodnessValue = 0;
	        station.pixelLocation = thisPixelLocation;
	        
	        String stnMapKey = getStationMapKey( station.info.latitude,
				      							 station.info.longitude );
	        
	        if( stationMap.put( stnMapKey, station) == null ) {
	        	
	        	stnLocList.add( stnMapKey );	        	
	            uniqueueStations++;
	        }
	        else {
	        	out.println("Updating StationMap	with " +stnMapKey );
	        }
	        return true;
	    }

	 // if it turns out that we need this, see raytheon's new DynamicProgDisclosure class

//	    private void calculateProgDiscRuntime(List<Station> stations, double threshold) {
//	    	
//	        double kmPerPixel = threshold * density
//	                / (plotRscData.getPixelSizeHint() * magnification);
//
//	        // get meters per pixel
//	        double mPerPixel = kmPerPixel * 1000;
//	        double pixelDist = mPerPixel * plotRscData.getPixelSizeHint() / 2;
//
//	        pixelDist /= density;
//
//	        List<Station> toUse = new ArrayList<Station>();
//	        for (Station a : stations) {
//	        	
//	            boolean use = true;
//	            
//	            if( a.projCoords == null ||
//	            	a.projCoords.length < 3 ) {
//	            	continue;
//	            }
//	            
//	            for (Station b : toUse) {
//	                // Make sure a does not overlap with b
//	            	
//	                double deltaX = a.projCoords[0] - b.projCoords[0];
//	                double deltaY = a.projCoords[1] - b.projCoords[1];
//	                // Absolute value logic inlined for performance
//	                deltaX = (deltaX <= 0.0D) ? 0.0D - deltaX : deltaX;
//	                deltaY = (deltaY <= 0.0D) ? 0.0D - deltaY : deltaY;
//
//	                if (deltaX < pixelDist && deltaY < pixelDist) {
//	                    use = false;
//	                    break;
//	                }
//	            }
//	            
//	            if (use) {
//	                toUse.add(a);
//	            }
//	        }
//	        
//	        stations.clear();
//	        stations.addAll(toUse);
//	    }

	    // set the distValues in the stationMap.
	    // called from populateFrame. 
	    // TODO what about stations that come in through auto update?
	    protected void calculateProgDisc() {

	    	if( stnLocList.isEmpty() ) {
	    		return;
	    	}
	    	
	    	int size = stnLocList.size();
	    	Coordinate[] latLonArray = new Coordinate[size];
	    	Integer[] goodnessArray = new Integer[size];
	    	Double[] distArray = new Double[size];
	    	int i = 0;
	    	for (String s : stnLocList ) {
	    		Station station = stationMap.get(s);
	    		latLonArray[i] = new Coordinate( 
	    				station.info.longitude, station.info.latitude);
	    		goodnessArray[i] = station.goodnessValue;
	    		distArray[i] = station.origDistValue;

	    		++i;
	    	}

	    	progDisc.setVaJustGoodness(false);
	    	progDisc.setVaDistPass( dynStations * dynStations / size < 3000.0 );
	    	progDisc.getVaAdvanced( latLonArray, goodnessArray, distArray );

	    	for (i = 0; i < size; ++i) {	    		
	    		stationMap.get( stnLocList.get(i) ).distValue = distArray[i];
	    	}

// if it turns out that we need this, see raytheon's new DynamicProgDisclosure class
//	        } else {
//	            if (stnLocList.size() == 0) {
//	                return;
//	            }
//	            for (Station s : stationMap.values()) {
//	                s.distValue = -1.0;
//	                if (s.projCoords == null) {
//	                    double[] in = new double[] { s.info.longitude, s.info.latitude, 0 };
//	                    double[] out = new double[3];
//	                    
//	                    try {
//	                        transform.transform(in, 0, out, 0, 1);
//	                    } catch (TransformException e) {
//	                        e.printStackTrace();
//	                    }
//	                    s.projCoords = out;
//	                }
//	            }
//
//	            runtimeProgDisc = true;
//	        }
//	        computeProgDisc = false;
	    }
	    
	    public void modelGenerated(PlotInfo key, IImage image) {
	    	String stnMapKey = getStationMapKey(key.latitude, key.longitude);
	    	Station stn = stationMap.get( stnMapKey );
	    	if( image == null ||  stn == null ) {
	    		return;
	    	}
	    	
	    	// I think this might happen if a prog disclosure is run twice before
	    	// the image is generated
	    	if( stn.plotImage != null ) {
	    		out.println("modelGenerated image for existing Station?"+
	    				key.stationId );
	    		stn.plotImage.dispose();
	    		stn.plotImage = null;
	    	}
	    	stn.plotImage = image;
	    }
	    
	    public void dispose() {
	    	super.dispose();
	    	
            for (String stnLoc : stnLocList ) {
                Station s = stationMap.get( stnLoc );
                if (s != null && s.plotImage != null) {
                    s.plotImage.dispose();
                    s.plotImage = null;
                }
                stationMap.remove( stnLoc );
            }
            stationMap.clear();
            stnLocList.clear();            
            populated = false;
            
            dynStations = 0;
            uniqueueStations = 0;
	    }	
	    
	    public boolean isStationMapEmpty() {	    	
	    	if (stationMap.isEmpty())
	    		return true;
	    	else
	    		return false;
	    }
    }

    /**
     * Create a surface plot resource.
     * 
     * @param target
     *            The graphic target to draw to
     * @param refTime
     *            The reference time to request data against
     * @throws VizException
     */
    public PlotResource2(PlotResourceData data, LoadProperties props) {
        super(data, props);
        plotRscData = (PlotResourceData) resourceData;
        
        // The object which is called by the NcAutoUpdater to get a PlotInfo
        // object from the alert uri.
        if( data.getAlertParser() == null ) {
  	 		data.setAlertParser( new PlotAlertParser() );
        }
        
        if( data.getPlotInfoRetriever() == null ) {
        	data.setPlotInfoRetriever(
            		new PointDataPlotInfoRetriever());
        }
                
        this.dataTimes =  new ArrayList<DataTime>();
        this.progDisc = new VA_Advanced();

//        data.addChangeListener(this);
        
        // allow for no SPI file to be given
        if (plotRscData.getSpiFile() != null && !plotRscData.getSpiFile().isEmpty() ) {
            this.spi = StaticPlotInfoPV.readStaticPlotInfoPV(plotRscData
                    .getSpiFile());
        }
    }

	@Override
	public String getName() {
		String legendString = super.getName();
		FrameData fd = (FrameData) getCurrentFrame();
		
		if (fd == null || fd.getFrameTime() == null || fd.isStationMapEmpty()) {
			return legendString + "-No Data";
		}
		
		if (legendString == null || legendString.equalsIgnoreCase("")) {
			return "Plot Data";
		}
		else {
			return legendString + " "+ NmapCommon.getTimeStringFromDataTime( fd.getFrameTime(), "/");
		}
	}
	

    // override to process PlotInfoRscDataObj instead of PlotInfo
    @Override
	protected IRscDataObject[] processRecord( Object pltInfo ) {
		if( !(pltInfo instanceof PlotInfo) ) {
			out.println( "PlotResource2.processRecord method expecting PlotInfoRscData objects "+
					"instead of: " + pltInfo.getClass().getName() );
			return new PlotInfoRscDataObj[0];
		}
		
		return new PlotInfoRscDataObj[]{ new PlotInfoRscDataObj( (PlotInfo)pltInfo ) };
	}
    
    protected void populateFrame( FrameData frameData ) throws VizException {
		if( !frameData.isPopulated() ) {
			frameData.populateFrame();
		}
    }
    
    public void paintFrame( AbstractFrameData fd,
            IGraphicsTarget aTarget, PaintProperties paintProps )
    		throws VizException {

    	if( needsUpdate ) {
			needsUpdate = false;
    	    initResource(aTarget);
    	}	
    	    	
        if (disclosureThread == null) {
            disclosureThread = new ProgDisc();
            disclosureThread.update();
        }
        
        // if zooming out, clear the stations to avoid the flickr
        //
    	if( paintProps.isZooming() ) {
    		//out.println("Zooming");
    		return; 
    	}
    	
    	FrameData frameData = (FrameData) fd;
    	
    	populateFrame( frameData );
    	
        boolean update = false;
        
        if( lastProps == null || frameData != lastFrameData ||
        		imagesArrived || lastProps.getView().getExtent().equals(
	        				                  paintProps.getView().getExtent()) == false ) {
        	update  = true;
        }
// a wireframe to show locations of stations with available data that arent plotted
//    	if( frameData.stnLocWF == null ) {
//    		frameData.stnLocWF = aTarget.createWireframeShape( true, this.descriptor );    		
//    	}


        lastProps = new PaintProperties(paintProps, (IView) paintProps.getView().clone());
        lastFrameData = frameData;
        canvasWidth = paintProps.getCanvasBounds().width;
        
        List<Station> stationList = null;
        
        if (disclosureThread == null) {
            disclosureThread = new ProgDisc();
            disclosureThread.update();
        } else if (update) {
            disclosureThread.update();
            imagesArrived = false;
        }
        synchronized (disclosureThread) {
            stationList = new LinkedList<Station>(disclosureThread
                    .getLastComputed());
        }
        
		this.screenToWorldRatio = paintProps.getCanvasBounds().width
				/ paintProps.getView().getExtent().getWidth();
		double scaleValue = this.plotWidth / screenToWorldRatio;


        for (Station station : stationList) {
            if (station.plotImage == null) {
                imagesArrived = true;
                continue;
            }
            // this happens briefly when a frame is changed and 
            // the progressive disclosure hasn't finished
//             ...but unfortunately that happens on first paint after *every* frame
//             change (...disclosureThread.update() above causes a schedule() of the
//             ProgDisc job, but it won't run until after triggering paint is finished...),
//             resulting in blanking of the display.  Subsequent paint of same frame
//             (after ProgDisc.run() completes) will show correct data for the frame,
//             bug this can lag -- esp. with high station density -- due to long times
//             to execute all those drawRaster's (or even newer drawRasters for whole
//             array of images).  This results in "blinking", which gets worse (longer
//             'dark' time) as number of stations increases.  For now, we conclude it's
//             better to eliminate the "blinking" (Task#529), and so comment out this
//             check -- the downside being that there will be a momentary mismatch
//             between the indicated frame time on the status bar (which changes very
//             quickly when frame change is initiated) and the displayed data (which
//             will still be for the previous frame).  Still, this is a more generic
//             problem which also affects other resources with slow draw times, so a
//             more general solution is needed.  (Apologies for long essay; feel free
//             to remove once decision is firmed up on this...)
//             
//          if( frameData.timeMatch( station.info.dataTime ) == -1) {
//              out.println("********non timematching station being plotted????!!!!!");
//            }
//            else {
                PixelCoverage pc = new PixelCoverage( station.pixelLocation, scaleValue, scaleValue );
                aTarget.drawRaster(station.plotImage, pc, paintProps, RasterMode.SYNCHRONOUS);
//            }
        }
    }


    public void initResource( IGraphicsTarget aTarget ) throws VizException {
    	
        // we may want to implement this as an attribute
        magnification = 1.0;//paintProps.getMagnification();
        
        density = plotRscData.getPlotDensity() / 10.0;
        
        dataAreaEnvelope = getDataAreaEnvelope();
        
        if( generator != null ) {
    		generator.cancel();
    		generator.cleanImages();
    	}
    	
    	// generator for creating plots for stations within the current display area
        generator = new PlotModelGenerator2(aTarget, descriptor,
                plotRscData.getPlotModel(), 
                (plotRscData.isSurfaceOnly() ? null : plotRscData.getLevelKey() ),
                plotRscData.getMetadataMap(),  plotRscData.getConditionalFilter(), this);  
        // apply mutexRule for scheduling this job to run first
        generator.setRule(mutexRule);        
        this.generator.setPlotMissingData(plotRscData.isPlotMissingData() );
  
        this.actualPlotWidth = this.plotWidth = generator.getPlotModelWidth();
        
        if( bgGenerator != null ) {
        	bgGenerator.cancel();
        	bgGenerator.cleanImages();
    	}
    	
        // generator for creating plots for stations within the data area but outside of the current display area
        bgGenerator = new PlotModelGenerator2(aTarget, descriptor,
                plotRscData.getPlotModel(), 
                (plotRscData.isSurfaceOnly() ? null : plotRscData.getLevelKey() ),
                plotRscData.getMetadataMap(),  plotRscData.getConditionalFilter(), this);       
        // apply mutexRule for scheduling this job to run after the first generator job is complete
        bgGenerator.setRule(mutexRule);        
        this.bgGenerator.setPlotMissingData(plotRscData.isPlotMissingData() );
       
        
        this.distFloor = ( descriptor.getMapWidth() / 1000.0)
        						* plotRscData.getPixelSizeHint() / 32000.0;
        // not sure where this calculation comes from but the above value yield
        distFloor /= 3; // 
        
        this.worldExtent = new PixelExtent(0, descriptor.getGridGeometry()
                .getGridRange().getHigh(0), 0, descriptor.getGridGeometry()
                .getGridRange().getHigh(1));
        
        try {
            transform = MapUtil.getTransformFromLatLon(descriptor.getCRS());
        } catch (FactoryException e) {
            throw new VizException("Error retrieving MathTransform", e);
        }

        // Most resources call queryRecords() here to populate the frames
        // but since this query may take a while whe postpone until a frame is 
        // painted and then populate just that frame. 
        
    }

    @Override
	public void disposeInternal() {
    	if( generator != null ) {
    		generator.shutdown();
    	}
    	if( bgGenerator != null ) {
    		bgGenerator.shutdown();
    	}
    	initialized = false;
    }
    
	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
		FrameData newFrame = new FrameData( frameTime, timeInt );		
		return newFrame;
	}

    @Override
    public void clearImages() {
        if (disclosureThread != null) {
            disclosureThread.cleared = true;
        }

        for( AbstractFrameData frameData : frameDataMap.values() ) {
            for( Station station : ((FrameData)frameData).stationMap.values()) {
                if (station.plotImage != null) {
                    station.plotImage.dispose();
                    station.plotImage = null;
                }
            }
        }
        
        imagesArrived = true;
    }

    @Override
    public void modelGenerated(PlotInfo[] keys, IImage image) {
    	for ( PlotInfo key : keys ) {
    		for( AbstractFrameData frameData : frameDataMap.values() ) {
    			if( frameData.isRscDataObjInFrame( new PlotInfoRscDataObj( key ) ) ) {
    				((FrameData)frameData).modelGenerated(key, image);
    			}
    		}
    	}
        imagesArrived = true;
        issueRefresh();
    }
    
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
    	// TODO : we shouldn't have to requery everything but this is the 
    	// easiest thing to do for now...
        clearFrames(); 
        
        this.distFloor = (descriptor.getMapWidth() / 1000.0)
        	           * this.plotRscData.getPixelSizeHint() / 32000.0;
        this.worldExtent = new PixelExtent(0, descriptor.getGridGeometry()
                .getGridRange().getHigh(0), 0, descriptor.getGridGeometry()
                .getGridRange().getHigh(1));
        try {
            transform = MapUtil.getTransformFromLatLon(descriptor.getCRS());
        } catch (FactoryException e) {
            throw new VizException("Error retrieving MathTransform", e);
        }
    }
    	
	@Override
	public void resourceAttrsModified() {		
		// Repaint the data
		needsUpdate = true; 
		issueRefresh();
	}

	// 
	@Override
	public void resourceChanged(ChangeType type, Object object) {
		// TODO Raytheon's PlotResource2 is implementing this and calling 
		// different version of getStations to call resourceChanged....		
	}
	
	@Override
	 public void messageGenerated(String dataURI, String message) {
	}

	// generate a string used as the key for the StationMap
	// 
	private String getStationMapKey( Double lat, Double lon ) {
    	return new String( ""+Math.round(lat*1000.0)  + ","+
				  			  Math.round(lon*1000.0) ); 

	}	
	
	private DirectPosition getStationPosition(Station station) {
		try {
			if (WGS84toPROJCRS != null && station != null) 	{
				DirectPosition stnPos = WGS84toPROJCRS.transform(new DirectPosition2D(station.info.longitude, station.info.latitude), null);
				return stnPos;
			}
	        		
		} catch (Exception e) {
			System.out.println("Error while getting station position: " + e.getMessage());
		}

		return null;
	}
	
	/*
	 * Gets the data area (from File -> Preferences) in the form of latitudes and longitudes 
	 * and returns a GeneralEnvelope based on it
	 */
	private GeneralEnvelope getDataAreaEnvelope () {

		//expression
		String expr = "((-|\\+)?[0-9]+(\\.[0-9]+)?)+";
		
		IPreferenceStore prefs = NmapCommon.getNcepPreferenceStore();
    	String llLat = prefs.getString(GraphicsAreaPreferences.LLLAT);
    	String llLon = prefs.getString(GraphicsAreaPreferences.LLLON);
    	String urLat = prefs.getString(GraphicsAreaPreferences.URLAT);
    	String urLon = prefs.getString(GraphicsAreaPreferences.URLON);
    	
    	if ( llLat == null || llLon == null || urLat == null ||	urLon == null ) 
    		return null;
    	
    	if ( llLat == "" || llLon == "" || urLat == "" ||	urLon == "" ) 
    		return null;
    	
    	String garea = llLat+";"+llLon+";"+urLat+";"+urLon;
      		     	
     	if ( !llLat.matches(expr) || !llLon.matches(expr) ||
    			!urLat.matches(expr) || !urLon.matches(expr)) return null;    	
    	
     	GraphicsAreaCoordinates gareaCoordObj = new GraphicsAreaCoordinates( garea ); 
    	if ( ! gareaCoordObj.parseGraphicsAreaString(garea)) 
    		return null;	    	
    	
    	try {
       
	        WGS84toPROJCRS = MapUtil.getTransformFromLatLon(MapUtil.LATLON_PROJECTION);    
	        
	        GeneralEnvelope dataAreaEnvelope = new GeneralEnvelope(2);
	
	        DirectPosition ll = WGS84toPROJCRS.transform(new DirectPosition2D(
	        		Double.parseDouble(llLon), Double.parseDouble(llLat)), null);
	
	        DirectPosition ur = WGS84toPROJCRS.transform(new DirectPosition2D(
	        		Double.parseDouble(urLon), Double.parseDouble(urLat)), null);
	
	        dataAreaEnvelope.setRange(0,
	                Math.min(ll.getOrdinate(0), ur.getOrdinate(0)),
	                Math.max(ll.getOrdinate(0), ur.getOrdinate(0)));
	        dataAreaEnvelope.setRange(1,
	                Math.min(ll.getOrdinate(1), ur.getOrdinate(1)),
	                Math.max(ll.getOrdinate(1), ur.getOrdinate(1)));
	
	        dataAreaEnvelope.setCoordinateReferenceSystem(MapUtil.LATLON_PROJECTION);
	        
	        return dataAreaEnvelope;
        
        } catch (Exception e) {
        	System.out.println("Error while getting data area from Preferences: " + e.getMessage());	    
        	return null;
        }
    }
		 
	// Mutex Rule for scheduling generator in order 
	class MutexRule implements ISchedulingRule {

        public boolean contains(ISchedulingRule rule) {
            return (rule == this);
        }

        public boolean isConflicting(ISchedulingRule rule) {
            return (rule == this);
        }

	}
}
