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
import org.eclipse.core.runtime.jobs.Job;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
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
import com.raytheon.uf.viz.core.rsc.capabilities.IPlotDataResource;
import com.raytheon.viz.pointdata.IPlotModelGeneratorCaller;
import com.raytheon.viz.pointdata.PlotAlertParser;
import com.raytheon.viz.pointdata.PlotInfo;
import com.raytheon.viz.pointdata.rsc.retrieve.PointDataPlotInfoRetriever;
import com.vividsolutions.jts.geom.Coordinate;

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
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class PlotResource2 extends AbstractNatlCntrsResource<PlotResourceData, MapDescriptor> 
	   implements  IResourceDataChanged, IPlotDataResource, IPlotModelGeneratorCaller, INatlCntrsResource {
	
	protected PlotResourceData plotRscData = null;

	boolean plotAll = false;
	
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
                }
                double displayHintSize = plotRscData.getPixelSizeHint() * magnification;
                
                double threshold = (displayHintSize * kmPerPixel) / density;
                
                LinkedList<Station> stationList = new LinkedList<Station>();
                List<String> toRemove = new ArrayList<String>();
                
                for (String s : frameData.stationsList) {
                    Station station = frameData.stationMap.get(s);
                    Coordinate location = station.pixelLocation;
                    
                    if (station.info != null) {
                        if (!extent.contains(new double[] { location.x,
                                location.y })) {
                            continue;
                        }
                        
                    	if( (station.distValue == null || station.distValue >= threshold)
                    			|| runtimeProgDisc) {
                            stationList.addLast(station);
                        }
                    } else {
                        toRemove.add(station.info.stationId);
                    }
                }

                frameData.stationsList.removeAll(toRemove);

                if (runtimeProgDisc) {
                	frameData.calculateProgDiscRuntime(stationList, threshold);
                }
                frameData.currentStationList = stationList;
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

                generator.queueStations(newStations);

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

    private boolean runtimeProgDisc = false;

    private ProgDisc disclosureThread = null;

    private PixelExtent worldExtent;

    private PlotModelGenerator2 generator;

    private boolean metarEnabled;
    private boolean mesowestEnabled;

    private VA_Advanced progDisc;

    private double actualPlotWidth;

    private double plotWidth;
    
    private static final double MAX_SAMPLE_DISANCE = 2;

//    Map<String, RequestConstraint> queryList;

    private MathTransform transform = null;

    private int canvasWidth;

    private double magnification;

    private double zoomLevel;

    private double density;

    private PaintProperties lastProps;
    private FrameData       lastFrameData;

    private StaticPlotInfoPV spi;
    
    private int dynProgDisc;

    private double distFloor;

    private double screenToWorldRatio;

    private boolean needsUpdate = false;
    
 //   private double filterValue = 10.0;

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
    	// make this frame specific so we can do the time matching before parsing
    	// stations
    	// map from the station Id to the station info (plotInfo and image)
        private Map<String, Station> stationMap = new HashMap<String, Station>();
        
        private List<String> stationsList = new ArrayList<String>();

        private List<Station> currentStationList = new ArrayList<Station>();

        private int     uniqueueStations;
		private int     dynStations;
//		private double  minDist;
		private boolean computeProgDisc = true;
        		
        protected FrameData( DataTime time, int interval ) {
			super(time, interval);
	        uniqueueStations = 0;
	        dynStations = 0;
//	        minDist = 22222.0;
        }

		@Override
		public boolean updateFrameData( IRscDataObject rscDataObj ) {
			if( !(rscDataObj instanceof PlotInfoRscDataObj) ) { 
				return false;
			}
			
			PlotInfo plotInfo = ((PlotInfoRscDataObj)rscDataObj).getPlotInfo();
			
			// TODO : The dataTime doesn't get set in the data query. 
			//
			if( plotInfo.dataTime == null ) {
				plotInfo.dataTime = getFrameTime();
			}
			
    		Station stn = stationMap.get( plotInfo.stationId );

    		if( stn != null ) {
        		if( stn.plotImage != null ) {
        			stn.plotImage.dispose();
        			stn.plotImage = null;
        		}
//        		if( stn.info.latitude != plotInfo.latitude || 
//        			stn.info.longitude != plotInfo.longitude ) {
//        			System.out.println("??Updating PlotInfo for station " + stn.info.stationId +
//        					" with different lat/lon???" + stn.info.latitude +"," + stn.info.longitude + "and " +
//        					plotInfo.latitude + "," + plotInfo.longitude );
//        		}
        		stn.info = plotInfo;
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
	      //  System.out.println("populateFrame: "  + frameTime.toString());
	        RequestConstraint time = new RequestConstraint();
	        String[] constraintList = { startTime.toString(), endTime.toString() };
	        time.setBetweenValueList(constraintList);
	        time.setConstraintType(RequestConstraint.ConstraintType.BETWEEN);

	        HashMap<String, RequestConstraint> metadataMap = new HashMap<String, RequestConstraint>(
	                plotRscData.getMetadataMap() );
	        metadataMap.put("dataTime", time);
	        long t0 = System.currentTimeMillis();
	        
	        List<PlotInfo> plotInfoObjs = plotRscData.getPlotInfoRetriever().getStations(
	        								 metadataMap);
	        
	        long t1 = System.currentTimeMillis();
	        System.out.println("InitFrame Took: " + (t1 - t0) + " To find "+ 
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
						System.out.println("Error getting dataTime from Plot Query");
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
//	        			System.out.println("plotInfo obj doesn't time match to the frame which queried it???");
	        		}
	        	}
//	        	PlotInfo stationInfo = info.get(i);
//	            Station station = new Station();
//	            // TODO Which is correct? stationInfo.dataTime or frameTime?
//	            // raytheon's code used the frametime but this may not
//	            // be correct for our time matching method. 
//	            stationInfo.dataTime = stationInfo.dataTime;//frameTime;
//	            station.info = stationInfo;
//	            calcStaticStationInfo( station );
	        }

	        System.out.println( "Number of uniq stations is " + stationMap.size() );
	        
	        calculateProgDisc();

	        setPopulated();

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
//	                minDist = distFloor;
	                station.origDistValue = distFloor; // minDist;
	            } else {
	                station.origDistValue = obsStation.distance;
//	                if (obsStation.distance < minDist) {
//	                    minDist = obsStation.distance;
//	                }
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
	        if (stationMap.put(station.info.stationId, station) == null ) {
	        	stationsList.add(station.info.stationId);
	            uniqueueStations++;
	        }
	        return true;
	    }

	    private void calculateProgDiscRuntime(List<Station> stations, double threshold) {
	    	
	        double kmPerPixel = threshold * density
	                / (plotRscData.getPixelSizeHint() * magnification);

	        // get meters per pixel
	        double mPerPixel = kmPerPixel * 1000;
	        double pixelDist = mPerPixel * plotRscData.getPixelSizeHint() / 2;

	        pixelDist /= density;

	        List<Station> toUse = new ArrayList<Station>();
	        for (Station a : stations) {
	        	
	            boolean use = true;
	            
	            if( a.projCoords == null ||
	            	a.projCoords.length < 3 ) {
	            	continue;
	            }
	            
	            for (Station b : toUse) {
	                // Make sure a does not overlap with b
	            	
	                double deltaX = a.projCoords[0] - b.projCoords[0];
	                double deltaY = a.projCoords[1] - b.projCoords[1];
	                // Absolute value logic inlined for performance
	                deltaX = (deltaX <= 0.0D) ? 0.0D - deltaX : deltaX;
	                deltaY = (deltaY <= 0.0D) ? 0.0D - deltaY : deltaY;

	                if (deltaX < pixelDist && deltaY < pixelDist) {
	                    use = false;
	                    break;
	                }
	            }
	            
	            if (use) {
	                toUse.add(a);
	            }
	        }
	        
	        stations.clear();
	        stations.addAll(toUse);
	    }

	    
	    protected void calculateProgDisc() {
//	        boolean progressiveDisclosure = dynStations > 0
//	                && ((uniqueueStations * uniqueueStations / dynStations) < 3000);
//	        progressiveDisclosure = dynProgDisc > 1
//	                || (progressiveDisclosure && dynProgDisc > 0);
	        boolean progressiveDisclosure = ((double) ((double) dynStations
	                * (double) dynStations / (double) uniqueueStations) < 3000.0);
	        progressiveDisclosure = dynProgDisc > 1 || (progressiveDisclosure);

	        if (progressiveDisclosure) {
	            int size = stationsList.size();
	            Coordinate[] latLonArray = new Coordinate[size];
	            Integer[] goodnessArray = new Integer[size];
	            Double[] distArray = new Double[size];
	            int i = 0;
	            for (String s : stationsList) {
	                Station station = stationMap.get(s);
	                latLonArray[i] = new Coordinate(station.info.longitude,
	                        station.info.latitude);
	                goodnessArray[i] = station.goodnessValue;
	                distArray[i] = station.origDistValue;
	                ++i;
	            }
	            progDisc.setVaJustGoodness(false);
	            progDisc.setVaDistPass(dynProgDisc < 3);
	            progDisc.getVaAdvanced(latLonArray, goodnessArray, distArray);

	            for (i = 0; i < size; ++i) {
	                stationMap.get(stationsList.get(i)).distValue = 
	                	( distArray[i] > distFloor ? distArray[i] : distFloor );
	            }
	        } else {
	            if (stationsList.size() == 0) {
	                return;
	            }

	            for (Station s : stationMap.values()) {
	                s.distValue = -1.0;
	                if (s.projCoords == null) {
	                    double[] in = new double[] { s.info.longitude, s.info.latitude, 0 };
	                    double[] out = new double[3];
	                    
	                    try {
	                        transform.transform(in, 0, out, 0, 1);
	                    } catch (TransformException e) {
	                        e.printStackTrace();
	                    }
	                    s.projCoords = out;
	                }
	            }

	            runtimeProgDisc = true;
	        }
	        computeProgDisc = false;
	    }
	    
	    public void modelGenerated(PlotInfo key, IImage image) {
	    	Station stn = stationMap.get(key.stationId);
	    	if( image == null || stn == null ) {
	    		return;
	    	}
	    	
	    	if( stn.plotImage != null ) {
	    		stn.plotImage.dispose();
	    		stn.plotImage = null;
	    	}
	    	stn.plotImage = image;
	    }
	    
	    public void dispose() {
            for (String station : stationsList) {
                Station s = stationMap.get(station);
                if (s != null && s.plotImage != null) {
                    s.plotImage.dispose();
                    s.plotImage = null;
                }
                stationMap.remove(station);
            }
            currentStationList.clear();
            stationMap.clear();
            stationsList.clear();            
            populated = false;
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
        	if( data.getPluginName().equals("h5uair") ) {
        		data.setAlertParser(new H5UairAlertParser() );
        	}
        	else {
        		data.setAlertParser( new PlotAlertParser() );
        	}
        }
        
        if( data.getPlotInfoRetriever() == null ) {
        	if( data.getPluginName().equals("h5uair") ) {
        		data.setPlotInfoRetriever(
                		new H5UairPlotInfoRetriever() );
        	}
        	else {
        		data.setPlotInfoRetriever(
            		new PointDataPlotInfoRetriever());
        	}
        }
        
        plotAll = false; //true;
        
        this.dataTimes = new ArrayList<DataTime>();
        metarEnabled = true;
        mesowestEnabled = true;
        this.progDisc = new VA_Advanced();
        this.dynProgDisc = 1;

        // this.range = plotRscData.getBinOffset().getInterval() * 1000;
        // this.posOffset = plotRscData.getBinOffset().posOffset * 1000;

//        data.addChangeListener(this);
        
        if (plotRscData.getSpiFile() != null) {
            this.spi = StaticPlotInfoPV.readStaticPlotInfoPV(plotRscData
                    .getSpiFile());
        }
    }

    // override to process PlotInfoRscDataObj instead of PlotInfo
    @Override
	protected IRscDataObject[] processRecord( Object pltInfo ) {
		if( !(pltInfo instanceof PlotInfo) ) {
			System.out.println( "PlotResource2.processRecord method expecting PlotInfoRscData objects "+
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
    	
    	FrameData frameData = (FrameData) fd;
    	
    	populateFrame( frameData );
    	
        boolean update = false;
        if( lastProps == null || frameData != lastFrameData ||
        		imagesArrived || lastProps.getView().getExtent().equals(
	        				                  paintProps.getView().getExtent()) == false
	        				|| 1.0/*paintProps.getDensity()*/ != density
	        				|| 1.0 /*paintProps.getMagnification()*/ != magnification ) {
        	update = true;
        }

        lastProps = new PaintProperties(paintProps, (IView) paintProps.getView().clone());
        lastFrameData = frameData;
        canvasWidth = paintProps.getCanvasBounds().width;
        magnification = 1.0;//paintProps.getMagnification();
        density = 1.0;//paintProps.getDensity();
        
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
		double scaleValue = (this.plotWidth / 2.0)
        		/ screenToWorldRatio;


        for (Station station : stationList) {
            if (station.plotImage == null) {
                imagesArrived = true;
                continue;
            }
            double[] stationPixelLocation = new double[] {
                    station.pixelLocation.x, station.pixelLocation.y };

            double[] ul = new double[] {
                    stationPixelLocation[0] - scaleValue,
                    stationPixelLocation[1] - scaleValue, 0 };

            double[] ur = new double[] {
                    stationPixelLocation[0] + scaleValue,
                    stationPixelLocation[1] - scaleValue, 0 };

            double[] lr = new double[] {
                    stationPixelLocation[0] + scaleValue,
                    stationPixelLocation[1] + scaleValue, 0 };

            double[] ll = new double[] {
                    stationPixelLocation[0] - scaleValue,
                    stationPixelLocation[1] + scaleValue, 0 }; 


            PixelCoverage pc = new PixelCoverage(new Coordinate(ul[0],
                    ul[1], ul[2]), new Coordinate(ur[0], ur[1], ur[2]),
                    new Coordinate(lr[0], lr[1], lr[2]), new Coordinate(
                            ll[0], ll[1], ll[2]));

            aTarget.drawRaster(station.plotImage, pc, paintProps,
                    RasterMode.SYNCHRONOUS);
        }
    }


    public void initResource( IGraphicsTarget aTarget ) throws VizException {
        generator = new PlotModelGenerator2(aTarget, descriptor,
                plotRscData.getPlotModel(), 
                (plotRscData.isSurfaceOnly() ? null : plotRscData.getLevelKey() ), 
                plotRscData.getMetadataMap(), this);
        
        this.generator.setPlotMissingData(
        					plotRscData.isPlotMissingData() );
  
//        this.generator.setLowerLimit(plotRscData.getLowerLimit());
//        this.generator.setUpperLimit(plotRscData.getUpperLimit());
        // generator.schedule();
        this.actualPlotWidth = this.plotWidth = generator.getPlotModelWidth();
        
        if( plotAll ) {
        	distFloor = 99999;
        }
        else {
        	this.distFloor = (descriptor.getMapWidth() / 1000.0)
        	* plotRscData.getPixelSizeHint() / 32000.0;
        }
        
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

    // Time match the PlotInfo object to the appropriate frame and add it 
    // to the list of stations to be processed later
    //
//    public synchronized void addRecord(Object[] dataObjs) throws VizException {
//        if( dataObjs == null ) 
//        	return;
//
//        DataTime latestTime = null;
//        
//        for( Object dataObj : dataObjs ) {
//        	boolean foundFrame = false;
//        	
//        	// TODO : The dataTime doesn't get set in the query. Find out how to do this.
//        	DataTime dataTime = ((PlotInfo)dataObj).dataTime;
//        	
//        	for( AbstractFrameData frameData : frameDataMap.values() ) {
//            	if( frameData.isDataObjectInFrame( dataObj ) ) {
////            		((FrameData)frameData).stationsToParse.add( (PlotInfo)obj );
//            		((FrameData)frameData).updateFrameData( dataObj );
//            		foundFrame = true; 
//            	}
//            	
//            	if( latestTime == null ||
//            		latestTime.greaterThan( frameData.getFrameEndTime() ) ) {
//            		latestTime = frameData.getFrameEndTime();
//            	}
//            }
//        	
//        	// TODO : Currently the AbstractNatlCntrsResource is not handling auto update
//        	// when a new frame needs to be created and synchronized with other resources. 
//        	// Here we will also need to create a new frame for the new record.
//        	if( !foundFrame && 
//        		dataTime.greaterThan( latestTime ) ) {
//        		System.out.println("PlotResource2:addRecord() : Not processing new Data " + 
//        				"for new Frame");
//        	}
//        }
//    }

    @Override
	public void disposeInternal() {
    	if( generator != null ) {
    		generator.shutdown();
    	}
    }

    /**
     * Returns if mesowest data is being displayed by the layer .
     * 
     * @return Whether mesowest is enabled or not
     */
    public boolean isMesowestEnabled() {
        return mesowestEnabled;
    }

    /**
     * Returns if metar data is being displayed by the layer.
     * 
     * @return Whether metar is enabled or not
     */
    public boolean isMetarEnabled() {
        return metarEnabled;
    }

    /**
     * Enables/disables mesowest data.
     * 
     * @param flag
     *            Enable or disable mesowest on the display
     */
    public void setMesowestMode(boolean flag) {
        mesowestEnabled = flag;
    }

    /**
     * Enables/disables metar data.
     * 
     * @param flag
     *            Enable or disable metar on the display
     */
    public void setMetarMode(boolean flag) {
        metarEnabled = flag;
    }
    
    public void setDynProgDisc(String progDisc) {
        if ("none".matches(progDisc)) {
            this.dynProgDisc = 0;
        } else if ("missing".matches(progDisc)) {
            this.dynProgDisc = 2;
        } else if ("all".matches(progDisc)) {
            this.dynProgDisc = 3;
        } else if ("off".matches(progDisc)) {
            this.dynProgDisc = 4;
        }
    }

    public String getDynProgDisc() {
        switch (this.dynProgDisc) {
        case 0:
            return "none";
        case 2:
            return "missing";
        case 3:
            return "all";
        case 4:
            return "off";
        }
        return null;
    }

	protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
		FrameData newFrame = new FrameData( frameTime, timeInt );		
		return newFrame;
	}

// We currently don't support the sampling behaviour but this can be added in the future
// when we decide what we want/need. 
//    @Override
//    public String inspect(ReferencedCoordinate coord) throws VizException {
//    	FrameData curFrame = (FrameData) getCurrentFrame();
//    	
//    	if( curFrame == null ) {
//            return "error getting active frame?";
//    	}
//    	
//    	Coordinate latlon = null;
//    	try {
//    		latlon = coord.asLatLon();
//    	} catch (Exception e) {
//    		UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
//    				StatusConstants.CATEGORY_WORKSTATION, "plot",
//    				"Error transforming coordinate", e);
//    	}
//
//        double[] ml = { latlon.x, latlon.y };
//        double[] pml = descriptor.worldToPixel(ml);
//        double scaleValue = (this.plotWidth / 2.0)
//                / this.screenToWorldRatio;
//        PixelExtent interrogationExtent = new PixelExtent(pml[0]
//                - scaleValue, pml[0] + scaleValue, pml[1] - scaleValue,
//                pml[1] + scaleValue);
//        Envelope llExtent = descriptor.pixelToWorld(interrogationExtent);
//        List<Station> currStations = curFrame.currentStationList;
//        List<Station> availableStations = new ArrayList<Station>();
//        for (Station station : currStations) {
//            Coordinate pixelLocation = station.pixelLocation;
//            double[] location = descriptor.pixelToWorld(new double[] {
//                    pixelLocation.x, pixelLocation.y });
//            if (!llExtent.contains(location[0], location[1])) {
//                continue;
//            }
//            availableStations.add(station);
//        }
//
//        if (availableStations.size() == 1) {
//            return this.generator.getStationMessage(availableStations
//                    .get(0).info.id);
//        } else if (availableStations.size() > 1) {
//            int index = findClosestPlot(latlon, availableStations);
//
//            if (index != -1) {
//                return this.generator.getStationMessage(availableStations
//                        .get(index).info.id);
//            }
//        }
//    	return "Station not found";
//    }

    /**
     * Returns the index of the closest available plot in the same area as the
     * sampling position.
     * 
     * @param latlon
     *            The coordinate of the sampling position.
     * @param availableStations
     *            List of available plots in the same area as latlon.
     * @return The index of the plot closest to latlon.
     */
//    private int findClosestPlot(Coordinate latlon,
//            List<Station> availableStations) {
//        double x1 = latlon.x;
//        double y1 = latlon.y;
//        double minDistance = MAX_SAMPLE_DISANCE;
//        int minIndex = -1;
//
//        for (int i = 0; i < availableStations.size(); i++) {
//            PlotInfo info = availableStations.get(i).info;
//            double x2 = info.longitude;
//            double y2 = info.latitude;
//            double d = Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2));
//
//            if (d < minDistance) {
//                minDistance = d;
//                minIndex = i;
//            }
//        }
//
//        return minIndex;
//    }

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
        clearFrames(); //frameInfo.clear();
        
        if( plotAll ) {
        	distFloor = 99999;
        }
        else {
        	this.distFloor = (descriptor.getMapWidth() / 1000.0)
        	           * this.plotRscData.getPixelSizeHint() / 32000.0;
        }        
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
	}

	// 
	@Override
	public void resourceChanged(ChangeType type, Object object) {
		// TODO Raytheon's PlotResource2 is implementing this and calling 
		// different version of getStations to call resourceChanged....
		
	}
	
	@Override
	public void messageGenerated(int id, String message) {
		// TODO Auto-generated method stub

	}

}