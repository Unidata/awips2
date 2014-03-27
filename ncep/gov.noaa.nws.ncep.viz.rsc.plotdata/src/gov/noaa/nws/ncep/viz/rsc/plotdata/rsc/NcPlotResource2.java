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

import static java.lang.System.out;
import gov.noaa.nws.ncep.edex.common.metparameters.AbstractMetParameter;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.display.IVector;
import gov.noaa.nws.ncep.ui.pgen.elements.SymbolLocationSet;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilter;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefns;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefnsMngr;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.NcPlotImageCreator.IPointInfoRenderingListener;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.NcPlotImageCreator.Position;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.NcPlotModelHdf5DataRequestor;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.StaticPlotInfoPV;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.StaticPlotInfoPV.SPIEntry;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModelElement;
import gov.noaa.nws.ncep.viz.rsc.plotdata.progdisc.ProgressiveDisclosure;
import gov.noaa.nws.ncep.viz.rsc.plotdata.progdisc.ProgressiveDisclosure.IProgDiscListener;
import gov.noaa.nws.ncep.viz.rsc.plotdata.queue.QueueEntry;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Semaphore;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableBasics;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
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
 *  04/28/2010     #275    ghull       Refactor raytn's class to work with 
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
 *  10/18/2012     896     sgurung     Refactored PlotResource2 to use new generator class: NcPlotDataThreadPool. Added FrameLoaderJob to populate all frames.
 *  								   Added code to plot stations within 25% of the area outside of the current display area.
 *  05/20/2013     988     Archana.S   Refactored this class for performance improvement	
 *  11/07/2013             sgurung     Added fix for "no data for every other frame" issue (earlier fix was added to 13.5.2 on 10/24/2013)
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class NcPlotResource2 extends
        AbstractNatlCntrsResource<PlotResourceData, NCMapDescriptor> implements
        IPlotModelGeneratorCaller, INatlCntrsResource, IProgDiscListener,
        IPointInfoRenderingListener

{

    protected static final float MAX_DENSITY = 3.0f;

    protected static int DEFAULT_PLOT_WIDTH = 105;

    private JobPool frameRetrievalPool = null;

    private ConcurrentLinkedQueue<SpecialQueueEntry> queueOfFrameTimesAndStations = null;

    private ProgressiveDisclosure progressiveDisclosure = null;

    private ResourceName rscName = null;

    private String cycleTimeStr = null;

    private boolean isFcst = false;

    private double plotDensity = Double.MIN_NORMAL;

    private double plotWidth = Double.MIN_NORMAL;

    private PixelExtent worldExtent;

    private NcPlotModelHdf5DataRequestor dataRequestor = null;

    public StaticPlotInfoPV spi;

    private FrameLoaderTask frameLoaderTask;

    private FcstFrameLoaderTask fcstFrameLoaderTask = null;

    private TimeLogger timeLogger;

    private HashMap<String, RequestConstraint> metadataMap;

    private NCMapDescriptor mapDescriptor;

    Rectangle canvasBounds = null;

    private PlotModel plotModel = null;

    private int existingLevel = -1;

    private ConditionalFilter existingCondFilter = null;

    protected PlotResourceData plotRscData = null;

    boolean matchFound = false;

    boolean requeryDataAndReCreateAllStnImages = false;

    boolean onlyReCreateExistingImages = false;

    boolean densityChanged = false;

    boolean isThereAConditionalFilter = false;

    PaintProperties currPaintProp;

    RGB rgb = new RGB(200, 200, 0);

    private DisplayElementFactory df = null;

    List<SymbolLocationSet> listOfSymbolLocationSet = new ArrayList<SymbolLocationSet>();

    Semaphore semaphore = new Semaphore(1);

    protected class PlotInfoRscDataObj implements IRscDataObject {
        private PlotInfo plotInfo;

        public PlotInfoRscDataObj(PlotInfo pltInfo) {
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

    protected class FcstFrameLoaderTask implements Runnable {
        DataTime frameTime;

        DataTime stationTimeToSet;

        Collection<Station> stationList;

        FcstFrameLoaderTask(DataTime frameTimeToMatch, DataTime stnTimeToSet,
                Collection<Station> listOfStations) {
            Tracer.print("> Entry");
            frameTime = frameTimeToMatch;
            stationTimeToSet = stnTimeToSet;
            stationList = listOfStations;
            Tracer.print("< Exit");
        }

        @Override
        public void run() {
            Tracer.print("> Entry  START TASK "
                    + Tracer.shortTimeString(frameTime));
            Semaphore sm = new Semaphore(1);
            sm.acquireUninterruptibly();
            String trialKey = null;
            synchronized (frameTime) {
                FrameData frameData = (FrameData) getFrame(frameTime);
                if ((frameData != null && frameData.stationMap != null && frameData.stationMap
                        .isEmpty())
                        && (stationList != null && !stationList.isEmpty())
                        && (frameTime != null)) {

                    if (!getFrameTimes().get(0).equals(frameTime)) {/*
                                                                     * Except
                                                                     * for the
                                                                     * first
                                                                     * frame...
                                                                     */
                        synchronized (stationList) {
                            for (Station station : stationList) {
                                String stnKey = getStationMapKey(
                                        station.info.latitude.doubleValue(),
                                        station.info.longitude.doubleValue());

                                if (trialKey == null)
                                    trialKey = new String(stnKey);
                                Station newStation = new Station();
                                newStation.info = new PlotInfo(
                                        station.info.stationId,
                                        station.info.latitude,
                                        station.info.longitude,
                                        stationTimeToSet, null);
                                newStation.distValue = new Double(
                                        station.distValue.doubleValue());
                                newStation.origDistValue = new Double(
                                        station.origDistValue.doubleValue());
                                newStation.goodnessValue = new Integer(
                                        station.goodnessValue.intValue());

                                synchronized (frameData.stationMap) {
                                    frameData.stationMap
                                            .put(stnKey, newStation);
                                }
                            }

                            Tracer.print("FcstFrameLoaderTask - for frame "
                                    + frameData.getShortFrameTime()
                                    + " the station datatime is: "
                                    + frameData.stationMap.get(trialKey).info.dataTime);
                        }
                    }

                    Tracer.print("From FcstFrameLoaderTask - about to queue frame: "
                            + frameData.getShortFrameTime() + " for PD");
                    progressiveDisclosure.setDensity(plotDensity);
                    synchronized (frameData.stationMap) {
                        Tracer.print("For frame "
                                + frameData.getShortFrameTime()
                                + " the size of stationMap - "
                                + frameData.stationMap.size());
                        frameData.progressiveDisclosureInProgress = true;
                        progressiveDisclosure.queueListOfStationsToBeDisclosed(
                                frameTime, frameData.stationMap.values());
                        issueRefresh();
                    }

                }

            }

            sm.release();
            Tracer.print("< Exit  END TASK "
                    + Tracer.shortTimeString(frameTime));

        }
    }

    protected class FrameLoaderTask implements Runnable {

        DataTime dataTime;

        HashMap<String, RequestConstraint> frameLoaderTaskMetadataMap;

        FrameLoaderTask(DataTime dt) {
            dataTime = dt;
            frameLoaderTaskMetadataMap = new HashMap<String, RequestConstraint>(
                    metadataMap);
        }

        @Override
        public void run() {
            Tracer.print("> Entry  START TASK "
                    + Tracer.shortTimeString(dataTime));
            Tracer.print("About to run postgres query for frame: "
                    + Tracer.shortTimeString(dataTime));

            FrameData frameData = (FrameData) getFrame(dataTime);
            RequestConstraint timeConstraint = new RequestConstraint();

            if (!isFcst) {

                String[] constraintList = {
                        frameData.getFrameStartTime().toString(),
                        frameData.getFrameEndTime().toString() };
                timeConstraint.setBetweenValueList(constraintList);
                timeConstraint
                        .setConstraintType(RequestConstraint.ConstraintType.BETWEEN);
                frameLoaderTaskMetadataMap.put("dataTime", timeConstraint);
            }

            try {
                // Tracer.print("frameLoaderTaskMetadataMap = "
                // + frameLoaderTaskMetadataMap);

                frameData.plotInfoObjs = plotRscData.getPlotInfoRetriever()
                        .getStations(frameLoaderTaskMetadataMap);

                if (frameData.plotInfoObjs != null
                        && !frameData.plotInfoObjs.isEmpty()) {
                    Tracer.print("PointDataPlotInfoRetriever.getStations returned "
                            + frameData.plotInfoObjs.size()
                            + " stations for frame "
                            + frameData.getShortFrameTime());
                    synchronized (frameData.plotInfoObjs) {

                        boolean isFramePopulated = frameData.populateFrame();
                        if (isFramePopulated) {
                            Collection<Station> stationsToBeQueuedForProgDisc = progressiveDisclosure
                                    .calculateStaticProgDiscDistancesForStations(
                                            frameData.stationMap.values(),
                                            frameData.dynStations);
                            if (stationsToBeQueuedForProgDisc != null
                                    && !stationsToBeQueuedForProgDisc.isEmpty()) {
                                synchronized (stationsToBeQueuedForProgDisc) {
                                    for (Station stn : stationsToBeQueuedForProgDisc) {
                                        String stnMapKey = getStationMapKey(
                                                stn.info.latitude,
                                                stn.info.longitude);
                                        Station frameStn = frameData.stationMap
                                                .get(stnMapKey);
                                        frameStn.distValue = stn.distValue;
                                        synchronized (frameData.stationMap) {
                                            frameData.stationMap.put(stnMapKey,
                                                    frameStn);
                                        }
                                    }

                                    Tracer.print("About to schedule the frame: "
                                            + frameData.getShortFrameTime()
                                            + " for progressive disclosure");
                                    frameData.progressiveDisclosureInProgress = true;
                                    progressiveDisclosure
                                            .queueListOfStationsToBeDisclosed(
                                                    frameData.getFrameTime(),
                                                    stationsToBeQueuedForProgDisc);

                                    issueRefresh();

                                }
                            }

                        }

                    }

                } else {
                    Tracer.print("PointDataPlotInfoRetriever.getStations returned null or empty station list for frame "
                            + frameData.getShortFrameTime());
                }

            } catch (VizException e) {

                e.printStackTrace();
            }
            Tracer.print("< Exit   END TASK   " + frameData.getShortFrameTime());

        }
    }

    public class Station {

        public PlotInfo info;

        public Double distValue;

        public Double origDistValue;

        public Coordinate pixelLocation;

        public Integer goodnessValue;

        public double[] projCoords;

        public Set<AbstractMetParameter> listOfParamsToPlot;

        public Set<AbstractMetParameter> setOfConditionalColorParams;

        public Map<Position, DrawableBasics> stnPlotMap = null;

        public Station() {
            listOfParamsToPlot = new HashSet<AbstractMetParameter>(0);
            setOfConditionalColorParams = new HashSet<AbstractMetParameter>(0);
            stnPlotMap = new HashMap<Position, DrawableBasics>(6);
        }

    }

    private class SpecialQueueEntry extends QueueEntry {
        DataTime stationTimeToSet;

        SpecialQueueEntry(DataTime frameTime, DataTime stnTimeToSet,
                Collection<Station> stnCollection) {
            super(frameTime, stnCollection);
            stationTimeToSet = stnTimeToSet;
        }
    }

    public class FrameData extends AbstractFrameData {
        // map from the station Id to the station info (plotInfo and image)
        private Map<String, Station> stationMap = new HashMap<String, Station>();

        private List<PlotInfo> plotInfoObjs = new ArrayList<PlotInfo>();

        // Map<String, Station> lastDisclosed;
        private boolean isFramePaintedFirstTime = false;

        private int uniqueStations;

        private int dynStations;

        private boolean progDiscCalculated;

        private boolean progressiveDisclosureInProgress = false;

        private boolean screenExtentsChangedForCurrentFrame = false;

        private List<DrawableString> drawableStrings = null;

        private List<IVector> listOfWindVectors = null;

        private List<SymbolLocationSet> listOfSymbolLocSet = null;

        private Set<Station> setOfStationsLastRendered = null;

        protected FrameData(DataTime time, int interval) {
            super(time, interval);
            Tracer.print("> Entry");
            uniqueStations = 0;
            dynStations = 0;
            // lastDisclosed = new HashMap<String, Station>();
            drawableStrings = new ArrayList<DrawableString>(0);
            listOfWindVectors = new ArrayList<IVector>(0);
            listOfSymbolLocSet = new ArrayList<SymbolLocationSet>(0);
            setOfStationsLastRendered = new HashSet<Station>(0);
            Tracer.print("< Exit");
        }

        @Override
        public boolean updateFrameData(IRscDataObject rscDataObj) {
            Tracer.printX("> Entry");

            if (!(rscDataObj instanceof PlotInfoRscDataObj)) { // sanity check
                return false;
            }

            PlotInfo plotInfo = ((PlotInfoRscDataObj) rscDataObj).getPlotInfo();
            if (plotInfo.dataTime == null) {
                plotInfo.dataTime = getFrameTime();
                out.println("dataTime from plotInfo is null. Setting to FrameTime");
            }

            /*
             * This check is to remove stations that have been decoded and
             * stored in the database with missing/invalid lat/lon values
             */

            if (plotInfo.latitude.doubleValue() < -90
                    || plotInfo.latitude.doubleValue() > 90
                    || plotInfo.longitude.doubleValue() < -180
                    || plotInfo.longitude.doubleValue() > 180) {
                return false;
            }

            String stnMapKey = getStationMapKey(plotInfo.latitude,
                    plotInfo.longitude);

            Station stn = stationMap.get(stnMapKey);// plotInfo.stationId );

            // This can happen during an auto update or if there are multiple
            // reports for this frame.
            //
            if (stn != null) {

                if (stn.info == null) { // shouldn't happen
                    out.println("Sanity check: Found existing Station in stationMap with a null plotInfo???");
                    return false;// ??
                } else {
                    // out.println(" updating station info "+stn.info.dataURI+
                    // " with " + plotInfo.dataURI );

                    if (!stn.info.stationId.equals(plotInfo.stationId)) {
                        out.println("2 stations " + stn.info.stationId
                                + " and " + plotInfo.stationId
                                + " have the same location?" + "\nLat = "
                                + stn.info.latitude.doubleValue() + ",Lon = "
                                + stn.info.longitude.doubleValue()
                                + " for the time: " + stn.info.dataTime);
                    }
                    // if these are the same time, should we check which one
                    // should be used,
                    // (or combine them?) How to determine which to use?
                    //
                    else if (stn.info.dataTime.getValidTime().getTimeInMillis() != plotInfo.dataTime
                            .getValidTime().getTimeInMillis()) {

                        if (timeMatch(plotInfo.dataTime) < timeMatch(stn.info.dataTime)) {
                            stn.info = plotInfo;

                        }
                    }
                }

            } else {
                stn = new Station();

                if (plotInfo.stationId == null) {
                    plotInfo.stationId = plotInfo.latitude + "#"
                            + plotInfo.longitude;
                }
                stn.info = plotInfo;

                calcStaticStationInfo(stn);
            }
            Tracer.printX("< Exit");

            return true;
        }

        // protected void setFrameData() throws VizException {
        //
        // RequestConstraint timeConstraint = new RequestConstraint();
        //
        // // set the constraints for the query.
        // // for forecast resources we need to match the cycle time and
        // // create a range constraint on the forecast hour.
        // if( isFcst ) {
        //
        // //cycleTimeStr - gets initialized during resource creation
        // timeConstraint = new RequestConstraint( cycleTimeStr );
        // metadataMap.put("dataTime.refTime", timeConstraint );
        //
        // // determine the fcst hr range for the frame span
        // long refTimeMs = rscName.getCycleTime().getRefTime().getTime();
        // long frmStartMs = this.startTime.getRefTime().getTime();
        // long frmEndMs = this.endTime.getRefTime().getTime();
        //
        // long beginFcstHr = (frmStartMs - refTimeMs) / 1000;
        // long endFcstHr = (frmEndMs - refTimeMs) / 1000;
        //
        // timeConstraint = new RequestConstraint( );
        // timeConstraint.setBetweenValueList(
        // new String[] { Long.toString( beginFcstHr ),
        // Long.toString( endFcstHr ) } );
        //
        // timeConstraint.setConstraintType( ConstraintType.BETWEEN );
        // metadataMap.put( "dataTime.fcstTime", timeConstraint );
        // }
        // else {
        //
        // String[] constraintList = { this.startTime.toString(),
        // this.endTime.toString() };
        // timeConstraint.setBetweenValueList( constraintList );
        // timeConstraint.setConstraintType(
        // RequestConstraint.ConstraintType.BETWEEN );
        // metadataMap.put("dataTime", timeConstraint );
        // }
        //
        // dataRequestor.setDefaultConstraintsMap(metadataMap);
        //
        // // plotInfoObjs =
        // plotRscData.getPlotInfoRetriever().getStations(metadataMap);
        // // plotInfoObjs =
        // plotRscData.getPlotInfoRetriever().getStations(this, getFrameTime(),
        // // metadataMap);
        //
        // }

        // if we are painting a frame which has not yet been loaded with data
        // then we
        // need to request data for this frame and then
        // update the frames with
        protected boolean populateFrame() throws VizException {
            Tracer.print("> Entry");

            Tracer.print("Starting populateFrame for " + getShortFrameTime());

            dynStations = 0;
            uniqueStations = 0;

            if (plotInfoObjs == null || plotInfoObjs.isEmpty()) {
                // setFrameData();
                return false;
            }

            for (PlotInfo pltInfo : plotInfoObjs) {

                // have to add this since the dataTime is not always getting set
                // by the
                // plotInfoRetriever?
                if (pltInfo.dataTime == null) {
                    if (plotRscData.isSurfaceOnly()) {
                        pltInfo.dataTime = getFrameTime();
                    } else { // upper air data uses the NcSoundingQuery which
                             // needs a start and stop time for
                             // the query which needs to be correct.
                        out.println("Error getting dataTime from Plot Query");
                    }
                }

                for (IRscDataObject rscDataObj : processRecord(pltInfo)) {
                    // sanity check: this should always be true since we
                    // constrained the
                    // query with the start/end frame times.
                    if (isRscDataObjInFrame(rscDataObj)) {
                        updateFrameData(rscDataObj);

                    } else {
                        // if we don't change the plotInfoRetriever to set the
                        // dataTime, then this
                        // really is a serious sanity check.....
                        // This happens when the dataTime is the same as the
                        // startTime. This satisfies the query constraint
                        // the not the time matching.
                        // else this is possible when the time is equal to one
                        // of the start/end times
                        // Updating this constraint to be a sanity check in the
                        // event that the rscDataObj doesn't time-match the
                        // start time or the end time
                        // if( plotRscData.isForecastResource()
                        // && ( !rscDataObj.getDataTime().equals(this.startTime)
                        // || (!rscDataObj.getDataTime().equals(this.endTime))))
                        // {
                        // // out.println(
                        // "plotInfo obj doesn't time match to the frame:" +
                        // this.frameTime + " which queried it???");
                        // // out.println( "pltInfo.URI = " + pltInfo.dataURI +
                        // "\npltInfo.datatime = " + pltInfo.dataTime );
                        // // newRscDataObjsQueue.add(rscDataObj);
                        // }else{
                        // if( plotRscData.isSurfaceOnly() ){
                        // // out.println( pltInfo.dataURI + " at "+
                        // rscDataObj.getDataTime().toString()
                        // // + " does not time match to frame: "
                        // // + this.getFrameTime().toString() );
                        // newRscDataObjsQueue.add(rscDataObj);
                        // }
                        // }

                    }
                }
            }

            setPopulated((this.stationMap != null)
                    && (this.stationMap.size() > 0));

            Tracer.print("Ending populateFrame for " + getShortFrameTime()
                    + " -- " + (populated ? this.stationMap.size() : "NO")
                    + " stations");
            Tracer.print("< Exit");

            return this.populated;
        }

        public boolean calcStaticStationInfo(Station station) {
            Tracer.printX("> Entry");
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
                /*
                 * Replaced distFloor with minDist to match RTS -
                 * SpiProgDisclosure logic;
                 */
                if (obsStation.distance < progressiveDisclosure.minDist) {
                    station.origDistValue = progressiveDisclosure.minDist;
                } else {
                    station.origDistValue = obsStation.distance;
                }
            } else {
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

            String stnMapKey = getStationMapKey(station.info.latitude,
                    station.info.longitude);

            if (stationMap.put(stnMapKey, station) == null) {

                uniqueStations++;
            } else {
                out.println("Updating StationMap with " + stnMapKey);
            }
            Tracer.printX("< Exit");
            return true;
        }

        public void dispose() {
            Tracer.print("> Entry");
            super.dispose();
            if (stationMap != null && !stationMap.isEmpty()) {
                stationMap.clear();
                stationMap = null;
            }

            if (setOfStationsLastRendered != null
                    && !setOfStationsLastRendered.isEmpty()) {
                setOfStationsLastRendered.clear();
                setOfStationsLastRendered = null;
            }

            if (listOfWindVectors != null && !listOfWindVectors.isEmpty()) {
                listOfWindVectors.clear();
                listOfWindVectors = null;
            }

            if (listOfSymbolLocSet != null && !listOfSymbolLocSet.isEmpty()) {
                listOfSymbolLocSet.clear();
                listOfSymbolLocSet = null;
            }

            if (drawableStrings != null && !drawableStrings.isEmpty()) {
                drawableStrings.clear();
                drawableStrings = null;
            }

            if (plotInfoObjs != null && !plotInfoObjs.isEmpty()) {
                plotInfoObjs.clear();
                plotInfoObjs = null;
            }

            isFramePaintedFirstTime = false;
            progressiveDisclosureInProgress = false;
            screenExtentsChangedForCurrentFrame = false;
            populated = false;

            dynStations = 0;
            uniqueStations = 0;
            Tracer.print("< Exit");
        }

        public boolean isStationMapEmpty() {
            if (stationMap.isEmpty())
                return true;
            else
                return false;
        }

        public boolean isProgDiscCalculated() {
            Tracer.print("> Entry");
            Tracer.print("< Exit");
            return progDiscCalculated;
        }

        public void setProgDiscCalculated(boolean p) {
            Tracer.print("> Entry");
            progDiscCalculated = p;
            Tracer.print("< Exit");
        }

        public Collection<DrawableString> getStringsToDraw() {
            return drawableStrings;
        }

        public List<IVector> getListOfWindVectors() {
            return listOfWindVectors;
        }

        public List<SymbolLocationSet> getListOfSymbols() {
            return listOfSymbolLocSet;
        }

        public String getShortFrameTime() {
            /*
             * Date frameDate = frameTime.getRefTime(); String returnString =
             * Integer.toString(frameDate.getDate()) + "/" +
             * Integer.toString(frameDate.getHours()) +
             * Integer.toString(frameDate.getMinutes()); if
             * (frameTime.getFcstTime() == 0) { returnString += "(" +
             * frameTime.getFcstTime() + ")"; } return returnString;
             */
            return Tracer.shortTimeString(frameTime);
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
    public NcPlotResource2(PlotResourceData data, LoadProperties props) {
        super(data, props);
        Tracer.print("> Entry");
        plotRscData = data;
        // The object which is called by the NcAutoUpdater to get a PlotInfo
        // object from the alert uri.
        if (plotRscData.getAlertParser() == null) {
            plotRscData.setAlertParser(new PlotAlertParser());
        }

        if (plotRscData.getPlotInfoRetriever() == null) {
            plotRscData.setPlotInfoRetriever(new PointDataPlotInfoRetriever());
        }

        this.dataTimes = new ArrayList<DataTime>();

        timeLogger = TimeLogger.getInstance();
        frameRetrievalPool = new JobPool("Querying stations all frames...", 8,
                false);

        rscName = plotRscData.getResourceName();
        isFcst = plotRscData.isForecastResource()
                && !plotRscData.getPluginName().equals("nctaf");

        this.plotWidth = DEFAULT_PLOT_WIDTH;

        setInitialPlotDensityFromRscAttrSet();
        if (plotRscData.getSpiFile() != null
                && !plotRscData.getSpiFile().isEmpty()) {
            this.spi = StaticPlotInfoPV.readStaticPlotInfoPV(plotRscData
                    .getSpiFile());
        }
        mapDescriptor = (NCMapDescriptor) NcDisplayMngr
                .getActiveNatlCntrsEditor().getActiveDisplayPane()
                .getRenderableDisplay().getDescriptor();
        // what if no SPI file exists?

        if ((progressiveDisclosure == null) && (spi != null)) {
            progressiveDisclosure = new ProgressiveDisclosure(this, spi);
            progressiveDisclosure.setPixelSizeHint(plotRscData
                    .getPixelSizeHint());
            progressiveDisclosure.setPlotWidth(plotWidth);
            progressiveDisclosure.setDensity(plotDensity);

            progressiveDisclosure.setMapDescriptor(mapDescriptor);
        }

        metadataMap = new HashMap<String, RequestConstraint>(
                plotRscData.getMetadataMap());
        if (isFcst) {
            String[] dts = rscName.getCycleTime().toString().split(" ");
            cycleTimeStr = new String(dts[0] + " "
                    + dts[1].substring(0, dts[1].length() - 2));
            timeLogger.append("Cycletime is:" + cycleTimeStr + "\n");
            // fcstFrameLoaderJobPool = new
            // JobPool("Loading stations for forecast point resource", 8,
            // false);

            queueOfFrameTimesAndStations = new ConcurrentLinkedQueue<SpecialQueueEntry>();
        }
        plotModel = plotRscData.getPlotModel();
        RscAttrValue levelKeyValue = plotRscData.getRscAttrSet().getRscAttr(
                "levelKey");
        if (levelKeyValue != null) {
            existingLevel = Integer.parseInt((String) levelKeyValue
                    .getAttrValue());
        }
        existingCondFilter = plotRscData.getConditionalFilter();
        dataRequestor = new NcPlotModelHdf5DataRequestor(plotModel,
                plotRscData.levelKey, metadataMap, this, plotDensity,
                existingCondFilter);
        Tracer.print("< Exit");

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
        } else {
            return legendString
                    + " "
                    + NmapCommon.getTimeStringFromDataTime(fd.getFrameTime(),
                            "/");
        }
    }

    // override to process PlotInfoRscDataObj instead of PlotInfo
    @Override
    protected IRscDataObject[] processRecord(Object pltInfo) {
        Tracer.printX("> Entry");
        if (!(pltInfo instanceof PlotInfo)) {
            out.println("NcPlotResource2.processRecord method expecting PlotInfoRscData objects "
                    + "instead of: " + pltInfo.getClass().getName());
            return new PlotInfoRscDataObj[0];
        }
        Tracer.printX("< Exit");

        return new PlotInfoRscDataObj[] { new PlotInfoRscDataObj(
                (PlotInfo) pltInfo) };
    }

    public void paintFrame(AbstractFrameData fd, final IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        Tracer.print("> Entry");
        currPaintProp = paintProps;
        if (fd == null)
            return;

        if (paintProps.isZooming())
            return;

        FrameData frameData = (FrameData) fd;
        Semaphore sem = new Semaphore(1);
        if (progressiveDisclosure == null) {
            progressiveDisclosure = new ProgressiveDisclosure(this, spi);// assumes
                                                                         // spi
                                                                         // is
                                                                         // not
                                                                         // null

            Collection<Station> stationsToDisclose = progressiveDisclosure
                    .calculateStaticProgDiscDistancesForStations(
                            frameData.stationMap.values(),
                            frameData.dynStations);
            if (stationsToDisclose != null && !stationsToDisclose.isEmpty()) {

                synchronized (stationsToDisclose) {
                    sem.acquireUninterruptibly();
                    for (Station stn : stationsToDisclose) {
                        String stnMapKey = getStationMapKey(stn.info.latitude,
                                stn.info.longitude);
                        Station frameStn = frameData.stationMap.get(stnMapKey);
                        frameStn.distValue = stn.distValue;
                        frameData.stationMap.put(stnMapKey, frameStn);
                    }
                    sem.release();
                }
                Tracer.print("paintFrame() - about to schedule progressive disclosure the frame: "
                        + frameData.getShortFrameTime());

                frameData.progressiveDisclosureInProgress = true;

                progressiveDisclosure.queueListOfStationsToBeDisclosed(
                        frameData.getFrameTime(), stationsToDisclose);
            }

        }

        synchronized (frameData) {
            // Tracer.print
            // ("calling checkAndUpdateProgDisclosureProperties on frame - " +
            // frameData.getShortFrameTime());
            frameData.screenExtentsChangedForCurrentFrame = progressiveDisclosure
                    .checkAndUpdateProgDisclosureProperties();

            sem.acquireUninterruptibly();

            if (frameData.screenExtentsChangedForCurrentFrame) {
                Tracer.print("Screen extents changed in  the frame - "
                        + frameData.getShortFrameTime());
                resetFramePaintFlagForAllFrames();
            }

            sem.release();

            if (frameData.screenExtentsChangedForCurrentFrame
                    || progressiveDisclosure.updateNextPaint
                    || !frameData.isFramePaintedFirstTime) {

                if (!frameData.isFramePaintedFirstTime)
                    frameData.isFramePaintedFirstTime = true;

                if (!frameData.stationMap.isEmpty()) {
                    Tracer.print("Calling from paintFrame() - about to schedule progressive disclosure the frame: "
                            + frameData.getShortFrameTime());

                    frameData.progressiveDisclosureInProgress = true;

                    progressiveDisclosure.queueListOfStationsToBeDisclosed(
                            frameData.getFrameTime(),
                            frameData.stationMap.values());
                } else {
                    Tracer.print("Calling from paintFrame() - no stations in stationMap for frame: "
                            + frameData.getShortFrameTime());
                }

                // TODO??CHECK frameData.progressiveDisclosureInProgress = true;

            }

        }

        List<IVector> listOfWindBarbs = frameData.getListOfWindVectors();

        Collection<DrawableString> collOfStringsToDraw = frameData
                .getStringsToDraw();
        List<SymbolLocationSet> symLocSet = frameData.getListOfSymbols();
        DataTime dt = frameData.getFrameTime();

        if (frameData.progressiveDisclosureInProgress) {
            Tracer.print("Progressive disclosure in progress...aborting paintFrame for "
                    + frameData.getShortFrameTime());
            return;
        }

        boolean symbolDisplayFlag = dataRequestor.imageCreator
                .areThereSymbolsToBeRenderedInTheCurrentFrame(dt);
        boolean textDisplayFlag = dataRequestor.imageCreator
                .areThereTextElementsToBeRenderedInTheCurrentFrame(dt);
        boolean vectorDisplayFlag = dataRequestor.imageCreator
                .areThereVectorsToBeRenderedInTheCurrentFrame(dt);

        Semaphore ss = new Semaphore(1);
        ss.acquireUninterruptibly();
        long t1 = 0;
        long t2 = 0;

        if (symbolDisplayFlag && (symLocSet != null && !symLocSet.isEmpty())) {
            t1 = System.nanoTime();
            synchronized (symLocSet) {
                List<IDisplayable> listOfDisplayables = df
                        .createDisplayElements(paintProps, symLocSet);
                for (IDisplayable each : listOfDisplayables) {
                    each.draw(target, paintProps);
                    each.dispose();
                }
            }
            t2 = System.nanoTime();
            Tracer.printX("Took " + (t2 - t1) / 1000000
                    + " ms to render the symbols in the frame "
                    + frameData.getShortFrameTime());
        }

        if (textDisplayFlag
                && (collOfStringsToDraw != null && !collOfStringsToDraw
                        .isEmpty())) {
            t1 = System.nanoTime();
            synchronized (collOfStringsToDraw) {
                target.drawStrings(collOfStringsToDraw);
            }

            t2 = System.nanoTime();
            Tracer.printX("Took " + (t2 - t1) / 1000000 + " ms to draw "
                    + frameData.drawableStrings.size()
                    + " strings in the frame " + frameData.getShortFrameTime());
        }

        if (vectorDisplayFlag
                && (listOfWindBarbs != null && !listOfWindBarbs.isEmpty())) {
            t1 = System.nanoTime();

            synchronized (listOfWindBarbs) {
                List<IDisplayable> displayElsPoint = df.createDisplayElements(
                        listOfWindBarbs, paintProps);
                for (IDisplayable each : displayElsPoint) {
                    each.draw(target, paintProps);
                    each.dispose();
                }
            }
            t2 = System.nanoTime();
            Tracer.printX("Took " + (t2 - t1) / 1000000
                    + " ms to draw the wind-barbs in the frame "
                    + frameData.getShortFrameTime());
        }
        ss.release();
        Tracer.print("< Exit");
    }

    public void initResource(IGraphicsTarget aTarget) throws VizException {

        Tracer.print("> Entry");
        setInitialPlotDensityFromRscAttrSet();
        this.worldExtent = new PixelExtent(0, descriptor.getGridGeometry()
                .getGridRange().getHigh(0), 0, descriptor.getGridGeometry()
                .getGridRange().getHigh(1));

        // load/populate all the frames in the frameDataMap
        loadFrameData();
        issueRefresh();
        Tracer.print("< Exit");

    }

    @Override
    public void disposeInternal() {
        Tracer.print("> Entry");
        timeLogger.append("\n Invoking NcplotResource2.disposeInternal()\n");
        initialized = false;
        dataRequestor.dispose();
        dataRequestor = null;
        clearImages();
        timeLogger.append("\n Clearing the frameMap\n");
        frameDataMap.clear();
        frameRetrievalPool.cancel();
        frameRetrievalPool = null;
        timeLogger.append("\n Clearing the stationsCache\n");
        progressiveDisclosure = null;

        Tracer.print(timeLogger.toString());
        timeLogger.clearLog();
        Tracer.print("< Exit");
    }

    protected AbstractFrameData createNewFrame(DataTime frameTime, int timeInt) {
        Tracer.print("> Entry");
        FrameData newFrame = new FrameData(frameTime, timeInt);
        if (df == null)
            df = new DisplayElementFactory(NcDisplayMngr
                    .getActiveNatlCntrsEditor().getActiveDisplayPane()
                    .getTarget(), getDescriptor());
        Tracer.print("< Exit");
        return newFrame;
    }

    @Override
    public void clearImages() {
        Tracer.print("> Entry");
        timeLogger.append("\n Invoking NcplotResource2.clearImages()\n");
        for (AbstractFrameData frameData : frameDataMap.values()) {
            Collection<Station> collStns = ((FrameData) frameData).stationMap
                    .values();
            for (Station station : collStns) {
                if (station.stnPlotMap != null) {
                    station.stnPlotMap.clear();
                    station.stnPlotMap = null;
                }
                if (station.info != null) {
                    station.info.plotQueued = false;
                }
            }

        }
        issueRefresh();
        Tracer.print("< Exit");

    }

    @Override
    public void modelGenerated(PlotInfo[] keys, IImage image) {
        Tracer.print("> Entry  [empty!]");
        Tracer.print("< Exit");

    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        Tracer.print("> Entry");
        this.worldExtent = new PixelExtent(0, descriptor.getGridGeometry()
                .getGridRange().getHigh(0), 0, descriptor.getGridGeometry()
                .getGridRange().getHigh(1));

        issueRefresh();
        Tracer.print("< Exit");
    }

    @Override
    public void resourceAttrsModified() {
        Tracer.print("> Entry");
        requeryDataAndReCreateAllStnImages = false;
        onlyReCreateExistingImages = false;
        densityChanged = false;
        ResourceAttrSet rscAttrSet = plotRscData.getRscAttrSet();
        PlotModel editedPlotModel = (PlotModel) rscAttrSet.getRscAttr(
                "plotModel").getAttrValue();
        if (editedPlotModel == null)
            return;

        RscAttrValue levelKeyAttr = rscAttrSet.getRscAttr("levelKey");
        if (levelKeyAttr != null) {
            String newLevelString = (String) levelKeyAttr.getAttrValue();
            int editedLevel = Integer.parseInt((newLevelString));

            if (editedLevel != existingLevel) {
                dataRequestor.setLevelStr(newLevelString);
                requeryDataAndReCreateAllStnImages = true;
            }

        }

        double newDensity = ((Integer) rscAttrSet.getRscAttr("plotDensity")
                .getAttrValue()).doubleValue() / 10;
        if (Math.abs(newDensity - plotDensity) > 0.0001) {
            plotDensity = newDensity;
            densityChanged = true;
        }

        boolean areALLCondFilterParamsInEditedPlotModel = false;
        List<PlotModelElement> oldPMEList = this.plotModel
                .getAllPlotModelElements();
        Set<String> setOfPlotParamsPrevPlotted = new HashSet<String>();
        synchronized (oldPMEList) {
            for (PlotModelElement oldPME : oldPMEList) {
                setOfPlotParamsPrevPlotted.add(oldPME.getParamName());
            }
        }

        Set<String> newPlotParamNamesSet = new HashSet<String>();
        List<PlotModelElement> newPMEList = editedPlotModel
                .getAllPlotModelElements();
        Set<String> setOfCondParamsAlreadyInPlotModel = new HashSet<String>(0);

        synchronized (newPMEList) {
            for (PlotModelElement newPME : newPMEList) {
                newPlotParamNamesSet.add(newPME.getParamName());
            }
        }

        RscAttrValue rscAttrValCondFilter = rscAttrSet
                .getRscAttr("conditionalFilter");
        ConditionalFilter cf = null;

        /*
         * Check if there is a current conditional filter
         */
        if (rscAttrValCondFilter != null) {
            cf = (ConditionalFilter) rscAttrValCondFilter.getAttrValue();
            if (cf.getConditionalFilterMap() != null
                    && !cf.getConditionalFilterMap().isEmpty()) {
                dataRequestor.setConditionalFilter(cf);
                dataRequestor.setUpConditionalFilterParameters();
                /*
                 * Replace the existing conditional filter
                 */
                this.existingCondFilter = new ConditionalFilter(cf);
                dataRequestor.imageCreator.isThereAConditionalFilter = false;
                isThereAConditionalFilter = true;
                Tracer.print("Added the conditional filter\n");
                Map<String, RequestConstraint> mapOfCondFilters = dataRequestor
                        .getConditionalFilterMap();
                if (mapOfCondFilters != null && !mapOfCondFilters.isEmpty()) {

                    synchronized (mapOfCondFilters) {
                        Set<String> keySet = mapOfCondFilters.keySet();
                        for (String condParamName : keySet) {
                            synchronized (newPlotParamNamesSet) {
                                if (!newPlotParamNamesSet
                                        .contains(condParamName)) {
                                    areALLCondFilterParamsInEditedPlotModel = false;
                                    break;
                                } else
                                    areALLCondFilterParamsInEditedPlotModel = true;
                            }
                        }

                        if (!areALLCondFilterParamsInEditedPlotModel) {
                            requeryDataAndReCreateAllStnImages = true;
                        }

                    }

                }
            } else {
                /*
                 * Else check if there was a previously existing conditional
                 * filter
                 */
                if (isThereAConditionalFilter) {
                    this.existingCondFilter = null;
                    dataRequestor.setConditionalFilter(null);
                    dataRequestor
                            .updateConditionalFilterMapFromConditionalFilter(null);
                    dataRequestor.imageCreator.isThereAConditionalFilter = false;
                    isThereAConditionalFilter = false;
                    densityChanged = true;
                    Tracer.print("Removed the conditional filter\n");
                }

            }

        }

        PlotParameterDefns ppdefs = PlotParameterDefnsMngr.getInstance()
                .getPlotParamDefns(this.plotModel.getPlugin());

        /*
         * Check for the presence of any conditional coloring parameter
         */

        synchronized (newPMEList) {
            for (PlotModelElement newPME : newPMEList) {
                if (newPME.hasAdvancedSettings()) {
                    if (newPlotParamNamesSet.contains(newPME
                            .getConditionalParameter())) {
                        setOfCondParamsAlreadyInPlotModel.add(newPME
                                .getConditionalParameter());
                    } else {

                        requeryDataAndReCreateAllStnImages = true;
                        break;
                    }
                }

            }
        }

        if (setOfPlotParamsPrevPlotted != null
                && !setOfPlotParamsPrevPlotted.isEmpty()) {
            if (setOfPlotParamsPrevPlotted.size() != newPlotParamNamesSet
                    .size()) {
                requeryDataAndReCreateAllStnImages = true;
            } else {

                synchronized (newPlotParamNamesSet) {
                    for (String newParam : newPlotParamNamesSet) {
                        if (!setOfPlotParamsPrevPlotted.contains(newParam)) {
                            requeryDataAndReCreateAllStnImages = true;
                            break;
                        }
                    }
                }

            }

        }

        this.plotModel = editedPlotModel;
        dataRequestor.setPlotModel(editedPlotModel);

        if (requeryDataAndReCreateAllStnImages) {
            Tracer.print("Need to requery data (and recreate all station images)");
            try {
                dataRequestor
                        .updateListOfParamsToPlotFromCurrentPlotModel(editedPlotModel);
                dataRequestor
                        .determineConditionalColoringParameters(editedPlotModel);
                // if( cf != null ){
                // dataRequestor.setConditionalFilter(cf);
                // dataRequestor.setUpConditionalFilterParameters();
                // }

                dataRequestor.imageCreator.setPlotModel(editedPlotModel);
                dataRequestor.imageCreator
                        .setUpPlotPositionToPlotModelElementMapping(editedPlotModel);

                /* To remove the obsolete strings from the diff maps */
                synchronized (newPMEList) {
                    for (PlotModelElement newPME : newPMEList) {
                        synchronized (oldPMEList) {
                            for (PlotModelElement oldPME : oldPMEList) {
                                if (newPME.getPosition().compareTo(
                                        oldPME.getPosition()) == 0) {
                                    if (newPME.getParamName().compareTo(
                                            oldPME.getParamName()) != 0) {
                                        dataRequestor.imageCreator
                                                .removeObsoletePlotEntriesAtThisPositionForAllFrames(dataRequestor.imageCreator
                                                        .getPositionFromPlotModelElementPosition(newPME
                                                                .getPosition()));
                                    }

                                }
                            }
                        }
                    }
                }

                dataRequestor.imageCreator
                        .removeObsoletePMEEntries(editedPlotModel);
                dataRequestor.imageCreator.setUpSymbolMappingTables();
                List<DataTime> dtList = getFrameTimes();
                synchronized (dtList) {
                    for (DataTime dt : dtList) {
                        FrameData fd = (FrameData) getFrame(dt);
                        if (densityChanged) {
                            Tracer.print("About to queue stations for prog disc");
                            progressiveDisclosure.setDensity(plotDensity);
                            dataRequestor.setPlotDensity(plotDensity);
                            fd.progressiveDisclosureInProgress = true;
                            progressiveDisclosure
                                    .queueListOfStationsToBeDisclosed(dt,
                                            fd.stationMap.values());

                        } else {
                            Tracer.print("About to queue stations for data requery");
                            dataRequestor.queueStationsForHdf5Query(dt,
                                    fd.setOfStationsLastRendered);
                        }

                        issueRefresh();
                    }
                }
            } catch (VizException e) {
                e.printStackTrace();
            }
        }

        else {

            if (!setOfCondParamsAlreadyInPlotModel.isEmpty())
                dataRequestor
                        .determineConditionalColoringParameters(editedPlotModel);

            dataRequestor.imageCreator.setPlotModel(editedPlotModel);
            dataRequestor.imageCreator
                    .setUpPlotPositionToPlotModelElementMapping(editedPlotModel);
            dataRequestor.imageCreator
                    .removeObsoletePMEEntries(editedPlotModel);
            dataRequestor.imageCreator.setUpSymbolMappingTables();
            List<DataTime> dtList = getFrameTimes();
            synchronized (dtList) {
                for (DataTime dt : dtList) {
                    FrameData fd = (FrameData) getFrame(dt);
                    if (densityChanged) {
                        progressiveDisclosure.setDensity(plotDensity);
                        dataRequestor.setPlotDensity(plotDensity);
                        Tracer.print("About to queue stations for pg disc");

                        fd.progressiveDisclosureInProgress = true;
                        progressiveDisclosure.queueListOfStationsToBeDisclosed(
                                dt, fd.stationMap.values());
                        issueRefresh();
                    } else {
                        if (areALLCondFilterParamsInEditedPlotModel) {
                            Tracer.print("About to apply conditional filter to stns last rendered");

                            dataRequestor
                                    .updateListOfStationsPerConditionalFilter(
                                            dt, fd.setOfStationsLastRendered);
                            if (!setOfCondParamsAlreadyInPlotModel.isEmpty()) {
                                synchronized (fd.setOfStationsLastRendered) {
                                    for (Station currentStation : fd.setOfStationsLastRendered) {
                                        synchronized (setOfCondParamsAlreadyInPlotModel) {
                                            for (String condParamName : setOfCondParamsAlreadyInPlotModel) {
                                                dataRequestor
                                                        .processConditionalParameterForEachStation(
                                                                currentStation.listOfParamsToPlot,
                                                                currentStation,
                                                                ppdefs.getPlotParamDefn(
                                                                        condParamName)
                                                                        .getMetParamName());
                                            }
                                        }
                                    }
                                }

                            }
                        } else {
                            if (!requeryDataAndReCreateAllStnImages) {
                                if (!setOfCondParamsAlreadyInPlotModel
                                        .isEmpty()) {
                                    synchronized (fd.setOfStationsLastRendered) {
                                        for (Station currentStation : fd.setOfStationsLastRendered) {
                                            synchronized (setOfCondParamsAlreadyInPlotModel) {
                                                for (String condParamName : setOfCondParamsAlreadyInPlotModel) {
                                                    dataRequestor
                                                            .processConditionalParameterForEachStation(
                                                                    currentStation.listOfParamsToPlot,
                                                                    currentStation,
                                                                    ppdefs.getPlotParamDefn(
                                                                            condParamName)
                                                                            .getMetParamName());
                                                }
                                            }
                                        }
                                    }
                                }
                                {
                                    Tracer.print("Queueing stations only for image creation");
                                    dataRequestor.imageCreator
                                            .queueStationsToCreateImages(
                                                    dt,
                                                    fd.setOfStationsLastRendered,
                                                    plotDensity);
                                }
                            } else {
                                Tracer.print("Queueing stns for data retrieval");
                                dataRequestor.queueStationsForHdf5Query(dt,
                                        fd.setOfStationsLastRendered);
                            }
                        }
                    }
                    issueRefresh();
                }
            }
        }
        Tracer.print("< Exit");

    }

    @Override
    public void disclosureComplete(DataTime time,
            Collection<Station> disclosedStations) {
        Tracer.print("> Entry");
        Semaphore sem = new Semaphore(1);
        sem.acquireUninterruptibly();

        if (disclosedStations != null && disclosedStations.size() > 0) {
            Tracer.print("Prog disc returned " + disclosedStations.size()
                    + " stations" + " for frame: "
                    + Tracer.shortTimeString(time));

            dataRequestor.queueStationsForHdf5Query(time, disclosedStations);
        }
        sem.release();
        Tracer.print("< Exit");

    }

    @Override
    public void renderingComplete(DataTime time,
            Collection<Station> collectionOfStationsToBeRendered,
            List<DrawableString> listOfStringsToDraw,
            List<IVector> listOfVectors,
            List<SymbolLocationSet> listOfSymbolLocSet) {

        Tracer.print("> Entry");
        Semaphore sm = new Semaphore(1);
        sm.acquireUninterruptibly(1);
        FrameData fd = ((FrameData) getFrame(time));

        if (listOfStringsToDraw != null && !listOfStringsToDraw.isEmpty())
            fd.drawableStrings = new ArrayList<DrawableString>(
                    listOfStringsToDraw);
        else
            fd.drawableStrings = new ArrayList<DrawableString>(0);

        if (listOfVectors != null && !listOfVectors.isEmpty())
            fd.listOfWindVectors = new ArrayList<IVector>(listOfVectors);
        else
            fd.listOfWindVectors = new ArrayList<IVector>(0);

        if (listOfSymbolLocSet != null && !listOfSymbolLocSet.isEmpty()) {
            fd.listOfSymbolLocSet = new ArrayList<SymbolLocationSet>(
                    listOfSymbolLocSet);
        } else
            fd.listOfSymbolLocSet = new ArrayList<SymbolLocationSet>(0);

        fd.setOfStationsLastRendered = new HashSet<Station>();
        fd.setOfStationsLastRendered.addAll(collectionOfStationsToBeRendered);

        for (Station stn : collectionOfStationsToBeRendered) {
            String stnKey = getStationMapKey(stn.info.latitude.doubleValue(),
                    stn.info.longitude.doubleValue());
            synchronized (fd.stationMap) {
                fd.stationMap.put(stnKey, stn);
            }
        }

        fd.progressiveDisclosureInProgress = false;
        Tracer.print("renderingComplete() called for the frame "
                + Tracer.shortTimeString(time) + " with "
                + collectionOfStationsToBeRendered.size() + " stations");
        sm.release(1);
        issueRefresh();
        Tracer.print("< Exit");

    }

    // private void getStationsForFrame( DataTime frameTime ){
    // FrameData frameData = ( FrameData ) getFrame(frameTime);
    // RequestConstraint timeConstraint = new RequestConstraint();
    //
    // // set the constraints for the query.
    // // for forecast resources we need to match the cycle time and
    // // create a range constraint on the forecast hour.
    // if( isFcst ) {
    //
    // //cycleTimeStr - gets initialized during resource creation
    // timeConstraint = new RequestConstraint( cycleTimeStr );
    // metadataMap.put("dataTime.refTime", timeConstraint );
    //
    // // determine the fcst hr range for the frame span
    // long refTimeMs = rscName.getCycleTime().getRefTime().getTime();
    // long frmStartMs = frameData.getFrameStartTime().getRefTime().getTime();
    // long frmEndMs = frameData.getFrameEndTime().getRefTime().getTime();
    //
    // long beginFcstHr = (frmStartMs - refTimeMs) / 1000;
    // long endFcstHr = (frmEndMs - refTimeMs) / 1000;
    //
    // timeConstraint = new RequestConstraint( );
    // // timeConstraint.setBetweenValueList(
    // // new String[] { Long.toString( beginFcstHr ),
    // // Long.toString( endFcstHr ) } );
    // //
    // // timeConstraint.setConstraintType( ConstraintType.BETWEEN );
    // // metadataMap.put( "dataTime.fcstTime", timeConstraint );
    // timeConstraint.setConstraintValue(frameData.getFrameTime().toString());
    // metadataMap.put( "dataTime", timeConstraint );
    // timeConstraint = new RequestConstraint("64800", ConstraintType.EQUALS);
    // metadataMap.put("forecasttime", timeConstraint);
    // }
    // else{
    //
    // String[] constraintList = { frameData.getFrameStartTime().toString(),
    // frameData.getFrameEndTime().toString() };
    // timeConstraint.setBetweenValueList( constraintList );
    // timeConstraint.setConstraintType(
    // RequestConstraint.ConstraintType.BETWEEN );
    // metadataMap.put("dataTime", timeConstraint );
    //
    // }
    //
    // try {
    // Tracer.print("metadataMap = " + metadataMap);
    // plotRscData.getPlotInfoRetriever().getStations( metadataMap );
    // /*
    // * The retrieved stations are time-matched to the frame using the
    // resourceChanged() method
    // */
    // } catch (VizException e) {
    //
    // e.printStackTrace();
    // }
    //
    // }

    // generate a string used as the key for the StationMap

    private String getStationMapKey(Double lat, Double lon) {
        return new String("" + Math.round(lat * 1000.0) + ","
                + Math.round(lon * 1000.0));
    }

    private void resetFramePaintFlagForAllFrames() {
        Tracer.print("> Entry");
        List<DataTime> dtList = getFrameTimes();
        Semaphore sm1 = new Semaphore(1);
        sm1.acquireUninterruptibly();
        synchronized (dtList) {
            for (DataTime datatime : dtList) {
                ((FrameData) getFrame(datatime)).isFramePaintedFirstTime = false;
            }
        }
        sm1.release();
        Tracer.print("< Exit");
    }

    private void setInitialPlotDensityFromRscAttrSet() {
        Tracer.print("> Entry");
        double newPlotDensity = ((Integer) plotRscData.getRscAttrSet()
                .getRscAttr("plotDensity").getAttrValue()).doubleValue() / 10;
        if (plotDensity == Double.MIN_NORMAL
                || Math.abs(plotDensity - newPlotDensity) > 0.00000001) {
            plotDensity = newPlotDensity;

        }
        Tracer.print("< Exit");

    }

    private void loadFrameData() {
        Tracer.print("> Entry");
        List<DataTime> listOfFrameTimes = new ArrayList<DataTime>(
                getFrameTimes());
        int frameTimesListSize = (listOfFrameTimes != null) ? listOfFrameTimes
                .size() : 0;
        // int index = ( isFcst ) ? 0 : frameTimesListSize - 1;

        long t0 = System.nanoTime();

        if (!isFcst) {

            for (int index = frameTimesListSize - 1; index >= 0; --index) {
                frameLoaderTask = new FrameLoaderTask(
                        listOfFrameTimes.get(index));
                frameRetrievalPool.schedule(frameLoaderTask);
                // --index;
            }
        } else {
            populateStationsForTheFcstPointRscFrames();
        }

        long t1 = System.nanoTime();
        timeLogger.append("Finished loading " + frameTimesListSize + " in "
                + ((t1 - t0) / 1000) + " microseconds" + "\n");
        Tracer.print("< Exit");
    }

    @Override
    public void messageGenerated(PlotInfo[] key, String message) {
        Tracer.print("> Entry [empty!]");
        Tracer.print("< Exit");
    }

    private void populateStationsForTheFcstPointRscFrames() {
        Tracer.print("> Entry");
        List<DataTime> datatimeList = getFrameTimes();
        synchronized (datatimeList) {
            FrameData firstFrame = (FrameData) getFrame(datatimeList.get(0));
            String tableName = this.metadataMap.get("pluginName")
                    .getConstraintValue();
            String query = "select distinct(" + tableName + ".forecastTime), "
                    + tableName + ".rangeEnd" + " from " + tableName
                    + " where reftime = '" + cycleTimeStr + "';";
            try {
                List<Object[]> results = null;
                results = DirectDbQuery.executeQuery(query, "metadata",
                        QueryLanguage.SQL);
                List<Integer> intList = new ArrayList<Integer>();
                List<Timestamp> timeStampList = new ArrayList<Timestamp>();
                if (results != null) {
                    for (Object[] objArr : results) {
                        for (Object o : objArr) {
                            if (o instanceof Integer) {
                                Integer i = (Integer) o;
                                intList.add((Integer) i);
                            }
                            if (o instanceof Timestamp) {
                                timeStampList.add((Timestamp) o);
                            }

                        }
                    }
                    Integer[] iArr = new Integer[0];
                    Timestamp[] tArr = new Timestamp[0];
                    if (!intList.isEmpty()) {
                        iArr = intList.toArray(new Integer[0]);
                        Arrays.sort(iArr);

                        if (!timeStampList.isEmpty()) {
                            tArr = timeStampList.toArray(new Timestamp[0]);
                            Arrays.sort(tArr);

                            Date cycleTimeDate = rscName.getCycleTime()
                                    .getRefTime();

                            int index = 1;
                            boolean retrievedStationsForFirstFrame = false;
                            for (index = 0; index < datatimeList.size(); index++) {
                                DataTime nextFrameTime = datatimeList
                                        .get(index);
                                int indexOfTimeStamp = 0;
                                int sizeOfTimestampList = tArr.length;
                                for (indexOfTimeStamp = 0; indexOfTimeStamp < sizeOfTimestampList; indexOfTimeStamp++) {
                                    Timestamp timeStamp = tArr[indexOfTimeStamp];
                                    if (timeStamp.getTime() == nextFrameTime
                                            .getRefTime().getTime()) {
                                        int fcstHr = iArr[indexOfTimeStamp]
                                                .intValue();
                                        DataTime newTime = new DataTime(
                                                cycleTimeDate);
                                        newTime.setFcstTime(fcstHr);
                                        newTime.setUtilityFlags(firstFrame
                                                .getFrameTime()
                                                .getUtilityFlags());
                                        Tracer.print("nextFrameTime = "
                                                + Tracer.shortTimeString(nextFrameTime)
                                                + "\n" + "newTime = "
                                                + newTime.toString());

                                        /*
                                         * For a fore-cast point resource, the
                                         * number of stations remain the same
                                         * for all fore-cast hours. So a
                                         * Postgres query is carried out only
                                         * once (for the first frame) and the
                                         * same set of stations is subsequently
                                         * copied to the remaining frames, after
                                         * updating the stations' data-times to
                                         * match that of the frame it will now
                                         * belong to....
                                         */

                                        if (!retrievedStationsForFirstFrame) {

                                            /*
                                             * If this is the first frame
                                             * retrieve the stations from
                                             * Postgres
                                             */
                                            FrameData frameData = (FrameData) getFrame(nextFrameTime);
                                            HashMap<String, RequestConstraint> frameRCMap = new HashMap<String, RequestConstraint>();
                                            frameRCMap.put("pluginName",
                                                    metadataMap
                                                            .get("pluginName"));
                                            frameRCMap
                                                    .put("dataTime.fcstTime",
                                                            new RequestConstraint(
                                                                    String.valueOf(iArr[indexOfTimeStamp]
                                                                            .intValue())));
                                            frameRCMap.put("dataTime.refTime",
                                                    new RequestConstraint(
                                                            cycleTimeStr));
                                            frameData.plotInfoObjs = plotRscData
                                                    .getPlotInfoRetriever()
                                                    .getStations(frameRCMap);

                                            if (frameData.plotInfoObjs != null
                                                    && !frameData.plotInfoObjs
                                                            .isEmpty()) {
                                                synchronized (frameData.plotInfoObjs) {

                                                    boolean isFramePopulated = frameData
                                                            .populateFrame();
                                                    if (isFramePopulated) {
                                                        retrievedStationsForFirstFrame = true;
                                                        Collection<Station> stationsToBeQueuedForProgDisc = progressiveDisclosure
                                                                .calculateStaticProgDiscDistancesForStations(
                                                                        frameData.stationMap
                                                                                .values(),
                                                                        frameData.dynStations);
                                                        if (stationsToBeQueuedForProgDisc != null
                                                                && !stationsToBeQueuedForProgDisc
                                                                        .isEmpty()) {
                                                            synchronized (stationsToBeQueuedForProgDisc) {
                                                                for (Station stn : stationsToBeQueuedForProgDisc) {
                                                                    String stnMapKey = getStationMapKey(
                                                                            stn.info.latitude,
                                                                            stn.info.longitude);
                                                                    Station frameStn = frameData.stationMap
                                                                            .get(stnMapKey);
                                                                    frameStn.distValue = stn.distValue;
                                                                    synchronized (frameData.stationMap) {
                                                                        frameData.stationMap
                                                                                .put(stnMapKey,
                                                                                        frameStn);
                                                                    }
                                                                }

                                                            }
                                                        }

                                                    }

                                                }

                                            }
                                        }

                                        queueStationsForRemainingFcstFrames(
                                                nextFrameTime, newTime,
                                                firstFrame.stationMap.values());
                                    }
                                }

                            }

                        }
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }

        }
        Tracer.print("< Exit");
    }

    private void queueStationsForRemainingFcstFrames(DataTime frameTime,
            DataTime stnTimeToSet, Collection<Station> stationCollection) {
        Tracer.print("> Entry");
        SpecialQueueEntry queueEntry = new SpecialQueueEntry(frameTime,
                stnTimeToSet, stationCollection);
        queueOfFrameTimesAndStations.add(queueEntry);
        scheduleFcstFrameLoaderTask();
        Tracer.print("< Exit");

    }

    private void scheduleFcstFrameLoaderTask() {
        Tracer.print("> Entry");
        if (queueOfFrameTimesAndStations == null
                || queueOfFrameTimesAndStations.isEmpty())
            return;
        Semaphore sm = new Semaphore(1);
        synchronized (queueOfFrameTimesAndStations) {
            while (queueOfFrameTimesAndStations.peek() != null) {
                sm.acquireUninterruptibly();
                SpecialQueueEntry currEntry = queueOfFrameTimesAndStations
                        .poll();
                if (currEntry == null)
                    continue;

                fcstFrameLoaderTask = new FcstFrameLoaderTask(
                        currEntry.getDataTime(), currEntry.stationTimeToSet,
                        currEntry.getStations());
                sm.release();
                frameRetrievalPool.schedule(fcstFrameLoaderTask);

            }
        }
        Tracer.print("< Exit");

    }

}
