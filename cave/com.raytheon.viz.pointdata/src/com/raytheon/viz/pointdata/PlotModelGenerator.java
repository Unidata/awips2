/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.viz.pointdata;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.ResourceType;

/**
 * A Eclipse Job thread that will listen for new stations on a queue and request
 * the data to create the plots.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/20/2006              brockwoo    Initial creation
 * 12/06/2006              brockwoo    Implemented code review changes
 * 05/11/2007   #273       brockwoo    Implemented JavaScript obs request
 * 04/16/2008              njensen     createActionASCII uses ScriptCreator
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */
public class PlotModelGenerator extends Job {

    private final ConcurrentLinkedQueue<String> stationQueue;

    private String inspectUri = null;

    // private boolean isRunning = true;
    private final Connector asciiConnection;

    private final ConcurrentHashMap<String, ImageContainer> icaoImageMap;

    private final ConcurrentHashMap<String, IImage> icaoImages;

    private boolean looping;

    private final PlotModelFactory plotCreator;

    private final IGraphicsTarget target;

    private final String plugin;

    private class ImageContainer {
        public PluginDataObject record = null;

        public boolean queued = false;

        public boolean available = false;
    }

    /**
     * Initializes the thread with the station's reference time and the target
     * to create textures for.
     * 
     * @param refTime
     *            A string with the reference time needed to get the station out
     *            of Lucene
     * @param target
     *            The graphic target to create the tiles for
     * @throws VizException
     */
    public PlotModelGenerator(IGraphicsTarget aTarget,
            IMapDescriptor mapDescriptor, String plotModelFile, String plugin)
            throws VizException {
        super("Creating Plots...");
        stationQueue = new ConcurrentLinkedQueue<String>();
        asciiConnection = Connector.getInstance();
        icaoImageMap = new ConcurrentHashMap<String, ImageContainer>();
        this.icaoImages = new ConcurrentHashMap<String, IImage>();
        plotCreator = new PlotModelFactory(mapDescriptor, plotModelFile);
        this.target = aTarget;
        this.plugin = plugin;
        this.looping = false;
    }

    public int getPlotModelWidth() {
        return this.plotCreator.getDefinedPlotModelWidth();
    }

    public void setPlotModelSize(long width) {
        this.plotCreator.setPlotDimensions(width, width);
        this.cleanImages();
    }

    public void setPlotModelColor(RGB color) {
        this.plotCreator.setColor(color);
        this.cleanImages();
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        // while (isRunning) {
        while (stationQueue.size() > 0 || inspectUri != null) {
            IImage image = null;
            ArrayList<String> queuePurgeList = new ArrayList<String>();
            ArrayList<String> stationQuery = new ArrayList<String>();

            synchronized (this) {
                if (inspectUri != null) {
                    if (!icaoImageMap.containsKey(inspectUri)) {
                        ImageContainer newPlot = new ImageContainer();
                        newPlot.queued = true;
                        icaoImageMap.put(inspectUri, newPlot);
                        this.stationQueue.add(inspectUri);
                    } else {
                        if (!this.icaoImageMap.get(inspectUri).queued) {
                            this.icaoImageMap.get(inspectUri).queued = true;
                            this.stationQueue.add(inspectUri);
                        }
                    }
                    if (icaoImageMap.get(inspectUri).record == null) {
                        stationQuery.add(inspectUri);
                    }
                    inspectUri = null;
                } else {
                    String[] stationList = stationQueue
                            .toArray(new String[stationQueue.size()]);
                    int stationCounter;
                    for (stationCounter = 0; stationCounter < stationList.length; stationCounter++) {
                        queuePurgeList.add(stationList[stationCounter]);
                        if (this.icaoImageMap.get(stationList[stationCounter]).record == null) {
                            stationQuery.add(stationList[stationCounter]);
                        }
                        if (stationCounter == 74) {
                            break;
                        }
                    }
                }
            }

            if (stationQuery.size() > 0) {
                try {
                    String script = createActionASCII(stationQuery);
                    Object[] obs = asciiConnection.connect(script, null, 60000);

                    for (Object plugindo : obs) {
                        PluginDataObject ob = (PluginDataObject) plugindo;
                        ImageContainer ic = icaoImageMap.get(ob.getDataURI());
                        if (ic != null) {
                            ic.record = ob;
                        } else {
                            System.out.println("Missing: " + ob.getDataURI());
                        }
                    }
                } catch (VizException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
            int counter = 0;
            for (int uriCounter = 0; uriCounter < queuePurgeList.size(); uriCounter++) {
                try {
                    String dataUri = queuePurgeList.get(uriCounter);
                    PluginDataObject ob = this.icaoImageMap.get(dataUri).record;
                    if (ob != null && !this.icaoImageMap.get(dataUri).available) {
                        image = target.initializeRaster(new IODataPreparer(
                                plotCreator.getStationPlot(ob),
                                ob.getDataURI(), 0), null);
                        icaoImages.put(ob.getDataURI(), image);
                        icaoImageMap.get(ob.getDataURI()).available = true;
                        icaoImageMap.get(ob.getDataURI()).queued = false;
                    } else { // For what ever reason, the datauri did not
                        // return a PluginDataObject
                        // System.out.println(dataUri);
                        BufferedImage bufferedImage = new BufferedImage(10, 10,
                                BufferedImage.TYPE_BYTE_INDEXED);
                        image = target.initializeRaster(new IODataPreparer(
                                bufferedImage, dataUri, 0), null);
                        icaoImages.put(dataUri, image);
                        icaoImageMap.get(dataUri).available = true;
                        icaoImageMap.get(dataUri).queued = false;
                    }

                } catch (VizException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                counter++;
            }
            if (queuePurgeList.size() > 0) {
                stationQueue.removeAll(queuePurgeList);
                queuePurgeList.clear();
                if (!this.looping) {
                    target.setNeedsRefresh(true);
                }
            }
        }
        /*
         * try { Thread.sleep(5); } catch (InterruptedException e) { }
         */
        // this.schedule(5);
        // }
        return Status.OK_STATUS;
    }

    /**
     * Adds a station to the queue
     * 
     * @param station
     *            A string for a valid station within Lucene
     * @return If the station was added to the queue properly
     */
    public void queueStations(ArrayList<String> stationList, boolean looping) {
        for (String dataUri : stationList) {
            if (!icaoImageMap.containsKey(dataUri)) {
                ImageContainer newPlot = new ImageContainer();
                newPlot.queued = true;
                icaoImageMap.put(dataUri, newPlot);
                this.stationQueue.add(dataUri);
            } else {
                if (!this.icaoImageMap.get(dataUri).queued) {
                    this.icaoImageMap.get(dataUri).queued = true;
                    this.stationQueue.add(dataUri);
                }
            }
        }
        if (!(this.getState() == Job.RUNNING)) {
            this.schedule();
        }
        this.looping = looping;
    }

    public void setStation(String dataUri) {
        icaoImageMap.put(dataUri, new ImageContainer());
    }

    public void setStation(PluginDataObject pdo) {
        ImageContainer newDataUri = new ImageContainer();
        newDataUri.record = pdo;
        this.icaoImageMap.put(pdo.getDataURI(), newDataUri);
    }

    public String getStationMessage(String dataUri) {

        String message = null;
        if (icaoImageMap.get(dataUri) != null
                && icaoImageMap.get(dataUri).record != null) {
            message = (String) icaoImageMap.get(dataUri).record
                    .getMessageData();
        } else {
            message = "Generating...";
            synchronized (this) {
                if (dataUri != null && !dataUri.equals(inspectUri)) {
                    inspectUri = dataUri;
                    if (!(this.getState() == Job.RUNNING)) {
                        this.schedule();
                    }
                }
            }
        }

        return message;
    }

    public PluginDataObject getStationObject(String dataUri) {
        if (!this.icaoImageMap.containsKey(dataUri)) {
            return null;
        } else {
            return this.icaoImageMap.get(dataUri).record;
        }
    }

    /**
     * Will return the texture for a station.
     * 
     * @param station
     *            The station to get the texture for
     * @return The texture
     */
    public IImage getStation(String dataUri) {
        return icaoImages.get(dataUri);
    }

    /**
     * Checks to see if the station is in the queue. This can be called to make
     * sure that a station is not added twice to be created.
     * 
     * @param station
     *            The station to check for
     * @return A boolean indicating if the station is already set to be
     *         processed
     */
    public boolean isQueued(String dataUri) {
        if (!this.icaoImageMap.containsKey(dataUri)) {
            return false;
        }
        return icaoImageMap.get(dataUri).queued;
    }

    /**
     * Will check to see if the station already has a texture available.
     * 
     * @param station
     *            The station to check for
     * @return A boolean indicating the texture status
     */
    public boolean hasImage(String dataUri) {
        if (!this.icaoImageMap.containsKey(dataUri)) {
            return false;
        }
        return icaoImageMap.get(dataUri).available;
    }

    /**
     * Kills the thread.
     * 
     */
    public void shutdown() {
        this.cancel();
        cleanImages();
    }

    public void cleanImages() {
        // Clean up images
        Set<String> imageDataUris = icaoImages.keySet();
        for (String imageDataUri : imageDataUris) {
            icaoImages.get(imageDataUri).dispose();
            icaoImages.remove(imageDataUri);
            this.icaoImageMap.get(imageDataUri).available = false;
            this.icaoImageMap.get(imageDataUri).queued = false;
        }
    }

    private String createActionASCII(ArrayList<String> stationQuery)
            throws VizException {

        LayerProperty prop = new LayerProperty();

        HashMap<String, RequestConstraint> params = new HashMap<String, RequestConstraint>();

        RequestConstraint pluginName = new RequestConstraint();
        pluginName.setConstraintValue(this.plugin);
        params.put("pluginName", pluginName);

        RequestConstraint ids = new RequestConstraint();
        ids.setConstraintType(ConstraintType.IN);
        ids.setConstraintValueList(stationQuery.toArray(new String[stationQuery
                .size()]));
        params.put("dataURI", ids);

        prop.setDesiredProduct(ResourceType.PLAN_VIEW);
        prop.setEntryQueryParameters(params, false);
        prop.setNumberOfImages(stationQuery.size());

        return ScriptCreator.createScript(prop, "plot");
    }

    public void setPlotModelLineStyle(LineStyle lineStyle) {
        plotCreator.setLineStyle(lineStyle);
        cleanImages();
    }

    public void setPlotModelLineWidth(int outlineWidth) {
        plotCreator.setLineWidth(outlineWidth);
        cleanImages();
    }

}
