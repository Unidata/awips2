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
package com.raytheon.uf.viz.d2d.nsharp.rsc;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.d2d.nsharp.display.D2DNSharpPartListener;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpElementDescription;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpAbstractPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;

/**
 * 
 * Minimal wrapper around ncep resource to handle updates and frame counts and
 * initializing data from a requestable resource data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ------------------------------------------
 * Apr 14, 2011           bsteffen  Initial creation
 * Nov 12, 2014  3810     bsteffen  Synchronize access to dataTimes and pdos.
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * Oct 05, 2018  7480     bsteffen  Fix removal of times no longer needed.
 * Dec 14, 2018  6872     bsteffen  Track time more accurately.
 * Jan 15, 2019  7697     bsteffen  Individually add soundings so station info
 *                                  is not shared.
 * Apr 15  2019  7480     bhurley   Improved auto-update
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class D2DNSharpResource
        extends AbstractVizResource<D2DNSharpResourceData, AbstractDescriptor> {

    /*
     * This object should be synchronized whenever accessing dataTimes or pdos.
     */
    private final Object timeLock = new Object();

    /*
     * This object should be synchronized whenever accessing soundingsToRemove.
     */
    private final Object soundingRemovalLock = new Object();

    /* A map of all the plugin data objects for times we have data. */
    private Map<DataTime, D2DNSharpDataObject> pdos = new HashMap<>();

    private List<String> soundingsToRemove = new ArrayList<>();

    private BlockingQueue<D2DNSharpDataObject> dataRequestQueue = new LinkedBlockingQueue<>();

    private BlockingQueue<D2DNSharpDataObject> dataResponseQueue = new LinkedBlockingQueue<>();

    private Job dataRequestJob = new Job("Requesting NSharp Data") {

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            D2DNSharpDataObject pdo = dataRequestQueue.poll();
            while (pdo != null && !monitor.isCanceled()) {
                if (isDataObjectCurrent(pdo)) {
                    resourceData.populateDataObject(pdo);
                    dataResponseQueue.add(pdo);
                    issueRefresh();
                }

                pdo = dataRequestQueue.poll();
            }
            return Status.OK_STATUS;
        }

    };

    private D2DNSharpPartListener partListener;

    public D2DNSharpResource(D2DNSharpResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);

    }

    protected void addDataObject(D2DNSharpDataObject pdo) {
        synchronized (timeLock) {
            for (DataTime dataTime : dataTimes) {
                if ((dataTime.getMatchValid() == pdo.getDataTime()
                        .getMatchValid())
                        && (pdo.getDataTime().getMatchRef() < dataTime
                                .getMatchRef())) {
                    return;
                }
            }
            this.dataTimes.add(pdo.getDataTime());
            pdos.put(pdo.getDataTime(), pdo);
        }
        dataRequestQueue.offer(pdo);
        dataRequestJob.schedule();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        NsharpResourceHandler handler = getHandler();
        /*
         * Last time I checked if you try to add or remove data from nsharp on
         * non-UI threads it can cause major sync issues so all changes to
         * nsharp are done in the paint loop.
         */
        synchronized (soundingRemovalLock) {
            if (!soundingsToRemove.isEmpty()) {
                List<String> soundingsToRemove = this.soundingsToRemove;
                this.soundingsToRemove = new ArrayList<>();
                handler.deleteRsc(soundingsToRemove,
                        resourceData.getPreferedSoundingTitle());
                issueRefresh();
            }
        }
        if (!dataResponseQueue.isEmpty()) {
            NsharpStationInfo stnInfo = null;
            D2DNSharpDataObject pdo = dataResponseQueue.poll();
            while (pdo != null) {
                if (isDataObjectCurrent(pdo) && pdo.getLayers() != null) {
                    stnInfo = pdo.getStationInfo();
                    NsharpElementDescription description = null;
                    String station = stnInfo.getStnId(); 
                    String type = stnInfo.getSndType();
                    Date refTime = stnInfo.getReftime();
                    Date fcstTime = stnInfo.getRangestarttime();
                    if(fcstTime != null){
                        Duration fcstAmount = Duration.between(refTime.toInstant(), fcstTime.toInstant());
                        description = new NsharpElementDescription(station, type, fcstTime.toInstant(), fcstAmount);
                    } else {
                        description = new NsharpElementDescription(station, type, refTime.toInstant());
                    }
                    Map<NsharpElementDescription, List<NcSoundingLayer>> myDataMap = Collections.singletonMap(description, pdo.getLayers());
                    handler.addRsc(stnInfo, myDataMap, false);
                }
                pdo = dataResponseQueue.poll();
            }
            if (stnInfo == null) {
                return;
            }
            issueRefresh();
        }
    }

    private NsharpResourceHandler getHandler() {
        List<NsharpAbstractPaneResource> paneRscs = descriptor.getResourceList()
                .getResourcesByTypeAsType(NsharpAbstractPaneResource.class);
        for (NsharpAbstractPaneResource paneRsc : paneRscs) {
            NsharpResourceHandler handler = paneRsc.getRscHandler();
            if (handler != null) {
                return handler;
            }
        }
        NsharpResourceHandler handler = new NsharpResourceHandler(
                new IRenderableDisplay[] { descriptor.getRenderableDisplay() },
                null);
        for (NsharpAbstractPaneResource paneRsc : paneRscs) {
            paneRsc.setRscHandler(handler);
        }
        return handler;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        getHandler().setSoundingType(resourceData.getSoundingType());
        partListener = new D2DNSharpPartListener(this);
        partListener.enable();
    }

    @Override
    protected void resourceDataChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE && object instanceof Object[]) {
            for (Object obj : (Object[]) object) {
                if (obj instanceof D2DNSharpDataObject) {
                    addDataObject((D2DNSharpDataObject) obj);
                }
            }
        }
    }

    @Override
    public void remove(DataTime dataTime) {
        D2DNSharpDataObject pdo = null;
        synchronized (timeLock) {
            pdo = pdos.remove(dataTime);
            dataTimes.remove(dataTime);
        }
        if (pdo != null) {
            NsharpStationInfo stnInfo = pdo.getStationInfo();
            if (stnInfo != null) {
                synchronized (soundingRemovalLock) {
                    soundingsToRemove.add(stnInfo.getStnDisplayInfo());
                }
            }
        }
    }

    @Override
    protected void disposeInternal() {
        List<DataTime> dataTimes = new ArrayList<>(this.dataTimes);
        for (DataTime time : dataTimes) {
            this.remove(time);
        }
        dataRequestJob.cancel();
        if (partListener != null) {
            partListener.disable();
        }
    }

    public Collection<String> getTimeLineElements() {
        List<String> elements = new ArrayList<>();
        synchronized (timeLock) {
            for (DataTime time : dataTimes) {
                elements.add(
                        pdos.get(time).getStationInfo().getStnDisplayInfo());
            }
        }
        return elements;
    }

    @Override
    public String getName() {
        return "D2D NSharp " + resourceData.getSoundingType() + " "
                + resourceData.getPointName();
    }

    @Override
    public ResourceOrder getResourceOrder() {
        /*
         * Have to be highest resource since the builtin nsharp resources are
         * unknown and they have to be at index 0 on the list or nsharp code
         * breaks.
         */
        return ResourceOrder.HIGHEST;
    }

    @Override
    protected void setProperties(ResourceProperties properties) {
        /*
         * Have to be highest resource since the builtin nsharp resources are
         * unknown and they have to be at index 0 on the list or nsharp code
         * breaks.
         */
        properties.setRenderingOrder(ResourceOrder.HIGHEST);
    }

    @Override
    public DataTime[] getDataTimes() {
        synchronized (timeLock) {
            return super.getDataTimes();
        }
    }

    private boolean isDataObjectCurrent(D2DNSharpDataObject pdo) {
        synchronized (timeLock) {
            return pdos.get(pdo.getDataTime()) == pdo;
        }
    }

}
