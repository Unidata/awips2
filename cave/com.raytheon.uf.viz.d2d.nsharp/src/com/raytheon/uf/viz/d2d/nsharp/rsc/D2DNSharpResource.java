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

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpAbstractPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
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
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.d2d.nsharp.display.D2DNSharpPartListener;

/**
 * 
 * Minimal wrapper arounf ncep resource to handle updates and frame counts and
 * initializing data from a requestable resource data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class D2DNSharpResource extends
        AbstractVizResource<D2DNSharpResourceData, AbstractDescriptor> {

    // A map of all the plugin data objects for times we have data.
    private Map<DataTime, D2DNSharpDataObject> pdos = new HashMap<DataTime, D2DNSharpDataObject>();

    private List<String> soundingsToRemove = new ArrayList<String>();

    private BlockingQueue<D2DNSharpDataObject> dataRequestQueue = new LinkedBlockingQueue<D2DNSharpDataObject>();

    private BlockingQueue<D2DNSharpDataObject> dataResponseQueue = new LinkedBlockingQueue<D2DNSharpDataObject>();

    private Job dataRequestJob = new Job("Requesting NSharp Data") {

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            D2DNSharpDataObject pdo = dataRequestQueue.poll();
            while (pdo != null && !monitor.isCanceled()) {
                if (pdos.get(pdo.getDataTime()) != pdo) {
                    pdo = dataRequestQueue.poll();
                    continue;
                }
                resourceData.populateDataObject(pdo);
                dataResponseQueue.add(pdo);
                issueRefresh();
                pdo = dataRequestQueue.poll();
            }
            return Status.OK_STATUS;
        }

    };

    private D2DNSharpPartListener partListener;

    public D2DNSharpResource(D2DNSharpResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        this.dataTimes = new ArrayList<DataTime>();
    }

    protected void addDataObject(D2DNSharpDataObject pdo) {
        if (!this.dataTimes.contains(pdo.getDataTime())) {
            this.dataTimes.add(pdo.getDataTime());
            Collections.sort(this.dataTimes);
        }
        pdos.put(pdo.getDataTime(), pdo);
        dataRequestQueue.offer(pdo);
        dataRequestJob.schedule();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        NsharpResourceHandler handler = getHandler();
        // Last time I checked if you try to add or remove data from nsharp on
        // non-UI threads it can cause major sync issues so all changes to
        // nsharp are done in the paint loop.
        if (!soundingsToRemove.isEmpty()) {
            List<String> soundingsToRemove = this.soundingsToRemove;
            this.soundingsToRemove = new ArrayList<String>();
            handler.deleteRsc(soundingsToRemove);
            issueRefresh();
        }
        if (!dataResponseQueue.isEmpty()) {
            NsharpStationInfo stnInfo = null;
            Map<String, List<NcSoundingLayer>> myDataMap = new HashMap<String, List<NcSoundingLayer>>();
            D2DNSharpDataObject pdo = dataResponseQueue.poll();
            while (pdo != null) {
                if (pdos.get(pdo.getDataTime()) == pdo) {
                    if (isValidSounding(pdo)) {
                        stnInfo = pdo.getStationInfo();
                        myDataMap.put(stnInfo.getStnDisplayInfo(),
                                pdo.getLayers());
                    }
                }
                pdo = dataResponseQueue.poll();
            }
            if (stnInfo == null) {
                return;
            }
            handler.addRsc(myDataMap, stnInfo, false);
            issueRefresh();
        }
    }

    /**
     * Determine if a dataObject has enough valid layers to build a sounding
     * without freezing or crashing CAVE.
     * 
     * @param pdo
     * @return
     */
    protected boolean isValidSounding(D2DNSharpDataObject pdo) {
        if (pdo.getLayers() == null) {
            return false;
        }

        int numberOfWindLayers = 0;
        for (NcSoundingLayer layer : pdo.getLayers()) {
            if (layer.getWindSpeed() > 0) {
                numberOfWindLayers += 1;
                if (numberOfWindLayers >= 2) {
                    return true;
                }
            }
        }

        return false;
    }

    private NsharpResourceHandler getHandler() throws VizException {
        List<NsharpAbstractPaneResource> paneRscs = descriptor
                .getResourceList().getResourcesByTypeAsType(
                        NsharpAbstractPaneResource.class);
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
        // listen for updates
        resourceData.addChangeListener(new IResourceDataChanged() {

            @Override
            public void resourceChanged(ChangeType type, Object object) {
                if (type == ChangeType.DATA_UPDATE
                        && object instanceof Object[]) {
                    for (Object obj : (Object[]) object) {
                        if (obj instanceof D2DNSharpDataObject) {
                            addDataObject((D2DNSharpDataObject) obj);
                        }
                    }
                }
            }

        });
        partListener = new D2DNSharpPartListener(this);
        partListener.enable();
    }

    @Override
    public void remove(DataTime dataTime) {
        D2DNSharpDataObject pdo = null;
        pdo = pdos.remove(dataTime);
        if (pdo != null) {
            NsharpStationInfo stnInfo = pdo.getStationInfo();
            if (stnInfo != null) {
                soundingsToRemove.add(stnInfo.getStnDisplayInfo());
            }
        }
        dataTimes.remove(dataTime);
    }

    @Override
    protected void disposeInternal() {
        List<DataTime> dataTimes = new ArrayList<DataTime>(this.dataTimes);
        for (DataTime time : dataTimes) {
            this.remove(time);
        }
        dataRequestJob.cancel();
        if (partListener != null) {
            partListener.disable();
        }
    }

    public Collection<String> getTimeLineElements() {
        List<String> elements = new ArrayList<String>();
        for (DataTime time : dataTimes) {
            elements.add(pdos.get(time).getStationInfo().getStnDisplayInfo());
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
        // Have to be highest resource since the builtin nsharp resources are
        // unknown and they have to be at index 0 on the list or nsharp code
        // breaks.
        return ResourceOrder.HIGHEST;
    }

    @Override
    protected void setProperties(ResourceProperties properties) {
        // Have to be highest resource since the builtin nsharp resources are
        // unknown and they have to be at index 0 on the list or nsharp code
        // breaks.
        properties.setRenderingOrder(ResourceOrder.HIGHEST);
    }

}
