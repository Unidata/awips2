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
 */
package com.raytheon.uf.viz.monitor.safeseas.resources;

import java.util.Calendar;
import java.util.Date;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.fog.FogRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.monitor.safeseas.SafeSeasMonitor;
import com.raytheon.uf.viz.monitor.safeseas.listeners.ISSResourceListener;

/**
 * SafeSeasResource
 * 
 * Implements empty display for SAFESEAS
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date           Ticket#     Engineer    Description
 *    ------------   ----------  ----------- --------------------------
 *    July 21, 2010  4891        skorolev    Initial Creation.
 *    Jan  28, 2016  DR 16771    arickert    Opening the SafeSeas dialog here instead of SafeSeasAction.java
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1
 */
public class SafeSeasResource extends
        AbstractVizResource<SafeSeasResourceData, MapDescriptor> implements
        IResourceDataChanged, ISSResourceListener {
	private static final transient IUFStatusHandler statusHandler = UFStatus
			.getHandler(SafeSeasResource.class);

    public DataTime displayedDataTime;

    public Date previousDataTime;

	public FogRecord record;

	private boolean algUpdated = false;

    private Date refHour;

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected SafeSeasResource(SafeSeasResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        resourceData.getSafeSeasMonitor().removeSSResourceListener(this);
        resourceData.getSafeSeasMonitor().closeDialog();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // Open the Monitor for this resource just before we've completed the
        // resource initialization. That is, when
        //     status = ResourceStatus.INITIALIZED in AbstractVizResource.java
        VizApp.runAsync(new Runnable() {
            
            @Override
            public void run() {
                SafeSeasMonitor monitor = SafeSeasMonitor.getInstance();                                                                                           
                if (monitor.getZoneDialog() == null || 
                    monitor.getZoneDialog().isDisposed()) {                                                                                                 
                    Shell shell = PlatformUI.getWorkbench()
                                            .getActiveWorkbenchWindow()                                                                             
                                            .getShell();                                                                                                                           
                    monitor.launchDialog("zone", shell);                                                                                                           
                }       
            } 
        });
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        this.displayedDataTime = paintProps.getDataTime();
        FramesInfo info = paintProps.getFramesInfo();
        int currentFrame = info.getFrameIndex();
        if (info.getFrameTimes() != null) {
            refHour = info.getFrameTimes()[currentFrame].getRefTime();
        }else{
            return;
        }
        this.record = resourceData.dataObjectMap.get(refHour);
        Boolean plot = resourceData.plotted.get(refHour);
        if (plot == null || !plot) {
            if (record != null) {
                record = resourceData.getSSFogThreat().getFogThreat(record);
                resourceData.plotted.put(refHour, new Boolean(true));
                algUpdated = true;
            }
        }
        // updates the dialogs
        if (!refHour.equals(previousDataTime) || algUpdated) {
            this.previousDataTime = refHour;
            algUpdated = false;
            updateDialogTime(refHour);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
		if (type.equals(ChangeType.DATA_UPDATE)) {
			PluginDataObject[] pdos = (PluginDataObject[]) object;
            for (PluginDataObject pdo : pdos) {
                if (pdo instanceof FogRecord) {
                    addRecord((FogRecord) pdo);
                }
            }
		}
        issueRefresh();
    }

    /**
     * @param pdo
     */
    private void addRecord(FogRecord pdo) {
        Calendar rh = pdo.getRefHour();
        if (resourceData.dataObjectMap.containsKey(rh)) {
            try {
                record = resourceData.populateRecord(pdo);
                resourceData.dataObjectMap.put(record.getRefHour().getTime(),
                        record);
                resourceData.plotted.put(record.getRefHour().getTime(), false);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error updating SafeSeas resource", e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        if (this.refHour != null) {
            DataTime dispTime = this.descriptor.getFramesInfo().getFrameTimes()[this.descriptor
                    .getFramesInfo().getFrameIndex()];
            return "SAFESEAS Table Display " + dispTime.getLegendString();
        }
        return "";
    }

    @Override
    public void updateDialogTime(Date time) {
        resourceData.getSafeSeasMonitor().updateDialogTime(time);
    }

	/** grab monitor instance **/
	public SafeSeasMonitor getSSMonitor() {
		return SafeSeasMonitor.getInstance();
	}

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.safeseas.listeners.ISSResourceListener#fogUpdate
     * ()
     */
    @Override
    public void fogUpdate() {
        resourceData.plotted.clear();
        resourceData.fogThreatSS = null;
        issueRefresh();
    }

}
