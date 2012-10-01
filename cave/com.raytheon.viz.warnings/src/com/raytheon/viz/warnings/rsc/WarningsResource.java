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

package com.raytheon.viz.warnings.rsc;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.texteditor.util.SiteAbbreviationUtil;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Resource for displaying warnings
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 1, 2010            jsanchez     Initial creation
 * Aug 22, 2011  10631    njensen  Major refactor
 * May 3, 2012  DR 14741  porricel     Stop setting end time of orig.
 *                                     warning to start time of update.
 * Jun 04, 2012 DR14992  mgamazaychikov Fix the problem with plotting expiration time for 
 *                                  NEW warning when CAN warning is issued
 * Sep 27, 2012  1149     jsanchez     Refactored methods from AbstractWarningsResource into this class.
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class WarningsResource extends AbstractWWAResource {

    protected static class RepaintHeartbeat extends TimerTask {

        private final HashSet<AbstractVizResource<?, ?>> resourceSet = new HashSet<AbstractVizResource<?, ?>>();

        private boolean cancelled = false;

        public RepaintHeartbeat() {

        }

        /**
         * copy resources from old task, just in case some were added after it
         * should have been replaced (threads are fun)
         **/
        public void copyResourceSet(RepaintHeartbeat oldTask) {
            // copy resources, in case one was added after a cancel
            Set<AbstractVizResource<?, ?>> oldResourceSet = oldTask
                    .getResourceSet();
            synchronized (oldResourceSet) {
                for (AbstractVizResource<?, ?> rsc : oldResourceSet) {
                    this.addResource(rsc);
                }
            }
        }

        public Set<AbstractVizResource<?, ?>> getResourceSet() {
            return resourceSet;
        }

        @Override
        public void run() {
            // get the unique displays from all the added resources
            ArrayList<IRenderableDisplay> displaysToRefresh = new ArrayList<IRenderableDisplay>(
                    1);
            synchronized (resourceSet) {
                for (AbstractVizResource<?, ?> rsc : resourceSet) {
                    try {
                        IRenderableDisplay disp = rsc.getDescriptor()
                                .getRenderableDisplay();
                        if (!displaysToRefresh.contains(disp)) {
                            displaysToRefresh.add(disp);
                        }
                    } catch (Exception e) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Encountered error during Warnings Heartbeat, continuing with other Warnings ",
                                        e);
                    }
                }
            }

            // create an array with final modifier
            final IRenderableDisplay[] refreshList = displaysToRefresh
                    .toArray(new IRenderableDisplay[displaysToRefresh.size()]);

            // execute refersh in UI thread
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    for (IRenderableDisplay disp : refreshList) {
                        disp.refresh();
                    }
                }

            });

            // cancel the task if there are no more resources
            boolean cancel = false;
            synchronized (resourceSet) {
                if (resourceSet.size() < 1) {
                    cancel = true;
                }
            }

            if (cancel) {
                doCancel();
            }
        }

        public void addResource(AbstractVizResource<?, ?> rsc) {
            // if task has no resources then it needs to be started when the
            // first is added
            boolean start = false;
            synchronized (resourceSet) {
                // if this is the first resource added to an empty set start the
                // timer
                if (resourceSet.size() < 1) {
                    start = true;
                }
                resourceSet.add(rsc);
            }
            if (start) {
                WarningsResource.scheduleHeartBeat();
            }
        }

        public void removeResource(AbstractVizResource<?, ?> rsc) {
            synchronized (resourceSet) {
                resourceSet.remove(rsc);
                // cancel the task if there are no more resources
                if (resourceSet.size() < 1) {
                    doCancel();
                }
            }
        }

        private void doCancel() {
            synchronized (heartBeatChangeLock) {
                if (cancelled == false) {
                    cancelled = true;
                    heartBeatTimer.cancel();
                    heartBeatTask = new RepaintHeartbeat();
                    heartBeatTimer = new Timer();
                    heartBeatTask.copyResourceSet(this);
                }
            }
        }
    }

    /** lock when changing heartBeatTask **/
    protected static final Object heartBeatChangeLock = new Object();

    protected static RepaintHeartbeat heartBeatTask = null;

    protected static Timer heartBeatTimer = null;

    /**
     * Constructor
     */
    public WarningsResource(WWAResourceData data, LoadProperties props) {
        super(data, props);
        resourceName = "Warnings";
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        DataTime earliest = this.descriptor.getFramesInfo().getFrameTimes()[0];
        requestData(earliest);
        synchronized (heartBeatChangeLock) {
            if (heartBeatTask == null) {
                heartBeatTask = new RepaintHeartbeat();
            }
            heartBeatTask.addResource(this);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        synchronized (heartBeatChangeLock) {
            heartBeatTask.removeResource(this);
        }
        for (WarningEntry entry : entryMap.values()) {
            if (entry.shadedShape != null) {
                entry.shadedShape.dispose();
            }
            if (entry.wireframeShape != null) {
                entry.wireframeShape.dispose();
            }
        }

        entryMap.clear();
        if (warningsFont != null) {
            warningsFont.dispose();
        }
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdo = (PluginDataObject[]) object;
            synchronized (WarningsResource.this) {
                {
                    try {
                        addRecord(pdo);
                    } catch (VizException e) {
                        statusHandler.handle(Priority.SIGNIFICANT,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
        } else if (type == ChangeType.CAPABILITY) {
            if (color != null
                    && color.equals(getCapability((ColorableCapability.class))
                            .getColor()) == false) {
                color = getCapability((ColorableCapability.class)).getColor();

                // TODO this needs to be fixed to work with watches which are
                // shaded
                // for (String dataUri : entryMap.keySet()) {
                // WarningEntry entry = entryMap.get(dataUri);
                // TODO init a shape somewhere else
                // if (entry.shadedShape != null) {
                // entry.shadedShape.dispose();
                // try {
                // initShape(entry.record);
                // } catch (VizException e) {
                // statusHandler.handle(Priority.PROBLEM,
                // e.getLocalizedMessage(), e);
                // }
                // }
                // }
            }
        }
        issueRefresh();
    }

    @Override
    protected void initShape(IGraphicsTarget target,
            AbstractWarningRecord record) throws VizException {
        Geometry geo;

        if (record.getGeometry() != null) {
            try {
                WarningEntry entry = entryMap.get(record.getDataURI());
                if (entry == null) {
                    entry = new WarningEntry();
                    entry.record = record;
                    entryMap.put(record.getDataURI(), entry);
                }
                IWireframeShape wfs = entry.wireframeShape;

                if (wfs != null) {
                    wfs.dispose();
                }

                wfs = target.createWireframeShape(false, descriptor);
                geo = (Geometry) record.getGeometry().clone();

                JTSCompiler jtsCompiler = new JTSCompiler(null, wfs, descriptor);
                jtsCompiler.handle(geo);
                wfs.compile();
                entry.wireframeShape = wfs;
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Error creating wireframe", e);
            }
        }
    }

    @Override
    protected synchronized void updateDisplay(IGraphicsTarget target)
            throws VizException {
        if (!this.recordsToLoad.isEmpty()) {
            FramesInfo info = getDescriptor().getFramesInfo();
            DataTime[] frames = info.getFrameTimes();
            for (AbstractWarningRecord warnrec : recordsToLoad) {
                WarningAction act = WarningAction.valueOf(warnrec.getAct());
                if (act == WarningAction.CON || act == WarningAction.CAN
                        || act == WarningAction.EXP || act == WarningAction.EXT) {
                    AbstractWarningRecord createShape = null;
                    for (String key : entryMap.keySet()) {
                        WarningEntry entry = entryMap.get(key);
                        AbstractWarningRecord rec = entry.record;
                        if (rec.getPhensig().equals(warnrec.getPhensig())
                                && rec.getOfficeid().equals(
                                        warnrec.getOfficeid())
                                && rec.getEtn().equals(warnrec.getEtn())) {

                            if (!entry.altered) {
                                // if it's a con, can, exp, or ext mark the
                                // original one as altered
                                entry.altered = true;
                                // make note of alteration time without
                                // changing end time
                                entry.timeAltered = warnrec.getStartTime()
                                        .getTime();
                                // prevents the original entry and the modified
                                // entry to be displayed in the same frame
                                entry.frameAltered = frames[info
                                        .getFrameIndex()].getRefTime();

                                // if cancellation, set end time to start time
                                // of this action

                                // DR14992: fix the problem with plotting
                                // expiration time for
                                // NEW warning when CAN warning is issued
                                if (act == WarningAction.CAN
                                        && WarningAction.valueOf(entry.record
                                                .getAct()) == WarningAction.CAN) {
                                    entry.record.setEndTime((Calendar) warnrec
                                            .getStartTime().clone());
                                }

                                if (!rec.getCountyheader().equals(
                                        warnrec.getCountyheader())
                                        && act == WarningAction.CAN) {
                                    entry.partialCancel = true;
                                }

                                // if it's a con, need to have a new entry for a
                                // new
                                // polygon
                                if (act == WarningAction.CON
                                        || act == WarningAction.EXT) {
                                    createShape = warnrec;
                                }
                            } else if ((entry.altered && entry.partialCancel)) {
                                // if it's a con, need to have a new entry for a
                                // new
                                // polygon
                                // TODO - do we need to create shape when action
                                // is EXT here, is that even possible?
                                if (act == WarningAction.CON) {
                                    createShape = warnrec;
                                }
                            }
                        }
                    }
                    // create the new polygon for the CON outside of the above
                    // for loop
                    if (createShape != null) {
                        initShape(target, warnrec);
                    }
                } else {
                    warnrec.setPil(SiteAbbreviationUtil.getSiteNode(warnrec
                            .getXxxid())
                            + warnrec.getPil()
                            + warnrec.getXxxid());
                    initShape(target, warnrec);
                }
            }

            recordsToLoad.clear();
        }

    }

    /**
     * schedule the heart beat for the next minute
     */
    protected static void scheduleHeartBeat() {
        // get simulated time
        Date currentTime = SimulatedTime.getSystemTime().getTime();
        // get a calendar
        Calendar now = Calendar.getInstance();
        // set calendar time to simulated time
        now.setTime(currentTime);
        // add one to the minutes field
        now.add(Calendar.MINUTE, 1);
        // reset second and milisecond to 0
        now.set(Calendar.SECOND, 0);
        now.set(Calendar.MILLISECOND, 0);
        // schedule task to fire every minute
        synchronized (heartBeatChangeLock) {
            try {
                if (heartBeatTimer == null) {
                    heartBeatTimer = new Timer();
                }
                // schedule on the minute every minute
                heartBeatTimer.schedule(heartBeatTask, now.getTime(),
                        1 * 60 * 1000);
            } catch (Exception e) {
                try {
                    heartBeatTimer.cancel();
                } catch (Exception e2) {
                    // ignore, we just want to make sure the timer is cancelled
                } finally {
                    // create a new task if there was an error when scheduling
                    heartBeatTask = new RepaintHeartbeat();
                    heartBeatTimer = new Timer();
                }
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Error scheduling warnings heart beat ", e);
            }
        }
    }

    @Override
    protected String getEventKey(WarningEntry entry) {
        AbstractWarningRecord r = entry.record;
        return r.getOfficeid() + '.' + r.getPhensig() + '.' + r.getEtn();
    }

}
