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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
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

    protected static class RefreshTimerTask extends TimerTask {

        private final Set<WarningsResource> resourceSet = new HashSet<WarningsResource>();

        @Override
        public void run() {
            List<WarningsResource> rscs;
            synchronized (resourceSet) {
                rscs = new ArrayList<WarningsResource>(resourceSet);
            }
            for (WarningsResource rsc : rscs) {
                rsc.issueRefresh();
            }
        }

        public void addResource(WarningsResource rsc) {
            synchronized (resourceSet) {
                resourceSet.add(rsc);
            }
        }

        public void removeResource(WarningsResource rsc) {
            synchronized (resourceSet) {
                resourceSet.remove(rsc);
            }
        }

    }

    protected static RefreshTimerTask refreshTask;

    protected static Timer refreshTimer;

    /**
     * Constructor
     */
    public WarningsResource(WWAResourceData data, LoadProperties props) {
        super(data, props);
        resourceName = "Warnings";
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        FramesInfo info = descriptor.getFramesInfo();
        DataTime[] times = info.getFrameTimes();
        if (times != null && times.length > 0) {
            // Request data for "earliest" time
            requestData(times[0]);
        }
        scheduleRefreshTask(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        cancelRefreshTask(this);
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
     * Cancel the heart beat timer task
     * 
     * @param resource
     */
    protected static void cancelRefreshTask(WarningsResource resource) {
        synchronized (RefreshTimerTask.class) {
            if (refreshTask != null) {
                refreshTask.removeResource(resource);
                if (refreshTask.resourceSet.isEmpty()) {
                    refreshTimer.cancel();
                    refreshTimer = null;
                    refreshTask = null;
                }
            }
        }
    }

    /**
     * schedule the heart beat for the next minute
     */
    protected static void scheduleRefreshTask(WarningsResource resource) {
        synchronized (RefreshTimerTask.class) {
            if (refreshTask == null) {
                refreshTimer = new Timer(true);
                refreshTask = new RefreshTimerTask();

                // get a calendar
                Calendar now = Calendar.getInstance();
                // add one to the minutes field
                now.add(Calendar.MINUTE, 1);
                // reset second and milisecond to 0
                now.set(Calendar.SECOND, 0);
                now.set(Calendar.MILLISECOND, 0);

                refreshTimer.scheduleAtFixedRate(refreshTask, now.getTime(),
                        60 * 1000);
            }
            refreshTask.addResource(resource);
        }
    }

    @Override
    protected String getEventKey(WarningEntry entry) {
        AbstractWarningRecord r = entry.record;
        return r.getOfficeid() + '.' + r.getPhensig() + '.' + r.getEtn();
    }

}
