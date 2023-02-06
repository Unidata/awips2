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
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.JTSCompiler.JTSGeometryData;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.JTSCompiler;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.time.TimeMatchingJob;
import org.locationtech.jts.geom.Geometry;

/**
 * Resource for displaying warnings
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 1, 2010            jsanchez     Initial creation
 * Aug 22, 2011  10631    njensen      Major refactor
 * May 3, 2012  DR 14741  porricel     Stop setting end time of orig.
 *                                     warning to start time of update.
 * Jun 04, 2012 DR14992  mgamazaychikov Fix the problem with plotting expiration time for
 *                                  NEW warning when CAN warning is issued
 * Sep 27, 2012  1149     jsanchez     Refactored methods from AbstractWarningsResource into this class.
 * Apr 18, 2013  1877     jsanchez     Ordered the records the same for update and initial load.
 *                                     Removed no longer needed frameAltered. Do not set wire frame for a CAN.
 * Jul 24, 2013 DR16350  mgamazaychikov Fix the problem with plotting EXP warning
 * Sep  5, 2013 2176       jsanchez    Disposed the emergency font.
 * Feb 19, 2014 2819       randerso    Removed unnecessary .clone() call
 * Mar 04, 2014 2832       njensen     Moved disposeInternal() to abstract class
 * Apr 07, 2014 2959       njensen     Correct handling of color change
 * Apr 14, 2014 DR 17257  D. Friedman  Redo time matching on per-minute refresh.
 * Apr 28, 2015 ASM #15008 D. Friedman Create polygon for EXTs even if original product is not found.
 * Aug 22, 2016 #5842      dgilling    Remove dependency on viz.texteditor plugin.
 * Oct 21, 2021              srcarter    modified initShape to set the shape on watches, and wireframe on warnings and advisories
 * Mar 15, 2022			srcarter@ucar  Change initshape() to add a shadedshape and wireframeshape to every entry
 * 
 *
 * </pre>
 *
 * @author jsanchez
 */

public class WarningsResource extends AbstractWWAResource {

    protected static class RefreshTimerTask extends TimerTask {

        private final Set<WarningsResource> resourceSet = new HashSet<>();

        @Override
        public void run() {
            List<WarningsResource> rscs;
            synchronized (resourceSet) {
                rscs = new ArrayList<>(resourceSet);
            }
            for (WarningsResource rsc : rscs) {
                rsc.issueRefresh();
                rsc.redoTimeMatching();
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
        comparator = new WarningRecordComparator();
        resourceName = "Warnings";
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        FramesInfo info = descriptor.getFramesInfo();
        DataTime[] times = info.getFrameTimes();
        if ((times != null) && (times.length > 0)) {
            // Request data for "earliest" time
            requestData(times[0]);
        }
        scheduleRefreshTask(this);
    }

    @Override
    protected void disposeInternal() {
        cancelRefreshTask(this);
        super.disposeInternal();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdo = (PluginDataObject[]) object;
            synchronized (WarningsResource.this) {
                {
                    try {
                        addRecord(sort(pdo));
                    } catch (VizException e) {
                        statusHandler.handle(Priority.SIGNIFICANT,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
        } else if (type == ChangeType.CAPABILITY) {
            if ((color != null)
                    && (color.equals(getCapability((ColorableCapability.class))
                            .getColor()) == false)) {
                color = getCapability((ColorableCapability.class)).getColor();
                for (String dataUri : entryMap.keySet()) {
                    WarningEntry entry = entryMap.get(dataUri);
                    entry.project = true;
                }
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
                
                WarningAction act = WarningAction.valueOf(record.getAct());
               
                //give every entry a fill and outline
                //add fill (shadedshape)
            	IShadedShape ss = target.createShadedShape(false, descriptor.getGridGeometry());
                geo = record.getGeometry();
                JTSCompiler jtsCompiler = new JTSCompiler(ss, null, this.descriptor);
                JTSGeometryData geoData = jtsCompiler.createGeometryData();
                geoData.setGeometryColor(color);
                jtsCompiler.handle(geo, geoData);
                ss.setFillPattern(FillPatterns.getGLPattern(record.getPhen()
                        .equals("TO") ? "VERTICAL" : "HORIZONTAL"));
                ss.compile();

                entry.shadedShape = ss;

                //add outline (wireshape)
                IWireframeShape wfs = entry.wireframeShape;

                if (wfs != null) {
                    wfs.dispose();
                }

                wfs = target.createWireframeShape(false, descriptor);
                geo = record.getGeometry();

                jtsCompiler = new JTSCompiler(null, wfs, descriptor);
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
            for (AbstractWarningRecord warnrec : recordsToLoad) {
                WarningAction act = WarningAction.valueOf(warnrec.getAct());
                if ((act == WarningAction.CON) || (act == WarningAction.CAN)
                        || (act == WarningAction.EXT)) {
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

                                // DR14992: fix the problem with plotting
                                // expiration time for NEW warning when CAN
                                // warning is issued
                                if (act == WarningAction.CAN) {
                                    if (!rec.getCountyheader().equals(
                                            warnrec.getCountyheader())) {
                                        entry.partialCancel = true;
                                    } else {
                                        // complete cancellation
                                        createShape = warnrec;
                                    }
                                }

                                // if it's a con, need to have a new entry for a
                                // new
                                // polygon
                                if ((act == WarningAction.CON)
                                        || (act == WarningAction.EXT)) {
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
                    /* Create a new polygon for the follow-up to the original
                     * product found in the above loop.  Also create a polygon
                     * for EXT actions even if the original was not found.
                     */
                    if (createShape != null || act == WarningAction.EXT) {
                        initShape(target, warnrec);
                    }
                } else {
                    warnrec.setPil(SiteMap.getInstance()
                            .getCCCFromXXXCode(warnrec.getXxxid())
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

    /**
     * Redo the time matching
     */
    protected void redoTimeMatching() {
        AbstractTimeMatcher timeMatcher = this.getDescriptor().getTimeMatcher();
        if (timeMatcher != null) {
            timeMatcher.redoTimeMatching(this);
            TimeMatchingJob.scheduleTimeMatch(this.getDescriptor());
        }
    }
}
