package com.raytheon.viz.warnings.rsc;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.time.ISimulatedTimeChangeListener;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Displays WOUs updated by WCNs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2014-08-28   ASM #15682 D. Friemdan Initial creation
 * </pre>
 * 
 */
public class WouWcnWatchesResource extends WatchesResource implements ISimulatedTimeChangeListener {

    private static Timer timer;

    private TimerTask timerTask;

    // If this is changed to use the maps database, could probably be static
    private Map<String, Set<String>> cwaUgcMap = new HashMap<String, Set<String>>();

    static final ThreadLocal<SimpleDateFormat> sdf = new ThreadLocal<SimpleDateFormat>() {
        @Override protected SimpleDateFormat initialValue() {
            return new SimpleDateFormat("yyyyMMddHHmm");
        }
    };

    public WouWcnWatchesResource(WWAResourceData data, LoadProperties props) {
        super(data, props);
        comparator = WouWcnWatchesComparator.getInstance();
        resourceName = "Watches";
    }

    private AbstractWarningRecord getPreviousRecordForEvent(AbstractWarningRecord rec) {
        String phenSig = rec.getPhensig();
        String etn = rec.getEtn();
        if (phenSig == null || etn == null)
            return null;
        AbstractWarningRecord best = null;
        for (WarningEntry e : entryMap.values()) {
            if (!phenSig.equals(e.record.getPhensig()) ||
                    !etn.equals(e.record.getEtn()))
                continue;

            if (best == null || WouWcnWatchesComparator.getInstance().
                    compare(best, e.record) < 0) {
                best = e.record;
            }
        }
        return best;
    }

    private Set<String> maskCwaUgcs(Set<String> ugcs, AbstractWarningRecord rec) {
        Set<String> cwaUgcs = getUgcsForCwa(rec.getXxxid());
        if (cwaUgcs != null) {
            HashSet<String> result = new HashSet<String>(ugcs);
            result.removeAll(cwaUgcs);
            return result;
        } else {
            return ugcs;
        }
    }

    private Set<String> getUgcsForCwa(String cwa) {
        return cwaUgcMap.get(cwa.toUpperCase());
    }

    private Set<String> safe(Set<String> set) {
        return set != null ? set : new HashSet<String>();
    }

    @Override
    protected void updateDisplay(IGraphicsTarget target) throws VizException {
        if (recordsToLoad.isEmpty())
            return;

        List<AbstractWarningRecord> mergedWatches = mergeWatches(recordsToLoad);
        for (AbstractWarningRecord watchRec : mergedWatches) {
            /* If these things are missing, we can't do anything with the warning. */
            if (watchRec.getPhensig() == null || watchRec.getEtn() == null ||
                    watchRec.getIssueTime() == null || watchRec.getStartTime() == null ||
                    watchRec.getEndTime() == null || watchRec.getXxxid() == null ||
                    watchRec.getWmoid() == null || watchRec.getAct() == null) {
                continue;
            }

            WarningAction watchAct = WarningAction.valueOf(watchRec.getAct());

            AbstractWarningRecord createShape = null;
            boolean isWOU = "WOU".equals(watchRec.getPil());

            AbstractWarningRecord prevRec = getPreviousRecordForEvent(watchRec);
            Set<String> prevUgcs = new HashSet<String>(safe(
                    prevRec != null ? prevRec.getUgcZones() : null));
            Set<String> newUgcs = null;

            if (watchAct == WarningAction.NEW) {
                if (isWOU) {
                    createShape = watchRec;
                } else {
                    noteCwaUgcs(watchRec);
                    // As per requirements, we do not create frames for these.
                }
            } else if (watchAct == WarningAction.CON && isWOU) {
                // As per requirements, we do not create frames for these.
            } else if (watchAct == WarningAction.CON) {
                /* No need to do anything because we really only care about
                 * the segments paired with the CON.
                 */
            } else if (watchAct == WarningAction.CAN) {
                /* Not really expecting this for a WOU, but shouldn't cause
                 * a problem if there is one.
                 */
                newUgcs = prevUgcs;
                newUgcs.removeAll(safe(watchRec.getUgcZones()));
                createShape = watchRec;
            } else if (watchAct == WarningAction.EXA || watchAct == WarningAction.EXB) {
                if (!isWOU) {
                    noteCwaUgcs(watchRec);
                }
                newUgcs = prevUgcs;
                newUgcs.addAll(safe(watchRec.getUgcZones()));
                createShape = watchRec;
            } else if (watchAct == WarningAction.EXP) {
                if (isWOU) {
                    if (prevRec != null) {
                        if (! prevRec.getEndTime().equals(watchRec.getEndTime())) {
                            prevRec.setEndTime(watchRec.getEndTime());
                        }
                    }
                    /*
                     * Ideally we do not need to create a shape, but if we do
                     * not and time matching creates a frame for an EXP that is
                     * issued before the expiration time, the warning would show
                     * as still active on that frame.
                     */
                    newUgcs = new HashSet<String>();
                    createShape = watchRec;
                } else {
                    newUgcs = maskCwaUgcs(prevUgcs, watchRec);
                    createShape = watchRec;
                }
            }
            if (watchAct == WarningAction.EXT || watchAct == WarningAction.EXB) {
                /* This resource does not handle different expiration times
                 * for different UGCs.
                 *
                 * Also assuming this does not add/remove UGCs.
                 */
                if (prevRec != null && watchRec.getEndTime() != null) {
                    if (isWOU && watchRec.getUgcZones() != null && watchRec.getUgcZones().isEmpty()) {
                        /*
                         * This probably does not actually happen, but this
                         * is the only way we can support shortening the
                         * expiration time with the current design.
                         */
                        prevRec.setEndTime(watchRec.getEndTime());
                    } else {
                        if (prevRec.getEndTime().before(watchRec.getEndTime())) {
                            prevRec.setEndTime(watchRec.getEndTime());
                        }
                    }
                }
            }

            if (createShape != null) {
                if (newUgcs != null)
                    createShape.setUgcZones(newUgcs);
                else if (createShape.getUgcZones() == null)
                    createShape.setUgcZones(new HashSet<String>());
                insertShape(target, createShape);
            }
        }

        recordsToLoad.clear();
        scheduleNextTime();
    }


    @Override
    protected void initShape(IGraphicsTarget target,
            AbstractWarningRecord record) throws VizException {
        String key = getEntryMapKey(record);
        WarningEntry entry = entryMap.get(key);
        if (entry != null) {
            createShape(target, entry);
        }
    }

    protected void insertShape(IGraphicsTarget target, AbstractWarningRecord record) throws VizException {
        String key = getEntryMapKey(record);
        WarningEntry entry = entryMap.get(key);
        if (entry == null) {
            entry = new WarningEntry();
            entryMap.put(key, entry);
        }
        entry.record = record; // ...possibly replacing an existing record
        if (! record.getUgcZones().isEmpty()) {
            setGeometry(record);
        } else {
            entry.record.setGeometry(null);
        }
        createShape(target, entry);
    }

    protected void createShape(IGraphicsTarget target, WarningEntry entry) throws VizException {
        if (entry.shadedShape != null) {
            entry.shadedShape.dispose();
            entry.shadedShape = null;
        }
        AbstractWarningRecord record = entry.record;
        if (record.getGeometry() != null) {
            IShadedShape ss = target.createShadedShape(false,
                    descriptor.getGridGeometry());
            Geometry geo = (Geometry) record.getGeometry().clone();
            JTSCompiler jtsCompiler = new JTSCompiler(ss, null,
                    this.descriptor, PointStyle.CROSS);
            jtsCompiler.handle(geo, color);
            ss.setFillPattern(FillPatterns.getGLPattern(record.getPhen()
                    .equals("TO") ? "VERTICAL" : "HORIZONTAL"));
            ss.compile();
            entry.shadedShape = ss;
        }
    }

    /**
     * Groups all the ugc zones with the same action, phensig, ETN, site, and
     * issuance time.
     */
    protected List<AbstractWarningRecord> mergeWatches(
            List<AbstractWarningRecord> watchrecs) {
        SimpleDateFormat sdfi = sdf.get();

        Map<String, AbstractWarningRecord> watches = new HashMap<String, AbstractWarningRecord>();
        for (AbstractWarningRecord watchrec : watchrecs) {
            if (watchrec.getIssueTime() == null)
                continue;

            String key = watchrec.getAct() + '.' + watchrec.getPhensig() + '.'
                    + watchrec.getEtn() + '.' + watchrec.getOfficeid() + '.'
                    + sdfi.format(watchrec.getIssueTime().getTime());
            AbstractWarningRecord watch = watches.get(key);
            if (watch == null) {
                watch = watchrec;
                watches.put(key, watch);
            } else {
                Set<String> ugcZones = watch.getUgcZones();
                if (ugcZones != null) {
                    ugcZones.addAll(watchrec.getUgcZones());
                }
            }
        }

        ArrayList<AbstractWarningRecord> mergedWatches = new ArrayList<AbstractWarningRecord>(
                watches.values());
        Collections.sort(mergedWatches, comparator);

        return mergedWatches;
    }

    protected String getEntryMapKey(AbstractWarningRecord rec) {
        return sdf.get().format(rec.getIssueTime().getTime()) + '.'
                + rec.getWmoid() + '.' + rec.getPhensig() + '.' + rec.getEtn();
    }

    @Override
    protected String getEventKey(WarningEntry entry) {
        AbstractWarningRecord r = entry.record;
        return r.getPhensig() + '.' + r.getEtn();
    }

    private void noteCwaUgcs(AbstractWarningRecord watchRec) {
        String siteKey = watchRec.getXxxid();
        Set<String> recUgcs = watchRec.getUgcZones();
        if (siteKey == null || recUgcs == null)
            return;

        synchronized (cwaUgcMap) {
            Set<String> ugcs = cwaUgcMap.get(siteKey);
            if (ugcs == null) {
                ugcs = new HashSet<String>();
                cwaUgcMap.put(siteKey, ugcs);
            }
            ugcs.addAll(recUgcs);
        }
    }

    @Override
    protected void disposeInternal() {
        synchronized(this) {
            if (timerTask != null)
                timerTask.cancel();
        }
        super.disposeInternal();
    }

    private void scheduleNextTime() {
        /* TODO: This is a race condition.  Need the last frame time,
         * but getLastFrameTimeRange() is also a race condition.  So really need
         * last drawn last frame time, but LAST_FRAME_ADJ is an hour ahead??!
         */
        long vnow = SimulatedTime.getSystemTime().getMillis();
        AbstractWarningRecord best = null;
        for (WarningEntry entry : entryMap.values()) {
            AbstractWarningRecord rec = entry.record;
            if (rec.getEndTime().getTimeInMillis() >= vnow &&
                    (best == null || rec.getEndTime().before(best.getEndTime()))) {
                best = rec;
            }
        }
        if (best != null) {
            scheduleTimer(best.getEndTime().getTimeInMillis() - vnow);
        } else {
            scheduleTimer(-1);
        }
    }

    private synchronized void scheduleTimer(long delay) {
        if (timerTask != null) {
            timerTask.cancel();
        }
        if (delay >= 0) {
            timerTask = new WatchesTimerTask();
            getTimer().schedule(timerTask, delay);
        } else {
            timerTask = null;
        }
    }

    private Timer getTimer() {
        if (timer == null) {
            synchronized (WouWcnWatchesResource.class) {
                if (timer == null) {
                    timer = new Timer(WouWcnWatchesResource.class.getName() + " Timer");
                }
            }
        }
        return timer;
    }

    @Override
    public void timechanged() {
        issueRefresh();
        scheduleNextTime();
    }

    protected class WatchesTimerTask extends TimerTask {

        @Override
        public void run() {
            timechanged();
        }
    }

    protected static class WouWcnWatchesComparator implements Comparator<AbstractWarningRecord> {

        static final WouWcnWatchesComparator instance = new WouWcnWatchesComparator();

        public static Comparator<AbstractWarningRecord> getInstance() {
            return instance;
        }

        @Override
        public int compare(AbstractWarningRecord a, AbstractWarningRecord b) {
            int r;
            Calendar ca = a.getIssueTime();
            Calendar cb = b.getIssueTime();
            if (ca == null) {
                if (cb == null)
                    r = 0;
                else
                    return 1;
            } else if (cb == null)
                return -1;
            else
                r = ca.compareTo(cb);
            if (r != 0)
                return r;

            // The point of this is to handle the BBB field, but it makes the TTAAii part significant too...
            String sa = safeWmo(a);
            String sb = safeWmo(b);
            r = sa.compareTo(sb);
            if (r != 0)
                return r;

            r = a.getSeg() - b.getSeg();
            if (r != 0)
                return r;

            return 0;
        }
    }

    private static String safeWmo(AbstractWarningRecord record) {
        String wmo = record.getWmoid();
        return wmo != null ? wmo : "TTAA00 CCCC 000000";
    }

}
