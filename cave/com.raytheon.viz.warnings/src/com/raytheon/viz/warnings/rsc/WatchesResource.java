package com.raytheon.viz.warnings.rsc;

import java.lang.ref.WeakReference;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.JTSCompiler;
import com.raytheon.uf.viz.core.drawables.JTSCompiler.JTSGeometryData;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryCollection;
import org.locationtech.jts.geom.GeometryFactory;

/**
 * 
 * A resource for displaying watches as shaded polygons
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012 1149       jsanchez    Refactored methods from AbstractWarningsResource into this class.
 * May 06, 2013 1930       bsteffen    Check for null in WatchesResource.
 * May 10, 2013 1951       rjpeter     Updated ugcZones references
 * Sep  5, 2013 2176       jsanchez    Disposed the emergency font.
 * Nov  8, 2013 16758      mgamazaychikov Changed access modifier of mergeWatches to protected 
 *                                        so a child class can override the implementation.
 * Feb 19, 2014 2819       randerso    Removed unnecessary .clone() call
 * Mar 04, 2014 2832       njensen     Moved disposeInternal() to abstract class
 * Apr 07, 2014 2959       njensen     Correct handling of color change
 * Jan 16, 2017 5976       bsteffen    Update shaded shape constructor.
 * 
 * </pre>
 * 
 * @author jsanchez
 */
public class WatchesResource extends AbstractWWAResource {

    /**
     * 
     * this task calls redoTimeMatching on the resource, it should be scheduled
     * to run for when a warning is set to expire
     * 
     * @author ekladstrup
     * @version 1.0
     */
    protected class WarningExpirationTask extends TimerTask {

        private WatchesResource rsc = null;

        public WarningExpirationTask(WatchesResource rsc) {
            this.rsc = rsc;
        }

        @Override
        public void run() {
            // System.err.println("warning expired");
            // some warning has expired
            rsc.redoTimeMatching(this.scheduledExecutionTime());
            rsc.issueRefresh();
        }

    }

    private final Map<String, WeakReference<Geometry>> geometryMap = new HashMap<>();

    private final Timer timer;

    private final Set<Long> expTaskSet;

    private final SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmm");

    public WatchesResource(WWAResourceData data, LoadProperties props) {
        super(data, props);
        comparator = new WatchesComparator();
        timer = new Timer();
        expTaskSet = new HashSet<>();
        resourceName = "Watches";
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        synchronized (this) {
            try {
                addRecord(getWarningRecordArray());
            } catch (VizException e) {
                statusHandler.error("Error initializing watches", e);
            }
        }

        // force creation of a frame for any currently active warnings, this
        // frame might get displayed in place of the last frame.
        requestData(new DataTime(SimulatedTime.getSystemTime().getTime()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        timer.cancel();
        super.disposeInternal();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdo = (PluginDataObject[]) object;
            synchronized (WatchesResource.this) {
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
            if ((color != null)
                    && (color.equals(getCapability((ColorableCapability.class))
                            .getColor()) == false)) {
                color = getCapability((ColorableCapability.class)).getColor();

                for (String dataUri : entryMap.keySet()) {
                    WarningEntry entry = entryMap.get(dataUri);
                    // project will ensure it gets recreated on next paint
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

        if (!record.getUgcZones().isEmpty()) {
            setGeometry(record);
            if ((record.getGeometry() != null) && (record.getPhen() != null)) {
                IShadedShape ss = target.createShadedShape(false,
                        descriptor.getGridGeometry());
                geo = record.getGeometry();
                JTSCompiler jtsCompiler = new JTSCompiler(ss, null,
                        this.descriptor);
                JTSGeometryData geoData = jtsCompiler.createGeometryData();
                geoData.setGeometryColor(color);
                jtsCompiler.handle(geo, geoData);
                ss.setFillPattern(FillPatterns.getGLPattern(record.getPhen()
                        .equals("TO") ? "VERTICAL" : "HORIZONTAL"));
                ss.compile();
                WarningEntry entry = entryMap.get(record.getDataURI());
                if (entry == null) {
                    entry = new WarningEntry();
                    entry.record = record;
                    entryMap.put(record.getDataURI(), entry);
                }
                entry.shadedShape = ss;
            }
        }
    }

    @Override
    protected synchronized void updateDisplay(IGraphicsTarget target)
            throws VizException {

        if (!recordsToLoad.isEmpty()) {
            // Merges all the zones for the same vtec and time
            List<AbstractWarningRecord> mergedWatches = mergeWatches(recordsToLoad);
            for (AbstractWarningRecord watchrec : mergedWatches) {

                WarningAction watchact = WarningAction.valueOf(watchrec
                        .getAct());
                int watchSize = watchrec.getUgcZones().size();

                if (watchact != WarningAction.NEW) {
                    AbstractWarningRecord createShape = null;
                    if ((watchact == null) || (watchact.toString() == null)) {
                        createShape = watchrec;
                    }
                    for (String entryKey : entryMap.keySet()) {
                        WarningEntry entry = entryMap.get(entryKey);
                        AbstractWarningRecord rec = entry.record;

                        // checks for any possible null pointer exceptions in
                        // the following block of code, since there is the
                        // possibility of null values
                        if ((rec.getPhensig() != null)
                                && (watchrec.getPhensig() != null)
                                && (rec.getOfficeid() != null)
                                && (watchrec.getOfficeid() != null)
                                && (rec.getUgcZones() != null)
                                && (rec.getStartTime() != null)
                                && (watchrec.getStartTime() != null)) {
                            if (rec.getPhensig().equals(watchrec.getPhensig())
                                    && rec.getOfficeid().equals(
                                            watchrec.getOfficeid())
                                    && rec.getEtn().equals(watchrec.getEtn())) {
                                int recSize = rec.getUgcZones().size();
                                if (!entry.partialCancel) {
                                    if ((watchact == WarningAction.EXP)
                                            || (watchact == WarningAction.CAN)) {
                                        entry.partialCancel = true;
                                        entry.record
                                                .setEndTime((Calendar) watchrec
                                                        .getStartTime().clone());
                                    } else if ((watchact == WarningAction.CON)
                                            && (recSize > watchSize)
                                            && watchrec.getStartTime().after(
                                                    rec.getStartTime())) {
                                        entry.partialCancel = true;
                                        entry.record
                                                .setEndTime((Calendar) watchrec
                                                        .getStartTime().clone());
                                        createShape = watchrec;
                                    }
                                }
                            }
                        }
                    }

                    if (createShape != null) {
                        WarningEntry entry = entryMap.get(createShape
                                .getDataURI());
                        if ((entry != null) && (entry.shadedShape != null)) {
                            entry.shadedShape.dispose();
                        }
                        initShape(target, createShape);
                    }
                } else {
                    initShape(target, watchrec);
                }
            }

            recordsToLoad.clear();
        }
    }

    protected void setGeometry(AbstractWarningRecord record) {
        List<String> county = new ArrayList<>();
        List<String> marinezone = new ArrayList<>();
        List<Geometry> geometries = new ArrayList<>();

        for (String ugc : record.getUgcZones()) {
            Geometry geom = null;
            WeakReference<Geometry> geomRef = geometryMap.get(ugc);
            if (geomRef != null) {
                geom = geomRef.get();
            }
            if (geom != null) {
                geometries.add(geom);
            } else if (ugc.charAt(2) == 'C') {
                county.add("'" + ugc + "'");
            } else if (ugc.charAt(2) == 'Z') {
                marinezone.add("'" + ugc + "'");
            }
        }

        SpatialQueryResult[] features = null;
        Map<String, RequestConstraint> map = new HashMap<>();
        RequestConstraint constraint = null;
        if (county.isEmpty() == false) {
            String field = "state||'C'||substring(fips,3,3)";
            String source = "county";
            constraint = new RequestConstraint(field, ConstraintType.IN);
            constraint.setConstraintValueList(county.toArray(new String[county
                    .size()]));
            map.put(field, constraint);
            try {
                features = SpatialQueryFactory.create().query(source,
                        "the_geom_0_001", new String[] { field }, null, map,
                        SearchMode.WITHIN);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error querying county table. ", e);
            }

            if (features != null) {
                for (SpatialQueryResult feature : features) {
                    String key = String.valueOf(feature.attributes.get(field));
                    geometries.add(feature.geometry);
                    geometryMap.put(key, new WeakReference<>(
                            (Geometry) feature.geometry.clone()));
                }
            }
        }

        if (marinezone.isEmpty() == false) {
            String field = "id";
            String source = "marinezones";
            constraint = new RequestConstraint(field, ConstraintType.IN);
            constraint.setConstraintValueList(marinezone
                    .toArray(new String[marinezone.size()]));
            map.clear();
            map.put(field, constraint);
            try {
                features = SpatialQueryFactory.create().query(source,
                        "the_geom_0_001", new String[] { field }, null, map,
                        SearchMode.WITHIN);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error querying marinezones table. ", e);
            }

            if (features != null) {
                for (SpatialQueryResult feature : features) {
                    String key = String.valueOf(feature.attributes.get(field));
                    geometries.add(feature.geometry);
                    geometryMap.put(key, new WeakReference<>(
                            (Geometry) feature.geometry.clone()));
                }
            }
        }

        Geometry geometry = null;
        if (geometries.isEmpty() == false) {
            GeometryCollection geometryCollection = new GeometryFactory()
                    .createGeometryCollection(geometries
                            .toArray(new Geometry[geometries.size()]));
            geometry = geometryCollection;
        }
        record.setGeometry(geometry);
    }

    /**
     * Groups all the ugc zones with the same 'product.act.phensig.etn'
     */
    protected List<AbstractWarningRecord> mergeWatches(
            List<AbstractWarningRecord> watchrecs) {
        Map<String, AbstractWarningRecord> watches = new HashMap<>();
        for (AbstractWarningRecord watchrec : watchrecs) {
            String key = watchrec.getAct() + '.' + watchrec.getPhensig() + '.'
                    + watchrec.getEtn() + '.'
                    + sdf.format(watchrec.getStartTime().getTime());
            AbstractWarningRecord watch = watches.get(key);
            if (watch == null) {
                watch = watchrec;
            } else if (watchrec.getUgcZones() != null) {
                Set<String> ugcZones = watch.getUgcZones();
                ugcZones.addAll(watchrec.getUgcZones());
            }
            watches.put(key, watch);
        }

        List<AbstractWarningRecord> mergedWatches = new ArrayList<>(
                watches.values());
        Collections.sort(mergedWatches, comparator);

        return mergedWatches;
    }

    /**
     * Schedules a WarningExpirationTask for the end time of the passing in
     * record
     * 
     * @param rec
     *            a WarningRecord
     */
    protected void scheduleTimer(AbstractWarningRecord rec) {
        // only schedule if record has not expired already
        long now = SimulatedTime.getSystemTime().getTime().getTime();
        long endTime = rec.getEndTime().getTimeInMillis();
        synchronized (expTaskSet) {
            if ((endTime > now) && !expTaskSet.contains(new Long(endTime))) {
                WarningExpirationTask task = new WarningExpirationTask(this);
                timer.schedule(task, rec.getEndTime().getTime());
                expTaskSet.add(new Long(endTime));
            }
        }
    }

    /**
     * Redo time matching and remove the passed in time from the map of
     * scheduled times
     * 
     * @param triggerTime
     */
    public void redoTimeMatching(long triggerTime) {
        redoTimeMatching();
        Long time = new Long(triggerTime);
        // remove the instance of the trigger time from the map
        synchronized (expTaskSet) {
            if ((expTaskSet != null) && expTaskSet.contains(time)) {
                expTaskSet.remove(time);
            }
        }
    }

    /**
     * Redo the time matching
     */
    public void redoTimeMatching() {
        try {
            this.getDescriptor().getTimeMatcher().redoTimeMatching(this);
            this.getDescriptor().getTimeMatcher()
                    .redoTimeMatching(this.getDescriptor());
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    @Override
    protected String getEventKey(WarningEntry entry) {
        AbstractWarningRecord r = entry.record;
        return r.getAct() + '.' + r.getPhensig() + '.' + r.getEtn() + '.'
                + sdf.format(entry.record.getStartTime().getTime());
    }

}
