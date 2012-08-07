package com.raytheon.viz.warnings.rsc;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.UGCZone;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.raytheon.viz.texteditor.util.SiteAbbreviationUtil;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;

public class WatchesResource extends AbstractWatchesResource {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WatchesResource.class);

    private Map<String, WeakReference<Geometry>> geometryMap = new HashMap<String, WeakReference<Geometry>>();

    public WatchesResource(WWAResourceData data, LoadProperties props) {
        super(data, props);
    }

    @Override
    public String getName() {
        DataTime time = displayedDate;
        if (time == null) {
            time = descriptor.getTimeForResource(this);
        }
        String name = resourceData.name != null ? resourceData.name : "Watches";
        if (time != null) {
            name += " " + time.getLegendString();
        }
        return name;
    }

    @Override
    protected synchronized void initNewFrame(DataTime thisFrameTime)
            throws VizException {
        long t0 = System.currentTimeMillis();
        HashMap<String, RequestConstraint> map = (HashMap<String, RequestConstraint>) resourceData
                .getMetadataMap().clone();

        LayerProperty property = new LayerProperty();
        property.setDesiredProduct(ResourceType.PLAN_VIEW);
        property.setNumberOfImages(9999);

        /*
         * Retrieve all the watches that have a have a range containing the
         * frame time
         */
        map.put("startTime", new RequestConstraint(thisFrameTime.toString(),
                ConstraintType.LESS_THAN_EQUALS));
        map.put("endTime", new RequestConstraint(thisFrameTime.toString(),
                ConstraintType.GREATER_THAN_EQUALS));
        property.setEntryQueryParameters(map, false);

        Object[] resp = DataCubeContainer.getData(property, 60000).toArray(
                new Object[] {});

        ArrayList<AbstractWarningRecord> sortedWatches = new ArrayList<AbstractWarningRecord>();
        for (Object o : resp) {
            if (((PluginDataObject) o) instanceof AbstractWarningRecord) {
                sortedWatches.add((AbstractWarningRecord) o);
            }
        }

        /* Sorts by phensig, etn, starttime (descending), act */
        Collections.sort(sortedWatches, comparator);
        ArrayList<AbstractWarningRecord> remove = new ArrayList<AbstractWarningRecord>();

        AbstractWarningRecord conRecord = null;
        AbstractWarningRecord expRecord = null;
        for (AbstractWarningRecord w : sortedWatches) {
            WarningAction act = WarningAction.valueOf(w.getAct());
            // Do not plot watches that have been Cancelled or Expired
            if ((act == WarningAction.CAN || act == WarningAction.EXP)
                    && w.getStartTime().after(thisFrameTime.getValidTime()) == false
                    && w.getUgcsString() != null
                    && w.getUgcsString()[0] != null
                    && w.getUgcsString()[0].endsWith("000")) {
                expRecord = w;
            } else if (expRecord != null
                    && expRecord.getEtn().equals(w.getEtn())
                    && expRecord.getPhensig().equals(w.getPhensig())
                    && expRecord.getStartTime().after(w.getStartTime())) {

                remove.add(w);
            }

            // Do not plot watches that out dated CONs
            if (act == WarningAction.CON
                    && (conRecord == null || conRecord.getEtn().equals(
                            w.getEtn()) == false)) {
                conRecord = w;
            } else if (conRecord != null
                    && (act == WarningAction.CON || act == WarningAction.NEW)
                    && conRecord.getEtn().equals(w.getEtn())
                    && conRecord.getPhensig().equals(w.getPhensig())
                    && conRecord.getStartTime().after(w.getStartTime())) {
                remove.add(w);
            }
        }

        for (AbstractWarningRecord w : remove) {
            sortedWatches.remove(w);
        }

        Map<String, AbstractWarningRecord> watches = new HashMap<String, AbstractWarningRecord>();
        for (AbstractWarningRecord watchRec : sortedWatches) {
            TimeRange tr = new TimeRange(watchRec.getStartTime(),
                    watchRec.getEndTime());
            if (tr.contains(thisFrameTime.getValidTime().getTime())) {
                addWatch(watches, watchRec);
            }
        }

        ArrayList<AbstractWarningRecord> watchList = new ArrayList<AbstractWarningRecord>();
        if (watches.isEmpty() == false) {
            for (String key : watches.keySet()) {
                AbstractWarningRecord watch = watches.get(key);
                watch.setPil(SiteAbbreviationUtil.getSiteNode(watch.getXxxid())
                        + watch.getPil() + watch.getXxxid());
                watchList.add(watch);
                scheduleTimer(watch);
                initShape(watch);

                WarningEntry entry = entryMap.get(watch.getDataURI());
                if (entry == null) {
                    entry = new WarningEntry();
                    entry.record = watch;
                    entryMap.put(watch.getDataURI(), entry);
                }
                List<DataTime> list = entry.times;
                if (list == null) {
                    list = new ArrayList<DataTime>();
                }
                list.add(thisFrameTime);
                entry.times = list;
            }
        }

        this.frames.put(thisFrameTime, watchList);
        System.out.println("Init Frame: " + (System.currentTimeMillis() - t0)
                + " ms");
    }

    @Override
    protected synchronized void updateFrames() throws VizException {
        if (!this.recordsToLoad.isEmpty()) {
            for (AbstractWarningRecord watchRec : recordsToLoad) {
                WarningAction watchAct = WarningAction.valueOf(watchRec
                        .getAct());
                TimeRange tr = new TimeRange(watchRec.getStartTime(),
                        watchRec.getEndTime());
                synchronized (frames) {
                    for (DataTime dt : frames.keySet()) {
                        if (tr.contains(dt.getValidTime().getTime())) {
                            boolean add = true;
                            ArrayList<AbstractWarningRecord> remove = new ArrayList<AbstractWarningRecord>();
                            List<AbstractWarningRecord> watchList = frames
                                    .get(dt);
                            for (AbstractWarningRecord watch : watchList) {
                                // Do not plot watches that have been Cancelled
                                // or
                                // Expired
                                if ((watchAct == WarningAction.CAN || watchAct == WarningAction.EXP)
                                        && watchRec.getStartTime().after(dt) == false
                                        && watchRec.getEtn().equals(
                                                watch.getEtn())
                                        && watchRec.getPhensig().equals(
                                                watch.getPhensig())
                                        && watchRec.getUgcsString() != null
                                        && watchRec.getUgcsString()[0] != null
                                        && watchRec.getUgcsString()[0]
                                                .endsWith("000")) {
                                    remove.add(watch);
                                }
                                if (watchAct == WarningAction.CON
                                        && (WarningAction.valueOf(watch
                                                .getAct()) == WarningAction.CON || WarningAction
                                                .valueOf(watch.getAct()) == WarningAction.NEW)
                                        && watchRec.getEtn().equals(
                                                watch.getEtn())
                                        && watchRec.getPhensig().equals(
                                                watch.getPhensig())) {
                                    if (watchRec.getStartTime().after(
                                            watch.getStartTime())) {
                                        remove.add(watch);
                                    } else {
                                        add = false;
                                    }
                                }
                            }

                            for (AbstractWarningRecord w : remove) {
                                watchList.remove(w);
                            }

                            if (add) {
                                watchList.add(watchRec);
                                scheduleTimer(watchRec);
                                initShape(watchRec);

                                WarningEntry entry = entryMap.get(watchRec
                                        .getDataURI());
                                if (entry == null) {
                                    entry = new WarningEntry();
                                    entry.record = watchRec;
                                    entryMap.put(watchRec.getDataURI(), entry);
                                }
                                List<DataTime> list = entry.times;
                                if (list == null) {
                                    list = new ArrayList<DataTime>();
                                }
                                list.add(dt);
                                entry.times = list;
                            }
                        }
                    }
                }
                if (frames.containsKey(new DataTime(watchRec.getStartTime())) == false) {
                    initNewFrame(new DataTime(watchRec.getStartTime()));
                }
            }
            recordsToLoad.clear();
        }
    }

    @Override
    protected void initShape(AbstractWarningRecord record) throws VizException {
        Geometry geo;

        if (record.getUgczones().size() > 0) {
            setGeometry(record);
            if (record.getGeometry() != null) {
                IShadedShape ss = target.createShadedShape(false, descriptor,
                        false);
                geo = (Geometry) record.getGeometry().clone();
                JTSCompiler jtsCompiler = new JTSCompiler(ss, null,
                        this.descriptor, PointStyle.CROSS);
                jtsCompiler.handle(geo, color);
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

    private void setGeometry(AbstractWarningRecord record) {
        List<String> county = new ArrayList<String>();
        List<String> marinezone = new ArrayList<String>();
        List<Geometry> geometries = new ArrayList<Geometry>();

        for (String ugc : record.getUgcsString()) {
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
        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
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
                    geometryMap.put(key, new WeakReference<Geometry>(
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
                    geometryMap.put(key, new WeakReference<Geometry>(
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

    /*
     * Combining watches with the same ETN (Watch No.) into a single record.
     * This will also help performance to make a single DB query.
     */
    private void addWatch(Map<String, AbstractWarningRecord> watches,
            AbstractWarningRecord watchrec) {
        String key = watchrec.getProductClass() + "." + watchrec.getPhensig()
                + "." + watchrec.getEtn();

        AbstractWarningRecord watch = null;
        WarningAction act = WarningAction.valueOf(watchrec.getAct());

        if (watches.containsKey(key)) {
            watch = watches.get(key);
            Set<UGCZone> ugcZones = watch.getUgczones();
            for (UGCZone zone : watchrec.getUgczones()) {
                if (act == WarningAction.CAN || act == WarningAction.EXP) {
                    ugcZones.remove(zone);
                } else {
                    ugcZones.add(zone);
                }
            }
            watch.setUgczones(ugcZones);
        } else if (act != WarningAction.CAN && act != WarningAction.EXP) {
            watch = watchrec;
        }

        if (watch != null) {
            watches.put(key, watch);
        }
    }
}
