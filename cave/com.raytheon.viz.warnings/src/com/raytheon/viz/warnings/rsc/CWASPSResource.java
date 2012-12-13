package com.raytheon.viz.warnings.rsc;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.raytheon.viz.warnings.DateUtil;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;
import com.vividsolutions.jts.io.WKTReader;


public class CWASPSResource extends WatchesResource {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CWASPSResource.class);

    private static final Set<String> marinezones = new HashSet<String>(
            Arrays.asList(new String[] { "AM", "AN", "GM", "LE", "LH", "LM",
                    "LO", "LS", "PH", "PK", "PM", "PS", "SL" }));

    public CWASPSResource(WWAResourceData data, LoadProperties props) {
        super(data, props);
    }

    /**
     * Creates a new geometry for the record based on the county list. It will
     * completely ignore the geometry stored in the record, you should test for
     * that before calling this method.
     * 
     * @param record
     * @return a geometry based on the counties in the record
     */
    private Geometry getGeometry(AbstractWarningRecord record) {

        String countyList = "";
        String marinezoneList = "";
        // state_zone
        String zoneList = "";
        List<String> queries = new ArrayList<String>();

        for (String ugc : record.getUgcsString()) {

            if (ugc.charAt(2) == 'Z'
                    && marinezones.contains(ugc.substring(0, 2))) {
                if (marinezoneList.length() > 0) {
                    marinezoneList += ",";
                }
                marinezoneList += "'" + ugc + "'";
            } else if (ugc.charAt(2) == 'Z') {
                if (zoneList.length() > 0) {
                    zoneList += ",";
                }
                ugc = ugc.substring(0, 2) + ugc.substring(3);
                zoneList += "'" + ugc + "'";
            } else if (ugc.charAt(2) == 'C') {
                if (countyList.length() > 0) {
                    countyList += ",";
                }
                countyList += "'" + ugc + "'";
            }
        }

        if (countyList.length() > 0) {
            queries.add("select asBinary(the_geom) from mapdata.county where state||'C'||substring(fips,3,3) in ("
                    + countyList + ")");
        }
        if (zoneList.length() > 0) {
            queries.add("select asBinary(the_geom) from mapdata.zone where state_zone in ("
                    + zoneList + ")");
        }
        if (marinezoneList.length() > 0) {
            queries.add("select asBinary(the_geom) from mapdata.marinezones where id in ("
                    + marinezoneList + ")");
        }

        List<Geometry> geometries = new ArrayList<Geometry>();
        for (String sql : queries) {
            try {
                List<Object[]> result = DirectDbQuery.executeQuery(sql, "maps",
                        QueryLanguage.SQL);
                if (result != null && result.size() > 0
                        && result.get(0)[0] != null) {
                    for (Object[] obj : result) {
                        if (obj[0] != null) {
                            WKBReader wkbReader = new WKBReader();
                            geometries.add(wkbReader.read((byte[]) obj[0]));
                        }
                    }
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Error setting the geometry", e);
            }
        }

        Geometry geometry = null;
        if (geometries.isEmpty() == false) {
            GeometryCollection geometryCollection = new GeometryFactory()
                    .createGeometryCollection(geometries
                            .toArray(new Geometry[geometries.size()]));
            geometry = geometryCollection;
        }

        return geometry;

    }

    /**
     * creates a shape and places in wireframeShape for records that had
     * geometries in the text product or places in shadedShape if there was no
     * geometry in the text product. will set the geometry in the record for
     * faster access if there was no geometry before.
     * 
     * @param record
     */
    @Override
    protected void initShape(IGraphicsTarget target,
            AbstractWarningRecord record) {
        WKTReader wktr = new WKTReader();
        Geometry geo;

        // check both the maps
        WarningEntry entry = entryMap.get(record.getDataURI());
        if (entry == null) {
            entry = new WarningEntry();
            entry.record = record;
            entryMap.put(record.getDataURI(), entry);
        }

        // default to a wireframe shape
        boolean isShaded = false;

        if (entry.shadedShape != null) {
            // if the shape was in the shadedShape map then create a shaded
            // shape
            isShaded = true;
        } else if (entry.wireframeShape == null && record.getGeometry() == null) {
            // if it is not in the wireframeShape map and the geometry is null
            // then create a shaded shape
            isShaded = true;
        }

        if (isShaded) {
            if (record.getUgczones().size() > 0) {
                // if the geometry is null get a geometry based on the county
                // list
                if (record.getGeometry() == null) {
                    record.setGeometry(getGeometry(record));
                }
                if (record.getGeometry() != null) {

                    // dispose old shape
                    if (entry.shadedShape != null) {
                        entry.shadedShape.dispose();
                    }

                    entry.shadedShape = target.createShadedShape(false,
                            descriptor, false);
                    try {
                        geo = wktr.read(record.getGeometry().toString());

                        JTSCompiler jtsCompiler = new JTSCompiler(
                                entry.shadedShape, null, this.descriptor,
                                PointStyle.CROSS);
                        try {
                            jtsCompiler.handle(geo, color);
                        } catch (VizException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                            entryMap.remove(entry.record.getDataURI());
                            return;
                        }
                        entry.shadedShape.setFillPattern(FillPatterns
                                .getGLPattern("VERTICAL_DOTTED"));
                        entry.shadedShape.compile();
                    } catch (ParseException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
        } else {

            try {
                // dispose old shape
                if (entry.wireframeShape != null) {
                    entry.wireframeShape.dispose();
                }

                entry.wireframeShape = target.createWireframeShape(false,
                        descriptor);
                geo = wktr.read(record.getGeometry().toString());

                JTSCompiler jtsCompiler = new JTSCompiler(null,
                        entry.wireframeShape, descriptor);
                jtsCompiler.handle(geo);
                entry.wireframeShape.compile();
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Error creating wireframe", e);
            }
        }
    }

    /**
     * returns the text that should be printed in an array in reverse order
     */
    @Override
    protected String[] getText(AbstractWarningRecord record, double mapWidth) {
        DateUtil du = new DateUtil();
        String[] textToPrint = new String[] { "", "", "" };

        textToPrint[2] = record.getPil();

        SimpleDateFormat startFormat = AbstractWWAResource.DEFAULT_FORMAT;
        SimpleDateFormat endFormat = AbstractWWAResource.DEFAULT_FORMAT;
        if (mapWidth == 0) {
            startFormat = AbstractWWAResource.LONG_FORMAT;
            endFormat = AbstractWWAResource.DAY_FORMAT;
        } else if (mapWidth <= 200) {
            startFormat = AbstractWWAResource.DAY_FORMAT;
            endFormat = AbstractWWAResource.DAY_FORMAT;
        }

        synchronized (startFormat) {
            textToPrint[1] = "Valid "
                    + du.format(record.getStartTime().getTime(), startFormat);
        }
        synchronized (endFormat) {
            textToPrint[0] = "Thru "
                    + du.format(record.getEndTime().getTime(), endFormat);
        }

        return textToPrint;
    }
}
