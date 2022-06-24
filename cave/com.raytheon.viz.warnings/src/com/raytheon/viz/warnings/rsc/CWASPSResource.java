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

import java.text.DateFormat;
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
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.JTSCompiler;
import com.raytheon.uf.viz.core.drawables.JTSCompiler.JTSGeometryData;
import com.raytheon.uf.viz.core.drawables.JTSCompiler.PointStyle;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.io.WKBReader;
import com.vividsolutions.jts.io.WKTReader;

/**
 * CWASPSResource
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 10, 2013 1951       rjpeter     Initial history entry, updated ugcZones references
 * Nov 08, 2013 16758      mgamazaychikov Added mergeWatches to simplify SPS processing
 *                                        and getEventKey to create SPS-unique key
 * Aug 01, 2014 3471       mapeters    Updated deprecated createShadedShape() calls.
 * Aug 22, 2016 5842       dgilling    Remove dependency on viz.texteditor plugin.
 * Sep 14, 2016 3241       bsteffen    Update deprecated JTSCompiler method calls
 * Jun 24, 2022			   srcarter@ucar  Always create outline (wireshapes) and fill (shadedshapes)
 * </pre>
 *
 * @author rjpeter
 */
public class CWASPSResource extends WatchesResource {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CWASPSResource.class);

    private static final Set<String> marinezones = new HashSet<>(
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
        List<String> queries = new ArrayList<>();

        for (String ugc : record.getUgcZones()) {

            if ((ugc.charAt(2) == 'Z')
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
            queries.add(
                    "select ST_AsBinary(the_geom) from mapdata.county where state||'C'||substring(fips,3,3) in ("
                    + countyList + ")");
        }
        if (zoneList.length() > 0) {
            queries.add(
                    "select ST_AsBinary(the_geom) from mapdata.zone where state_zone in ("
                    + zoneList + ")");
        }
        if (marinezoneList.length() > 0) {
            queries.add(
                    "select ST_AsBinary(the_geom) from mapdata.marinezones where id in ("
                    + marinezoneList + ")");
        }

        List<Geometry> geometries = new ArrayList<>();
        for (String sql : queries) {
            try {
                List<Object[]> result = DirectDbQuery.executeQuery(sql, "maps",
                        QueryLanguage.SQL);
                if ((result != null) && (result.size() > 0)
                        && (result.get(0)[0] != null)) {
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
        
        if (!record.getUgcZones().isEmpty()) {
            // if the geometry is null get a geometry based on the county
            // list
            if (record.getGeometry() == null) {
                record.setGeometry(getGeometry(record));
            }
            if (record.getGeometry() != null) {
                try {
                	//give every entry a fill and outline
                    //add fill (shadedshape)
                	IShadedShape ss = target.createShadedShape(false, descriptor.getGridGeometry());
                	geo = wktr.read(record.getGeometry().toString());
                    JTSCompiler jtsCompiler = new JTSCompiler(ss, null, this.descriptor);
                    JTSGeometryData geoData = jtsCompiler.createGeometryData();
                    geoData.setGeometryColor(color);
                    geoData.setPointStyle(PointStyle.CROSS);
                    jtsCompiler.handle(geo, geoData);
                    ss.setFillPattern(FillPatterns.getGLPattern("VERTICAL_DOTTED"));
                    ss.compile();

                    entry.shadedShape = ss;
                	
                    //add outline (wireshape)
                    IWireframeShape wfs = entry.wireframeShape;

                    if (wfs != null) {
                        wfs.dispose();
                    }

                    wfs = target.createWireframeShape(false, descriptor);

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
    }

    /**
     * returns the text that should be printed in an array in reverse order
     */
    @Override
    protected String[] getText(AbstractWarningRecord record, double mapWidth) {
        String[] textToPrint = new String[] { "", "", "" };

        textToPrint[2] = record.getPil();

        String startFormatString = DEFAULT_FORMAT;
        String endFormatString = DEFAULT_FORMAT;
        if (mapWidth == 0) {
            startFormatString = AbstractWWAResource.LONG_FORMAT;
            endFormatString = AbstractWWAResource.DAY_FORMAT;
        } else if (mapWidth <= 200) {
            startFormatString = AbstractWWAResource.DAY_FORMAT;
            endFormatString = AbstractWWAResource.DAY_FORMAT;
        }

        DateFormat startFormat = new SimpleDateFormat(startFormatString);
        startFormat.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        textToPrint[1] = "Valid "
                + startFormat.format(record.getStartTime().getTime());

        DateFormat endFormat = new SimpleDateFormat(endFormatString);
        endFormat.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        textToPrint[0] = "Thru "
                + endFormat.format(record.getEndTime().getTime());

        return textToPrint;
    }

    /**
     * Groups all the ugc zones with the same 'product.act.phensig.etn'
     *
     * Since there are no ugc zones in SPSs return the input watch records
     * without changing them.
     */
    @Override
    protected List<AbstractWarningRecord> mergeWatches(
            List<AbstractWarningRecord> watchrecs) {
        return watchrecs;
    }

    /**
     * Create unique enough key to be used in paint method entryMap
     *
     * Use wmoId and countyHeader fields
     **/
    @Override
    protected String getEventKey(WarningEntry entry) {
        AbstractWarningRecord rec = entry.record;
        return rec.getWmoid().replaceAll(" ", "_") + ':'
                + rec.getInsertTime().getTimeInMillis();
    }
}
