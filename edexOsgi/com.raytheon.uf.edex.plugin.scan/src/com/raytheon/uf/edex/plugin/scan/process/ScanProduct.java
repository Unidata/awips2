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
package com.raytheon.uf.edex.plugin.scan.process;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.scan.data.CellTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.monitor.scan.LightningStrike;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.plugin.scan.ScanURIFilter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * 
 * Process incoming Radar, Lightning data for SCAN
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/07/2009   2037      dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
public abstract class ScanProduct implements Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public String dataType = null;

    public ScanTables tableType = null;

    public ScanURIFilter filter = null;

    public String uri = null;

    public String radarSQL = null;

    public String lightningSQL = null;

    public static String RADAR = "radar";

    public static String BINLIGHTNING = "binlightning";

    public static String UA = "bufrua";

    public static GeometryFactory factory = new GeometryFactory();

    public String MESO_TYPE = RadarConstants.MapValues.MESO_TYPE.name();

    /** default time period back for initial load. **/
    public static String NO_DATA = "NO DATA";

    public static String UNKNOWN = "UNKNOWN";

    public static String NEW = "NEW";

    public static String UPDATE = "UPD";

    public static String BLANK = "";

    public static String NONE = "NONE";

    /** regex wild card filter */
    protected static String wildCard = "[\\w\\(\\)\\-_:.]+";

    /** Separator for URI's */
    public static String uriSeparator = "/";

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScanProduct.class);

    /**
     * 
     * @param uri
     * @param tableType
     * @param filter
     */
    public ScanProduct(String uri, ScanTables tableType, ScanURIFilter filter) {
        this.uri = uri;
        this.tableType = tableType;
        this.filter = filter;
    }

    /**
     * Ensures that folks set a data type
     * 
     * @param type
     */
    public abstract void setDataType();

    /**
     * get the type set
     * 
     * @return
     */
    public ScanTables getTableType() {
        return tableType;
    }

    /**
     * Ensures that folks set an allowance
     * 
     * @param
     */
    public abstract boolean getAllowNew();

    /**
     * get the Record you need
     * 
     * @return
     * @throws Exception
     */
    public abstract PersistablePluginDataObject getRecord() throws Exception;

    /**
     * Process the product, adding the data to the SCAN Table Data objects
     * 
     * @throws VizException
     */
    public abstract void process() throws Exception;

    /**
     * Does the actual writing of the table data
     * 
     * @throws VizException
     */
    public abstract ScanTableDataRow write(ScanTableDataRow row,
            PersistablePluginDataObject rec, String key);

    /**
     * Get County Query
     * 
     * @param c
     * @return
     */
    public static String getCountyBySpatialQuery(Coordinate c) {
        ISpatialQuery sq = null;
        Object[] results = null;
        String state = null;
        String county = null;
        String level = ScanUtils.getStandardResolutionLevel("county");

        try {
            sq = SpatialQueryFactory.create();
            String sql = "select state, countyname from mapdata.county where "
                    + "ST_Contains(" + level + ", ST_GeomFromText('"
                    + ScanUtils.getPointAsText(c) + "', 4326))";
            results = sq.dbRequest(sql, "maps");
            if (results.length == 0) {
                return new String("UNK UNK");
            }
            Object[] results2 = (Object[]) results[0];
            state = (String) results2[0];
            county = (String) results2[1];
        } catch (SpatialException e1) {
            e1.printStackTrace();
        }

        return new String(state + " " + county);
    }

    /**
     * Get CWA Query
     * 
     * @param c
     * @return
     */
    public static String getCWABySpatialQuery(Coordinate c) {
        ISpatialQuery sq = null;
        Object[] results = null;
        String cwa = null;
        String level = ScanUtils.getStandardResolutionLevel("cwa");

        try {
            sq = SpatialQueryFactory.create();

            String sql = "select cwa from mapdata.cwa where " + "ST_Contains("
                    + level + ", ST_GeomFromText('"
                    + ScanUtils.getPointAsText(c) + "', 4326))";

            results = sq.dbRequest(sql, "maps");

            if (results.length == 0) {
                cwa = new String("UNK");
            } else {
                cwa = (String) results[0];
            }
        } catch (SpatialException e1) {
            e1.printStackTrace();
        }

        return cwa;
    }

    /**
     * get Populated grib record
     * 
     * @param uri
     * @return
     */
    public static GridRecord getGridRecord(String uri) throws PluginException {

        GridRecord gr = new GridRecord(uri);
        PluginDao gd = PluginFactory.getInstance().getPluginDao(
                gr.getPluginName());
        gr = (GridRecord) gd.getMetadata(uri);
        IDataStore dataStore = gd.getDataStore(gr);

        try {
            IDataRecord[] dataRec = dataStore.retrieve(uri);
            for (int i = 0; i < dataRec.length; i++) {
                if (dataRec[i] instanceof FloatDataRecord) {
                    gr.setMessageData(dataRec[i]);
                }
            }
        } catch (Exception se) {
            se.printStackTrace();
        }

        return gr;
    }

    /**
     * process lightning
     * 
     * @param filter
     * @param startTime
     * @param stopTime
     * @param table
     */
    public static void processLightning(ScanURIFilter filter,
            RadarRecord radarRecord, Date startTime, Date stopTime,
            ScanTableData<?> table) {

        if (table.getTableData().size() > 0) {

            GeodeticCalculator gc = new GeodeticCalculator(radarRecord.getCRS());

            Iterator<String> it = table.getTableData().keySet().iterator();
            // get the data
            ArrayList<BinLightningRecord> recs = filter
                    .getLightningData(startTime);

            while (it.hasNext()) {
                String id = it.next();
                // System.out.println("Lightning ID: " + id);
                ArrayList<LightningStrike> strikeList = new ArrayList<LightningStrike>();

                Coordinate stormCoor = new Coordinate(
                        table.getRow(id).getLon(), table.getRow(id).getLat(),
                        0.0);

                for (BinLightningRecord rec : recs) {
                    for (int i = 0; i < rec.getLatitudes().length; i++) {
                        // check the time first
                        long obsTime = rec.getObsTimes()[i];
                        if (obsTime >= startTime.getTime()
                                && obsTime < stopTime.getTime()) {
                            Coordinate strikeCoor = new Coordinate(
                                    rec.getLongitudes()[i],
                                    rec.getLatitudes()[i], 0.0);

                            // Putting this to km instead of nautical miles DR
                            // #5490
                            double dist = ScanUtils.getDistance(stormCoor,
                                    strikeCoor, gc) / 1000.0;

                            if (ScanUtils.LIGHTNING_STORM_RANGE >= dist) {

                                // System.out
                                // .println("Found cell with Lightning: "
                                // + dist + " " + id);

                                LightningStrike strike = new LightningStrike(
                                        (double) rec.getLatitudes()[i],
                                        (double) rec.getLongitudes()[i],
                                        rec.getIntensities()[i],
                                        rec.getStrikeTypes()[i],
                                        rec.getMsgTypes()[i],
                                        rec.getStrikeCounts()[i]);

                                strikeList.add(strike);
                            }
                        }
                    }
                }

                if (strikeList.size() > 0) {

                    int totalPosStrikes = 0;
                    int totalCGStrikes = 0;
                    int totalStrikes = 0;

                    for (LightningStrike ls : strikeList) {
                        if (ls.getIntensity() > 0) {
                            totalPosStrikes++;
                        }
                        if (ls.getStrikeType() == LtgStrikeType.STRIKE_CG
                                .ordinal()) {
                            totalCGStrikes++;
                        }
                        totalStrikes++;
                    }

                    long start_time = startTime.getTime();
                    long stop_time = stopTime.getTime();
                    // only need int precision
                    double cg_rate = totalCGStrikes
                            / ((stop_time - start_time) / (1000.0 * 60.0));

                    double percentPos = 0.0;
                    if (totalPosStrikes > 0) {
                        percentPos = ((double) totalPosStrikes / (double) totalStrikes) * 100.0;
                    }

                    ((CellTableDataRow) table.getRow(id)).setCgRate(cg_rate);
                    ((CellTableDataRow) table.getRow(id)).setPos(percentPos);

                    // System.out.println("Cell Id: " + id + " cgRate: " +
                    // cg_rate
                    // + " % positive: " + percentPos);
                }
            }
        }
    }

    /**
     * Gets a list of rows that should be dumped
     * 
     * @param list
     * @param data
     * @return
     */
    public ArrayList<String> getDeletions(List<String> list,
            ScanTableData<?> data) {

        ArrayList<String> dies = new ArrayList<String>();
        ;
        for (String id : data.getTableData().keySet()) {
            if (!list.contains(id)) {
                dies.add(id);
            }
        }

        return dies;
    }

    /**
     * Gets a list of rows that should be added
     * 
     * @param list
     * @param data
     * @return
     */
    public ArrayList<String> getAdditions(ArrayList<String> list,
            ScanTableData<?> data) {

        ArrayList<String> news = new ArrayList<String>();
        if (data.getTableData().keySet().size() > 0) {
            for (String id : list) {
                if (!data.getTableData().keySet().contains(id)) {
                    news.add(id);
                }
            }
        }
        // first time
        else {
            news = list;
        }

        return news;
    }

    /**
     * Gets a list of rows that should be added
     * 
     * @param list
     * @param data
     * @return
     */
    public ArrayList<String> getUpdates(ArrayList<String> list,
            ScanTableData<?> data) {

        ArrayList<String> ups = new ArrayList<String>();
        for (String id : data.getTableData().keySet()) {
            if (list.contains(id)) {
                ups.add(id);
            }
        }

        return ups;
    }

    /**
     * Gets the dataTable entry by time
     * 
     * @param time
     * @return
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public <T extends ScanTableDataRow> ScanTableData<T> getTableData() {
        ScanTableData<T> table = null;
        // DMD is always a new record
        table = (ScanTableData) filter.getData(getTableType());

        if (table == null) {
            table = getNewTable(tableType);
        }

        return table;
    }

    /**
     * Sets up the spatial sections for the tables
     * 
     * @param row
     * @param key
     * @param pdo
     * @return
     */
    public abstract ScanTableDataRow setSpatial(ScanTableDataRow row,
            String key, PersistableDataObject pdo);

    /**
     * Create a new tabledata object
     * 
     * @param <T>
     * @param tableType
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T extends ScanTableDataRow> ScanTableData<T> getNewTable(
            ScanTables tableType) {
        ScanTableData<T> table = null;

        if (filter.getData(tableType) != null) {
            filter.getData(tableType).getTableData().clear();
        } else {
            filter.resetData();
            table = (ScanTableData<T>) filter.getData(tableType);
        }

        return table;
    }

}
