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
package com.raytheon.uf.edex.ohd.pproc;

import java.awt.Rectangle;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.mpe.util.XmrgFile.XmrgHeader;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Gen Areal FFG process.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2011            mpduff     Initial creation
 * Sep 5, 2013  16437      wkwock      Fix the "HiRes" issue
 * Mar 28, 2014   2952     mpduff      Changed to use UFStatus for logging.
 * Apr 10, 2014   2675     mpduff      Modified to be called from quartz timer.
 * Apr 21, 2014   2060     njensen     Remove dependency on grid dataURI column
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GAFF {
    private static final IUFStatusHandler log = UFStatus.getHandler(GAFF.class);

    private static final int MISSING_VALUE = -99;

    private static final String PROC_FLAG = "MPA01   ";

    private static final float VERNUM = 1.0f;

    private static final int MAXVAL = 32767;

    private static final String OS = "LX";

    private static final String USER = "AWIPS";

    // Token Constants
    private static final String ST3_RFC = "st3_rfc";

    private static final String MOSAIC_DIR = "gaff_mosaic_dir";

    private static final String LOOKBACK_LIMIT = "gaff_look_back_limit";

    private static final String MIN_AREA = "whfs_min_area_covered";

    private static final String RFC_LIST = "gaff_rfc_list";

    private static final String GAFF_DURATIONS = "gaff_durations";

    private static final String PROCESS_NAME = "gen_ffg";

    /**
     * Site ID from Apps_Defaults file
     */
    private String siteId = null;

    /**
     * Mosaic directory from Apps_defaults file
     */
    private String mosaicDir = null;

    /**
     * GAFF Lookback Limit from Apps_defaults file
     */
    private int lookbackLimit = 6;

    /**
     * whfs_min_area_covered from Apps_defaults file
     */
    private double minArea = 0.90;

    /**
     * List of RFC names from Apps_defaults file
     */
    private String[] rfcNames = null;

    /**
     * List of durations from Apps_defaults file
     */
    private int[] durations = null;

    /**
     * The HSA identifier from the admin table
     */
    private String hsa = "XXX";

    /**
     * The previous Duration
     */
    private int prevDuration = 0;

    /**
     * The ffg short data array
     */
    private short[] mosaicFfgShort;

    /**
     * The reference time
     */
    private Date grReftime = null;

    /**
     * The Duration in String format
     */
    private String durString = null;

    /**
     * The extent of the WFO grid
     */
    private Rectangle wfoExtent = null;

    private final GAFFDB db = new GAFFDB();

    /** RFC Site name to RFC lookup map */
    public static Map<String, String> RFCMAP = new HashMap<String, String>();

    /** RFC to RFC Site name lookup map */
    public static Map<String, String> RFC_SITEMAP = new HashMap<String, String>();

    static {
        RFCMAP.put("TUA", "ABRFC");
        RFCMAP.put("ACR", "AKRFC");
        RFCMAP.put("STR", "CBRFC");
        RFCMAP.put("RSA", "CNRFC");
        RFCMAP.put("ORN", "LMRFC");
        RFCMAP.put("RHA", "MARFC");
        RFCMAP.put("KRF", "MBRFC");
        RFCMAP.put("MSR", "NCRFC");
        RFCMAP.put("TAR", "NERFC");
        RFCMAP.put("PTR", "NWRFC");
        RFCMAP.put("TIR", "OHRFC");
        RFCMAP.put("ALR", "SERFC");
        RFCMAP.put("FWR", "WGRFC");

        RFC_SITEMAP.put("ABRFC", "TUA");
        RFC_SITEMAP.put("AKRFC", "ACR");
        RFC_SITEMAP.put("CBRFC", "STR");
        RFC_SITEMAP.put("CNRFC", "RSA");
        RFC_SITEMAP.put("LMRFC", "ORN");
        RFC_SITEMAP.put("MARFC", "RHA");
        RFC_SITEMAP.put("MBRFC", "KRF");
        RFC_SITEMAP.put("NCRFC", "MSR");
        RFC_SITEMAP.put("NERFC", "TAR");
        RFC_SITEMAP.put("NWRFC", "PTR");
        RFC_SITEMAP.put("OHRFC", "TIR");
        RFC_SITEMAP.put("SERFC", "ALR");
        RFC_SITEMAP.put("WGRFC", "FWR");
    }

    public GAFF() {
    }

    private void init() {
        getTokens();

        this.hsa = db.getHsa();
    }

    public void process() {
        long start = TimeUtil.currentTimeMillis();
        log.info("GAFF process starting");
        init();
        Rectangle extent;
        HRAP hrap;
        HRAPSubGrid subGrid;
        try {
            extent = HRAPCoordinates.getHRAPCoordinates();
            hrap = HRAP.getInstance();
            subGrid = hrap.getHRAPSubGrid(extent);
        } catch (Exception e) {
            log.error("Error setting up HRAP subgrid", e);
            return;
        }

        /* loop on FFG durations */
        for (int dur : this.durations) {
            if (log.isPriorityEnabled(Priority.DEBUG)) {
                log.debug("Processing for " + dur + " duration");
            }
            createFFGMosaic(dur, hrap, subGrid);
        }

        /* write record to PerfLog table */
        try {
            db.insertPerflog(PROCESS_NAME, start);
        } catch (DataAccessLayerException e) {
            log.error("Error updating perflog table", e);
        }

        long end = TimeUtil.currentTimeMillis();
        log.info("GAFF Process complete in " + (end - start) + " millis");
    }

    /**
     * Create the FFG xmrg file for the WFO office.
     * 
     * @param dur
     *            The Duration
     * @param hrap
     *            The HRAP object
     * @param subGrid
     *            The HRAP subgrid
     */

    private void createFFGMosaic(int dur, HRAP hrap, HRAPSubGrid subGrid) {
        Calendar cal = TimeUtil.newGmtCalendar();
        TimeUtil.minCalendarFields(cal, Calendar.HOUR, Calendar.HOUR_OF_DAY,
                Calendar.MINUTE, Calendar.SECOND, Calendar.MILLISECOND);
        ThreadLocal<SimpleDateFormat> sdf = TimeUtil
                .buildThreadLocalSimpleDateFormat("yyyy-MM-dd HH:mm:ss",
                        TimeZone.getTimeZone("GMT"));
        String today = sdf.get().format(cal.getTime());
        IDataRecord dataRec;
        Rectangle rfcExtent = null;

        try {
            wfoExtent = HRAPCoordinates.getHRAPCoordinates();
        } catch (Exception e2) {
            log.error("Error setting up the wfo extent", e2);
        }

        // Initialize arrays for each calculation
        if (prevDuration != dur) {
            int nx = subGrid.getNx();
            int ny = subGrid.getNy();
            mosaicFfgShort = new short[nx * ny];
            for (int i = 0; i < mosaicFfgShort.length; i++) {
                mosaicFfgShort[i] = MISSING_VALUE;
            }

            prevDuration = dur;
        }

        if (dur < 10) {
            durString = "0" + dur;
        } else {
            durString = dur + "";
        }

        Map<String, Rectangle> extentsMap = new HashMap<String, Rectangle>();
        Map<String, float[]> gridMap = new HashMap<String, float[]>();

        for (String rfc : this.rfcNames) {
            if (log.isPriorityEnabled(Priority.DEBUG)) {
                log.debug("Getting data for " + rfc + " for " + durString
                        + " duration");
            }
            rfc = rfc.trim();
            if (rfc.length() != 3) {
                rfc = rfcSiteLookup(rfc);
            }

            IDataStore dataStore = null;

            try {
                GridRecord gr = db.getGridRecord(rfc, durString, today);
                if (gr == null) {
                    gr = db.getGridRecord(rfc + "-HiRes", durString, today);
                }
                if (gr == null) {
                    continue;
                }

                PluginDao gd = null;
                gd = PluginFactory.getInstance().getPluginDao(
                        gr.getPluginName());
                grReftime = gr.getDataTime().getRefTime();
                dataStore = gd.getDataStore(gr);

                int nx = gr.getSpatialObject().getNx();
                int ny = gr.getSpatialObject().getNy();

                HRAP rfcHrap;
                Coordinate org = MapUtil.gridCoordinateToLatLon(new Coordinate(
                        0, 0), PixelOrientation.CENTER, gr.getSpatialObject());

                rfcHrap = HRAP.getInstance();

                Coordinate ulRfcNationalScale = rfcHrap.latLonToGridCoordinate(
                        org, PixelOrientation.CENTER);

                rfcExtent = new Rectangle((int) ulRfcNationalScale.x,
                        (int) ulRfcNationalScale.y - ny, nx, ny);
                extentsMap.put(rfc, rfcExtent);

                dataRec = dataStore.retrieve(gr.getDataURI(), "Data",
                        Request.ALL);

                if (dataRec instanceof FloatDataRecord) {
                    gridMap.put(rfc, ((FloatDataRecord) dataRec).getFloatData());
                }

            } catch (PluginException e1) {
                log.error("Error querying grids", e1);
            } catch (Exception e) {
                log.error("Error creating wfo ffg grid", e);
            }
        }

        int wfoMinX = (int) wfoExtent.getMinX();
        int wfoMinY = (int) wfoExtent.getMinY();
        int wfoMaxX = (int) wfoExtent.getMaxX();
        int wfoMaxY = (int) wfoExtent.getMaxY();

        for (String rfc : gridMap.keySet()) {
            // process grids
            Rectangle extent = extentsMap.get(rfc);
            int rfcMinX = (int) extent.getMinX();
            int rfcMinY = (int) extent.getMinY();
            int rfcMaxX = (int) extent.getMaxX();
            int rfcMaxY = (int) extent.getMaxY();

            int height = rfcMaxY - rfcMinY;
            int width = rfcMaxX - rfcMinX;
            int k = 0;

            float[] data = gridMap.get(rfc);

            // Pull data from each grid
            int x = rfcMinX;
            int y = rfcMaxY;
            try {
                for (int i = 0; i < height; i++) {
                    x = rfcMinX;
                    for (int j = 0; j < width; j++) {
                        if ((x >= wfoMinX) && (x < wfoMaxX) && (y >= wfoMinY)
                                && (y < wfoMaxY)) {
                            if (data[(i * width) + j] > -999) {
                                this.mosaicFfgShort[k] = ((short) (data[(i * width)
                                        + j] * 100));
                            }
                            k++;
                        }
                        x++;
                    }
                    y--;
                }
            } catch (Exception e) {
                log.error("Caught exception.", e);
            }

            writeXmrgFile();

            /*
             * extract basin area info in form of line segs from db for each
             * basin, attempt to compute FFG value averaged over basin area if
             * successful and minimum areal coverage exceeded, then write record
             * to ContingencyValue table
             */
            GAFFAreaProcessor areaProcessor = new GAFFAreaProcessor(
                    wfoExtent.x, wfoExtent.y, wfoExtent.height,
                    wfoExtent.width, this.mosaicFfgShort, minArea, dur,
                    grReftime.getTime());
            areaProcessor.processAreas();
        }
    }

    /**
     * Write out the FFG Xmrg file
     */
    private void writeXmrgFile() {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHH");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        // Generate Header
        XmrgHeader xmhead = new XmrgHeader();
        xmhead.setValidDate(grReftime);
        xmhead.setSaveDate(grReftime);
        xmhead.setOperatingSystem(OS);
        xmhead.setVersionNumber(VERNUM);
        xmhead.setUserId(USER);
        xmhead.setMaxValue(MAXVAL);
        xmhead.setProcessFlag(PROC_FLAG);

        // Filename
        String name = this.mosaicDir + "/" + this.hsa + sdf.format(grReftime)
                + durString + ".ffg";

        XmrgFile xmrg = new XmrgFile(name);
        xmrg.setHrapExtent(wfoExtent);
        xmrg.setHeader(xmhead);
        xmrg.setData(this.mosaicFfgShort);

        try {
            log.debug("Writing xmrg file:  " + name);
            xmrg.save(name);
        } catch (IOException e) {
            log.error("Error writing xmrg file", e);
        }

    }

    /**
     * Get the token values for GAFF.
     */
    private void getTokens() {
        AppsDefaults gad = AppsDefaults.getInstance();

        this.siteId = gad.getToken(ST3_RFC); // Fails if not set
        this.mosaicDir = gad.getToken(MOSAIC_DIR); // Fails if not set
        this.lookbackLimit = gad.getInt(LOOKBACK_LIMIT, 6);
        this.minArea = gad.getDouble(MIN_AREA, 0.9);

        String rfcs = gad.getToken(RFC_LIST);
        if ((rfcs != null) && (rfcs.length() > 0)) {
            this.rfcNames = rfcs.split(",");
        }

        String durationString = gad.getToken(GAFF_DURATIONS);
        if ((durationString != null) && (durationString.length() > 0)) {
            String[] sa = durationString.split(",");
            this.durations = new int[sa.length];

            for (int i = 0; i < sa.length; i++) {
                durations[i] = Integer.parseInt(sa[i]);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(
                "***********************************\n");
        sb.append("siteId = ").append(this.siteId);
        sb.append("\nmosaicDir = ").append(this.mosaicDir);
        sb.append("\nlookbackLimit = ").append(this.lookbackLimit);
        sb.append("\nminArea = ").append(this.minArea);
        sb.append("\nRFC Names");
        for (String s : rfcNames) {
            sb.append("\n   ").append(s);
        }
        sb.append("\nDurations");
        for (int i : this.durations) {
            sb.append("\n   ").append(i);
        }

        sb.append("\nHSA = ").append(this.hsa);
        return sb.toString();
    }

    /**
     * RFC to RFC Site name lookup.
     * 
     * @param site
     *            The RFC or Site Identifier
     * @return The RFC Name or the Site Identifier, or null if nothing found
     */
    public String rfcSiteLookup(String site) {
        if (RFCMAP.containsKey(site)) {
            return RFCMAP.get(site);
        }

        if (RFC_SITEMAP.containsKey(site)) {
            return RFC_SITEMAP.get(site);
        }

        return null;
    }
}