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
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TimeZone;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
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
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.ohd.MainMethod;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Quartz timer driven service to run process Areal QPE mosaics
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2011            snaples     Initial creation
 * Jan 10, 2013 1448       bgonzale    Added app context check in processArealQpe().
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class ArealQpeGenSrv {

    /**
     * <pre>
     * Grib Subcenter ID to RFC_ID to ICAO
     * 150 ABRFC TUA
     * 151 AKRFC ACR
     * 152 CBRFC STR
     * 153 CNRFC RSA
     * 154 LMRFC ORN
     * 155 MARFC RHA
     * 156 MBRFC KRF
     * 157 NCRFC MSR
     * 158 NERFC TAR
     * 159 NWRFC PTR
     * 160 OHRFC TIR
     * 161 SERFC ALR
     * 162 WGRFC FWR
     * </pre>
     */
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

    String token_delimiter = ",";

    String[] rfcs = new String[13];

    int[] durs = new int[5];

    private Rectangle extent;

    private HRAP hrap;

    private HRAPSubGrid subGrid;

    private Log log = LogFactory.getLog("GenArealQPE");

    private Rectangle wfoExtent;

    /**
     * The reference time
     */
    private Date grReftime = null;

    /**
     * The previous Duration
     */
    private int prevDuration = 0;

    /**
     * The qpe short data array
     */
    private short[] mosaicQpeShort;

    private String durString;

    private Rectangle rfcExtent;

    private static final String RFC_QPE_BASE_STRING = "RFCMOSAIC";

    private static final String PROC_FLAG = "MPA01   ";

    private static final float VERNUM = 1.0f;

    private static final int MAXVAL = 32767;

    private static final String OS = "LX";

    private static final String USER = "SANmdY";

    private static final String GRIBIT = "gribit.LX";

    private Map<String, Rectangle> extentsMap = new HashMap<String, Rectangle>();

    private Map<String, float[]> gridMap = new HashMap<String, float[]>();

    private int dur;

    private AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private String gaq_xmrg_1hr_dir;

    private String gaq_xmrg_6hr_dir;

    private String gaq_xmrg_24hr_dir;

    private String gaq_temp_xmrg_dir;

    private String gaq_grib_dir;

    private String d2d_input_dir;

    private boolean qpe_out;

    private SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    private SimpleDateFormat fdf = new SimpleDateFormat("yyyyMMddHH");

    public Object processArealQpe() {
        if (!AppsDefaults.getInstance().setAppContext(this)) {
            return null;
        }

        // Check to see if we need to run
        String gen = appsDefaults.getToken("mpe_generate_areal_qpe");
        if (gen.equalsIgnoreCase("OFF")) {
            return null;
        }

        Date dt = new Date();
        Calendar cc = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cc.setTime(dt);
        log.info("Started process_qpe_mosaic at " + cc.getTime());

        String gribout = appsDefaults.getToken("mpe_d2d_display_grib");
        qpe_out = false;
        getOptions();
        setExtent();

        /* loop on QPE durations */
        for (int dd : durs) {
            if (log.isDebugEnabled()) {
                log.debug("Processing for " + dd + " hr duration");
            }
            processSingleQpe(dd, hrap, subGrid);
        }
        if (gribout.equalsIgnoreCase("ON") && (qpe_out == true)) {
            gribTempFiles();
        }

        dt = new Date();
        cc.setTime(dt);
        log.info("Finished process_qpe_mosaic at " + cc.getTime());
        dt = null;
        return null;
    }

    /**
     * 
     */
    private void gribTempFiles() {
        log.info("Searching for QPE Mosaic files in " + gaq_temp_xmrg_dir);
        File f = new File(gaq_temp_xmrg_dir);
        int fl = f.listFiles().length;
        File[] fa = new File[fl];
        fa = f.listFiles();
        if (fa.length > 0) {
            for (File ff : fa) {
                log.info(ff.getAbsolutePath());
            }
            // gribit environment vars
            Map<String, String> envVars = new HashMap<String, String>();

            if (AppsDefaults.getInstance().setAppContext(this)) {
                envVars.put("grib_in_dir", gaq_temp_xmrg_dir);
                envVars.put("grib_out_dir", gaq_temp_xmrg_dir);
                envVars.put("grib_set_subcenter_0", "on");

                String pproc_bin = appsDefaults.getToken("pproc_bin");
                MainMethod m = new MainMethod(new ProcessBuilder(pproc_bin
                        + "/" + GRIBIT));

                for (File fr : fa) {
                    String inFile = "";
                    String gribOutFile = "";
                    inFile = fr.getName();
                    gribOutFile = inFile + ".grib";
                    envVars.put("grib_in_file", inFile);
                    envVars.put("grib_out_file", gribOutFile);
                    if (envVars != null) {
                        m.getProcessBuilder().environment().putAll(envVars);
                    }
                    int exitValue = m.execute();

                    // Output result
                    if (exitValue == 0) {
                        log.info("Areal QPE wrote output grib file: "
                                + gribOutFile);
                    } else {
                        log.error("Areal QPE grib file out process terminated with exit code: "
                                + exitValue);
                        return;
                    }

                    try {
                        RandomAccessFile raf = new RandomAccessFile(new File(
                                fr.getParentFile(), gribOutFile), "rw");
                        raf.seek(0);

                        int idx = 0;
                        byte b = raf.readByte();

                        boolean found = false;
                        while (!found) {
                            if (b == (byte) 'G') {
                                b = raf.readByte();
                                if (b == (byte) 'R') {
                                    b = raf.readByte();
                                    if (b == (byte) 'I') {
                                        b = raf.readByte();
                                        if (b == (byte) 'B') {
                                            found = true;
                                        } else {
                                            idx += 3;
                                        }
                                    } else {
                                        idx += 2;
                                    }
                                } else {
                                    idx += 1;
                                }
                            } else {
                                idx += 1;
                                b = raf.readByte();
                            }
                        }

                        // Index 8 bytes for Section 0 of grib spec, and 5 bytes
                        // for
                        // Section 1 of grib spec to get to gen process id index
                        idx += (8 + 5);

                        // Increment gen process id
                        raf.seek(idx);
                        byte gpid = raf.readByte();
                        raf.seek(idx);
                        raf.writeByte(gpid + 1);

                        raf.close();
                    } catch (Exception e) {
                        log.error("Error modifying grib output file", e);
                        return;
                    }

                    String newFname = fr.getName() + ".grib";
                    File mvFile = new File(gaq_grib_dir + "/" + newFname);
                    File goFile = new File(gaq_temp_xmrg_dir + "/"
                            + gribOutFile);
                    Date dt = new Date();
                    Calendar cc = Calendar.getInstance(TimeZone
                            .getTimeZone("GMT"));
                    cc.setTime(dt);
                    SimpleDateFormat ffn = new SimpleDateFormat("ddHHmmss");
                    String dString = ffn.format(cc.getTime());
                    File gribIngestFile = new File(d2d_input_dir
                            + File.separator + "arealQpeGenSrv"
                            + File.separator + fr.getName() + "_" + dString
                            + ".grib");
                    log.info("Move and rename grib file " + goFile + " to "
                            + mvFile);
                    boolean fmv = goFile.renameTo(mvFile);

                    // The move failed. Remove the grib file.
                    if (fmv == false) {
                        log.error("Move and rename grib file " + goFile
                                + " to " + mvFile + " failed. ");
                        goFile.delete();
                    } else {
                        try {
                            FileUtil.copyFile(mvFile, gribIngestFile);
                            log.info("Copied grib file " + mvFile.getName()
                                    + " to " + d2d_input_dir + File.separator
                                    + "arealQpeGenSrv" + " for ingest to D2D. ");
                        } catch (IOException e) {
                            log.error("Copy grib file " + mvFile.getName()
                                    + " to " + d2d_input_dir + File.separator
                                    + "arealQpeGenSrv" + " failed. ");
                            e.printStackTrace();
                        }
                    }
                    // Remove the xmrg file from the temp directory.
                    fr.delete();
                    if (log.isDebugEnabled()) {
                        log.debug("Removed file " + fr
                                + " from rfcqpe_temp directory.");
                    }
                    mvFile = null;
                    goFile = null;
                }
                m = null;
            }
        } else {
            log.info("No QPE mosaic files found in " + gaq_temp_xmrg_dir);
            Date dt = new Date();
            Calendar cc = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cc.setTime(dt);
            log.info("Finished process_qpe_mosaic at " + cc.getTime());
            dt = null;
        }
        f = null;
        fa = null;
        return;
    }

    /**
     * 
     */
    private void writeXmrg(int d, short[] data) {
        int duration = d;
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        Date dt = new Date();
        cal.setTime(dt);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        String time_string = fdf.format(cal.getTime());

        String xmrg_temp_filename = "";
        String xmrg_saved_filename = "";
        xmrg_temp_filename = String.format("%s/%s%02d%sz", gaq_temp_xmrg_dir,
                RFC_QPE_BASE_STRING, duration, time_string);

        switch (duration) {
        case 1:
            xmrg_saved_filename = String.format("%s/%s%02d%sz",
                    gaq_xmrg_1hr_dir, RFC_QPE_BASE_STRING, duration,
                    time_string);
            break;
        case 6:
            xmrg_saved_filename = String.format("%s/%s%02d%sz",
                    gaq_xmrg_6hr_dir, RFC_QPE_BASE_STRING, duration,
                    time_string);
            break;
        case 24:
            xmrg_saved_filename = String.format("%s/%s%02d%sz",
                    gaq_xmrg_24hr_dir, RFC_QPE_BASE_STRING, duration,
                    time_string);
            break;
        default:
            log.error("Unrecognized duration: " + duration);
        }

        // Save RFC QPE file
        // Generate Header
        XmrgHeader xmhead = new XmrgHeader();
        xmhead.setValidDate(cal.getTime());
        xmhead.setSaveDate(cal.getTime());
        xmhead.setOperatingSystem(OS);
        xmhead.setVersionNumber(VERNUM);
        xmhead.setUserId(USER);
        xmhead.setMaxValue(MAXVAL);
        xmhead.setProcessFlag(PROC_FLAG);

        XmrgFile xmrg = new XmrgFile(xmrg_saved_filename);
        xmrg.setHrapExtent(wfoExtent);
        xmrg.setHeader(xmhead);
        xmrg.setData(mosaicQpeShort);

        try {
            log.info("Writing RFC QPE file:  " + xmrg_saved_filename);
            xmrg.save(new File(xmrg_saved_filename));
            xmhead = null;
        } catch (IOException e) {
            log.error("Error writing RFC QPE file", e);
            e.printStackTrace();
            xmhead = null;
            return;
        }

        // Save TEMP QPE file
        // Generate Header
        xmhead = new XmrgHeader();
        xmhead.setValidDate(cal.getTime());
        xmhead.setSaveDate(cal.getTime());
        xmhead.setOperatingSystem(OS);
        xmhead.setVersionNumber(VERNUM);
        xmhead.setUserId(USER);
        xmhead.setMaxValue(MAXVAL);
        xmhead.setProcessFlag(PROC_FLAG);

        xmrg = new XmrgFile(xmrg_saved_filename);
        xmrg.setHrapExtent(wfoExtent);
        xmrg.setHeader(xmhead);
        xmrg.setData(mosaicQpeShort);

        try {
            log.info("Writing Temp QPE file:  " + xmrg_temp_filename);
            xmrg.save(xmrg_temp_filename);
            xmhead = null;
        } catch (IOException e) {
            log.error("Error writing Temp QPE file", e);
            e.printStackTrace();
            xmhead = null;
            return;
        }
        qpe_out = true;
        return;
    }

    /**
     * @param dd
     * @param hrap
     * @param subGrid
     * @return
     */
    private void processSingleQpe(int dd, HRAP hrap, HRAPSubGrid subGrid) {
        dur = dd;
        boolean dataFound = false;
        boolean status = false;
        for (String rf : rfcs) {
            String rfc = rfcSiteLookup(rf);

            try {
                wfoExtent = HRAPCoordinates.getHRAPCoordinates();
            } catch (Exception e2) {
                log.error("Error setting up the wfo extent", e2);
                e2.printStackTrace();
                return;
            }

            // Initialize arrays for each calculation
            if (prevDuration != dur) {
                int nx = subGrid.getNx();
                int ny = subGrid.getNy();
                mosaicQpeShort = new short[nx * ny];
                prevDuration = dur;
            }
            if (dur < 10) {
                durString = "0" + dur;
            } else {
                durString = dur + "";
            }

            rfc = rfc.trim();
            status = getData(rfc);
            if (status == true) {
                dataFound = true;
            }
        }
        if (dataFound == true) {
            processGrids();
            writeXmrg(dd, mosaicQpeShort);
        } else {
            if (log.isDebugEnabled()) {
                log.debug("Getting data for duration " + durString
                        + " returned no data.");
            }
        }
        return;
    }

    private void getOptions() {
        String rfclist = appsDefaults.getToken("gaq_rfc_list");
        String durlist = appsDefaults.getToken("gaq_dur_list");
        StringTokenizer st = new StringTokenizer(rfclist, token_delimiter);
        int num = st.countTokens();
        rfcs = new String[num];
        for (int i = 0; i < num; i++) {
            rfcs[i] = st.nextToken();
        }
        st = new StringTokenizer(durlist, token_delimiter);
        num = st.countTokens();
        durs = new int[num];
        for (int i = 0; i < num; i++) {
            durs[i] = Integer.parseInt(st.nextToken());
        }
        gaq_xmrg_1hr_dir = appsDefaults.getToken("gaq_xmrg_1hr_dir");
        gaq_xmrg_6hr_dir = appsDefaults.getToken("gaq_xmrg_6hr_dir");
        gaq_xmrg_24hr_dir = appsDefaults.getToken("gaq_xmrg_24hr_dir");
        gaq_temp_xmrg_dir = appsDefaults.getToken("gaq_temp_xmrg_dir");
        gaq_grib_dir = appsDefaults.getToken("gaq_grib_dir");
        d2d_input_dir = appsDefaults.getToken("d2d_input_dir");
        st = null;

    }

    private void setExtent() {
        try {
            extent = HRAPCoordinates.getHRAPCoordinates();
            hrap = HRAP.getInstance();
            subGrid = hrap.getHRAPSubGrid(extent);
        } catch (Exception e) {
            log.error("Error setting up HRAP subgrid", e);
            e.printStackTrace();
            return;
        }
    }

    /**
     * Get the data URI
     * 
     * @param rfc
     *            The RFC
     * @param duration
     *            The duration
     * @param today
     *            Today's date in database string format
     * @return The database uri, or null if no data
     * @throws DataAccessLayerException
     */
    private String getDataURI(String rfc, String duration, String today)
            throws DataAccessLayerException {
        String uri = null;

        // Query for uri
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addReturnedField("dataURI");
        query.addQueryParam(GridConstants.DATASET_ID, "QPE-" + rfc);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, "QPE"
                + duration + "%", "like");
        query.addQueryParam("dataTime.validPeriod.end", today);

        CoreDao dao = null;
        dao = new CoreDao(DaoConfig.forDatabase("metadata"));
        List<?> rs = dao.queryByCriteria(query);
        if ((rs != null) && (!rs.isEmpty())) {
            if ((rs.get(0) != null) && (rs.get(0) instanceof String)) {
                uri = (String) rs.get(0);
            }
        } else {
            uri = null;
        }
        return uri;
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

    private boolean getData(String rfc) {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        Date dt = new Date();
        cal.setTime(dt);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        String today = sdf.format(cal.getTime());
        String uri = null;
        IDataRecord dataRec;
        IDataStore dataStore = null;

        try {
            uri = getDataURI(rfc, durString, today);
            if (uri == null) {
                grReftime = cal.getTime();
                return false;
            }

            GridRecord gr = new GridRecord(uri);
            PluginDao gd = null;

            gd = PluginFactory.getInstance().getPluginDao(gr.getPluginName());
            gr = (GridRecord) gd.getMetadata(uri);
            grReftime = gr.getDataTime().getRefTime();

            dataStore = gd.getDataStore(gr);

            int nx = gr.getSpatialObject().getNx();
            int ny = gr.getSpatialObject().getNy();

            HRAP rfcHrap;
            Coordinate org = MapUtil.gridCoordinateToLatLon(
                    new Coordinate(0, 0), PixelOrientation.CENTER,
                    gr.getSpatialObject());

            rfcHrap = HRAP.getInstance();

            Coordinate ulRfcNationalScale = rfcHrap.latLonToGridCoordinate(org,
                    PixelOrientation.CENTER);

            rfcExtent = new Rectangle((int) ulRfcNationalScale.x,
                    (int) ulRfcNationalScale.y - ny, nx, ny);
            extentsMap.put(rfc, rfcExtent);

            dataRec = dataStore.retrieve(uri, "Data", Request.ALL);

            if (dataRec instanceof FloatDataRecord) {
                gridMap.put(rfc, ((FloatDataRecord) dataRec).getFloatData());
            }

        } catch (PluginException e1) {
            log.error("Error querying grids", e1);
            e1.printStackTrace();
            return false;
        } catch (Exception e) {
            log.error("Error creating rfc qpe grid", e);
            e.printStackTrace();
            return false;
        }
        return true;
    }

    void processGrids() {
        int wfoMinX = (int) wfoExtent.getMinX();
        int wfoMinY = (int) wfoExtent.getMinY();
        int wfoMaxX = (int) wfoExtent.getMaxX();
        int wfoMaxY = (int) wfoExtent.getMaxY();

        for (String rfci : gridMap.keySet()) {
            // process grids
            Rectangle extent = extentsMap.get(rfci);
            int rfcMinX = (int) extent.getMinX();
            int rfcMinY = (int) extent.getMinY();
            int rfcMaxX = (int) extent.getMaxX();
            int rfcMaxY = (int) extent.getMaxY();

            int height = rfcMaxY - rfcMinY;
            int width = rfcMaxX - rfcMinX;
            int k = 0;

            float[] data = gridMap.get(rfci);

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
                                mosaicQpeShort[k] = ((short) (data[(i * width)
                                        + j] * 100));
                            }
                            k++;
                        }
                        x++;
                    }
                    y--;
                }
            } catch (Exception e) {
                if (log.isDebugEnabled()) {
                    log.debug("Error in populating getData::mosaicQpeShort.");
                }
                e.printStackTrace();
            }
        }
        return;
    }

    public static void main(String[] args) {
        ArealQpeGenSrv q = new ArealQpeGenSrv();
        q.processArealQpe();

    }

}
