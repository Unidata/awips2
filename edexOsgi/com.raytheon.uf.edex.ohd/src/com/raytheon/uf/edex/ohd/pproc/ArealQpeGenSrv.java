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
import java.nio.file.Files;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TimeZone;

import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.mpe.constants.FilePermissionConstants;
import com.raytheon.uf.common.mpe.util.RFCSiteLookup;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.common.xmrg.XmrgFile.XmrgHeader;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.ohd.MainMethod;
import org.locationtech.jts.geom.Coordinate;

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
 * Jan 10, 2013   1448     bgonzale    Added app context check in processArealQpe().
 * Mar 28, 2014   2952     mpduff      Changed to use UFStatus for logging.
 * Apr 21, 2014   2060     njensen     Remove dependency on grid dataURI column
 * Apr 08, 2016   5344     bkowal      Fix hdf5 dataset lookup path determination.
 * Aug 23, 2016   5631     bkowal      Utilize {@link XmrgHeader} defaults.
 * Nov 02, 2016 DR13977    K.Steinfeld Add a check for the existence of Grib D2D Ingest
 *                                     director, and try to create if if it does not exist
 * Aug 07, 2017   6334     bkowal      Directories are now created with 770 permissions and files 660.
 * Nov 18, 2017   17911    wkwock      Move RFC_SITEMAP and RFCMAP to com.raytheon.uf.common.mpe.util.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class ArealQpeGenSrv {
    private static final IUFStatusHandler log = UFStatus
            .getHandler(ArealQpeGenSrv.class);

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

    String token_delimiter = ",";

    String[] rfcs = new String[13];

    int[] durs = new int[5];

    private Rectangle extent;

    private HRAP hrap;

    private HRAPSubGrid subGrid;

    private Rectangle wfoExtent;

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

    private static final String D2D_GRIB_INGEST_DIR = "arealQpeGenSrv";

    private static final String RFC_QPE_BASE_STRING = "RFCMOSAIC";

    private static final String GRIBIT = "gribit.LX";

    private final Map<String, Rectangle> extentsMap = new HashMap<>();

    private final Map<String, float[]> gridMap = new HashMap<>();

    private int dur;

    private final AppsDefaults appsDefaults = AppsDefaults.getInstance();

    private String gaq_xmrg_1hr_dir;

    private String gaq_xmrg_6hr_dir;

    private String gaq_xmrg_24hr_dir;

    private String gaq_temp_xmrg_dir;

    private String gaq_grib_dir;

    private String d2d_input_dir;

    private boolean qpe_out;

    private final SimpleDateFormat sdf = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    private final SimpleDateFormat fdf = new SimpleDateFormat("yyyyMMddHH");

    public Object processArealQpe() {
        if (!AppsDefaults.getInstance().setAppContext(this)) {
            return null;
        }

        // Check to see if we need to run
        String gen = appsDefaults.getToken("mpe_generate_areal_qpe");
        if ("OFF".equalsIgnoreCase(gen)) {
            return null;
        }

        Date dt = new Date();
        Calendar cc = Calendar.getInstance(TimeUtil.GMT_TIME_ZONE);
        cc.setTime(dt);
        log.info("Started process_qpe_mosaic at " + cc.getTime());

        String gribout = appsDefaults.getToken("mpe_d2d_display_grib");
        qpe_out = false;
        getOptions();
        setExtent();

        /* loop on QPE durations */
        for (int dd : durs) {
            if (log.isPriorityEnabled(Priority.DEBUG)) {
                log.debug("Processing for " + dd + " hr duration");
            }
            processSingleQpe(dd, hrap, subGrid);
        }
        if ("ON".equalsIgnoreCase(gribout) && qpe_out) {
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

        SimpleDateFormat dateFormat = new SimpleDateFormat("ddHHmmss");
        Calendar calendar = Calendar.getInstance(TimeUtil.GMT_TIME_ZONE);
        calendar.setTime(new Date());
        String dString = dateFormat.format(calendar.getTime());

        boolean isGribIngestDirAvailable = isGribIngestDirAvailable();

        File f = new File(gaq_temp_xmrg_dir);
        int fl = f.listFiles().length;
        File[] fa = new File[fl];
        fa = f.listFiles();
        if (fa.length > 0) {
            for (File ff : fa) {
                log.info(ff.getAbsolutePath());
            }
            // gribit environment vars
            Map<String, String> envVars = new HashMap<>();

            if (AppsDefaults.getInstance().setAppContext(this)) {
                envVars.put("grib_in_dir", gaq_temp_xmrg_dir);
                envVars.put("grib_out_dir", gaq_temp_xmrg_dir);
                envVars.put("grib_set_subcenter_0", "on");

                String pproc_bin = appsDefaults.getToken("pproc_bin");
                MainMethod m = new MainMethod(new ProcessBuilder(
                        pproc_bin + File.separator + GRIBIT));

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
                        log.error(
                                "Areal QPE grib file out process terminated with exit code: "
                                        + exitValue);
                        return;
                    }

                    try {
                        RandomAccessFile raf = new RandomAccessFile(
                                new File(fr.getParentFile(), gribOutFile),
                                "rw");
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
                    File mvFile = new File(
                            gaq_grib_dir + File.separator + newFname);
                    File goFile = new File(
                            gaq_temp_xmrg_dir + File.separator + gribOutFile);

                    log.info("Move and rename grib file " + goFile + " to "
                            + mvFile);
                    boolean fmv = goFile.renameTo(mvFile);

                    // The move failed. Remove the grib file.
                    if (fmv == false) {
                        log.error("Move and rename grib file " + goFile + " to "
                                + mvFile + " failed. ");
                        goFile.delete();
                    } else {
                        try {
                            // copy Grib files to D2D ingest directory
                            if (isGribIngestDirAvailable) {
                                File gribIngestFile = new File(d2d_input_dir
                                        + File.separator + D2D_GRIB_INGEST_DIR
                                        + File.separator + fr.getName() + "_"
                                        + dString + ".grib");

                                Files.copy(mvFile.toPath(),
                                        gribIngestFile.toPath());

                                log.info("Copied grib file " + mvFile.getName()
                                        + " to " + d2d_input_dir
                                        + File.separator + D2D_GRIB_INGEST_DIR
                                        + " for ingest to D2D. ");
                            } else {
                                log.error("Unable to copy " + mvFile.getName()
                                        + " to " + d2d_input_dir
                                        + File.separator + D2D_GRIB_INGEST_DIR
                                        + "; directory does not exist. ");
                            }
                        } catch (IOException e) {
                            log.error("Copy grib file " + mvFile.getName()
                                    + " to " + d2d_input_dir + File.separator
                                    + D2D_GRIB_INGEST_DIR
                                    + " failed with exception. ", e);
                        } catch (Exception e) {
                            log.error("Copy grib file " + mvFile.getName()
                                    + " to " + d2d_input_dir + File.separator
                                    + D2D_GRIB_INGEST_DIR
                                    + " failed with exception. ", e);
                        }

                    }
                    // Remove the xmrg file from the temp directory.
                    fr.delete();
                    if (log.isPriorityEnabled(Priority.DEBUG)) {
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
     * Check that the full D2D Grib ingest directory exists. If the last
     * directory in the path does not exist, it is ok. Just try to create it.
     * 
     * @return true if directory exists or has been successfully created
     */
    private boolean isGribIngestDirAvailable() {

        File fl = new File(d2d_input_dir);

        if (!fl.exists()) {
            // main d2d_input_dir does not exist; can't proceed
            log.error(fl.getAbsolutePath() + " directory does not exist.");
            return false;
        }

        fl = new File(d2d_input_dir + File.separator + D2D_GRIB_INGEST_DIR);

        if (!fl.exists()) {

            try {
                // last part of the dir is missing; try to create it
                if (!fl.mkdir()) {
                    log.error("Unable to create D2D ingest directory "
                            + fl.getAbsolutePath() + " for Grib files.");
                    return false;
                }
            } catch (SecurityException ex) {
                log.error("Unable to create D2D ingest directory "
                        + fl.getAbsolutePath()
                        + " for Grib files due to SecurityManager " + ex);
                return false;
            }
        }
        return true;
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

        XmrgFile xmrg = new XmrgFile(xmrg_saved_filename);
        xmrg.setHrapExtent(wfoExtent);
        xmrg.setHeader(xmhead);
        xmrg.setData(mosaicQpeShort);

        try {
            log.info("Writing RFC QPE file:  " + xmrg_saved_filename);
            xmrg.save(new File(xmrg_saved_filename),
                    FilePermissionConstants.POSIX_FILE_SET);
            xmhead = null;
        } catch (IOException e) {
            log.error("Error writing RFC QPE file", e);
            xmhead = null;
            return;
        }

        // Save TEMP QPE file
        // Generate Header
        xmhead = new XmrgHeader();
        xmhead.setValidDate(cal.getTime());
        xmhead.setSaveDate(cal.getTime());

        xmrg = new XmrgFile(xmrg_saved_filename);
        xmrg.setHrapExtent(wfoExtent);
        xmrg.setHeader(xmhead);
        xmrg.setData(mosaicQpeShort);

        try {
            log.info("Writing Temp QPE file:  " + xmrg_temp_filename);
            xmrg.save(xmrg_temp_filename,
                    FilePermissionConstants.POSIX_FILE_SET);
            xmhead = null;
        } catch (IOException e) {
            log.error("Error writing Temp QPE file", e);
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
            String rfc = RFCSiteLookup.rfcSiteLookup(rf);

            try {
                wfoExtent = HRAPCoordinates.getHRAPCoordinates();
            } catch (Exception e2) {
                log.error("Error setting up the wfo extent", e2);
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
                durString = Integer.toString(dur);
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
            if (log.isPriorityEnabled(Priority.DEBUG)) {
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
            return;
        }
    }

    /**
     * Get the grid record
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
    private GridRecord getGridRecord(String rfc, String duration, String today)
            throws DataAccessLayerException {
        GridRecord rec = null;

        // Query for uri
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addQueryParam(GridConstants.DATASET_ID, "QPE-" + rfc);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION,
                "QPE" + duration + "%", "like");
        query.addQueryParam("dataTime.validPeriod.end", today);

        CoreDao dao = null;
        dao = new CoreDao(DaoConfig.forDatabase("metadata"));
        List<?> rs = dao.queryByCriteria(query);
        if ((rs != null) && (!rs.isEmpty())) {
            Object result = rs.get(0);
            if (result != null && result instanceof GridRecord) {
                rec = ((GridRecord) result);
            }
        }
        return rec;
    }

    private boolean getData(String rfc) {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        Date dt = new Date();
        cal.setTime(dt);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        String today = sdf.format(cal.getTime());
        IDataRecord dataRec;
        IDataStore dataStore = null;

        try {
            GridRecord gr = getGridRecord(rfc, durString, today);
            if (gr == null) {
                return false;
            }

            PluginDao gd = null;
            gd = PluginFactory.getInstance().getPluginDao(gr.getPluginName());
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

            final String dataPath = DataURIUtil.createDataURI(gr);
            if (dataPath == null) {
                log.error("Failed to construct a Data URI for grid record: "
                        + gr.toString() + ".");
                return false;
            }
            dataRec = dataStore.retrieve(dataPath, "Data", Request.ALL);

            if (dataRec instanceof FloatDataRecord) {
                gridMap.put(rfc, ((FloatDataRecord) dataRec).getFloatData());
            }
        } catch (PluginException e1) {
            log.error("Error querying grids", e1);
            return false;
        } catch (Exception e) {
            log.error("Error creating rfc qpe grid", e);
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
                log.error("Error in populating getData::mosaicQpeShort.", e);
            }
        }
        return;
    }

    public static void main(String[] args) {
        ArealQpeGenSrv q = new ArealQpeGenSrv();
        q.processArealQpe();

    }

}
