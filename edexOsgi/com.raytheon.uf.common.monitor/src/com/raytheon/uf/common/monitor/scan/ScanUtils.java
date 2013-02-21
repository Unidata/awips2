package com.raytheon.uf.common.monitor.scan;

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

import java.awt.geom.Point2D;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;
import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.dataplugin.bufrua.UAObsAdapter;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.DHRValues;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;
import com.vividsolutions.jts.io.WKTWriter;

/**
 * 
 * SCAN Utilities, mostly static methods.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/11/2009   1981       dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ScanUtils {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScanUtils.class);

    private static String MAPS_DB = "maps";

    public static String MAXVIL = "maxvil";

    public static String HVYPR = "hvypr";

    public static String SVRWX = "svrwx";

    public static String POLH = "polh";

    public static String VIL10 = "vil>10";

    public static String VIL20 = "vil>20";

    public static double meterToNM = 0.000539956803;

    public static float NMI_TO_KM = 1.852f;

    public static double KM_TO_NMI = 0.539956803;

    public static float KM_TO_KFT = 3.28084f;

    public static double M_PER_SEC_TO_KTS = 1.94384449;

    public static double KTS_TO_M_PER_SEC = 0.514444445;

    public static int MIN_TO_SEC = 60;

    public static double R_EARTH = 6371.009;

    public static int SCAN_STI_HALFWORD_INDEX = 3;

    public static int SCAN_GRID_DIM = 116; // square SCAN 4km grid

    public static int SCAN_GRID_DIM_RESOLUTION = 4000; // 4km resolution in
                                                       // meters

    public static int SCAN_GRID_1K_DIM_RESOLUTION = 1000; // 1km resolution in
                                                          // meters

    public static int SCAN_GRID_HALFK_DIM_RESOLUTION = 500; // 1/2km resolution
                                                            // in meters

    public static int SCAN_GRID_DIM_HALFKM = 928; // square SCAN 1km grid

    public static int SCAN_GRID_DIM_1KM = 464; // square SCAN 1km grid

    public static int SCAN_GRID_DIM_2KM = 232; // square SCAN 2km grid

    public static int SCAN_GRID_DIM_SQ = SCAN_GRID_DIM * SCAN_GRID_DIM; // 1D

    // array
    // length 4KM array

    public static int SCAN_GRID_DIM_1KM_SQ = SCAN_GRID_DIM_1KM
            * SCAN_GRID_DIM_1KM; // 1D array length 1KM array

    public static int SCAN_GRID_DIM_HALFKM_SQ = SCAN_GRID_DIM_HALFKM
            * SCAN_GRID_DIM_HALFKM; // 1D array length 1/2KM array

    public static int SCAN_GRID_DIM_2KM_SQ = SCAN_GRID_DIM_2KM
            * SCAN_GRID_DIM_2KM; // 1D array length 2KM array

    public static float SCAN_GRID_SIZE = 4.0f; // km.

    public static float SCAN_GRID_SIZE_1KM = 1.0f; // km.

    public static int A_LEN = SCAN_GRID_DIM_SQ; // old definition still used by

    // QPF

    public static int STORM_MODE = 2; // Precip mode for RADAR

    public static int CLEAR_AIR_MODE = 1; // Clear Air mode for RADAR

    public static int TEST_MODE = 0; // TEST MODE for radar

    public static int IRDRWE4KM = 58;

    public static int IRDRNS4KM = 58;

    public static int IRDRWE1KM = 232;

    public static int IRDRNS1KM = 232;

    public static int HRAP_GRID_DIM = 131; // square HRAP (DPA) grid.

    public static int MAX_RADIALS = 400; // Maximum number of radials handled in

    // a radar radial product.

    public static int MAX_RANGEBINS = 230; // Maximum radar bins (range).

    public static int MAX_RANGEBINS_8BIT = 460; // Max radar bins for 8-bit

    // prods (range).

    public static int MAX_POLGRID = MAX_RADIALS * MAX_RANGEBINS; // Max number

    public static GeometryFactory factory = new GeometryFactory();

    // grid points

    // for 4-bit polar grid
    public static int MAX_POLGRID_8BIT = MAX_RADIALS * MAX_RANGEBINS_8BIT; // Max

    // number

    // grid points for 8-bit polar grid
    public static float KM_PER_NM = 1.852f; // km per nautical mile.

    public static int CO_LENGTH = 20; // length of the county name char string,

    // also

    // used with the alphanumeric zones & counties.
    public static int SCTI_BIN = 600; // the number of seconds for the time bins

    // to

    // calculate the max SCTI values for locations
    // with multiple radars.
    public static int SCTI_LOW_THRESH = 10; // low threshold for SCTI button

    // color.

    public static int SCTI_MID_THRESH = 30; // middle threshold for SCTI button

    // color.

    public static int SCTI_UPP_THRESH = 80; // upper threshold for SCTI button

    // color.

    /** Radius of WFO in nm for comparison in SCAN **/
    public static double RADIUS_OF_WFO = 120;

    public static int RADIUS_OF_30NM = 30;

    public static int RADIUS_OF_10NM = 10;

    public static float LOWPCT = 10.0f; // Low-range percentage for use in areal

    // summary message.

    public static float MIDPCT = 25.0f; // Mid-range percentage "" "".

    public static float HIGHPCT = 55.0f; // High-range percentage "" "" .

    public static int CUSHION = 60; // The number of seconds after the due time

    // of the next

    // cycle. Ex: If we are in clear air mode, the _vsLength should be 10 min.
    // If data traveled instantaneously, we would get the next CZ product
    // exactly
    // 600 sec after the previous one, but, due to various lagging times it can
    // be
    // received a number of seconds later. So, in order to not switch to ltg
    // only
    // prematurely, we extend the wait time this number of seconds, which has
    // been
    // artbitrarily chosen.

    public static int ET_MSG_CODE = 41; // Message code for Echo Tops product

    public static int EET_MSG_CODE = 135; // Message code for Enhanced Echo Tops

    // product

    public static int STI_MSG_CODE = 58; // Message code for STI radar product

    public static int MESO_MSG_CODE = 141; // Message code for MD radar product

    public static int TVS_MSG_CODE = 61; // Message code for TVS radar product

    // res).

    public static int POW2_16 = 65536; // Used in key munging.

    public static int CZ_DEPICT_TEMPLATE = 50404; // 1 km Compositive

    // Reflectivity template
    // depict key

    public static final double DEG_TO_RAD = 180.0 / Math.PI; // old C code used
                                                             // these

    public static final double RAD_TO_DEG = Math.PI / 180.0; // old C code uses
                                                             // this.

    public static final double COSELE = Math.cos(0.5 * DEG_TO_RAD);

    public static double LIGHTNING_STORM_RANGE = 10.0;

    /** MM to inch **/
    public static double MM_TO_INCH = 0.03937;

    public static double MAX_DHR_DBZ = 94.5;

    public static double MIN_DHR_DBZ = -32.0;

    public static double DHR_DBZ_STEP = 0.5;

    public static String TORNADO_WARNING_PHENSIG = "TO.W";

    public static String SEVERE_THUNDERSTORM_PHENSIG = "SV.W";

    private static Map<String, String> tableStdResLevels = new HashMap<String, String>();

    // private static String standardResolutionLevel = null;

    private static Map<String, String> tableHighResLevels = new HashMap<String, String>();

    // private static String highResolutionLevel = null;

    private static String prevTable = "";

    /**
     * Populate the Lightning record
     * 
     * @param uri
     * @return
     */
    public static BinLightningRecord getLightningRecord(String uri)
            throws PluginException {
        BinLightningRecord lightRec = null;
        try {
            lightRec = new BinLightningRecord(uri);
            PluginDao ld = (PluginDao) PluginFactory.getInstance()
                    .getPluginDao(lightRec.getPluginName());
            lightRec = (BinLightningRecord) ld.getMetadata(uri);
            IDataStore dataStore = ld.getDataStore(lightRec);
            lightRec.retrieveFromDataStore(dataStore);
        } catch (Exception e) {
            e.printStackTrace();
        }

        return lightRec;
    }

    /**
     * Populate the sounding record for the listener strategy
     * 
     * @param uri
     * @return
     */
    public static VerticalSounding[] getSoundingRecord(String uri) {
        VerticalSounding[] soundingRec = null;
        UAObs upperAirObs = null;
        UAObsAdapter obsAdapt = null;
        try {
            upperAirObs = new UAObs(uri);
            obsAdapt = new UAObsAdapter();
            VerticalSounding s = obsAdapt.createVerticalSounding(upperAirObs);
            if (s != null) {
                soundingRec = new VerticalSounding[] { s };
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return soundingRec;
    }

    /**
     * Translated to Java D Hladky 16Feb2009
     * 
     * @param n_radials
     * @param iazm_array
     * @param i_ref
     * @return
     */
    public static short[] mapRadialsToGrid(int n_radials, short[] iazm_array,
            short[] i_ref) {

        float range = 0.0f;
        float grange = 0.0f;
        float azm = 0.0f;
        float cosazm = 0.0f;
        float sinazm = 0.0f;
        float rwedisp = 0.0f;
        float rnsdisp = 0.0f;
        int iwebox = 0;
        int insbox = 0;
        int nbin = 0;
        short jrefl;
        short[] ireflGrid = new short[SCAN_GRID_DIM_SQ];

        //
        // Initialize reflectivty grid values to 0
        //
        for (int aa = 0; aa < SCAN_GRID_DIM_SQ; aa++) {
            ireflGrid[aa] = 0;
        }
        //
        // Get one radial of reflectivity data at a time.
        //
        for (int nrad = 0; nrad < n_radials; nrad++) {
            // Convert azimuths from tenth-of-degrees integers to float degrees.
            azm = (float) (iazm_array[nrad] / 10.0);
            if (azm < 0)
                azm += 360.0;
            cosazm = (float) Math.cos((azm - 90.) * DEG_TO_RAD);
            sinazm = (float) Math.sin((azm - 90.) * DEG_TO_RAD);

            // Map this radial's data to grid.
            // Follow apparent OSF convention - distance of radar volume
            // from radar is at far boundary of range bin, minus 0.5 km.

            for (nbin = 0; nbin < MAX_RANGEBINS; nbin++) {

                // In the FORTRAN code, range is nbin -0.5; however, since our
                // bin counter (in C) starts at 0 (as opposed to 1 in FORTRAN),
                // we add +0.5 instead of subtracting.
                range = (float) (nbin + 0.5);
                grange = (float) (range * COSELE);
                rwedisp = grange * cosazm;
                rnsdisp = grange * sinazm;

                if (rwedisp >= 0)
                    iwebox = IRDRWE4KM + (int) (rwedisp / SCAN_GRID_SIZE);
                else
                    iwebox = IRDRWE4KM + (int) (rwedisp / SCAN_GRID_SIZE) - 1;

                if (rnsdisp >= 0)
                    insbox = IRDRNS4KM + (int) (rnsdisp / SCAN_GRID_SIZE);
                else
                    insbox = IRDRNS4KM + (int) (rnsdisp / SCAN_GRID_SIZE) - 1;

                jrefl = i_ref[(MAX_RANGEBINS * nrad) + nbin];
                if (jrefl > ireflGrid[(SCAN_GRID_DIM * insbox) + iwebox]) {
                    ireflGrid[(SCAN_GRID_DIM * insbox) + iwebox] = jrefl;
                }
            }
        }

        return ireflGrid;
    }

    /**
     * 
     * makeReflGrid1km Greg Stumpf, CIMMS/MDL January 2005 Modelled after
     * makeReflGrid (for 4 km grids) For this version, each Cartesian grid cell
     * is used to compute nearest radial grid cell, and and the assigment is
     * made. In the previous version, it was done opposite, which left large
     * portions of the cartesian grid empty especially for 1 km grids.
     * 
     * Translated to Java D Hladky 16Feb2009
     * 
     * @param n_radials
     * @param iazm_array
     * @param i_ref
     * @return
     */
    public static short[] mapRadialsToGrid1km(int n_radials, int n_bins,
            short[] iazm_array, short[] i_ref) {

        float range = 0.0f;
        float azm = 0.0f;
        float rwedisp = 0.0f;
        float rnsdisp = 0.0f;
        int iwebox = 0;
        int insbox = 0;
        int nbin = 0;
        int nrad = 0;
        short[] ireflGrid = new short[SCAN_GRID_DIM_1KM_SQ];

        // Loop for all cartesian grid points

        for (int aa = 0; aa < SCAN_GRID_DIM_1KM_SQ; aa++) {
            ireflGrid[aa] = 0; // Initialize reflectivty grid values to 0

            // Compute nearest radial grid point

            iwebox = aa % (SCAN_GRID_DIM_1KM);
            insbox = SCAN_GRID_DIM_1KM - (aa / (SCAN_GRID_DIM_1KM));

            rwedisp = SCAN_GRID_SIZE_1KM * (iwebox - IRDRWE1KM);
            rnsdisp = SCAN_GRID_SIZE_1KM * (insbox - IRDRNS1KM);

            range = (float) Math.sqrt(rwedisp * rwedisp + rnsdisp * rnsdisp);
            nbin = (int) (range - 0.5);

            if (rwedisp >= 0)
                azm = (float) (Math.atan2(rwedisp, rnsdisp) * RAD_TO_DEG);
            else
                azm = (float) ((Math.atan2(rwedisp, rnsdisp) * RAD_TO_DEG) + 360);

            azm = (float) (azm + 0.5);
            if (azm > 360.0)
                azm = (float) (azm - 360.0);
            if (azm < 0.0)
                azm = (float) (azm + 360.0);

            for (nrad = 0; nrad < n_radials; nrad++) {
                if (iazm_array[nrad] == (int) azm * 10.0)
                    break;
            }

            // Assign radial value to cartesian grid value
            ireflGrid[aa] = i_ref[(n_bins * nrad) + nbin];

            // logEvent << "aa,web,nsb,wed,nsd,range,nbin,azm,nrad,val=\n  "
            // << aa << "," << iwebox << "," << insbox << "," << rwedisp << ","
            // << rnsdisp << "," << range << "," << nbin << "," << azm << ","
            // << nrad << "," << ireflGrid[aa] << std::endl;

        }

        return ireflGrid;
    }

    /**
     * 
     * makeReflGrid4km Greg Stumpf, CIMMS/MDL January 2005 Modelled after
     * makeReflGrid (for 4 km grids) For this version, each Cartesian grid cell
     * is used to compute nearest radial grid cell, and and the assigment is
     * made. In the previous version, it was done opposite, which left large
     * portions of the cartesian grid empty especially for 1 km grids.
     * 
     * @param n_radials
     * @param iazm_array
     * @param i_ref
     * @return
     */
    public static short[] mapRadialsToGrid4km(int n_radials, int n_bins,
            short[] iazm_array, short[] i_ref) {

        float range = 0.0f;
        float azm = 0.0f;
        float rwedisp = 0.0f;
        float rnsdisp = 0.0f;
        int iwebox = 0;
        int insbox = 0;
        int nbin = 0;
        int nrad = 0;
        short[] ireflGrid = new short[SCAN_GRID_DIM_SQ];

        // Loop for all cartesian grid points

        for (int aa = 0; aa < SCAN_GRID_DIM_SQ; aa++) {

            ireflGrid[aa] = 0; // Initialize reflectivty grid values to 0

            // Compute nearest radial grid point

            iwebox = aa % (SCAN_GRID_DIM);
            insbox = SCAN_GRID_DIM - ((aa / (SCAN_GRID_DIM)));

            rwedisp = SCAN_GRID_SIZE * (iwebox - IRDRWE4KM);
            rnsdisp = SCAN_GRID_SIZE * (insbox - IRDRNS4KM);

            range = (float) (Math.sqrt(rwedisp * rwedisp + rnsdisp * rnsdisp) / COSELE);
            nbin = (int) (range - 0.5);

            if (rwedisp >= 0)
                azm = (float) (Math.atan2(rwedisp, rnsdisp) * RAD_TO_DEG);
            else
                azm = (float) ((Math.atan2(rwedisp, rnsdisp) * RAD_TO_DEG) + 360);

            azm = (float) (azm + 0.5);
            if (azm > 360.)
                azm = (float) (azm - 360.0);
            if (azm < 0.)
                azm = (float) (azm + 360.0);

            for (nrad = 0; nrad < n_radials; nrad++) {
                if (iazm_array[nrad] == (int) azm * 10.0)
                    break;
            }

            // Assign radial value to cartesian grid value
            ireflGrid[aa] = i_ref[(n_bins * nrad) + nbin];

            // logEvent << "aa,web,nsb,wed,nsd,range,nbin,azm,nrad,val=\n  "
            // << aa << "," << iwebox << "," << insbox << "," << rwedisp << ","
            // << rnsdisp << "," << range << "," << nbin << "," << azm << ","
            // << nrad << "," << ireflGrid[aa] << std::endl;

        }

        return ireflGrid;
    }

    /**
     * Used to re size raster reflectivity grids for comparison in SCAN
     * 
     * @param inputGrid
     * @param outputGridSize
     * @param nx
     * @param ny
     * @return
     */
    public static short[] reSizeGrid(short[] inputGrid, int outputGridSize,
            int nx, int ny) {

        short[] outputGrid = new short[outputGridSize];
        // 4KM resolution
        if ((outputGridSize == SCAN_GRID_DIM_SQ)
                && (inputGrid.length == SCAN_GRID_DIM_1KM_SQ)) {
            // decimate
            if ((nx == SCAN_GRID_DIM_1KM) && (ny == SCAN_GRID_DIM_1KM)) {

                for (int i = 0; i < SCAN_GRID_DIM; i++) {
                    for (int j = 0; j < SCAN_GRID_DIM; j++) {
                        short avValue = 0;
                        for (int x = 0; x < 4; x++) {
                            for (int y = 0; y < 4; y++) {
                                // average the grid values
                                avValue += inputGrid[(SCAN_GRID_DIM_1KM * (i * 4 + x))
                                        + (j * 4 + y)];
                            }
                        }
                        outputGrid[(SCAN_GRID_DIM * i) + j] = (short) (avValue / 16);
                    }
                }
            }
        }
        // 2KM resolution
        else if ((outputGrid.length == SCAN_GRID_DIM_SQ)
                && (inputGrid.length == SCAN_GRID_DIM_2KM_SQ)) {
            // decimate
            if ((nx == SCAN_GRID_DIM_2KM) && (ny == SCAN_GRID_DIM_2KM)) {

                for (int i = 0; i < SCAN_GRID_DIM; i++) {
                    for (int j = 0; j < SCAN_GRID_DIM; j++) {
                        short avValue = 0;
                        for (int x = 0; x < 2; x++) {
                            for (int y = 0; y < 2; y++) {
                                // average the grid values
                                avValue += inputGrid[(SCAN_GRID_DIM_2KM * (i * 2 + x))
                                        + (j * 2 + y)];
                            }
                        }
                        outputGrid[(SCAN_GRID_DIM * i) + j] = (short) (avValue / 4);
                    }
                }
            }
        }

        return outputGrid;

    }

    public static short[] reSizeGridMax(short[] inputGrid, int outputGridSize,
            int nx, int ny) {

        short[] outputGrid = new short[outputGridSize];
        // 4KM resolution
        if ((outputGridSize == SCAN_GRID_DIM_SQ)
                && (inputGrid.length == SCAN_GRID_DIM_1KM_SQ)) {
            // decimate
            if ((nx == SCAN_GRID_DIM_1KM) && (ny == SCAN_GRID_DIM_1KM)) {

                for (int i = 0; i < SCAN_GRID_DIM; i++) {
                    for (int j = 0; j < SCAN_GRID_DIM; j++) {
                        short mxValue = 0;
                        for (int x = 0; x < 4; x++) {
                            for (int y = 0; y < 4; y++) {
                                // average the grid values
                                mxValue = (short) Math
                                        .max(inputGrid[(SCAN_GRID_DIM_1KM * (i * 4 + x))
                                                + (j * 4 + y)], mxValue);
                            }
                        }
                        outputGrid[(SCAN_GRID_DIM * i) + j] = mxValue;
                    }
                }
            }
        }
        // 2KM resolution
        else if ((outputGrid.length == SCAN_GRID_DIM_SQ)
                && (inputGrid.length == SCAN_GRID_DIM_2KM_SQ)) {
            // decimate
            if ((nx == SCAN_GRID_DIM_2KM) && (ny == SCAN_GRID_DIM_2KM)) {

                for (int i = 0; i < SCAN_GRID_DIM; i++) {
                    for (int j = 0; j < SCAN_GRID_DIM; j++) {
                        short mxValue = 0;
                        for (int x = 0; x < 2; x++) {
                            for (int y = 0; y < 2; y++) {
                                // average the grid values
                                mxValue = (short) Math
                                        .max(inputGrid[(SCAN_GRID_DIM_2KM * (i * 2 + x))
                                                + (j * 2 + y)], mxValue);
                            }
                        }
                        outputGrid[(SCAN_GRID_DIM * i) + j] = mxValue;
                    }
                }
            }
        }

        return outputGrid;

    }

    /**
     * Converts the raw bytes array into DBZ short arrays
     * 
     * @param inputArray
     * @param radRec
     * @return
     */
    public static short[] convertToGrid(RadarRecord radRec, int gridSize) {
        return convertToGrid(radRec, gridSize, false);
    }

    /**
     * Converts the raw bytes array into DBZ short arrays
     * 
     * @param inputArray
     * @param radRec
     * @return
     */
    public static short[] convertToGrid(RadarRecord radRec, int gridSize,
            boolean max) {

        short[] grid = null;

        if (radRec.getFormat().equals("Radial")) {

            short[] iazm_array = new short[radRec.getNumRadials()];
            short[] irad_array = new short[radRec.getNumBins()
                    * radRec.getNumRadials()];

            for (int irad = 0; irad < radRec.getNumRadials(); irad++) {

                iazm_array[irad] = (short) radRec.getAngleData()[irad];

                for (int ibin = 0; ibin < radRec.getNumBins(); ibin++) {
                    irad_array[(radRec.getNumBins() * irad) + ibin] = findValue(
                            radRec, irad, ibin);
                }
            }
            // Map to a Cartesian grid
            grid = ScanUtils.getCartesianGrid(gridSize, radRec.getNumRadials(),
                    radRec.getNumBins(), iazm_array, irad_array);
        }

        else if (radRec.getFormat().equals("Raster")) {

            short[] ogrid = new short[radRec.getNumBins()
                    * radRec.getNumRadials()];

            for (int irow = 0; irow < radRec.getNumRadials(); irow++) {
                for (int icol = 0; icol < radRec.getNumBins(); icol++) {
                    ogrid[(radRec.getNumBins() * irow) + icol] = findValue(
                            radRec, irow, icol);
                }
            }

            if ((radRec.getNumBins() * radRec.getNumRadials()) != gridSize) {
                if (max)
                    grid = ScanUtils.reSizeGridMax(ogrid, gridSize,
                            radRec.getNumRadials(), radRec.getNumBins());
                else
                    grid = ScanUtils.reSizeGrid(ogrid, gridSize,
                            radRec.getNumRadials(), radRec.getNumBins());
            } else {
                grid = ogrid;
            }
        }

        else if (radRec.getFormat().equals("Graphic")) {

            short[] ogrid = new short[radRec.getNumBins()
                    * radRec.getNumRadials()];

            for (int irow = 0; irow < radRec.getNumRadials(); irow++) {
                for (int icol = 0; icol < radRec.getNumBins(); icol++) {
                    ogrid[(radRec.getNumBins() * irow) + icol] = radRec
                            .getRawDataValue(irow, icol);
                    ;
                }
            }
            if ((radRec.getNumBins() * radRec.getNumRadials()) != gridSize) {
                if (max)
                    grid = ScanUtils.reSizeGridMax(ogrid, gridSize,
                            radRec.getNumRadials(), radRec.getNumBins());
                else
                    grid = ScanUtils.reSizeGrid(ogrid, gridSize,
                            radRec.getNumRadials(), radRec.getNumBins());
            } else {
                grid = ogrid;
            }
        }

        return grid;
    }

    /**
     * Find the direct DBz conversion, gets you the threshold data falls into.
     * 
     * @param radRec
     * @param irad
     * @param ibin
     * @return
     */
    public static short findValue(RadarRecord radRec, int i, int j) {
        short dataValue = radRec.getRawDataValue(i, j);
        short value = 0;

        if (radRec.getNumLevels() <= 16) {
            Object thresh1 = radRec.getDecodedThreshold(dataValue);
            if (thresh1 instanceof Float) {
                value = ((Float) thresh1).shortValue();
            }
        }
        // has 256 levels
        else {
            value = dataValue;
        }

        return value;
    }

    /**
     * Find the direct ET value
     * 
     * @param radRec
     * @param dataValue
     * @return
     */
    public static float decodeETValue(RadarRecord radRec, int dataValue) {

        float value = 0.0f;
        if (dataValue > 0) {
            if (dataValue > 15) {
                dataValue = 15;
            }
            Object th0 = radRec.getDecodedThreshold(dataValue);
            if (th0 instanceof Float) {
                value = (Float) th0;
            }
        }

        return value;
    }

    /**
     * Find the direct DPR value
     * 
     * @param dataValue
     * @return
     */
    public static float decodeDPRValue(int dataValue) {

        // DPR is shown in 1000th of an inch

        return dataValue / 1000;
    }

    /**
     * Find the direct ET value
     * 
     * @param radRec
     * @param dataValue
     * @return
     */
    public static float decodeVilValue(RadarRecord radRec, int dataValue) {

        float value = 0.0f;
        if (dataValue > 0) {
            if (dataValue > 15) {
                dataValue = 15;
            }
            Object th0 = radRec.getDecodedThreshold(dataValue);
            if ((th0 != null) && (th0 instanceof Number)) {
                value = ((Number) th0).floatValue();
            }
        } else {
            value = dataValue;
        }

        return value;
    }

    /**
     * Finds the Double value of the radar data
     * 
     * @param orig_val
     * @return
     */
    public static double dvlcoeff(short orig_val) {
        int hex1, hex2, hex3, hex4, dig1, dig2, dig3, dig4;
        int bin1, bin2, bin3, bin4, cS, cE, cF, dS, dE, dF;
        long comp_val;
        double coeff;

        if (orig_val >= 0) {
            comp_val = orig_val;
        } else {
            comp_val = 65536 + orig_val;
        }

        hex1 = (int) (comp_val / 16);
        hex2 = (hex1 / 16);
        hex3 = (hex2 / 16);
        hex4 = (hex3 / 16);
        dig1 = hex3 - (16 * hex4); // combine with above
        dig2 = hex2 - (16 * hex3); // combine with above
        dig3 = hex1 - (16 * hex2); // combine with above
        dig4 = (int) comp_val - (16 * hex1); // combine with above
        bin1 = hex2bin(dig1);
        bin2 = hex2bin(dig2);
        bin3 = hex2bin(dig3);
        bin4 = hex2bin(dig4);
        cS = (bin1 / 1000);
        cE = ((bin1 - (cS * 1000)) * 100) + ((bin2 / 100));
        cF = ((bin2 - ((bin2 / 100) * 100)) * 100000000) + (bin3 * 10000)
                + (bin4);
        dS = bin2dec(cS);
        dE = bin2dec(cE);
        dF = bin2dec(cF);
        if (dE == 0) {
            coeff = Math.pow(-1, dS) * 2 * (0 + (dF / Math.pow(2, 10)));
        } else {
            coeff = Math.pow(-1, dS) * Math.pow(2, (dE - 16))
                    * (1 + (dF / Math.pow(2, 10)));
        }
        if (orig_val < 0) {
            coeff = -coeff;
        }
        return coeff;
    }

    /**
     * Gets the hexidecimal 2 binary conversion.
     * 
     * @param hex
     * @return
     */
    public static int hex2bin(int hex) {
        int binary = 0;

        if (hex == 0) {
            binary = 0;
        } else if (hex == 1) {
            binary = 1;
        } else if (hex == 2) {
            binary = 10;
        } else if (hex == 3) {
            binary = 11;
        } else if (hex == 4) {
            binary = 100;
        } else if (hex == 5) {
            binary = 101;
        } else if (hex == 6) {
            binary = 110;
        } else if (hex == 7) {
            binary = 111;
        } else if (hex == 8) {
            binary = 1000;
        } else if (hex == 9) {
            binary = 1001;
        } else if (hex == 10) {
            binary = 1010;
        } else if (hex == 11) {
            binary = 1011;
        } else if (hex == 12) {
            binary = 1100;
        } else if (hex == 13) {
            binary = 1101;
        } else if (hex == 14) {
            binary = 1110;
        } else if (hex == 15) {
            binary = 1111;
        }
        return binary;
    }

    /**
     * Get the binary to decimal conversion
     * 
     * @param binary
     * @return
     */
    public static int bin2dec(int binary) {
        int decimal = 0;

        for (int i = 0; i < 10; i++) {
            int mod = binary % 10;
            if (mod == 0) {
                decimal += 1 << i;
            }
            binary /= 10;
        }
        return decimal;
    }

    /**
     * Takes radar radials and values and converts to SCAN grid.
     * 
     * @param record
     * @return
     */
    public static short[] getCartesianGrid(int outputGridSize, int num_radials,
            int num_bins, short[] iazm_array, short[] irad_array) {

        short[] outputGrid = new short[outputGridSize];
        if (outputGridSize == SCAN_GRID_DIM_1KM_SQ) {
            outputGrid = mapRadialsToGrid1km(num_radials, num_bins, iazm_array,
                    irad_array);
        } else if (outputGridSize == SCAN_GRID_DIM_SQ) {
            outputGrid = mapRadialsToGrid4km(num_radials, num_bins, iazm_array,
                    irad_array);
        }

        return outputGrid;
    }

    /**
     * Gets the Hail prob for a particular freezing level, Eastern algorithm
     * 
     * @param frzLevel
     * @return
     */
    public static double getHail10East(double frzLevel) {
        return 14.22 + 10.0 * (.3 - .0031 * (frzLevel / 1000));
    }

    /**
     * Gets the Hail prob for a particular freezing level, Western algorithm
     * 
     * @param frzLevel
     * @param thick1000_500
     * @return
     */
    public static double getHail10West(double frzLevel, double thick1000_500) {
        return -375.43 + 10.0 * (0.19 - .00619 * (frzLevel / 1000)) + 20.57
                + 0.066 * thick1000_500;
    }

    /**
     * 
     * Obtain the max VIL around the cell and count the number of grid points
     * with >10 and >20 kg/m2. First, check the bounds to make sure 'around'
     * does not include grid points outside the SCAN grid.
     * 
     * @param vilRec
     * @param storms
     * @return
     */
    public static HashMap<String, Float> getVILDependentValues(short[] vilGrid,
            GridGeometry2D stationGeometry, Double frzLevel,
            Double thick1000_500, Double spd700, Double u500,
            Double totalTotals, Coordinate coor) {

        float maxvil = 0.0f;
        float vilfac = 0.0f;
        float hvyPrProb = 0.0f;
        float polh = 0.0f;
        float isvg10 = 0.0f;
        float isvg20 = 0.0f;
        float svrWxProb = 0.0f;
        int lower = 3;
        int upper = SCAN_GRID_DIM - 4;
        int ii = 0;
        int jj = 0;

        maxvil = 0;
        ReferencedCoordinate rc = new ReferencedCoordinate(coor);
        Coordinate cellCorr = null;
        try {
            cellCorr = rc.asGridCell(stationGeometry, PixelInCell.CELL_CENTER);
        } catch (TransformException e) {
            e.printStackTrace();
        } catch (FactoryException e) {
            e.printStackTrace();
        }

        Coordinate gridPt = new Coordinate(cellCorr.x, cellCorr.y, 0);
        if (gridPt.x < lower)
            gridPt.x = lower;
        if (gridPt.x > upper)
            gridPt.x = upper;
        if (gridPt.y < lower)
            gridPt.y = lower;
        if (gridPt.y > upper)
            gridPt.y = upper;

        for (ii = (int) (gridPt.x - 1); ii <= (int) (gridPt.x + 1); ii++) {
            for (jj = (int) (gridPt.y - 1); jj <= (int) (gridPt.y + 1); jj++) {
                if ((ii < 0) || (jj < 0) || (ii >= SCAN_GRID_DIM)
                        || (jj >= SCAN_GRID_DIM)) {
                    continue;
                }
                if (vilGrid[(ii * SCAN_GRID_DIM) + jj] > maxvil) {
                    maxvil = vilGrid[(ii * SCAN_GRID_DIM) + jj];
                }
            }
        }

        for (ii = (int) gridPt.x - 3; ii <= (int) gridPt.x + 3; ii++) {
            for (jj = (int) gridPt.y - 3; jj <= (int) gridPt.y + 3; jj++) {
                if ((ii < 0) || (jj < 0) || (ii >= SCAN_GRID_DIM)
                        || (jj >= SCAN_GRID_DIM)) {
                    continue;
                }
                if (vilGrid[(ii * SCAN_GRID_DIM) + jj] > 20)
                    isvg20++;
                if (vilGrid[(ii * SCAN_GRID_DIM) + jj] > 10)
                    isvg10++;
            }
        }

        // System.out.println("MaxVil: " + maxvil);

        if ((thick1000_500 != null) && (frzLevel != null) && (u500 != null)) {

            if (coor.x > -85.0) {
                svrWxProb = (float) (-16.37 + (2.33 * isvg20) + (1.02 * spd700) + (0.646 * maxvil));

                if (maxvil == 10.0) {
                    polh = (float) (14.22 + maxvil
                            * ((0.03 * maxvil) - (0.0031 * frzLevel)));
                } else {
                    polh = (float) (maxvil * getHail10East(frzLevel) * 0.1);
                }
            } else {
                svrWxProb = (float) Math.abs((-16.49 + maxvil
                        * ((0.025 * maxvil) - (0.00206 * frzLevel))
                        + (0.365 * u500) + (0.341 * totalTotals)));

                if (maxvil >= 10.0) {
                    polh = (float) (-375.43 + maxvil
                            * ((0.019 * maxvil) - (0.00619 * frzLevel))
                            + (2.057 * maxvil) + (0.066 * thick1000_500));
                } else {
                    polh = (float) (maxvil
                            * getHail10West(frzLevel, thick1000_500) * 0.1);
                }
            }

            // System.out.println("svrWxProb: " + svrWxProb);
            // System.out.println("POLH: " + polh);

            //
            // Truncate probabilities to the range 0-100
            //
            if (polh < 0.0) {
                polh = 0.0f;
            } else if (polh > 100.0) {
                polh = 100.0f;
            }
            if (svrWxProb < 0.0) {
                svrWxProb = 0.0f;
            } else if (svrWxProb > 100.0) {
                svrWxProb = 100.0f;
            }

        } // end if envDataBad
          //
          // If environmental data is missing or bizarre, use VIL-Only
          // relationships.
          //
        else {
            if (maxvil >= 70) {
                polh = svrWxProb = 99.0f;
            } else if (maxvil >= 50) {
                polh = 22.0f;
                svrWxProb = 40.0f;
            } else if (maxvil >= 40) {
                polh = 10.0f;
                svrWxProb = 29.0f;
            } else if (maxvil >= 30) {
                polh = 4.0f;
                svrWxProb = 21.0f;
            } else if (maxvil >= 20) {
                polh = 0.0f;
                svrWxProb = 12.0f;
            } else if (maxvil >= 10) {
                polh = 0.0f;
                svrWxProb = 8.0f;
            } else if (maxvil >= 5) {
                polh = 0.0f;
                svrWxProb = 4.0f;
            } else {
                polh = 0.0f;
                svrWxProb = 8.0f;
            }
        }

        vilfac = maxvil - 4.0f;
        hvyPrProb = 3.0f + vilfac * 2.19f;
        if (hvyPrProb > 40) {
            hvyPrProb = 40;
        }
        if (hvyPrProb < 0) {
            hvyPrProb = 0;
        }

        // System.out.println("hvyprProb: " + hvyPrProb);

        HashMap<String, Float> vals = new HashMap<String, Float>();
        vals.put(MAXVIL, maxvil);
        vals.put(VIL10, isvg10);
        vals.put(VIL20, isvg20);
        vals.put(SVRWX, svrWxProb);
        vals.put(POLH, polh);
        vals.put(HVYPR, hvyPrProb);

        return vals;
    }

    /**
     * Construct a 2D GridGeometry representing the SCAN GRID
     * 
     * @return
     */
    public static GridGeometry2D getStationGeometry(Coordinate c, int res,
            int dim) {
        ProjectedCRS crs = getCRS(c);

        GridGeometry2D gridGeometry2D = null;

        GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
        generalEnvelope.setCoordinateReferenceSystem(crs);

        double maxExtent = (res * (dim / 2));

        generalEnvelope.setRange(0, -maxExtent, maxExtent);
        generalEnvelope.setRange(1, -maxExtent, maxExtent);

        gridGeometry2D = new GridGeometry2D(new GeneralGridEnvelope(new int[] {
                0, 0 }, new int[] { dim, dim }, false), generalEnvelope);

        return gridGeometry2D;
    }

    /**
     * Gets the maximum Scan radar extent
     * 
     * @return
     */
    public static double getMaxExtent() {
        return (SCAN_GRID_DIM_RESOLUTION * (SCAN_GRID_DIM / 2));
    }

    /**
     * Gets the projected CRS
     * 
     * @return
     */
    public static ProjectedCRS getCRS(Coordinate c) {
        return CRSCache.getInstance().constructStereographic(
                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, c.y,
                c.x);
    }

    /**
     * Increment for byte data offsets
     * 
     * @param topValue
     * @param total_increments
     * @return
     */
    public static float getIncrement(double topValue, int total_increments) {

        return (float) (topValue / total_increments);
    }

    /**
     * figures out your total based on value, inc, total number of incs
     * 
     * @param value
     * @param inc
     * @param tot_inc
     * @return
     */
    public static int getOffsetValue(int value) {
        int rvalue = 0;
        if (value < 0) {
            rvalue = Math.abs(-127 - value);
        } else if (value > 0) {
            rvalue = 127 + value;
        }
        return rvalue;
    }

    /**
     * Gets the distance of two points point
     * 
     * @param coor1
     * @param coor2
     * @return
     */
    public static double getDistance(Coordinate coor1, Coordinate coor2,
            GeodeticCalculator gc) {
        gc.setStartingGeographicPoint(coor1.x, coor1.y);
        gc.setDestinationGeographicPoint(coor2.x, coor2.y);

        double distance = gc.getOrthodromicDistance();

        return distance;
    }

    /***
     * Collect lightning strikes for this particular cellCoor radius
     * 
     * @param rec
     * @param cellCoor
     * @param radarSite
     * @param distance
     * @return
     * @throws Exception
     */
    public static LightningReport getStrikeList(
            ArrayList<BinLightningRecord> recs, Coordinate stormCoor,
            GridGeometry2D geometry, double distance, Date previousVolScanTime,
            Date currVolScanTime) {

        ArrayList<LightningStrike> strikeList = new ArrayList<LightningStrike>();
        GeodeticCalculator gc = new GeodeticCalculator(getCRS(stormCoor));

        for (BinLightningRecord rec : recs) {
            for (int i = 0; i < rec.getLatitudes().length; i++) {

                Coordinate strikeCoor = new Coordinate(rec.getLongitudes()[i],
                        rec.getLatitudes()[i], 0.0);

                if (distance >= getDistance(stormCoor, strikeCoor, gc)
                        * meterToNM) {

                    LightningStrike strike = new LightningStrike(
                            (double) rec.getLatitudes()[i],
                            (double) rec.getLongitudes()[i],
                            rec.getIntensities()[i], rec.getStrikeTypes()[i],
                            rec.getMsgTypes()[i], rec.getStrikeCounts()[i]);

                    strikeList.add(strike);
                }
            }
        }

        LightningReport lr = null;
        // use the hash we collected
        int totalPosStrikes = 0;
        int totalCGStrikes = 0;
        int totalStrikes = 0;

        if (strikeList.size() > 0) {

            for (LightningStrike ls : strikeList) {
                if (ls.getIntensity() > 0) {
                    totalPosStrikes++;
                }
                if (ls.getStrikeType() == LtgStrikeType.STRIKE_CG.ordinal()) {
                    totalCGStrikes++;
                }
                totalStrikes++;
            }
        }

        lr = new LightningReport();

        long start_time = previousVolScanTime.getTime();
        long stop_time = currVolScanTime.getTime();
        // only need int precision

        if (totalCGStrikes > 0) {
            lr.setCgRate((int) ((double) totalCGStrikes / ((double) (stop_time - start_time) / (1000.0 * 60.0))));
        } else {
            lr.setCgRate(0);
        }

        if (totalPosStrikes > 0) {
            lr.setPercentPos(((double) totalPosStrikes / (double) totalStrikes) * 100.0);
        } else {
            lr.setPercentPos(0.0);
        }

        return lr;
    }

    /**
     * Gets the text string of the polygon describing the radar areal coverage
     * 
     * @param Coordinate
     * @param maxExtent
     * @return
     * @throws IOException
     */
    public static String getRadarPolygonText(Coordinate coor, double maxExtent) {

        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(coor.x, coor.y);
        Coordinate[] coors = new Coordinate[361];

        for (int i = 0; i < 361; i++) {
            double az = 0.00;
            if (i <= 180) {
                az = i;
            } else {
                az = -180 - (180 - i);
            }

            gc.setDirection(az, maxExtent);
            Point2D dstPoint = gc.getDestinationGeographicPoint();
            coors[i] = new Coordinate(dstPoint.getX(), dstPoint.getY());
        }

        gc.setDirection(0.0, maxExtent);
        Point2D dstPoint = gc.getDestinationGeographicPoint();
        coors[6] = new Coordinate(dstPoint.getX(), dstPoint.getY());

        LinearRing lr = factory.createLinearRing(coors);
        Polygon pg = factory.createPolygon(lr, null);

        WKTWriter wktWriter = new WKTWriter();

        return wktWriter.writeFormatted(pg);
    }

    /**
     * Write geometry as text
     * 
     * @param coor
     * @return
     */
    public static String getPointAsText(Coordinate coor) {

        WKTWriter wktWriter = new WKTWriter();

        return wktWriter.writeFormatted(factory.createPoint(coor));
    }

    /**
     * Gets the sites under CWA umbrella
     * 
     * @param cwa
     * @return
     */
    public static ArrayList<ThreatLocation> getCWASites(Coordinate sitePoint,
            ProjectedCRS crs) {

        WKBReader wkbReader = new WKBReader();
        String sql = "select stationid, name, AsBinary(the_geom) from common_obs_spatial where "
                + "ST_Contains(ST_GeomFromText('"
                + ScanUtils.getRadarPolygonText(sitePoint,
                        ScanUtils.getMaxExtent()) + "', -1), the_geom)";
        // System.out.println("Sites SQL:" + sql);
        ISpatialQuery sq = null;
        ArrayList<ThreatLocation> sites = new ArrayList<ThreatLocation>();
        GeodeticCalculator gc = null;
        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, "metadata");
            if (results.length > 0) {
                for (int i = 0; i < results.length; i++) {
                    Object[] results2 = (Object[]) results[i];
                    if (results2.length > 0) {
                        String stationid = null;
                        String name = null;
                        Coordinate loc = null;
                        if (results2[0] != null) {
                            stationid = (String) results2[0];
                        }
                        if (results2[1] != null) {
                            name = (String) results2[1];
                            ;
                        }
                        if (results2[2] != null) {
                            loc = readGeometry(results2[2], wkbReader);
                        }
                        ThreatLocation threat = new ThreatLocation(stationid,
                                name, loc);
                        if (gc == null) {
                            gc = new GeodeticCalculator(crs);
                        }
                        if (checkUnique(threat, sites, gc)) {
                            sites.add(threat);
                        }
                    }
                }
            }
        } catch (SpatialException e) {
            e.printStackTrace();
        }

        return sites;
    }

    /**
     * Check for Coordinates that are the same
     * 
     * @param threat
     * @param threats
     * @return
     */
    public static boolean checkUnique(ThreatLocation threat,
            ArrayList<ThreatLocation> threats, GeodeticCalculator gc) {
        for (ThreatLocation loc : threats) {

            double dist = getDistance(loc.getCoor(), threat.getCoor(), gc)
                    * ScanUtils.meterToNM;

            if (dist < RADIUS_OF_10NM) {
                return false;
            }
        }
        return true;
    }

    /**
     * fix translation of geodetic degrees to true degrees
     * 
     * @param dir
     * @return
     */
    public static double getGeodeticDirection(double dir) {
        if (dir > 180.0) {
            dir = dir * (-1); // switch it to negative
            dir = -180.0 - dir;
        } else {
            dir = (180 - dir) * -1;// switch it to positive
        }
        return dir;
    }

    /**
     * extract geometry
     * 
     * @param obj
     * @return
     */
    public static Coordinate readGeometry(Object obj, WKBReader wkbReader) {
        Geometry geometry = null;
        try {
            geometry = wkbReader.read((byte[]) obj);
        } catch (ParseException e) {
            e.printStackTrace();
        }

        return geometry.getCoordinate();
    }

    /**
     * get coordinate
     * 
     * @param obj
     * @return
     */
    public static Geometry readCoordinate(Object obj, WKBReader wkbReader) {

        Geometry geometry = null;
        try {
            geometry = wkbReader.read((byte[]) obj);
        } catch (ParseException e) {
            e.printStackTrace();
        }

        return geometry;
    }

    /**
     * Used by PrecipRate
     * 
     * @param dhrRec
     * @return
     */

    public static float[] processDHR(RadarRecord dhrRec,
            Map<DHRValues, Double> dhrMap) {

        float[] precipRate = new float[dhrRec.getNumBins()
                * dhrRec.getNumRadials()];

        for (int radial = 0; radial < dhrRec.getNumRadials(); radial++) {
            for (int bin = 0; bin < dhrRec.getNumBins(); bin++) {
                float value = 0.0f;
                float fvalue = (float) getDecodedDHRValue(dhrRec
                        .getRawIntDataValue(radial, bin));

                if (fvalue > 0.0) {
                    value = ScanUtils.getZRvalue(fvalue,
                            dhrMap.get(DHRValues.ZRMULTCOEFF),
                            dhrMap.get(DHRValues.MAXPRECIPRATEALLOW),
                            dhrMap.get(DHRValues.ZRPOWERCOEFF),
                            dhrMap.get(DHRValues.BIAS_TO_USE));
                } else {
                    value = 0.0f;
                }
                precipRate[(radial * dhrRec.getNumBins()) + bin] = value;
            }
        }
        return precipRate;
    }

    /**
     * Gets the value decoded for the record
     * 
     * @param radarRec
     * @param dataValue
     * @return
     */
    public static double getDecodedDHRValue(int dataValue) {
        double value = 0.0;

        try {
            if (dataValue < 2) {
                return value;
            } else {
                // it has a value
                value = MIN_DHR_DBZ + ((dataValue - 2) * DHR_DBZ_STEP);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return value;
    }

    /**
     * Gets the ZR value
     */
    public static float getZRvalue(double zValue, double coefficent,
            double hailCap, double power, double bias) {
        // The Fulton et al 1998 standard NWS Z-R relationship
        double rValue = 0.0f;
        if (zValue > 0.0f) {

            double rlogMult = Math.log10(coefficent);
            rValue = bias
                    * (Math.pow(10.0,
                            ((zValue - 10.0 * rlogMult) / (10.0 * power))));

            // hail cap check
            if (rValue > hailCap) {
                return (float) (MM_TO_INCH * hailCap);
            }
        } else {
            return (float) rValue;
        }

        return (float) (MM_TO_INCH * rValue);
    }

    /**
     * direct port of NWS code to find height of beam location
     * 
     * @param range
     * @param elevAngle
     * @return
     */
    public static double getRadarBeamHeight(double range, double elevAngle) {

        // System.out.println("range = " + range);
        // System.out.println("elevAngle = " + elevAngle);
        // System.out.println("Range: "+range);
        // System.out.println("ELEVATION: "+elevAngle);

        double frange = range * NMI_TO_KM; // Conversion from nmi to // km.
        double term1 = frange * Math.sin(Math.toRadians(elevAngle));
        double term2 = Math.pow(frange, 2.0) / (2 * (1.21 * R_EARTH));
        double height = (term1 + term2) * KM_TO_KFT;

        // System.out.println("height = " + height);

        return height;
    }

    /**
     * converts the vil value from the short
     * 
     * @param x
     * @return
     */
    public static float getDVILValue(short x) {
        int s = (x >> 15) & 0x1;
        int e = (x >> 10) & 0x1f;
        int f = x & 0x3ff;

        float value;
        if (e == 0) {
            value = (float) (Math.pow(-1, s) * 2 * (f / Math.pow(2, 10)));
        } else {
            value = (float) ((Math.pow(-1, s) * Math.pow(2, e - 16) * (1 + f
                    / Math.pow(2, 10))));
        }
        return value;
    }

    /**
     * threhold consideration of DVIL values
     * 
     * @param rec
     * @param dataValue
     * @return
     */
    public static float getDecodedDvil(RadarRecord rec, int dataValue) {
        double decoded[] = new double[] { 0, 0, 0, 0, 0 };
        float value = 0.0f;
        for (int i = 0; i < decoded.length; i++) {
            if (i == 2) {
                decoded[i] = rec.getThreshold(i);
                continue;
            }
            decoded[i] = getDVILValue(rec.getThreshold(i));
        }
        if (dataValue < decoded[2]) {
            value = (float) ((dataValue - decoded[1]) / decoded[0]);
        } else if (dataValue >= decoded[2]) {
            value = (float) Math.exp((dataValue - decoded[4]) / decoded[3]);
        }
        // System.out.println("Digital Vil : int "+dataValue+" value = "+value);
        return value;
    }

    /**
     * decode the EET value
     * 
     * @param dataValue
     * @return
     */
    public static float getDecodedEET(int dataValue) {

        dataValue -= 2;
        float value = 0.0f;
        if ((dataValue >= 70) && (dataValue < 130)) {
            value = 0.0f;
        } else if (dataValue >= 130) {
            dataValue -= 130;
            value = dataValue;
        } else {
            value = dataValue;
        }
        // System.out.println("EET : int "+dataValue+" value = "+value);
        return value;
    }

    /**
     * parse the MDA SR
     * 
     * @param cellText
     * @return
     */
    public static double parseMDASR(String cellText) {
        double value = 0.0;
        String suffix = null;
        if (cellText.matches("[0-9.]+") == true) {
            value = Double.valueOf(cellText);
        } else if (cellText.endsWith("L") || cellText.endsWith("M")) {
            try {
                suffix = cellText.substring(cellText.length() - 1);
                value = Double.valueOf(cellText.substring(0,
                        cellText.length() - 1));
            } catch (Exception ex) {
                value = -999.0;
                suffix = null;
            }
        }
        return value;
    }

    /**
     * @return the levels
     */
    private static void setResolutionLevels(String tablename) {
        if (tablename.equalsIgnoreCase(prevTable)) {
            return;
        }

        double[] levels = null;
        ISpatialQuery sq = null;
        String sql = "SELECT f_geometry_column FROM public.geometry_columns "
                + "WHERE f_table_schema='mapdata' AND f_table_name='"
                + tablename
                + "' "
                + "AND f_geometry_column LIKE 'the_geom_%' order by f_geometry_column desc;";
        List<String> levelList = new ArrayList<String>();
        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, MAPS_DB);
            levels = new double[results.length];
            Arrays.fill(levels, 9999.0);
            int i = 0;
            for (Object obj : results) {
                if (obj instanceof Object[]) {
                    obj = ((Object[]) obj)[0];
                }

                String columnName = obj.toString();
                levelList.add(columnName);
                String s = (columnName).substring(9).replace('_', '.');
                levels[i++] = Double.parseDouble(s);
            }
        } catch (SpatialException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error querying available levels", e);
        }

        if (levelList == null || levelList.size() == 0) {
            statusHandler.handle(Priority.ERROR, "Error querrying " + tablename
                    + " table.  No geometries found.");
            return;
        }

        tableHighResLevels.put(tablename, levelList.get(levelList.size() - 1));

        for (int i = 0; i < levels.length; i++) {
            if (levels[i] <= 0.016) {
                tableStdResLevels.put(tablename, levelList.get(i));
                break;
            }
        }

        prevTable = tablename;
    }

    /**
     * @return the levels
     */
    public static ArrayList<String> getResolutionLevels(String tablename) {

        double[] levels = null;
        ISpatialQuery sq = null;
        String sql = "SELECT f_geometry_column FROM public.geometry_columns "
                + "WHERE f_table_schema='mapdata' AND f_table_name='"
                + tablename
                + "' "
                + "AND f_geometry_column LIKE 'the_geom_%' order by f_geometry_column asc;";
        ArrayList<String> levelList = new ArrayList<String>();
        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, MAPS_DB);
            levels = new double[results.length];
            int i = 0;
            for (Object obj : results) {
                if (obj instanceof Object[]) {
                    obj = ((Object[]) obj)[0];
                }

                String columnName = obj.toString();
                levelList.add(columnName);
                String s = (columnName).substring(9).replace('_', '.');
                levels[i++] = Double.parseDouble(s);
            }
        } catch (SpatialException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error querying available levels", e);
        }

        if (levelList == null || levelList.size() == 0) {
            statusHandler.handle(Priority.ERROR, "Error querrying " + tablename
                    + " table.  No geometries found.");
            return null;
        }

        return levelList;
    }

    /**
     * Get the standard resolution level.
     * 
     * @param tablename
     *            the name of the table
     * @return the standardResolutionLevel column name
     */
    public static String getStandardResolutionLevel(String tablename) {
        String resLevel = tableStdResLevels.get(tablename);
        if (resLevel == null) {
            setResolutionLevels(tablename);
            resLevel = tableStdResLevels.get(tablename);
        }
        return resLevel;
    }

    /**
     * Get the high resolution level.
     * 
     * @param tablename
     *            the name of the table
     * @return the highResolutionLevel column name
     */
    public static String getHighResolutionLevel(String tablename) {
        String resLevel = tableHighResLevels.get(tablename);
        if (resLevel == null) {
            setResolutionLevels(tablename);
            resLevel = tableHighResLevels.get(tablename);
        }
        return resLevel;
    }

    /**
     * Gets the WMO number for nearest upper air station
     * 
     * @return
     */
    public static String getNearestWMO(Coordinate coor) {

        String wmo = null;
        @SuppressWarnings("unused")
        Geometry geo = null;
        String sql = "select wmoindex, asBinary(upperairgeom) from common_obs_spatial where "
                + "ST_Contains(ST_GeomFromText('"
                + ScanUtils.getRadarPolygonText(coor, ScanUtils.getMaxExtent())
                + "', -1), upperairgeom) and catalogtype = 22";
        @SuppressWarnings("unused")
        WKBReader wkbReader = new WKBReader();
        ISpatialQuery sq = null;
        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, "metadata");
            if (results.length > 0) {
                Object[] results2 = (Object[]) results[0];
                wmo = results2[0].toString();
            }
        } catch (Exception e) {
            System.out
                    .println("No BUFRUA data for stations near Radar, none will be used...");
        }

        return wmo;
    }

    public static ArrayList<Object> getStationGeometry(String icao) {
        ArrayList<Object> returns = new ArrayList<Object>(2);
        String sql = "select AsBinary(the_geom) from radar_spatial where rda_id = \'"
                + icao.toUpperCase() + "\'";
        ISpatialQuery sq = null;
        try {
            sq = SpatialQueryFactory.create();
            Object[] results = sq.dbRequest(sql, "metadata");
            if (results.length > 0) {
                WKBReader reader = new WKBReader();
                Geometry stationGeo = reader.read((byte[]) results[0]);
                Coordinate stationCoor = stationGeo.getCoordinate();
                returns.add(stationCoor);
                returns.add(ScanUtils.getStationGeometry(stationCoor,
                        ScanUtils.SCAN_GRID_DIM_RESOLUTION,
                        ScanUtils.SCAN_GRID_DIM));
            }
        } catch (Exception e) {
            System.out.println("Station geometry for " + icao
                    + " setter failed.....");
        }

        return returns;
    }
}
