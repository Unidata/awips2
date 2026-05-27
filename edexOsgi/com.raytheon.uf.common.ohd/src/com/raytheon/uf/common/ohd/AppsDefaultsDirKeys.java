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
package com.raytheon.uf.common.ohd;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * Defines the apps defaults key associated with directories that must be
 * validated.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 25, 2016 5264       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public final class AppsDefaultsDirKeys {

    public static final String APPS_DIR = "apps_dir";

    public static final String MPE_QMOSAIC_DIR = "mpe_qmosaic_dir";

    public static final String WHFS_LOCAL_DATA_DIR = "whfs_local_data_dir";

    public static final String BIAS_MESSAGE_DIR = "bias_message_dir";

    public static final String MPE_SATPRE_DIR = "mpe_satpre_dir";

    public static final String GAFF_MOSAIC_DIR = "gaff_mosaic_dir";

    public static final String GEO_DATA = "geo_data";

    /*
     * HPE Directories - many of which can be referenced within the FFMP Source
     * Config xml.
     */
    public static final String HPE_GRIB_INPUT_DIR = "hpe_grib_input_dir";

    public static final String HPE_SATPRE_DIR = "hpe_satpre_dir";

    public static final String HPE_INPUT_DIR = "hpe_input_dir";

    public static final String HPE_OUTPUT_DIR = "hpe_output_dir";

    public static final String HPE_SAT_STATEVAR_DIR = "hpe_sat_statevar_dir";

    public static final String HPE_LOG_DIR = "hpe_log_dir";

    public static final String HPE_DHRMOSAIC_DIR = "hpe_dhrmosaic_dir";

    public static final String HPE_BDHRMOSAIC_DIR = "hpe_bdhrmosaic_dir";

    public static final String HPE_ERMOSAIC_DIR = "hpe_ermosaic_dir";

    public static final String HPE_EBMOSAIC_DIR = "hpe_ebmosaic_dir";

    public static final String HPE_AVG_ERMOSAIC_DIR = "hpe_avg_ermosaic_dir";

    public static final String HPE_LSATPRE_DIR = "hpe_lsatpre_dir";

    public static final String HPE_DSPHEIGHT_DIR = "hpe_dspheight_dir";

    public static final String HPE_DSPINDEX_DIR = "hpe_dspindex_dir";

    public static final String HPE_HEIGHT_DIR = "hpe_height_dir";

    public static final String HPE_INDEX_DIR = "hpe_index_dir";

    public static final String HPE_DHRMOSAIC_GRIB_DIR = "hpe_dhrmosaic_grib_dir";

    public static final String HPE_ERMOSAIC_GRIB_DIR = "hpe_ermosaic_grib_dir";

    public static final String HPE_EBMOSAIC_GRIB_DIR = "hpe_ebmosaic_grib_dir";

    public static final String HPE_GIF_DIR = "hpe_gif_dir";

    public static final String HPE_JPEG_DIR = "hpe_jpeg_dir";

    public static final String HPE_NETCDF_DIR = "hpe_netcdf_dir";

    public static final String HPE_GRIB_DIR = "hpe_grib_dir";

    public static final String HPE_XMRG_DIR = "hpe_xmrg_dir";

    public static final String[] DIR_KEYS_TO_VALIDATE = { APPS_DIR,
            MPE_QMOSAIC_DIR, WHFS_LOCAL_DATA_DIR, BIAS_MESSAGE_DIR,
            MPE_SATPRE_DIR, GAFF_MOSAIC_DIR, GEO_DATA, HPE_GRIB_INPUT_DIR,
            HPE_SATPRE_DIR, HPE_INPUT_DIR, HPE_OUTPUT_DIR,
            HPE_SAT_STATEVAR_DIR, HPE_LOG_DIR, HPE_DHRMOSAIC_DIR,
            HPE_BDHRMOSAIC_DIR, HPE_ERMOSAIC_DIR, HPE_EBMOSAIC_DIR,
            HPE_AVG_ERMOSAIC_DIR, HPE_LSATPRE_DIR, HPE_DSPHEIGHT_DIR,
            HPE_DSPINDEX_DIR, HPE_HEIGHT_DIR, HPE_INDEX_DIR,
            HPE_DHRMOSAIC_GRIB_DIR, HPE_ERMOSAIC_GRIB_DIR,
            HPE_EBMOSAIC_GRIB_DIR, HPE_GIF_DIR, HPE_JPEG_DIR, HPE_NETCDF_DIR,
            HPE_GRIB_DIR, HPE_XMRG_DIR };

    private static final List<Path> validRootPaths;

    private static String validPathString;

    static {
        /*
         * For now, we will whitelist /tmp and the EDEX data directory.
         * EDEX_HOME is set by both the EDEX start.sh and the CAVE cave.sh.
         */
        validRootPaths = new ArrayList<>();
        validRootPaths.add(Paths.get(File.separator + "tmp"));
        validRootPaths
                .add(Paths.get(File.separator + "awips2", "edex", "data"));
    }

    protected AppsDefaultsDirKeys() {
    }

    /**
     * Validates that the specified directory is associated with an approved
     * location on the file system.
     * 
     * @param directory
     *            the directory to validate
     * @return true, if the directory is valid; false, otherwise.
     */
    public static boolean validateDirectory(final String directory) {
        return validatePath(Paths.get(directory));
    }

    /**
     * Validates that the specified {@link Path} is associated with an approved
     * location on the file system.
     * 
     * @param path
     *            the {@link Path} to validate
     * @return true, if the path is valid; false, otherwise.
     */
    public static boolean validatePath(Path path) {
        /*
         * Normalize the {@link Path} to resolve any path navigation.
         */
        path = path.normalize();
        for (Path validPath : validRootPaths) {
            if (path.startsWith(validPath)) {
                return true;
            }
        }

        return false;
    }

    public static String getValidRootsAsString() {
        if (validPathString == null) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Path validPath : validRootPaths) {
                if (first) {
                    first = false;
                } else {
                    sb.append(", ");
                }
                sb.append(validPath.toString());
            }
            sb.append("]");
            validPathString = sb.toString();
        }

        return validPathString;
    }
}