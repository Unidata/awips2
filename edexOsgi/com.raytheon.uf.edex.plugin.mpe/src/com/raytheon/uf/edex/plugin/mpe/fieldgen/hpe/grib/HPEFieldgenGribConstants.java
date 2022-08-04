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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.grib;

/**
 * Common location containing constants relevant to xmrg -> grib conversion
 * within hpe fieldgen.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class HPEFieldgenGribConstants {

    public static class AppsDefaults {

        public static final String HPE_DHRMOSAIC_GRIB_DIR = "hpe_dhrmosaic_grib_dir";

        public static final String HPE_BDHRMOSAIC_GRIB_DIR = "hpe_bdhrmosaic_grib_dir";

        public static final String HPE_EBMOSAIC_GRIB_DIR = "hpe_ebmosaic_grib_dir";

        public static final String HPE_ERMOSAIC_GRIB_DIR = "hpe_ermosaic_grib_dir";

    }

    protected HPEFieldgenGribConstants() {
    }
}