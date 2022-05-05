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
package com.raytheon.uf.edex.plugin.mpe.gather;

import java.text.SimpleDateFormat;

/**
 * Common, centralized declaration of constants that will be utilized by
 * {@link MpeRadarGather}s.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 30, 2016 4625       bkowal      Initial creation
 * Jul 24, 2018 5588       mapeters    Added some constants for addition of DSPGather, refactored from
 *                                     com.raytheon.uf.edex.plugin.mpe.gather.dhr.DHRGatherConstants
 *
 * </pre>
 *
 * @author bkowal
 */

public class MpeRadarGatherConstants {

    private static final String LOCK_DATE_FORMAT = "MM-dd-yyy HH:mm";

    public static final ThreadLocal<SimpleDateFormat> lockDF = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat(LOCK_DATE_FORMAT);
            return sdf;
        }
    };

    public static final String CLUSTER_LOCK_NAME_SUFFIX = "GatherLock";

    public static final String CLUSTER_LOCK_DETAILS_SUFFIX = "Gather";

    public static final String DHR_PRODUCT_TYPE = "DHR";

    public static final String DSP_PRODUCT_TYPE = "DSP";

    public static final int DHR_DEFAULT_VERSION = 8;

    public static final int DHR_PARAM_PRECIP = 4;

    public static class AppsDefaults {

        public static final String DHR_PROD_DIR = "dhr_prod_dir";

        public static final String DSP_PROD_DIR = "dsp_prod_dir";
    }

    private MpeRadarGatherConstants() {
    }
}