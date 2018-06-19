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
package com.raytheon.uf.edex.plugin.mpe.gather.dhr;

import java.text.SimpleDateFormat;

/**
 * Common, centralized declaration of constants that will be utilized by
 * {@link DHRGather}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 30, 2016 4625       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class DHRGatherConstants {

    private static final String LOCK_DATE_FORMAT = "MM-dd-yyy HH:mm";

    public static ThreadLocal<SimpleDateFormat> lockDF = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat(LOCK_DATE_FORMAT);
            return sdf;
        }
    };

    public static final String CLUSTER_LOCK_NAME = "DHRGatherLock";

    public static final String CLUSTER_LOCK_DETAILS = "gather";

    public static final int DHR_DEFAULT_VERSION = 8;

    public static final int DHR_PARAM_PRECIP = 4;

    public static class AppsDefaults {

        public static final String DHR_PROD_DIR = "dhr_prod_dir";

    }

    protected DHRGatherConstants() {
    }
}