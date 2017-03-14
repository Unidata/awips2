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
package com.raytheon.uf.edex.plugin.mpe.buildhourly;

import com.raytheon.uf.common.hydro.CommonHydroConstants;

/**
 * Common, centralized declaration of constants that will be utilized by
 * {@link BuildHourly}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2016 5571       skorolev    Initial creation
 * Jun 24, 2016 5699       bkowal      Relocated the missing value constant to a class
 *                                     containing constants for use by the entire mpe plugin.
 * 
 * </pre>
 * 
 * @author skorolev
 */
public class BuildHourlyConstants extends CommonHydroConstants {

    public static final String DB_DATE_STRING = "yyyy-MM-dd HH:mm:ss";

    public static final String FILE_DATE_STRING = ".MMdd.YYYYHHmmss";

    public static final String QCZ = "Z";

    public static final String SAME = "SAME";

    public static final String LOG_SAME = "[Retain existing Type]";

    public static final int INT_SAME = 11;

    public static final String PTYPE = "PTYPE";

    public static final String LOG_PTYPE = "[Change R to P in Type]";

    public static final int INT_PTYPE = 22;

    public static final String WORKFILENAME = "BUILD_HOURLY.";

    public final static String GPP_WORKFILE_SUFFIX = "work";

    public final static String INLINE_SEPARATOR = "|";

    public final static String BUILDHOURLY = "BUILDHOURLY";

    public BuildHourlyConstants() {
    }

}
