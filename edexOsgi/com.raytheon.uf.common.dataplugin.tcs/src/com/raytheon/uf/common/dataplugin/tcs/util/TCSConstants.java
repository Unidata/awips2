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
package com.raytheon.uf.common.dataplugin.tcs.util;

import java.text.DecimalFormat;
import java.text.NumberFormat;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2010            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public interface TCSConstants {

    public static final String LAT = "latitude";

    public static final String LON = "longitude";

    public static final String PRESSURE = "pressure";

    public static final String NAME = "name";

    public static final String DISPLAY_TIME = "displayTime";

    public static final String WIND_SPEED = "windSpeed";

    public static final String TROPICAL = "isTropical";

    public static final String SIZE = "size";

    public static final String TYPE = "productType";

    public static final String RAD_64 = "radius64KT";

    public static final String RAD_50 = "radius50KT";

    public static final String RAD_34 = "radius34KT";

    public static final String RAD_12 = "radius12FT";

    public static final String DISSIPATED = "Dissipated!";

    public static final String DATAURI = "dataURI";

    public static final String WMO_HEADER = "wmoHeader";

    public static final int PLOT_WIDTH = 13;

    public static final int ZOOM_LEVEL = 6000;

    public static final NumberFormat formatter = new DecimalFormat("###");

    public static final char STORM_SYMBOL = '\u0078';

    public static final char HURRICANE_SYMBOL = '\u0079';

    public static final char EXTRATROPICAL_SYMBOL = '\u005b';

    public static final char UNKNOWN_SYMBOL = '\u008e';

    public static final String LEGEND_12_FT = " ________________ :  1 2  F T  S e a s.";

    public static final String LEGEND_34_KT = " . . . . . . . . . . . . . . .  :  3 4  K T  W i n d  R a d i u s.";

    public static final String LEGEND_50_KT = " _ _ _ _ _ _ _ _ _ _ :  5 0  K T  W i n d  R a d i u s.";

    public static final String LEGEND_64_KT = " ______________ :  6 4  K T  W i n d  R a d i u s.";
}
