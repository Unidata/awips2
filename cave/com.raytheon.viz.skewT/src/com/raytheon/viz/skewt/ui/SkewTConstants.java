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

package com.raytheon.viz.skewt.ui;

import java.text.DecimalFormat;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.edex.util.Equations;
import com.raytheon.edex.util.UAPoint;
import com.raytheon.uf.common.sounding.WxMath;

/**
 * 
 * Used extensive work from SkewT and associated codebase.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28Sept2008  #1529       dhladky     initial.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class SkewTConstants {

    public static Rectangle skewTRectangle = new Rectangle(0, 0, 860, 1240);

    public static double TEMPERATURE_MIN = -115.0;

    public static double TEMPERATURE_MAX = 45.0;

    public static double WIND_SPEED_MIN = 0.0;

    public static double WIND_SPEED_MAX = 250.0;

    public static double WIND_DIR_MIN = 0.0;

    public static double WIND_DIR_MAX = 360.0;

    public static double PRESSURE_MIN = 100.0;

    public static double PRESSURE_MAX = 973.0;

    // horizontal pressure line that will be drawn.
    public static final double[] MAN_LEVELS = { 1050, 1000, 950, 900, 850, 800,
            750, 700, 650, 600, 550, 500, 450, 400, 350, 300, 250, 200, 150,
            100 };

    public static List<List<UAPoint>> saturatedPoints = Equations
            .getSaturatedAdiabats(1000, 100, 20, -60, 60, 5);

    public static List<List<UAPoint>> dryPoints = Equations.getDryAdiabats(
            1000, 100, 20, -40, 273, 10);

    // lightGray.
    public static final RGB backgroundColor = new RGB(191, 191, 191);

    /**
     * Color for moist adiabat lines
     */
    public static final RGB moistAdiabatColor = new RGB(0, 127, 255);

    /**
     * Color for dry adiabat lines
     */
    public static final RGB dryAdiabatColor = new RGB(0, 0, 255);

    /**
     * Color for mixing ratio lines
     */
    public static final RGB mixingRatioColor = new RGB(23, 255, 23);

    /**
     * Color for temperature lines
     */
    public static final RGB temperatureColor = new RGB(210, 180, 140);

    /**
     * Color for pressure lines
     */
    public static final RGB pressureColor = new RGB(191, 191, 191);

    /**
     * Color for wetbulb lines
     */
    public static final RGB wetBulbColor = new RGB(0, 255, 255);

    public static final int wetBulbLineWidth = 1;

    /**
     * parameter and label color
     */
    public static final RGB labelColor = new RGB(191, 191, 191);

    public static final RGB editColor = new RGB(255, 165, 0);

    public static final RGB pointEditColor = new RGB(255, 0, 0);

    public static final int editLineWidth = 2;

    public static final RGB parcelColor = new RGB(132, 112, 255);

    public static final int parcelLineWidth = 2;

    public static final int moistAdiabaticIncrement = 5;

    // lightGray.
    public static final RGB pointColor = new RGB(255, 255, 0);

    public static DecimalFormat pressFormat = new DecimalFormat("####.#");

    public static DecimalFormat tempFormat = new DecimalFormat("###.#");

    public static DecimalFormat windFormat = new DecimalFormat("###.#");

    public static char DEGREE_SYMBOL = '\u00B0';

    public static double endpointRadius = 4;

    public static final int LABEL_PADDING = 5;

    public static double bottom = WxMath.getSkewTXY(1050, 0).y;

    public static double top = WxMath.getSkewTXY(100, 0).y;

    public static double height = top - bottom;

    public static double left = (-height / 2) - 1;

    public static double right = (height / 2) + 1;

    public static double center = (left + right) / 2;

}
