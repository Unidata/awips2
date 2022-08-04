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
package com.raytheon.viz.hydro.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

/**
 * Constants used by the HydroPerspective
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 17, 2008				mpduff	Initial creation
 * 
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class HydroViewConstants {
    static {
        initialize();
    }
    
    public static Map<String, RGB> TS_COLORS = new HashMap<String, RGB>();
    public static ArrayList<RGB> TS_COLOR_LIST = new ArrayList<RGB>();
    
    
    public static final Integer DODGERBLUE = 0;
    public static final Integer YELLOW = 1;
    public static final Integer CYAN = 2;
    public static final Integer GREEN = 3;
    public static final Integer PURPLE = 4;
    public static final Integer MAGENTA = 5;
    public static final Integer RED = 6;
    public static final Integer AQUAMARINE = 7;
    public static final Integer SEAGREEN = 8;
    public static final Integer MAROON = 9;
    public static final Integer BLUEVIOLET = 10;
    public static final Integer CORAL = 11;
    public static final Integer HOTPINK = 12;
    public static final Integer MEDIUMPURPLE = 13;
    public static final Integer DEEPPINK = 14;
    public static final Integer FORESTGREEN = 15;
    public static final Integer LIMEGREEN = 16;
    public static final Integer ORANGE = 17;
    public static final Integer ALICEBLUE = 18;
    public static final Integer SKYBLUE = 19;
    public static final Integer WHITE = 20;
    public static final Integer DARKGREEN = 21;
    public static final Integer LIGHTBLUE = 22;
    public static final Integer WHEAT = 23;
    public static final Integer DARKORANGE = 24;
    public static final Integer GOLD = 25;
    public static final Integer GREENYELLOW = 26;
    public static final Integer DARKVIOLET = 27;
    public static final Integer INDIANRED = 28;
    public static final Integer LAWNGREEN = 29;
    public static final Integer CORNFLOWERBLUE = 30;
    public static final Integer MEDIUMBLUE = 31;
    public static final Integer PALEGREEN = 32;
    public static final Integer MEDIUMORCHID = 33;
    public static final Integer STEELBLUE = 34;
    
    
    private static void initialize() {
        TS_COLOR_LIST.add(new RGB(30, 144, 255));
        TS_COLOR_LIST.add(new RGB(255, 255, 0));
        TS_COLOR_LIST.add(new RGB(0, 255, 255));
        TS_COLOR_LIST.add(new RGB(0, 255, 0));
        TS_COLOR_LIST.add(new RGB(160, 32, 240));
        TS_COLOR_LIST.add(new RGB(255, 0, 255));
        TS_COLOR_LIST.add(new RGB(255, 0, 0));
        TS_COLOR_LIST.add(new RGB(127, 255, 212));
        TS_COLOR_LIST.add(new RGB(46, 139, 87));
        TS_COLOR_LIST.add(new RGB(176, 48, 96));
        TS_COLOR_LIST.add(new RGB(138, 43, 226));
        TS_COLOR_LIST.add(new RGB(255, 127, 80));
        TS_COLOR_LIST.add(new RGB(255, 105, 180));
        TS_COLOR_LIST.add(new RGB(147, 112, 219));
        TS_COLOR_LIST.add(new RGB(255, 20, 147));
        TS_COLOR_LIST.add(new RGB(34, 139, 34));
        TS_COLOR_LIST.add(new RGB(50, 205, 50));
        TS_COLOR_LIST.add(new RGB(255, 165, 0));
        TS_COLOR_LIST.add(new RGB(240, 248, 255));
        TS_COLOR_LIST.add(new RGB(135, 206, 235));
        TS_COLOR_LIST.add(new RGB(255, 255, 255));
        TS_COLOR_LIST.add(new RGB(0, 100, 0));
        TS_COLOR_LIST.add(new RGB(173, 216, 230));
        TS_COLOR_LIST.add(new RGB(245, 222, 179));
        TS_COLOR_LIST.add(new RGB(255, 140, 0));
        TS_COLOR_LIST.add(new RGB(255, 215, 0));
        TS_COLOR_LIST.add(new RGB(173, 255, 47));
        TS_COLOR_LIST.add(new RGB(148, 0, 211));
        TS_COLOR_LIST.add(new RGB(205, 92, 92));
        TS_COLOR_LIST.add(new RGB(124, 252, 0));
        TS_COLOR_LIST.add(new RGB(100, 149, 237));
        TS_COLOR_LIST.add(new RGB(50, 205, 50));
        TS_COLOR_LIST.add(new RGB(0, 0, 205));
        TS_COLOR_LIST.add(new RGB(152, 251, 152));
        TS_COLOR_LIST.add(new RGB(186, 85, 211));
        TS_COLOR_LIST.add(new RGB(70, 130, 180));
    }
}
