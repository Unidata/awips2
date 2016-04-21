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

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.RGBColors;

/**
 * Hydro Timeseries Color Utility Class
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2008            mpduff      Initial creation
 * Dec 01, 2011 11464      mpduff      Made separate group and non-group color lists
 *                                     Using the RGB class instead of the list from A1.  
 *                                     I ran across a group config file using a color not in 
 *                                     the list which caused errors.
 * May 08, 2012 14958      wkwock      prevent go over the list in getGroupModeColor
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class HydroUtils {
    private static ArrayList<RGB> TS_COLOR_LIST = null;
    private static ArrayList<RGB> TS_GROUP_COLOR_LIST = null;

    public static RGB getColor(String colorName) {
        return RGBColors.getRGBColor(colorName);
    }
    
    public static RGB getColor(int index) {
        if (TS_COLOR_LIST == null) {
            TS_COLOR_LIST = new ArrayList<RGB>();
            initializeColorList();
        }
        
        return TS_COLOR_LIST.get(index);
    }
    public static RGB getGroupModeColor(int index) {
        if (TS_GROUP_COLOR_LIST == null) {
            TS_GROUP_COLOR_LIST = new ArrayList<RGB>();
            initializeGroupModeColorList();
        }
        
        return TS_GROUP_COLOR_LIST.get(index % TS_GROUP_COLOR_LIST.size()); //not to go over the list
    } 
    
    private static void initializeGroupModeColorList() {           
        TS_GROUP_COLOR_LIST.add(new RGB(127, 255, 212));
        TS_GROUP_COLOR_LIST.add(new RGB(46, 139, 87));
        TS_GROUP_COLOR_LIST.add(new RGB(176, 48, 96));
        TS_GROUP_COLOR_LIST.add(new RGB(138, 43, 226));
        TS_GROUP_COLOR_LIST.add(new RGB(255, 127, 80));
        TS_GROUP_COLOR_LIST.add(new RGB(255, 105, 180));
        TS_GROUP_COLOR_LIST.add(new RGB(147, 112, 219));
        TS_GROUP_COLOR_LIST.add(new RGB(34, 139, 34));
        TS_GROUP_COLOR_LIST.add(new RGB(255, 20, 147));        
        TS_GROUP_COLOR_LIST.add(new RGB(50, 205, 50));        
        TS_GROUP_COLOR_LIST.add(new RGB(240, 248, 255));
        TS_GROUP_COLOR_LIST.add(new RGB(135, 206, 235));        
        TS_GROUP_COLOR_LIST.add(new RGB(0, 100, 0));
        TS_GROUP_COLOR_LIST.add(new RGB(173, 216, 230));
        TS_GROUP_COLOR_LIST.add(new RGB(245, 222, 179));
        TS_GROUP_COLOR_LIST.add(new RGB(255, 140, 0));
        TS_GROUP_COLOR_LIST.add(new RGB(255, 215, 0));
        TS_GROUP_COLOR_LIST.add(new RGB(173, 255, 47));
        TS_GROUP_COLOR_LIST.add(new RGB(148, 0, 211));
        TS_GROUP_COLOR_LIST.add(new RGB(205, 92, 92));
        TS_GROUP_COLOR_LIST.add(new RGB(124, 252, 0));
        TS_GROUP_COLOR_LIST.add(new RGB(100, 149, 237));
        TS_GROUP_COLOR_LIST.add(new RGB(50, 205, 50));
        TS_GROUP_COLOR_LIST.add(new RGB(0, 0, 205));
        TS_GROUP_COLOR_LIST.add(new RGB(152, 251, 152));
        TS_GROUP_COLOR_LIST.add(new RGB(186, 85, 211));
        TS_GROUP_COLOR_LIST.add(new RGB(70, 130, 180));
        TS_GROUP_COLOR_LIST.add(new RGB(0, 0, 255));                       
        TS_GROUP_COLOR_LIST.add(new RGB(255, 255, 255));
    }
    private static void initializeColorList() {    
        TS_COLOR_LIST.add(new RGB(30, 144, 255));
        TS_COLOR_LIST.add(new RGB(255, 255, 0));        
        TS_COLOR_LIST.add(new RGB(0, 255, 0));
        TS_COLOR_LIST.add(new RGB(165, 42, 42));        
        TS_COLOR_LIST.add(new RGB(160, 32, 240));
        TS_COLOR_LIST.add(new RGB(255, 0, 255));
        TS_COLOR_LIST.add(new RGB(255, 165, 0));
        TS_COLOR_LIST.add(new RGB(238, 130, 238));
        TS_COLOR_LIST.add(new RGB(255, 0, 0));   
        TS_COLOR_LIST.add(new RGB(0, 255, 255));
        TS_COLOR_LIST.add(new RGB(127, 255, 212));
        TS_COLOR_LIST.add(new RGB(46, 139, 87));
        TS_COLOR_LIST.add(new RGB(176, 48, 96));
        TS_COLOR_LIST.add(new RGB(138, 43, 226));
        TS_COLOR_LIST.add(new RGB(255, 127, 80));
        TS_COLOR_LIST.add(new RGB(255, 105, 180));
        TS_COLOR_LIST.add(new RGB(147, 112, 219));
        TS_COLOR_LIST.add(new RGB(34, 139, 34));
        TS_COLOR_LIST.add(new RGB(255, 20, 147));        
        TS_COLOR_LIST.add(new RGB(50, 205, 50));        
        TS_COLOR_LIST.add(new RGB(240, 248, 255));
        TS_COLOR_LIST.add(new RGB(135, 206, 235));        
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
        TS_COLOR_LIST.add(new RGB(0, 0, 255));                       
    	TS_COLOR_LIST.add(new RGB(255, 255, 255));
    }
}
