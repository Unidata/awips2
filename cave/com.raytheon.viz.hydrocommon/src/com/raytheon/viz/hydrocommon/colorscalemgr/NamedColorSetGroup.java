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
package com.raytheon.viz.hydrocommon.colorscalemgr;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 1, 2010            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class NamedColorSetGroup {
    private List<NamedColorUseSet> colorGroupArray;

    public void addNamedColorUseSet(NamedColorUseSet namedColorUseSet) {
        if (namedColorUseSet == null) {
            return;
        }
        
        if (colorGroupArray == null) {
            colorGroupArray = new ArrayList<NamedColorUseSet>();
        }

        colorGroupArray.add(namedColorUseSet);
    }

    /**
     * @return the colorGroupArray
     */
    public List<NamedColorUseSet> getColorGroupArray() {
        return colorGroupArray;
    }

    /**
     * @param colorGroupArray the colorGroupArray to set
     */
    public void setColorGroupArray(List<NamedColorUseSet> colorGroupArray) {
        this.colorGroupArray = colorGroupArray;
    }
}
