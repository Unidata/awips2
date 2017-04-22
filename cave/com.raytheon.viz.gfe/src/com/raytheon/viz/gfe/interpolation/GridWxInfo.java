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
package com.raytheon.viz.gfe.interpolation;

import java.util.ArrayList;
import java.util.List;

/**
 * Data class which acts as a structure to hold information about one weather
 * type stored in one weather "GridSlice."
 * 
 * Only needs public data members and a default constructor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 11, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */
public class GridWxInfo {

    // = to byte value for weather key
    public int wxType;

    // how many points of this type in the grid
    public int howMany;

    // max x value (in grid index units, 0 to _xDim-1)
    public int maxX;

    public int maxY;

    public int minX;

    public int minY;

    // is/is not(= 1 or 0) contiguous with other type(s)
    public boolean isContig;

    // other types this area touches
    public List<Integer> contigWxType;

    /**
     * Constructor for GridWxInfo class, which puts data members in an initial
     * state.
     */
    public GridWxInfo() {
        contigWxType = new ArrayList<Integer>();
    }
}
