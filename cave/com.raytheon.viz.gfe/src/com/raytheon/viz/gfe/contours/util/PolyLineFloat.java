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
package com.raytheon.viz.gfe.contours.util;

import java.util.ArrayList;

/**
 * Implements poly-line. A poly-line is an ordered set of "points"
 * with the added ability of identifying the the point in the set that
 * is closest to an arbitrary point in the same space. The points in
 * the space are {@link CartCoord2D CartCoord2D} objects.
 * <P>
 * This class is a port of the original C++ PolyLine class from gfe. 
 * This implementation does not use Java 6 style generics since only
 * float data is used in gfe code that uses this data structure.
 * <P>
 * Rather than extending a port of the C++ SeqOf class from gfe, this
 * implementation extends ArrayList, which has the desired functionality.
 *  
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04Mar2008    968        MW Fegan    Initial Creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class PolyLineFloat extends ArrayList<CartCoord2D> {

    private static final long serialVersionUID = 1L;

    /**
     * Constructor. Creates an empty PolyLine object. The 
     */
    public PolyLineFloat() {
        // intientionally empty
    }


    /**
     * Constructor. Creates a PolyLine object populated from the
     * list provided.
     * 
     * @param list list of {@link CartCoord2D CartCoord2D}
     *             objects used to populate the new object
     */
    public PolyLineFloat(ArrayList<CartCoord2D> list) {
        super(list);
    }
    /**
     * Finds the point on this poly-line that is closest to the specified
     * point.
     * 
     * @param point the point to compare
     * 
     * @return the index of the closest point
     */
    public int findClosest(CartCoord2D point) {
        if (this.size() == 0) {
            return -1;
        }
        float minLength = this.get(0).distance(point);
        int index = 0;
        for (int i = 1; i < this.size(); i++) {
            float l = this.get(i).distance(point);
            if (l < minLength) {
                index = i;
                minLength = l;
            }
        }
        return index;
    }
}
