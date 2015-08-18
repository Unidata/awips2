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
package com.raytheon.uf.viz.drawables.triangulated;

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Contains useful function for working with triangles.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 19, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class TriangleMath {

    /**
     * Determine if the point p is in the triangle formed by vertices v0,v1, v2.
     */
    public static boolean isInTriangle(Coordinate p, Coordinate v0,
            Coordinate v1, Coordinate v2) {
        int orientation0 = CGAlgorithms.orientationIndex(v0, v1, p);
        if (orientation0 == 0) {
            return true;
        }
        int orientation1 = CGAlgorithms.orientationIndex(v1, v2, p);
        if (orientation0 != orientation1) {
            if (orientation1 == 0) {
                return true;
            } else {
                return false;
            }
        }
        int orientation2 = CGAlgorithms.orientationIndex(v2, v0, p);
        if (orientation0 != orientation2) {
            if (orientation1 == 0) {
                return true;
            } else {
                return false;
            }
        }
        return true;
    }
}
