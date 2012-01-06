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

package com.raytheon.uf.viz.core.geom;

/**
 * This class defines a Ray
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  
 * </pre>
 * 
 * @author estrabal
 * @version 1
 */
import javax.vecmath.Vector3d;

public class Ray {
    public Vector3d origin = new Vector3d();

    /**
     * 
     */
    public Vector3d direction = new Vector3d();

    /**
     * 
     * @param origin
     * @param dir
     */
    public Ray(Vector3d origin, Vector3d dir) {
        this.origin = origin;
        this.direction = dir;
    }
}
