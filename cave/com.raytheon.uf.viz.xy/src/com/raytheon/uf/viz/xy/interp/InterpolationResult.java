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
package com.raytheon.uf.viz.xy.interp;

import org.geotools.coverage.grid.GridGeometry2D;

/**
 * Result from an {@link IInterpolation}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------
 * Feb 03, 2009           njensen   Initial creation
 * Nov 08, 2016  5976     bsteffen  Move to viz.xy plugin
 * 
 * </pre>
 * 
 * @author njensen
 */
public class InterpolationResult {

    private GridGeometry2D geometry;

    private float[] values;

    /**
     * @return the geometry
     */
    public GridGeometry2D getGeometry() {
        return geometry;
    }

    /**
     * @param geometry
     *            the geometry to set
     */
    public void setGeometry(GridGeometry2D geometry) {
        this.geometry = geometry;
    }

    /**
     * @return the values
     */
    public float[] getValues() {
        return values;
    }

    /**
     * @param values
     *            the values to set
     */
    public void setValues(float[] values) {
        this.values = values;
    }

}
