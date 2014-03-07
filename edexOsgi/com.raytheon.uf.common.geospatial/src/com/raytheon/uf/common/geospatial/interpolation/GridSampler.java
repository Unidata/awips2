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
package com.raytheon.uf.common.geospatial.interpolation;

import com.raytheon.uf.common.numeric.source.DataSource;

/**
 * Used for sampling data values from a source using an interpolation. It can be
 * more convenient to use a sampler rather than a interpolation and a source
 * seperatly.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridSampler {

    private Interpolation interpolation;

    private DataSource source;

    public GridSampler(Interpolation interpolation) {
        this.interpolation = interpolation;
    }

    public GridSampler(DataSource source, Interpolation interpolation) {
        this.interpolation = interpolation;
        this.source = source;
    }

    public double sample(double x, double y) {
        return interpolation.getInterpolatedValue(source, x, y);
    }

    public void setInterpolation(Interpolation interpolation) {
        this.interpolation = interpolation;
    }

    public void setSource(DataSource source) {
        this.source = source;
    }

}
