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
package com.raytheon.uf.edex.plugin.goesr.geospatial.envelope;

import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;

/**
 * 
 * Defines the non-crs attributes needed to construct a {@link SatMapCoverage}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 17, 2015  4336     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GoesrEnvelope {

    private double minX;

    private double minY;

    private Integer nx;

    private Integer ny;

    private double dx;

    private double dy;

    public double getMinX() {
        return minX;
    }

    public void setMinX(double minX) {
        this.minX = minX;
    }

    public double getMinY() {
        return minY;
    }

    public void setMinY(double minY) {
        this.minY = minY;
    }

    public Integer getNx() {
        return nx;
    }

    public void setNx(Integer nx) {
        this.nx = nx;
    }

    public Integer getNy() {
        return ny;
    }

    public void setNy(Integer ny) {
        this.ny = ny;
    }

    public double getDx() {
        return dx;
    }

    public void setDx(double dx) {
        this.dx = dx;
    }

    public double getDy() {
        return dy;
    }

    public void setDy(double dy) {
        this.dy = dy;
    }

    public double getWidth() {
        return nx * dx;
    }

    public double getHeight() {
        return ny * dy;
    }

    public void normalize() {
        if (dx < 0) {
            minX = minX + dx * nx;
            dx = -1 * dx;
        }
        if (dy < 0) {
            minY = minY + dy * ny;
            dy = -1 * dy;
        }
    }

    @Override
    public String toString() {
        return "GoesrEnvelope [minX=" + minX + ", minY=" + minY + ", nx=" + nx
                + ", ny=" + ny + ", dx=" + dx + ", dy=" + dy + "]";
    }

}
