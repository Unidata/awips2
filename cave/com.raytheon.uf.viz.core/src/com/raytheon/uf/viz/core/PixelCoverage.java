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

package com.raytheon.uf.viz.core;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * <pre>
 * 
 *   SOFTWARE HISTORY
 *  
 *   Date           Ticket#     Engineer    Description
 *   ------------   ----------  ----------- --------------------------
 *   8/15/06        chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class PixelCoverage {

    private Coordinate ul;

    private Coordinate ll;

    private Coordinate ur;

    private Coordinate lr;

    private IExtent extent;

    private IMesh mesh;

    public PixelCoverage(Coordinate ul, Coordinate ur, Coordinate lr,
            Coordinate ll) {
        this.ul = ul;
        this.ur = ur;
        this.lr = lr;
        this.ll = ll;

        if (Double.isNaN(ul.z)) {
            ul.z = 0.0;
            ur.z = 0.0;
            lr.z = 0.0;
            ll.z = 0.0;
        }

        extent = new PixelExtent(new Coordinate[] { ul, ur, lr, ll });
    }

    public PixelCoverage(Coordinate center, double width, double height) {
        this(new Coordinate(center.x - width / 2, center.y - height / 2),
                new Coordinate(center.x + width / 2, center.y - height / 2),
                new Coordinate(center.x + width / 2, center.y + height / 2),
                new Coordinate(center.x - width / 2, center.y + height / 2));
    }

    public PixelCoverage(IMesh mesh) {
        this(new Coordinate(0, 0), 0, 0);
        this.mesh = mesh;
    }

    public Coordinate getLl() {
        return ll;
    }

    public Coordinate getLr() {
        return lr;
    }

    public Coordinate getUl() {
        return ul;
    }

    public Coordinate getUr() {
        return ur;
    }

    public float getMaxX() {
        return (float) Math.max(ur.x, lr.x);
    }

    public float getMinX() {
        return (float) Math.min(ll.x, ul.x);
    }

    public float getMinY() {
        return (float) Math.min(ur.y, ul.y);
    }

    public float getMaxY() {
        return (float) Math.max(lr.y, ll.y);
    }

    public IMesh getMesh() {
        return mesh;
    }

    public void setMesh(IMesh mesh) {
        this.mesh = mesh;
    }

    /**
     * Dispose any references
     * 
     */
    public void dispose() {
        if (mesh != null) {
            mesh.dispose();
            mesh = null;
        }
    }

    /**
     * Check to see if the coverage intersects an extent
     * 
     * @param extent
     * @return
     */
    public boolean intersects(IExtent extent) {
        return this.extent.intersects(extent);
    }

    /**
     * Get the extent of the coverage
     * 
     * @return
     */
    public IExtent getExtent() {
        return extent;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "PIXELCOVERAGE { UL = " + this.ul.toString() + " UR= "
                + this.ur.toString() + " LL = " + this.ll.toString() + " LR = "
                + this.lr.toString() + " }";
    }

}
