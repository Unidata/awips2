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
package com.raytheon.viz.core.contours.cache;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.IExtent;

/**
 * Key for a cache for an envelope of a subgrid
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 4, 2011   7747   njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SubGridCacheKey {

    private IExtent extent;

    private GeneralGridGeometry mapGridGeometry;

    private GeneralGridGeometry imageGridGeometry;

    public SubGridCacheKey(IExtent extent, GeneralGridGeometry mapGridGeometry,
            GeneralGridGeometry imageGridGeometry) {
        this.extent = extent;
        this.mapGridGeometry = mapGridGeometry;
        this.imageGridGeometry = imageGridGeometry;
    }

    public IExtent getExtent() {
        return extent;
    }

    public void setExtent(IExtent extent) {
        this.extent = extent;
    }

    public GeneralGridGeometry getMapGridGeometry() {
        return mapGridGeometry;
    }

    public void setMapGridGeometry(GeneralGridGeometry mapGridGeometry) {
        this.mapGridGeometry = mapGridGeometry;
    }

    public GeneralGridGeometry getImageGridGeometry() {
        return imageGridGeometry;
    }

    public void setImageGridGeometry(GeneralGridGeometry imageGridGeometry) {
        this.imageGridGeometry = imageGridGeometry;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((extent == null) ? 0 : extent.hashCode());
        result = prime
                * result
                + ((imageGridGeometry == null) ? 0 : imageGridGeometry
                        .hashCode());
        result = prime * result
                + ((mapGridGeometry == null) ? 0 : mapGridGeometry.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SubGridCacheKey other = (SubGridCacheKey) obj;
        if (extent == null) {
            if (other.extent != null)
                return false;
        } else if (!extent.equals(other.extent))
            return false;
        if (imageGridGeometry == null) {
            if (other.imageGridGeometry != null)
                return false;
        } else if (!imageGridGeometry.equals(other.imageGridGeometry))
            return false;
        if (mapGridGeometry == null) {
            if (other.mapGridGeometry != null)
                return false;
        } else if (!mapGridGeometry.equals(other.mapGridGeometry))
            return false;
        return true;
    }

}
