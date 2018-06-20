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
package com.raytheon.uf.common.dataplugin.bufrncwf;

import java.io.Serializable;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Feature for NCWF
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * --/--/----              jkorman     Initial creation
 * Oct 15, 2013 2361       njensen     Remove XML annotations
 * May 12, 2014 3133       njensen     Remove dependency on DecoderTools
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
public class NCWFFeature implements Serializable {

    private static final long serialVersionUID = 1L;

    @DynamicSerializeElement
    private Coordinate centroidLocation;

    @DynamicSerializeElement
    List<Coordinate> featureBoundary;

    public NCWFFeature() {
    }

    /**
     * 
     * @param lat
     *            Feature centroid latitude.
     * @param lon
     *            Feature centroid longitude.
     */
    public NCWFFeature(Double lat, Double lon) {
        centroidLocation = new Coordinate(lon, lat);
    }

    /**
     * @return the centroidLocation
     */
    public Coordinate getCentroidLocation() {
        return centroidLocation;
    }

    /**
     * 
     * @return
     */
    public double getCentroidLatitude() {
        return centroidLocation.y;
    }

    /**
     * 
     * @return
     */
    public double getCentroidLongitude() {
        return centroidLocation.x;
    }

    /**
     * @param centroidLocation
     *            the centroidLocation to set
     */
    public void setCentroidLocation(Coordinate centroidLocation) {
        this.centroidLocation = centroidLocation;
    }

    /**
     * @return the featureBoundary
     */
    public List<Coordinate> getFeatureBoundary() {
        return featureBoundary;
    }

    /**
     * @param featureBoundary
     *            the featureBoundary to set
     */
    public void setFeatureBoundary(List<Coordinate> featureBoundary) {
        this.featureBoundary = featureBoundary;
    }

    /**
     * 
     * @return
     */
    public Iterator<Coordinate> boundaryIterator() {
        Iterator<Coordinate> it = null;
        if (featureBoundary != null) {
            it = getFeatureBoundary().iterator();
        } else {
            // No data so create an empty Iterator.
            it = new Iterator<Coordinate>() {
                @Override
                public boolean hasNext() {
                    return false;
                }

                @Override
                public Coordinate next() {
                    return null;
                }

                @Override
                public void remove() {
                }
            };
        }
        return it;
    }
}
