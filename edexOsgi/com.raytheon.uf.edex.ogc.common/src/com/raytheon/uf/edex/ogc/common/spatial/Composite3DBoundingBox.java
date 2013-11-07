/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.spatial;

import org.geotools.geometry.jts.ReferencedEnvelope;


/**
 * 3D bounding box composed of 2D referenced envelope for horizontal bounds and
 * vertical coordinate for vertical bounds
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class Composite3DBoundingBox {

    private final ReferencedEnvelope horizontal;

    private final String native2DCrsUrn;

    private final VerticalCoordinate vertical;

    /**
     * @param horizontal
     */
    public Composite3DBoundingBox(ReferencedEnvelope horizontal) {
        this(horizontal, null, null);
    }

    /**
     * @param horizontal
     * @param native2DCrsUrn
     */
    public Composite3DBoundingBox(ReferencedEnvelope horizontal,
            String native2DCrsUrn) {
        this(horizontal, native2DCrsUrn, null);
    }

    /**
     * @param horizontal
     * @param vertical
     */
    public Composite3DBoundingBox(ReferencedEnvelope horizontal,
            VerticalCoordinate vertical) {
        this(horizontal, null, vertical);
    }


    /**
     * @param horizontal
     * @param native2DCrsUrn
     * @param vertical
     */
    public Composite3DBoundingBox(ReferencedEnvelope horizontal,
            String native2DCrsUrn, VerticalCoordinate vertical) {
        this.horizontal = horizontal;
        this.native2DCrsUrn = native2DCrsUrn;
        this.vertical = vertical;
    }

    /**
     * @return the horizontal bbox
     */
    public ReferencedEnvelope getHorizontal() {
        return horizontal;
    }

    /**
     * @return the vertical
     */
    public VerticalCoordinate getVertical() {
        return vertical;
    }

    /**
     * @return the native2DCrsUrn
     */
    public String getNative2DCrsUrn() {
        return native2DCrsUrn;
    }

    /**
     * @return true if bounding box has vertical component
     */
    public boolean hasVertical() {
        return vertical != null;
    }

    /**
     * @return true if horizontal CRS has a native URN
     */
    public boolean hasNative2DCrs() {
        return native2DCrsUrn != null;
    }

}
