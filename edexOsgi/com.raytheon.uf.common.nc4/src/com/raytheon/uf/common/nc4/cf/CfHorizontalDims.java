/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.common.nc4.cf;

import com.raytheon.uf.common.nc4.NcDimension;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class CfHorizontalDims<T extends NcDimension> {

    private final T x;

    private final T y;

    private final String coords;

    private final String gridMapping;

    private final boolean mapped;

    public CfHorizontalDims(T x, T y) {
        this(x, y, null, null, false);
    }

    /**
     * @param dims
     * @param coords
     * @param gridMapping
     */
    public CfHorizontalDims(T x, T y, String coords,
            String gridMapping) {
        this(x, y, coords, gridMapping, true);
    }

    private CfHorizontalDims(T x, T y, String coords,
            String gridMapping, boolean mapped) {
        this.x = x;
        this.y = y;
        this.coords = coords;
        this.gridMapping = gridMapping;
        this.mapped = mapped;
    }

    /**
     * @return the x
     */
    public T getX() {
        return x;
    }

    /**
     * @return the y
     */
    public T getY() {
        return y;
    }

    /**
     * @return the coords
     */
    public String getCoords() {
        return coords;
    }

    /**
     * @return the gridMapping
     */
    public String getGridMapping() {
        return gridMapping;
    }

    /**
     * @return the mapped
     */
    public boolean isMapped() {
        return mapped;
    }

}
