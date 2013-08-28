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

import java.util.Arrays;

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
public class CfDimensions<T extends NcDimension> {

    private final CfHorizontalDims<T> xyDims;

    private final T zDim;

    private final T tDim;

    /**
     * @param xyDims
     * @param zDim
     * @param tDim
     */
    public CfDimensions(CfHorizontalDims<T> xyDims, T zDim, T tDim) {
        this.xyDims = xyDims;
        this.zDim = zDim;
        this.tDim = tDim;
    }

    /**
     * @return the xyDims
     */
    public CfHorizontalDims<T> getXyDims() {
        return xyDims;
    }

    /**
     * @return the zDim
     */
    public T getzDim() {
        return zDim;
    }

    /**
     * @return the tDim
     */
    public T gettDim() {
        return tDim;
    }

    @SuppressWarnings("unchecked")
    public T[] toArray() {
        T[] a = (T[]) java.lang.reflect.Array
                .newInstance(zDim.getClass(), 4);
        fill(a);
        return a;
    }

    public T[] toArray(T[] a) {
        if (a.length < 4) {
            a = Arrays.copyOf(a, 4);
        }
        fill(a);
        return a;
    }

    private void fill(T[] a) {
        a[0] = tDim;
        a[1] = zDim;
        a[2] = xyDims.getY();
        a[3] = xyDims.getX();
    }

}
