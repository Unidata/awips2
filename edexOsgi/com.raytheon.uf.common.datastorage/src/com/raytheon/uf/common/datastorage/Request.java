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
package com.raytheon.uf.common.datastorage;

import java.awt.Point;
import java.util.Arrays;
import java.util.LinkedHashSet;


import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents the style of request to perform (whole dataset, line, slab,
 * points, etc.)
 * 
 * To retrieve a whole dataset, use Request.ALL.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2009            chammack     Initial creation
 * Jun 18, 2013 DR 15662   dhuffman     Cross section terrain disappears if baseline is too short.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@DynamicSerialize
public class Request implements ISerializableObject {

    @DynamicSerializeElement
    private Point[] points;

    @DynamicSerializeElement
    private int[] indices;

    @DynamicSerializeElement
    private int[] minIndexForSlab;

    @DynamicSerializeElement
    private int[] maxIndexForSlab;

    @DynamicSerializeElement
    private Type type;

    public static enum Type {
        POINT, XLINE, YLINE, SLAB, ALL
    };

    public static final Request ALL;

    static {
        ALL = new Request();
        ALL.type = Type.ALL;
    }

    /**
     * Do NOT use this, only added for dynamic serialization
     */
    public Request() {

    }

    /**
     * Build a request that asks for specific points to be returned
     * 
     * @param points
     * @return
     */
    public static Request buildPointRequest(Point... points) {
        Request request = new Request();
        request.type = Type.POINT;
        request.points = new LinkedHashSet<Point>(Arrays.asList(points))
                .toArray(new Point[points.length]);	

        return request;
    }
    
    /**
     * Build a request that asks for specific cross points to be returned
     * 
     * @param points
     * @return
     */
    public static Request buildXsectPointRequest(Point... points) {
    	Request request = new Request();
    	request.type = Type.POINT;
    	request.points = new Point[points.length];
    	for(int x=0; x<points.length; x++)
    		request.points[x] = new Point(points[x]);
    
    	return request;
    }

    /**
     * Build a request that asks for all x values at a provided set of y values.
     * 
     * IMPORTANT NOTE: The results are not guaranteed to be in the same order as
     * the indices. The results will be returned in monotonically increasing
     * order of the index.
     * 
     * @param yIndices
     * @return
     */
    public static Request buildXLineRequest(int[] yIndices) {
        Request request = new Request();
        request.type = Type.XLINE;
        request.indices = yIndices;
        Arrays.sort(yIndices);
        return request;
    }

    /**
     * Build a request that asks for all y values at a provided set of x values.
     * 
     * IMPORTANT NOTE: The results are not guaranteed to be in the same order as
     * the indices. The results will be returned in monotonically increasing
     * order of the index.
     * 
     * @param xIndices
     * @return
     */
    public static Request buildYLineRequest(int[] xIndices) {
        Request request = new Request();
        request.type = Type.YLINE;
        request.indices = xIndices;
        Arrays.sort(request.indices);
        return request;
    }

    /**
     * Perform a hyperslab request (effectively a rectangle in 2d space)
     * 
     * @param minIndex
     * @param maxIndex
     * @return
     */
    public static Request buildSlab(int[] minIndex, int[] maxIndex) {
        Request request = new Request();
        request.type = Type.SLAB;
        request.minIndexForSlab = minIndex;
        request.maxIndexForSlab = maxIndex;
        return request;
    }

    /**
     * @return the points
     */
    public Point[] getPoints() {
        if (points == null) {
            points = new Point[0];
        }

        return points;
    }

    /**
     * @return the indices
     */
    public int[] getIndices() {
        return indices;
    }

    /**
     * @return the type
     */
    public Type getType() {
        return type;
    }

    /**
     * @return the minIndexForSlab
     */
    public int[] getMinIndexForSlab() {
        return minIndexForSlab;
    }

    /**
     * @return the maxIndexForSlab
     */
    public int[] getMaxIndexForSlab() {
        return maxIndexForSlab;
    }

    public void setPoints(Point[] points) {
        this.points = new LinkedHashSet<Point>(Arrays.asList(points))
                .toArray(new Point[points.length]);
    }

    public void setIndices(int[] indices) {
        this.indices = indices;
    }

    public void setMinIndexForSlab(int[] minIndexForSlab) {
        this.minIndexForSlab = minIndexForSlab;
    }

    public void setMaxIndexForSlab(int[] maxIndexForSlab) {
        this.maxIndexForSlab = maxIndexForSlab;
    }

    public void setType(Type type) {
        this.type = type;
    }

    /**
     * Perform a shallow copy into this object
     * 
     * @param request
     */
    public void copyFrom(Request request) {
        this.indices = request.indices;
        this.maxIndexForSlab = request.maxIndexForSlab;
        this.minIndexForSlab = request.minIndexForSlab;
        this.points = request.points;
        this.type = request.type;
    }

    @Override
    public String toString() {
        String str = getType().toString() + ":";
        switch (getType()) {
        case POINT: {
            for (Point p : points) {
                str += p.toString() + ":";
            }
            break;
        }
        case SLAB: {
            str += "[[";
            for (int i = 0; i < minIndexForSlab.length; ++i) {
                if (i > 0) {
                    str += ",";
                }
                str += minIndexForSlab[i];
            }
            str += "],[";
            for (int i = 0; i < maxIndexForSlab.length; ++i) {
                if (i > 0) {
                    str += ",";
                }
                str += maxIndexForSlab[i];
            }
            str += "]]";
            break;
        }
        case XLINE:
        case YLINE: {
            str += "[";
            for (int i = 0; i < indices.length; ++i) {
                if (i > 0) {
                    str += ",";
                }
                str += indices[i];
            }
            str += "]";
            break;
        }
        }
        return str;
    }

    @Override
    public int hashCode() {
        return toString().hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        return toString().equals(obj.toString());
    }

}
