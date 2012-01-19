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
package com.raytheon.viz.core.contours.util;

/**
 * Bidirectional x/y point buffer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2010 #4583      rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class PointBuffer {
    private float[] buffer;

    private boolean forwardDirection = true;

    // location of the next write
    private int index = 0;

    private int size = 0;

    // inclusive
    private int minIndex = 0;

    private float xOffset;

    private float yOffset;

    /**
     * 
     * @param capacity
     *            Capacity in x,y points
     * @param initialIndex
     * @param forwardDirection
     */
    public PointBuffer(int capacity, int initialIndex, boolean forwardDirection) {
        buffer = new float[capacity * 2];
        minIndex = initialIndex;
        index = initialIndex;
    }

    public void setDirection(boolean forward) {
        forwardDirection = forward;
    }

    public void setSize(int size) {
        this.size = size;
    }

    public void setNextWriteIndex(int index) {
        this.index = index;
        if (size == 0) {
            minIndex = this.index;
        }

        // not reseting size, will happen automatically first time data is
        // written
    }

    public void setXOffset(float offset) {
        xOffset = offset;
    }

    public void setYOffset(float offset) {
        yOffset = offset;
    }

    /**
     * 
     * @param numberToAdd
     *            number of vals to add to the buffer. Ex: for a single x/y
     *            point this should be 2
     */
    protected void ensureCapacityFor(int numberToAdd) {
        if (forwardDirection) {
            int minSize = index + numberToAdd;
            if (minSize >= buffer.length) {
                int newSize = buffer.length * 2;
                if (newSize < minSize) {
                    newSize = minSize;
                }

                float[] tmp = new float[newSize];
                System.arraycopy(buffer, minIndex, tmp, minIndex, size);
                buffer = tmp;
            }
        } else {
            int numNeeded = numberToAdd - index;
            if (numNeeded > 0) {
                int newSize = buffer.length * 2;
                if (newSize - buffer.length < numNeeded) {
                    newSize += numNeeded;
                }

                int sizeOffset = newSize - buffer.length;
                int newMinIndex = minIndex + sizeOffset;
                float[] tmp = new float[newSize];
                System.arraycopy(buffer, minIndex, tmp, newMinIndex, size);
                buffer = tmp;
                minIndex = newMinIndex;
                index += sizeOffset;
            }
        }
    }

    public void put(float x, float y) {
        ensureCapacityFor(2);
        buffer[index++] = x + xOffset;
        buffer[index++] = y + yOffset;

        if (forwardDirection) {
            if (index > minIndex + size) {
                size = index - minIndex;
            }
        } else {
            index -= 4;
            if (index < minIndex) {
                int oldIndex = minIndex;
                minIndex = index + 2;
                size += oldIndex - minIndex;
            }
        }
    }

    public int capacity() {
        return buffer.length;
    }

    public int size() {
        return size;
    }

    public float[] getPoints() {
        float[] rval = new float[size];
        System.arraycopy(buffer, minIndex, rval, 0, size);
        return rval;
    }

    public int getMinIndex() {
        return minIndex;
    }
}
