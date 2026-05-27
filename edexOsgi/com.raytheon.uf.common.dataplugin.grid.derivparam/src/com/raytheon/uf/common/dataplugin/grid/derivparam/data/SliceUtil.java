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
package com.raytheon.uf.common.dataplugin.grid.derivparam.data;

import java.awt.Point;

import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;

/**
 * Utility methods for slicing
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------
 * Nov 29, 2007  559      njensen   Initial creation
 * Jan 14, 2008  688      njensen   Fixed bresenham
 * Jan 26, 2011  7988     njensen   Broke apart into multiple classes
 * Mar 22, 2016  5439     bsteffen  Move to common
 * 
 * </pre>
 * 
 * @author njensen
 */

public class SliceUtil {

    /**
     * Slice an entire grid of data according to request
     * 
     * @param fdr
     * @param req
     * @return a slice
     */
    public static FloatDataRecord slice(FloatDataRecord fdr, Request req) {
        long[] dims = fdr.getSizes();
        float[] data = fdr.getFloatData();
        long[] newDims;
        float[] newData;
        switch (req.getType()) {
        case SLAB:
            int minX = req.getMinIndexForSlab()[0];
            int maxX = req.getMaxIndexForSlab()[0];
            int minY = req.getMinIndexForSlab()[1];
            int maxY = req.getMaxIndexForSlab()[1];

            newDims = new long[2];
            newDims[0] = maxX - minX;
            newDims[1] = maxY - minY;
            newData = new float[(int) ((newDims[0]) * newDims[1])];
            for (int i = minY; i < maxY; i++) {
                int srcIndex = (int) (i * dims[0] + minX);
                int destIndex = (int) ((i - minY) * newDims[0]);
                System.arraycopy(data, srcIndex, newData, destIndex,
                        (int) newDims[0]);
            }
            break;
        case POINT:
            Point[] points = req.getPoints();
            newDims = new long[] { 1, points.length };
            newData = new float[(int) ((newDims[0]) * newDims[1])];
            for (int i = 0; i < points.length; i++) {
                int index = (int) (points[i].y * dims[0] + points[i].x);
                newData[i] = data[index];
            }
            break;
        case XLINE:
            int[] yIndices = req.getIndices();
            newDims = new long[] { dims[0], yIndices.length };
            newData = new float[(int) ((newDims[0]) * newDims[1])];
            for (int i = 0; i < yIndices.length; i++) {
                int srcIndex = (int) (yIndices[i] * dims[0]);
                int destIndex = (int) (i * newDims[0]);
                System.arraycopy(data, srcIndex, newData, destIndex,
                        (int) newDims[0]);
            }
            break;
        case YLINE:
            int[] xIndices = req.getIndices();
            newDims = new long[] { xIndices.length, dims[1] };
            newData = new float[(int) ((newDims[0]) * newDims[1])];
            int c = 0;
            for (int i = 0; i < dims[1]; i++) {
                for (int j : xIndices) {
                    int index = (int) (j * dims[0] + i);
                    newData[c++] = data[index];
                }

            }
            break;
        case ALL:
        default:
            return fdr;
        }
        fdr = (FloatDataRecord) fdr.clone();
        fdr.setSizes(newDims);
        fdr.setFloatData(newData);
        return fdr;

    }
}
