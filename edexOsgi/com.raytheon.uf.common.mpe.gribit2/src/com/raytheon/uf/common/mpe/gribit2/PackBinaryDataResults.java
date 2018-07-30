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
package com.raytheon.uf.common.mpe.gribit2;

/**
 * POJO to store the calculated results of a binary data packing operation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2016 4619       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class PackBinaryDataResults {

    private final short[] packedData;

    private final int length;

    private final double scale;

    private final float minimum;

    public PackBinaryDataResults() {
        this(null, 0, Float.NaN, Float.NaN);
    }

    public PackBinaryDataResults(final short[] packedData, final int length,
            final double scale, final float minimum) {
        this.packedData = packedData;
        this.length = length;
        this.scale = scale;
        this.minimum = minimum;
    }

    public short[] getPackedData() {
        return packedData;
    }

    public int getLength() {
        return length;
    }

    public double getScale() {
        return scale;
    }

    public float getMinimum() {
        return minimum;
    }
}
