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
package com.raytheon.uf.viz.core.data.prep;

import java.awt.Rectangle;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;

import com.raytheon.uf.viz.core.data.BufferSlicer;
import com.raytheon.uf.viz.core.data.IColormappedDataPreparer;
import com.raytheon.uf.viz.core.data.IDataPreparer;
import com.raytheon.uf.viz.core.data.resp.NumericImageData;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * DEPRECATED: Construct IColorMapDataRetrievalCallback instead of using this to
 * create the deprecated IDataPreparer with the IColormappedImageExtension
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@Deprecated
public class CMDataPreparerManager {

    /**
     * DEPRECATED: Construct IColorMapDataRetrievalCallback instead of using
     * this to create the deprecated IDataPreparer with the
     * IColormappedImageExtension
     * 
     * @param data
     * @param datasetBounds
     * @param totalDataSetDimensions
     * @return
     */
    @Deprecated
    public static IDataPreparer getDataPreparer(Object data,
            final Rectangle datasetBounds, final int[] totalDataSetDimensions) {
        IDataPreparer preparer = null;

        // Wrap primitive arrays in a Buffer
        if (data instanceof float[]) {
            data = FloatBuffer.wrap((float[]) data);
        } else if (data instanceof byte[]) {
            data = ByteBuffer.wrap((byte[]) data);
        } else if (data instanceof int[]) {
            data = IntBuffer.wrap((int[]) data);
        } else if (data instanceof short[]) {
            data = ShortBuffer.wrap((short[]) data);
        }

        if (data instanceof Buffer) {
            if (totalDataSetDimensions != null) {
                // If total dimension set specified, slice data
                data = BufferSlicer.slice((Buffer) data, datasetBounds,
                        new Rectangle(0, 0, totalDataSetDimensions[0],
                                totalDataSetDimensions[1]));
            }
            final Buffer[] buffer = new Buffer[] { (Buffer) data };
            preparer = new IColormappedDataPreparer() {
                @Override
                public NumericImageData prepareData() throws VizException {
                    Buffer buf = buffer[0];
                    buffer[0] = null;
                    return new NumericImageData(buf, datasetBounds,
                            datasetBounds.height * datasetBounds.width);
                }
            };
        }
        return preparer;
    }
}
