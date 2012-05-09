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
package com.raytheon.uf.viz.remote.graphics.events.colormap;

import java.nio.Buffer;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapData;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapDataType;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;

/**
 * Event for sending ColorMapData, serializes object immediately to avoid
 * concurrency issues with other threads using the buffer async
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class ColorMapDataEvent extends AbstractDispatchingObjectEvent {

    @DynamicSerializeElement
    private int[] dimensions;

    @DynamicSerializeElement
    private ColorMapDataType dataType;

    @DynamicSerializeElement
    private Buffer buffer;

    /**
     * @return the colorMapData
     */
    public ColorMapData getColorMapData() {
        return new ColorMapData(buffer, dimensions, dataType);
    }

    /**
     * @param colorMapData
     *            the colorMapData to set
     */
    public void setColorMapData(ColorMapData colorMapData) {
        // Copy data via serialization
        this.dimensions = colorMapData.getDimensions();
        this.dataType = colorMapData.getDataType();
        try {
            // Copy the buffer since it is the same buffer that will be used for
            // rendering in a separate thread and serializing Buffer access is
            // not thread safe
            this.buffer = (Buffer) SerializationUtil
                    .transformFromThrift(SerializationUtil
                            .transformToThrift(colorMapData.getBuffer()));
        } catch (SerializationException e) {
            throw new RuntimeException("Error copying data Buffer: "
                    + e.getLocalizedMessage(), e);
        }
    }

    /**
     * @return the dimensions
     */
    public int[] getDimensions() {
        return dimensions;
    }

    /**
     * @param dimensions
     *            the dimensions to set
     */
    public void setDimensions(int[] dimensions) {
        this.dimensions = dimensions;
    }

    /**
     * @return the dataType
     */
    public ColorMapDataType getDataType() {
        return dataType;
    }

    /**
     * @param dataType
     *            the dataType to set
     */
    public void setDataType(ColorMapDataType dataType) {
        this.dataType = dataType;
    }

    /**
     * @return the buffer
     */
    public Buffer getBuffer() {
        return buffer;
    }

    /**
     * @param buffer
     *            the buffer to set
     */
    public void setBuffer(Buffer buffer) {
        this.buffer = buffer;
    }

}
