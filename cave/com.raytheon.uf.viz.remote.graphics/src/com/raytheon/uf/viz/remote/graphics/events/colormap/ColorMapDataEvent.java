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

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
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
 * Mar 09, 2012            mschenke    Initial creation
 * Apr 07, 2014 2968       njensen     Improved efficiency
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
    private ColorMapData.ColorMapDataType dataType;

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
        // serializing buffers is thread safe now so just directly use it
        this.buffer = colorMapData.getBuffer();
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
    public ColorMapData.ColorMapDataType getDataType() {
        return dataType;
    }

    /**
     * @param dataType
     *            the dataType to set
     */
    public void setDataType(ColorMapData.ColorMapDataType dataType) {
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
