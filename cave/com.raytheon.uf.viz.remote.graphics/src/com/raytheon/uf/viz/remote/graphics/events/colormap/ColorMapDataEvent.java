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

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapData;
import com.raytheon.uf.viz.remote.graphics.Activator;
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
    private byte[] serializedColorMapData;

    /**
     * @return the serializedColorMapData
     */
    public byte[] getSerializedColorMapData() {
        return serializedColorMapData;
    }

    /**
     * @param serializedColorMapData
     *            the serializedColorMapData to set
     */
    public void setSerializedColorMapData(byte[] serializedColorMapData) {
        this.serializedColorMapData = serializedColorMapData;
    }

    /**
     * @return the colorMapData
     */
    public ColorMapData getColorMapData() {
        if (serializedColorMapData != null) {
            try {
                ColorMapDataWrapper wrapper = (ColorMapDataWrapper) SerializationUtil
                        .transformFromThrift(serializedColorMapData);
                return wrapper.getColorMapData();
            } catch (SerializationException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
        return null;
    }

    /**
     * @param colorMapData
     *            the colorMapData to set
     */
    public void setColorMapData(ColorMapData colorMapData) {
        ColorMapDataWrapper wrapper = new ColorMapDataWrapper();
        wrapper.setColorMapData(colorMapData);
        try {
            serializedColorMapData = SerializationUtil
                    .transformToThrift(wrapper);
        } catch (SerializationException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

}
