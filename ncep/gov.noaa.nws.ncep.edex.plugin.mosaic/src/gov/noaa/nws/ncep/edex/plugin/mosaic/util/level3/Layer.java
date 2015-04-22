
package gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Layer is a class that will allow access to the actual radar data
 * contained in a symbology layer.
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 09/2009      143				L. Lin     	Initial coding
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

@DynamicSerialize
public class Layer implements ISerializableObject {
    @DynamicSerializeElement
    private int layerId;

    @DynamicSerializeElement
    SymbologyPacket[] packets;

    /**
     * @return the layerId
     */
    public int getLayerId() {
        return layerId;
    }

    /**
     * @param layerId
     *            the layerId to set
     */
    public void setLayerId(int layerId) {
        this.layerId = layerId;
    }

    /**
     * @return the packets
     */
    public SymbologyPacket[] getPackets() {
        return packets;
    }

    /**
     * @param packets
     *            the packets to set
     */
    public void setPackets(SymbologyPacket[] packets) {
        this.packets = packets;
    }

}