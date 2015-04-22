
package gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3;

import java.io.DataInputStream;
import java.io.IOException;

import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Abstract class that defines the binary values for the various symbology
 * layers that are sent out in Level III format.
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

public abstract class SymbologyPacket {

    @DynamicSerializeElement
    protected int packetId;

    public SymbologyPacket(int packetId, DataInputStream in) throws IOException {
        this.packetId = packetId;
        init(in);
    }

    protected SymbologyPacket() {

    }

    protected abstract void init(DataInputStream in) throws IOException;

    /**
     * @return the packetId
     */
    public int getPacketId() {
        return packetId;
    }

    /**
     * @param packetId
     *            the packetId to set
     */
    public void setPacketId(int packetId) {
        this.packetId = packetId;
    }

    @Override
    public String toString() {
        String s = String.format("\tPacket ID: 0x%04X", packetId);
        return s;
    }

}
