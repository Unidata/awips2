
package gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * SymbologyBlock creates an object that will allow the iteration over 1 to 15
 * layers contains within the symbology block.
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
public class SymbologyBlock extends AbstractBlock implements
        ISerializableObject {

    private static final int BLOCK_ID = 1;

    public static int getBlockId() {
        return BLOCK_ID;
    }

    @DynamicSerializeElement
    private Layer[] layers;

    /**
     * @param in
     * @throws IOException
     */
    public SymbologyBlock(DataInputStream in) throws IOException {
        super(in);
    }

    public SymbologyBlock() {

    }

    /**
     * @return the layers
     */
    public Layer[] getLayers() {
        return layers;
    }

    /**
     * @param layers
     *            the layers to set
     */
    public void setLayers(Layer[] layers) {
        this.layers = layers;
    }

    /**
     * Parses the symbology block header for all the important information.
     * 
     * @throws IOException
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
    	//numLayers starts at 65th half-word
        int numLayers = in.readShort();
        List<Layer> layerList = new ArrayList<Layer>();

        in.skip(2); // skip first layer divider
        NEXT_LAYER: for (int i = 0; i < 1; i++) {
            Layer layer = new Layer();
            layer.setLayerId(i);
            List<SymbologyPacket> packet = new ArrayList<SymbologyPacket>();
            int layerSize = in.readInt();
            in.mark(layerSize);

            int packetId;
            do {
            	//Raster Opcode at 69th half-word - packetId is the Opcode
                packetId = in.readUnsignedShort();

                if (packetId == 0xFFFF || packetId == 0x0000) {
                    break; // found layer divider break out of packet loop
                }
                
                SymbologyPacket symPacket = PacketFactory.createPacket(
                        packetId, in);

                if (symPacket != null) {
                    packet.add(symPacket);
                } else {
                    in.reset();
                    in.skip(layerSize);
                    if (in.available() >= 2) {
                        in.skip(2); // skip next layer divider
                    }
                    continue NEXT_LAYER;
                }
            } while (in.available() >= 2);
            
            layer.setPackets(packet.toArray(new SymbologyPacket[packet
                            .size()]));
            layerList.add(layer);
        }

        this.layers = layerList.toArray(new Layer[layerList.size()]);
    }

    /**
     * This method will return an object representing the layer number passed
     * in. This method will examine the layer and create and instance of its
     * actual type (i.e. Radial) and return that layer.<br/>
     * <br/>
     * <b>Important</b> Presently, only radial data is supported. All others
     * will return null.
     * 
     * @return An object for that layer
     * @throws IOException
     */
    public SymbologyPacket getSymbologyLayerContent(int layerNumber)
            throws IOException {
        SymbologyPacket symLayer = null;
        return symLayer;
    }

    /**
     * Returns the number of symbology layers contained with the block. Most
     * likely, this will be 1, but in case a file contains more, they can be
     * iterated through.
     * 
     * @return An integer of the number of layers, 1 to 15
     */
    public int getNumLayers() {
        if (layers != null) {
            return layers.length;
        }
        return 0;
    }

    public int getNumPackets(int l) {
        if (layers != null) {
            SymbologyPacket[] layer = layers[l].packets;
            if (layer != null) {
                return layer.length;
            }
        }
        return 0;
    }

    public SymbologyPacket[] getPackets(int layer) {
        return layers[layer].packets;
    }

    public SymbologyPacket getPacket(int layer, int packet) {
        return layers[layer].packets[packet];
    }

    @Override
    public String toString() {
        String s = "";
        if (layers != null) {
            for (int l = 0; l < layers.length; l++) {
                s += "\nLayer " + (l + 1);
                for (SymbologyPacket packet : layers[l].packets) {
                    s += "\n" + packet;
                }
            }
        }
        return s;
    }

}
