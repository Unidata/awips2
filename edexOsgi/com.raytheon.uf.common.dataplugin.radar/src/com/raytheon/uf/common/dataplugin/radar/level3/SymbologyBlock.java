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

package com.raytheon.uf.common.dataplugin.radar.level3;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.TextComponent;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * SymbologyBlock creates an object that will allow the iteration over 1 to 15
 * layers contains within the symbology block.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 02, 2007  465      randerso    Major revision to support full level 3 radar decode
 * Jul 29, 2013  2148     mnash       Refactor registering of packets to Spring
 * Jun 04, 2014  3232     bsteffen    Remove ISerializableObject
 * Apr 25, 2016  18796    jdynina     Implemented text retreival
 * 
 * </pre>
 * 
 * @author Bryan Rockwood
 * @version 1.0
 */
@DynamicSerialize
public class SymbologyBlock extends AbstractBlock {

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
        int numLayers = in.readShort();
        List<Layer> layerList = new ArrayList<>();

        if (in.readShort() == -1) {
            NEXT_LAYER: for (int i = 0; i < numLayers; i++) {
                Layer layer = new Layer();
                layer.setLayerId(i);
                List<SymbologyPacket> packet = new ArrayList<>();
                int layerSize = in.readInt();
                in.mark(layerSize);

                int packetId;
                do {
                    packetId = in.readUnsignedShort();
                    if (packetId == 0xFFFF) {
                        break; // found layer divider break out of packet loop
                    }

                    SymbologyPacket symPacket = PacketFactory.getInstance()
                            .createPacket(packetId, in);

                    if (symPacket != null) {
                        packet.add(symPacket);
                    } else {
                        in.reset();
                        in.skip(layerSize);
                        if (in.available() >= 2) {
                            in.skip(2); // skip next layer divider
                        }
                        layer.setPackets(packet
                                .toArray(new SymbologyPacket[packet.size()]));
                        layerList.add(layer);
                        continue NEXT_LAYER;
                    }
                } while (in.available() >= 2);

                layer.setPackets(packet.toArray(new SymbologyPacket[packet
                        .size()]));
                layerList.add(layer);
            }
        }
        this.layers = layerList.toArray(new Layer[layerList.size()]);
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
    
    public String getString() {
        StringBuilder s = new StringBuilder();

        if (layers != null) {
            for (int l = 0; l < layers.length; l++) {
                for (SymbologyPacket packet : layers[l].packets) {
                    if (packet instanceof GenericDataPacket) {
                        GenericDataPacket currPacket = (GenericDataPacket) packet;
                        List<GenericDataComponent> components =  currPacket.getComponents();

                        for (GenericDataComponent component: components) {
                            if (component instanceof TextComponent) {
                                TextComponent currComp = (TextComponent) component;
                                s.append(currComp.getText());
                            }
                        }
                    }
                }
            }
        }
        return s.toString();
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
