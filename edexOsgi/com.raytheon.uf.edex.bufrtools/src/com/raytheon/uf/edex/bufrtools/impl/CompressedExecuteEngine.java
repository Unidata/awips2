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
package com.raytheon.uf.edex.bufrtools.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.edex.bufrtools.EngineData;
import com.raytheon.uf.edex.bufrtools.ExecuteEngine;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRNullDescriptor;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRReplicationDescriptor;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRSublistDescriptor;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRTableB;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRTableC;
import com.raytheon.uf.edex.bufrtools.exceptions.BUFRDecoderException;
import com.raytheon.uf.edex.bufrtools.io.BUFRBitInputStream;
import com.raytheon.uf.edex.bufrtools.packets.BUFROperatorPacket;
import com.raytheon.uf.edex.bufrtools.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.bufrtools.packets.DataPacketTypes;
import com.raytheon.uf.edex.bufrtools.packets.IBUFRDataPacket;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 20080214            862 jkorman     BUFRMOS implementation changes.
 * 07/2009              55 T. Lee      Added number of bits to skip for Table C
 * 04/2010             208 F. J. Yen   Updated arguments for execute in processTableB
 * 9/16/2014    #3628      mapeters    Moved from uf.edex.decodertools plugin, replaced static imports.
 * 12/14/2015   5166       kbisanz     Update logging to use SLF4J
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class CompressedExecuteEngine extends ExecuteEngine {

    private Logger logger = LoggerFactory.getLogger(getClass());

    /**
     * Construct an instance of a BUFR decode engine for data stored in
     * compressed format. After construction, the processBUFR method is called
     * to actually decode the data.
     * 
     * @param subList
     *            The list of descriptors to execute.
     * @param repCount
     *            Number of times to execute each set of descCount descriptors.
     */
    public CompressedExecuteEngine(List<BUFRDescriptor> subList, int repCount) {
        super(subList, repCount);
    }

    /**
     * 
     * @param bitStrm
     *            The bit input stream to read data from.
     * @return The data read from the input stream.
     */
    protected EngineData processBUFR(BUFRBitInputStream bitStrm) {
        bitStream = bitStrm;

        EngineData data = new EngineData();

        List<IBUFRDataPacket> packetData = new ArrayList<IBUFRDataPacket>();

        // We need to visit each descriptor, repetitionCount times
        for (BUFRDescriptor descriptor : descriptors) {
            if (logger.isDebugEnabled()) {

                logger.debug(descriptor.getStringDescriptor());
            }
            try {
                if (descriptor instanceof BUFRTableB) {
                    processTableB((BUFRTableB) descriptor, packetData);
                    readOverChar = -1;
                    readOverBits = -1;
                } else if (descriptor instanceof BUFRTableC) {
                    processTableC(descriptor, packetData);
                    if (readOverChar > 0) {
                        for (int i = 0; i < repetitionCount; i++) {
                            bitStrm.read(readOverChar * 8);
                        }
                        readOverChar = -1;
                    }
                } else if (descriptor instanceof BUFRReplicationDescriptor) {
                    executeReplication(descriptor, packetData);
                } else if (descriptor instanceof BUFRNullDescriptor) {
                    // We can hit this if the Table B descriptor is not in
                    // the descriptor table.
                    if (readOverChar > 0) {
                        bitStrm.read(readOverChar * 8);
                        readOverChar = -1;
                    }
                    if (readOverBits > 0) {
                        ((BUFRNullDescriptor) descriptor)
                                .setBitWidth(readOverBits);
                        processUnknown(descriptor, packetData);
                        readOverBits = -1;
                    }
                }
            } catch (BUFRDecoderException bde) {
                if (logger.isDebugEnabled()) {
                    logger.debug("Error processing " + descriptor);
                    display(packetData, "", logger);
                }
                // throw bde;
                break;
            }

        } // for()
          // package all of the data into a SubListPacket
        BUFRSublistPacket p = new BUFRSublistPacket(packetData,
                DataPacketTypes.SubSetList.getPacketType(),
                new BUFRSublistDescriptor(
                        BUFRSublistDescriptor.SUBSET_LIST_DESC, 0));
        data.addPacket(p);

        if (documentRoot) {
            data = normalizeData(data);
        }

        return data;
    }

    /**
     * Process a Table B descriptor.
     * 
     * @param descriptor
     *            The Table B descriptor to execute.
     * @param packets
     *            List of data packets to receive the decoded data.
     */
    private List<IBUFRDataPacket> processTableB(BUFRTableB descriptor,
            List<IBUFRDataPacket> packets) {
        // This will be the compression minimum value
        // changed made by NCEP/NCO/SIB for Tamdar.
        //
        // NCEP/NCO/SIB added boolean argument for compressed flag and
        // repetition count
        // (number of subsets) to method execute. (4/2010)
        IBUFRDataPacket packet = descriptor.execute(bitStream,
                getDataScaleOverride(), getDataBitOverride(), getSkipBits(),
                true, repetitionCount);

        if (getDataScaleOverride() != 0) {
            int scale = descriptor.getScale();
            descriptor.setScale(scale + getDataScaleOverride());

            packet.readCompressed(repetitionCount, bitStream);

            descriptor.setScale(scale);
        } else {
            packet.readCompressed(repetitionCount, bitStream);
        }
        packets.add(packet);
        return packets;
    }

    /**
     * Process the occurrence of an UNKNOWN descriptor, i.e. 2_06_Y followed by
     * a descriptor not defined in Table B. An
     * 
     * @param descriptor
     *            The descriptor to execute.
     * @param packets
     *            List of data packets to receive the decoded data.
     */
    private List<IBUFRDataPacket> processUnknown(BUFRDescriptor descriptor,
            List<IBUFRDataPacket> packets) {
        // Execute the descriptor to read the data
        IBUFRDataPacket packet = descriptor.execute(bitStream);
        packet.readCompressed(repetitionCount, bitStream);

        packets.add(packet);

        return packets;
    }

    /**
     * Process a Table C descriptor.
     * 
     * @param descriptor
     *            The Table C descriptors to "execute".
     * @param packets
     *            List of Data Packets that may receive the descriptor.
     */
    private List<IBUFRDataPacket> processTableC(BUFRDescriptor descriptor,
            List<IBUFRDataPacket> packets) {
        switch (descriptor.getX()) {
        case 1: {
            // Change data width (bit width offset)
            if (descriptor.getY() == 0) {
                setDataBitOverride(0);
            } else {
                setDataBitOverride(descriptor.getY() - 128);
            }
            break;
        }
        case 2: {
            // Change scale
            if (descriptor.getY() == 0) {
                setDataScaleOverride(0);
            } else {
                setDataScaleOverride(descriptor.getY() - 128);
            }
            break;
        }
        case 3: {
            // TODO : Add change reference values.
            // Change reference values
            // changes made by NCEP/NCO/SIB: set associated fields, e.g.,
            // quality control information. Currently it is set to be the
            // bits to skip for Tamdar.
            setSkipBits(descriptor.getY());
            break;
        }
        case 4: {
            // TODO : Add processing for associated fields.
            // Associated field
            // changes made by NCEP/NCO/SIB: set associated fields, e.g.,
            // quality control information. Currently it is set to be the
            // bits to skip for Tamdar.
            setSkipBits(descriptor.getY());
            break;
        }
        case 5: {
            // Encode text data. Y is the number of characters.
            readOverChar = descriptor.getY();
            break;
        }

        case 6: {
            // Length of next Table B descriptor.
            readOverBits = descriptor.getY();
            break;
        }
        default: {
            packets.add(new BUFROperatorPacket(0, "", descriptor));
        }
        }
        return packets;
    }

    /**
     * Process the replication subList descriptors.
     * 
     * @param descriptor
     *            The replication descriptor to execute.
     * @param packets
     *            Data packet list to receive data.
     * @return
     */
    private List<IBUFRDataPacket> executeReplication(BUFRDescriptor descriptor,
            List<IBUFRDataPacket> packets) {

        List<BUFRDescriptor> subList = descriptor.getSubList();
        List<IBUFRDataPacket> sList = new ArrayList<IBUFRDataPacket>();
        for (int i = 0; i < descriptor.getY(); i++) {
            ExecuteEngine engine = new CompressedExecuteEngine(subList,
                    repetitionCount);
            // Persist any size/scale overrides.
            engine.setDataBitOverride(getDataBitOverride());
            engine.setDataScaleOverride(getDataScaleOverride());

            EngineData data = engine.descriptorExecutor(bitStream);

            List<IBUFRDataPacket> t = data.getPacketData();
            if (t.size() == 1) {
                sList.add(t.get(0));
            }
        }
        BUFRSublistPacket p = new BUFRSublistPacket(sList,
                DataPacketTypes.RepSubList.getPacketType(),
                new BUFRSublistDescriptor(
                        BUFRSublistDescriptor.REP_SUBLIST_DESC, 0));
        packets.add(p);

        return packets;
    }

    /**
     * 
     * @param packetData
     * @param receiver
     * @param index
     */
    @SuppressWarnings("unchecked")
    private void normalizeList(List<IBUFRDataPacket> packetData,
            List<IBUFRDataPacket> receiver, int index) {

        for (IBUFRDataPacket p : packetData) {
            if (p instanceof BUFRSublistPacket) {

                List<IBUFRDataPacket> nReceiver = new ArrayList<IBUFRDataPacket>();

                // go grab the sublist
                normalizeList((List<IBUFRDataPacket>) p.getValue(), nReceiver,
                        index);

                BUFRSublistPacket packet = new BUFRSublistPacket(nReceiver,
                        p.getUnits(), p.getReferencingDescriptor());
                receiver.add(packet);

            } else if (p instanceof BUFROperatorPacket) {
                receiver.add(p);
            } else {
                receiver.add(p.getSubsetData(index));
            }
        } // for()
    }

    /**
     * 
     * @param data
     * @return
     */
    private EngineData normalizeData(EngineData data) {

        EngineData tdata = new EngineData();

        List<IBUFRDataPacket> sList = data.getPacketData();
        if (sList != null) {
            for (int i = 0; i < repetitionCount; i++) {

                List<IBUFRDataPacket> receiver = new ArrayList<IBUFRDataPacket>();

                normalizeList(sList, receiver, i);

                if (receiver.size() == 1) {
                    tdata.addPacket(receiver.get(0));
                } else {
                    // what does this mean?
                }

            } // for()
        }
        return tdata;
    }

}
