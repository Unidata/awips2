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
import com.raytheon.uf.edex.bufrtools.packets.BUFRNumericPacket;
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
 * 07/2009      55          T. Lee      Added number of bits to skip for Table C
 * 04/21/2010   208         F. J. Yen   Updated arguments for execute in processTableB
 * 9/16/2014    #3628      mapeters    Moved from uf.edex.decodertools plugin, replaced static imports.
 * 12/14/2015   5166       kbisanz     Update logging to use SLF4J
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class NonCompressedExecuteEngine extends ExecuteEngine {

    private Logger logger = LoggerFactory.getLogger(getClass());

    private int readOverChar = -1;

    private int readOverBits = -1;

    /**
     * 
     * @param descCount
     *            Number of descriptors to execute.
     * @param repCount
     *            Number of times to execute each set of descCount descriptors.
     */
    public NonCompressedExecuteEngine(List<BUFRDescriptor> subList, int repCount) {
        super(subList, repCount);
    }

    /**
     * This executor, decodes non-compressed data.
     * 
     * @param descriptorList
     * @param bitStrm
     * @return
     */
    protected EngineData processBUFR(BUFRBitInputStream bitStrm) {
        EngineData data = new EngineData();

        bitStream = bitStrm;

        for (int i = 0; i < repetitionCount; i++) {

            List<IBUFRDataPacket> packetData = new ArrayList<IBUFRDataPacket>();

            for (BUFRDescriptor descriptor : descriptors) {
                try {
                    if (descriptor instanceof BUFRTableB) {
                        processTableB((BUFRTableB) descriptor, packetData);
                        readOverChar = -1;
                        readOverBits = -1;
                    } else if (descriptor instanceof BUFRTableC) {
                        processTableC((BUFRTableC) descriptor, packetData);
                        if (readOverChar > 0) {
                            bitStrm.read(readOverChar * 8);
                            readOverChar = -1;
                        }
                    } else if (descriptor instanceof BUFRReplicationDescriptor) {
                        executeReplication(
                                (BUFRReplicationDescriptor) descriptor,
                                packetData);
                    } else if (descriptor instanceof BUFRNullDescriptor) {
                        // We can hit this if the Table B descriptor is not in
                        // the descriptor table.
                        if (readOverChar > 0) {
                            bitStrm.read(readOverChar * 8);
                            readOverChar = -1;
                        }
                        if (readOverBits > 0) {
                            long x = bitStrm.read(readOverBits);
                            BUFRNumericPacket packet = new BUFRNumericPacket(x,
                                    "UNKNOWN", descriptor);
                            packetData.add(packet);
                            readOverBits = -1;
                        }
                    }
                } catch (BUFRDecoderException bde) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("Error processing " + descriptor);
                        display(packetData, "", logger);
                    }
                    throw bde;
                }
            }
            BUFRSublistPacket p = new BUFRSublistPacket(packetData,
                    DataPacketTypes.SubSetList.getPacketType(),
                    new BUFRSublistDescriptor(
                            BUFRSublistDescriptor.SUBSET_LIST_DESC, i));
            data.addPacket(p);
        }

        return data;
    }

    /**
     * Process Table B descriptors.
     * 
     * @param descriptor
     * @param packets
     *            List to receive the decoded data.
     * @return The list reference that was to receive the decoded data.
     */
    // changes made by NCEP/NCO/SIB for Tamdar
    // NCEP/NCO/SIB updated arguments for compressedFlag and repetition count
    // (number of subsets) for execute method.
    private List<IBUFRDataPacket> processTableB(BUFRTableB descriptor,
            List<IBUFRDataPacket> packets) {
        packets.add(descriptor.execute(bitStream, getDataScaleOverride(),
                getDataBitOverride(), getSkipBits(), false, 0));
        return packets;
    }

    /**
     * Process Table C descriptors.
     */
    private List<IBUFRDataPacket> processTableC(BUFRTableC descriptor,
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
            // Change reference values
            break;
        }
        case 4: {
            // Associated field
            // changes made by NCEP/NCO/SIB: set associated fields, e.g.,
            // quality control information. Currently it is set to be the
            // bits to skip for Tamdar
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
     * 
     * @param descriptor
     *            The replication descriptor to execute.
     * @param packets
     *            Data packet list to receive data.
     * @return
     */
    private List<IBUFRDataPacket> executeReplication(
            BUFRReplicationDescriptor descriptor, List<IBUFRDataPacket> packets) {

        List<BUFRDescriptor> subList = descriptor.getSubList();

        // How many times.
        int repCount = descriptor.getY();
        // If zero, we need to get the delayed count from the data.
        if (repCount == 0) {
            // Get the delayed descriptor operator.
            BUFRDescriptor descr = descriptor.getDelayedSuffix();
            if ((descr.getF() == 0) && (descr.getX() == 31)) {
                switch (descr.getY()) {
                // Replication sequences
                case 0:
                case 1:
                case 2: {
                    int delayedCount = 0;
                    // Get the delayed count.
                    IBUFRDataPacket packet = descr.execute(bitStream);
                    if ((packet != null)
                            && ("NUMERIC".equals(packet.getUnits()))) {
                        delayedCount = ((Long) packet.getValue()).intValue();
                    }
                    // If the repetition count is zero, no need to go
                    // further.
                    // This is used especially with the 0.31.0 one bit
                    // descriptor.
                    if (delayedCount == 0) {
                        // Insert an empty sublist for the missing data.
                        List<IBUFRDataPacket> d = new ArrayList<IBUFRDataPacket>();
                        packets.add(new BUFRSublistPacket(d,
                                DataPacketTypes.RepSubList.getPacketType(),
                                descriptor));
                        break;
                    }
                    processReplication(packets, subList, delayedCount);

                    break;
                }
                // Repetition sequences
                case 11:
                case 12: {
                    // Get the delayed count.
                    // IBUFRDataPacket packet = descr.execute(bitStream);
                    descr.execute(bitStream);
                    break;
                }
                case 21: {

                    break;
                }
                case 31: {

                    break;
                }
                default: {

                    break;
                }
                } // switch
            } else {
                logger.error("Error - Unexpected delayed descriptor "
                        + descriptor);
                throw new RuntimeException("Unexpected delayed descriptor: "
                        + descriptor);
            }
        } else {
            processReplication(packets, subList, repCount);
        }

        return packets;
    }

    /**
     * 
     * @param packets
     * @param subList
     * @param repCount
     */
    private void processReplication(List<IBUFRDataPacket> packets,
            List<BUFRDescriptor> subList, int repCount) {
        ExecuteEngine engine = new NonCompressedExecuteEngine(subList, repCount);

        engine.setDataBitOverride(getDataBitOverride());
        engine.setDataScaleOverride(getDataScaleOverride());

        EngineData tdata = engine.descriptorExecutor(bitStream);
        BUFRSublistPacket p = new BUFRSublistPacket(tdata.getPacketData(),
                DataPacketTypes.RepSubList.getPacketType(),
                new BUFRSublistDescriptor(
                        BUFRSublistDescriptor.REP_SUBLIST_DESC, 0));
        packets.add(p);
    }

}
