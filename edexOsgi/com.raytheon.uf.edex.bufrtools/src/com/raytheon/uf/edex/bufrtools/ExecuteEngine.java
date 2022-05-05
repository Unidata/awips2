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
package com.raytheon.uf.edex.bufrtools;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;

import com.raytheon.uf.edex.bufrtools.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.bufrtools.descriptors.BUFRTableB;
import com.raytheon.uf.edex.bufrtools.impl.CompressedExecuteEngine;
import com.raytheon.uf.edex.bufrtools.impl.NonCompressedExecuteEngine;
import com.raytheon.uf.edex.bufrtools.io.BUFRBitInputStream;
import com.raytheon.uf.edex.bufrtools.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.bufrtools.packets.IBUFRDataPacket;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * --------     ----------  ----------- ---------------
 * 20071127     382         jkorman     Initial coding
 * 07/2009      55          T. Lee      Added number of bits to skip
 *                                      for Table C
 * 04/21/2010   208         F. J. Yen   Added compressedBit for Associated Field handling
 * 9/16/2014    #3628       mapeters    Moved from uf.edex.decodertools plugin.
 * 12/14/2015   5166        kbisanz     Update logging to use SLF4J
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public abstract class ExecuteEngine {

    protected static int depth = -1;

    protected boolean documentRoot = false;

    protected int subSetCount = -1;

    protected int descriptorCount = -1;

    protected int repetitionCount = -1;

    protected int readOverChar = -1;

    protected int readOverBits = -1;

    private int dataBitOverride = 0;

    private int dataScaleOverride = 0;

    // changes made by NCEP/NCO/SIB for Tamdar
    private int skipBits = 0;

    // changes made by NCEP for compressed data (bufrsgwh)
    private int compressedBit = 0;

    protected BUFRSection3 section3;

    // Index the current descriptor to execute.
    protected int currDescriptor = 0;

    // Count the number of descriptors to be counted against descriptorCount.
    protected int descriptorsProcessed = 0;

    protected BUFRBitInputStream bitStream = null;

    protected List<BUFRDescriptor> descriptors = null;

    /**
     * 
     * @param descCount
     *            Number of descriptors to execute.
     * @param repCount
     *            Number of times to execute each set of descCount descriptors.
     */
    public ExecuteEngine(List<BUFRDescriptor> subList, int repCount) {
        descriptors = subList;
        descriptorCount = descriptors.size();
        repetitionCount = repCount;
    }

    /**
     * 
     * @param descCount
     * @param repCount
     * @param section
     * @return
     */
    public static ExecuteEngine getEngineInstance(BUFRSection3 section) {
        ExecuteEngine engineImpl = null;

        if (section.isCompressed()) {
            engineImpl = new CompressedExecuteEngine(
                    section.getDescriptorList(), section.getNumSubSets());
        } else {
            engineImpl = new NonCompressedExecuteEngine(
                    section.getDescriptorList(), section.getNumSubSets());
        }
        engineImpl.documentRoot = true;
        engineImpl.descriptors = section.getDescriptorList();
        engineImpl.subSetCount = section.getNumSubSets();
        depth = 0;

        return engineImpl;
    }

    /**
     * 
     * 
     * @param descriptorList
     * @param bitStrm
     * @return
     */
    protected abstract EngineData processBUFR(BUFRBitInputStream bitStrm);

    /**
     * 
     * @param descriptorList
     * @param bitStrm
     * @return
     */
    public EngineData descriptorExecutor(BUFRBitInputStream bitStrm) {

        return processBUFR(bitStrm);
    }

    // Changes made by NCEP/NCO/SIB for Tamdar
    /**
     * @return the number of skipped bits
     */
    public int getSkipBits() {
        return skipBits;
    }

    // NCEP changes
    /**
     * @param skipBits
     *            number of skipped bits to set
     */
    public void setSkipBits(int skipBits) {
        this.skipBits = skipBits;
    }

    // Changes made by NCEP/NCO/SIB for Compressed data
    /**
     * @return value of compressed bit
     */
    public int getCompressedBit() {
        return compressedBit;
    }

    // NCEP/NCO/SIB changes added for compressed data
    /**
     * @param compressedBit
     *            value of compressed bit
     */
    public void setCompressedBit(int compressedBit) {
        this.compressedBit = compressedBit;
    }

    /**
     * @return the descriptors
     */
    public List<BUFRDescriptor> getDescriptors() {
        return descriptors;
    }

    /**
     * @param descriptors
     *            the descriptors to set
     */
    public void setDescriptors(List<BUFRDescriptor> descriptors) {
        this.descriptors = descriptors;
    }

    /**
     * @return the subSetCount
     */
    public int getSubSetCount() {
        return subSetCount;
    }

    /**
     * @param subSetCount
     *            the subSetCount to set
     */
    public void setSubSetCount(int subSetCount) {
        this.subSetCount = subSetCount;
    }

    /**
     * 
     * @param descriptorList
     * @param startIndex
     * @return
     */
    public static ArrayList<BUFRDescriptor> getSublist(
            List<BUFRDescriptor> descriptorList, int startIndex) {

        ArrayList<BUFRDescriptor> subList = new ArrayList<BUFRDescriptor>();
        if (startIndex >= 0) {
            for (int ci = startIndex; ci < descriptorList.size(); ci++) {
                subList.add(descriptorList.get(ci));
            }
        }

        return subList;
    }

    /**
     * @return the dataBitOverride
     */
    public int getDataBitOverride() {
        return dataBitOverride;
    }

    /**
     * @param dataBitOverride
     *            the dataBitOverride to set
     */
    public void setDataBitOverride(int dataBitOverride) {
        this.dataBitOverride = dataBitOverride;
    }

    /**
     * @return the dataScaleOverride
     */
    public int getDataScaleOverride() {
        return dataScaleOverride;
    }

    /**
     * @param dataScaleOverride
     *            the dataScaleOverride to set
     */
    public void setDataScaleOverride(int dataScaleOverride) {
        this.dataScaleOverride = dataScaleOverride;
    }

    /**
     * 
     * @param descriptor
     * @param indent
     */
    @SuppressWarnings("unchecked")
    private static void displayDescriptor(IBUFRDataPacket packet,
            String indent, Logger logger) {

        if (packet instanceof BUFRSublistPacket) {
            BUFRSublistPacket p = (BUFRSublistPacket) packet;
            List<IBUFRDataPacket> dList = (List<IBUFRDataPacket>) p.getValue();
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < dList.size(); i++) {
                sb.append(String.format("%30s", dList.get(i)));
                sb.append("   ");
                BUFRDescriptor d = dList.get(i).getReferencingDescriptor();
                if (d instanceof BUFRTableB) {
                    BUFRTableB b = (BUFRTableB) d;
                    sb.append(b.getComments());
                }
                logger.debug(sb.toString());
            }
        }

    }

    /**
     * 
     */
    public static void display(List<IBUFRDataPacket> list, String indent,
            Logger logger) {
        for (IBUFRDataPacket p : list) {
            displayDescriptor(p, indent, logger);
        }
    }

}
