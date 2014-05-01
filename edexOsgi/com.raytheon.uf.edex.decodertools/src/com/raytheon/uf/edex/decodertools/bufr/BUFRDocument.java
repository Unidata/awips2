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
package com.raytheon.uf.edex.decodertools.bufr;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRNullDescriptor;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.DescriptorFactory;
import com.raytheon.uf.edex.decodertools.bufr.io.BUFRBitInputStream;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.DataPacketTypes;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 20080204            862 jkorman     Changes to support BUFRFile refactor.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRDocument {

    private BUFRSection0 section0;

    private BUFRSection1 section1;

    private BUFRSection2 section2;

    private BUFRSection3 section3;

    private BUFRSection4 section4;

    private BUFRSection5 section5;

    private BUFRDataDocument documentData;

    /**
     * 
     * @param buffer
     */
    public BUFRDocument(DescriptorFactory factory, ByteBuffer buffer) {
        section0 = new BUFRSection0(buffer);
        section1 = section0.getSection1(buffer);
        if (section1 == null) {
            throw new RuntimeException("Unable to create section 1");
        }
        section2 = section1.getSection2(buffer);
        section3 = new BUFRSection3(factory, buffer);
        section4 = new BUFRSection4(buffer);
        section5 = new BUFRSection5(buffer);
    }

    /**
     * 
     * @param data
     */
    public BUFRDocument(DescriptorFactory factory, byte[] data) {
        this(factory, ByteBuffer.wrap(data));
    }

    /**
     * 
     * @param data
     * @param offset
     */
    public BUFRDocument(DescriptorFactory factory, byte[] data,
            BUFROffsets offset) {
        this(factory, ByteBuffer.wrap(data, offset.getStartPos(), offset
                .getLength()));
    }
    
    /**
     * @return the section1
     */
    public BUFRSection1 getSection1() {
        return section1;
    }

    /**
     * @param section1 the section1 to set
     */
    public void setSection1(BUFRSection1 section1) {
        this.section1 = section1;
    }

    /**
     * @return the section2
     */
    public BUFRSection2 getSection2() {
        return section2;
    }

    /**
     * @param section2 the section2 to set
     */
    public void setSection2(BUFRSection2 section2) {
        this.section2 = section2;
    }

    /**
     * @return the section3
     */
    public BUFRSection3 getSection3() {
        return section3;
    }

    /**
     * @param section3 the section3 to set
     */
    public void setSection3(BUFRSection3 section3) {
        this.section3 = section3;
    }

    /**
     * @return the section4
     */
    public BUFRSection4 getSection4() {
        return section4;
    }

    /**
     * @param section4 the section4 to set
     */
    public void setSection4(BUFRSection4 section4) {
        this.section4 = section4;
    }

    /**
     * 
     * @return
     */
    public boolean isDocumentValid() {

        boolean valid = ((section0 != null) && (section0.isSectionValid()));

        valid &= ((section1 != null) && (section1.isSectionValid()));
        // Section 2 is optional
        if (section2 != null) {
            valid &= (section2.isSectionValid());
        }
        valid &= ((section3 != null) && (section3.isSectionValid()));
        valid &= ((section4 != null) && (section4.isSectionValid()));
        valid &= ((section5 != null) && (section5.isSectionValid()));

        return valid;
    }

    public String toString() {
        StringBuilder sb = null;
        if (section0 != null) {
            sb = section0.getStringData(sb);
            sb.append("\n");
        }
        if (section1 != null) {
            sb = section1.getStringData(sb);
            sb.append("\n");
        }
        if (section2 != null) {
            sb = section2.getStringData(sb);
            sb.append("\n");
        } else {
            sb.append("Section 2: Not defined\n");
        }

        if (section3 != null) {
            sb = section3.getStringData(sb);
            sb.append("\n");
        }
        if (section4 != null) {
            sb = section4.getStringData(sb);
            sb.append("\n");
        }
        if (section5 != null) {
            sb = section5.getStringData(sb);
            sb.append("\n");
        }
        return sb.toString();
    }

    public void displayDescriptors(boolean displayData) {
        if (section3 != null) {
            System.out.println("Compressed [" + section3.isCompressed() + "]");
            section3.display();
        }
        if (displayData && (section4 != null)) {
            section4.display();
        }
    }

    public BUFRDataDocument execute() {
        documentData = null;

        BUFRBitInputStream strm = ((BUFRSection4) section4).getStream();
        // The first 4 bytes are the section header data.
        strm.skip(32);

        List<IBUFRDataPacket> packetData = new ArrayList<IBUFRDataPacket>();

        ExecuteEngine engine = ExecuteEngine.getEngineInstance(section3);
        EngineData data = engine.descriptorExecutor(strm);

        packetData.addAll(data.getPacketData());

        documentData = new BUFRDataDocument(packetData);
        documentData.setEnclosingDocument(this);
        
        return documentData;
    }

    public BUFRDataDocument getData() {
        return documentData;
    }

    /**
     * 
     * @param packets
     * @param indent
     */
    @SuppressWarnings("unchecked")
    public static void showPackets(List<IBUFRDataPacket> packets, String indent) {
        if (packets != null) {
            IBUFRDataPacket lastPacket = null;
            for (IBUFRDataPacket p : packets) {
                if (p != null) {
                    BUFRDescriptor d = p.getReferencingDescriptor();
                    if (d == null) {
                        System.out.println(indent
                                + "No descriptor defined for this element");
                        d = new BUFRNullDescriptor(0);
                    }

                    if (p instanceof BUFRSublistPacket) {
                        String cIndent = "   " + indent;

                        StringBuilder b = new StringBuilder(indent);
                        String tag = null;
                        
                        DataPacketTypes type = DataPacketTypes.getType(p.getUnits());

                        tag = type.getPacketType();

                        b.append("<");
                        b.append(tag);
                        b.append(" descriptor=\"");
                        b.append(d.getStringDescriptor());
                        b.append("\">");
                        System.out.println(b);
                        showPackets((List<IBUFRDataPacket>) p.getValue(),
                                cIndent);
                        System.out.println(indent + "</" + tag + ">");
                    } else {
                        System.out.println(indent + "<packet descriptor=\""
                                + d.getStringDescriptor() + "\">");
                        System.out.print(indent + "   <value>");
                        if (p.isMissing()) {
                            System.out.print("MISSING");
                        } else {
                            System.out.print(p.getValue());
                        }
                        System.out.println("</value>");
                        System.out.println(indent + "   <units>" + p.getUnits()
                                + "</units>");
                        System.out.println(indent + "</packet>");
                    }
                } else {
                    if (lastPacket != null) {
                        System.out.println(indent + "Null packet encountered");
                    }
                }
                lastPacket = p;
            }
        }
    }
}
