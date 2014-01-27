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

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/29/2013   2148       mnash       Refactor registering of packets to Spring
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@DynamicSerialize
public class GraphicBlock extends AbstractBlock implements ISerializableObject {

    private static final int BLOCK_ID = 2;

    public static int getBlockId() {
        return BLOCK_ID;
    }

    @DynamicSerializeElement
    protected Layer[] pages;

    /**
     * @return the pages
     */
    public Layer[] getPages() {
        return pages;
    }

    /**
     * @param pages
     *            the pages to set
     */
    public void setPages(Layer[] pages) {
        this.pages = pages;
    }

    /**
     * @param in
     * @throws IOException
     */
    public GraphicBlock(DataInputStream in) throws IOException {
        super(in);
    }

    public GraphicBlock() {

    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.common.dataplugin.radar.level3.AbstractBlock#init(java
     * .io. DataInputStream)
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        int numPages = in.readUnsignedShort();
        List<Layer> pagesLst = new ArrayList<Layer>();
        NEXT_PAGE: for (int p = 0; p < numPages; p++) {
            Layer page = new Layer();
            List<SymbologyPacket> pageLst = new ArrayList<SymbologyPacket>();

            in.skip(2);
            int pageLen = in.readUnsignedShort();
            in.mark(pageLen);

            byte[] buf = new byte[pageLen];
            in.readFully(buf);
            DataInputStream str = new DataInputStream(new ByteArrayInputStream(
                    buf));

            while (str.available() > 2) {
                int packetId = str.readUnsignedShort();
                SymbologyPacket symPacket = PacketFactory.getInstance()
                        .createPacket(packetId, str);
                if (symPacket != null) {
                    pageLst.add(symPacket);
                } else {
                    in.reset();
                    in.skip(pageLen);
                    continue NEXT_PAGE;
                }
            }
            page.setPackets(pageLst.toArray(new SymbologyPacket[pageLst.size()]));
            pagesLst.add(page);
        }

        this.pages = pagesLst.toArray(new Layer[pagesLst.size()]);
    }

    @Override
    public String toString() {
        String s = "";
        if (pages != null) {
            for (int p = 0; p < pages.length; p++) {
                s += "\nPage " + (p + 1);
                for (SymbologyPacket packet : pages[0].getPackets()) {
                    s += "\n" + packet;
                }
            }
        }
        return s;
    }
}
