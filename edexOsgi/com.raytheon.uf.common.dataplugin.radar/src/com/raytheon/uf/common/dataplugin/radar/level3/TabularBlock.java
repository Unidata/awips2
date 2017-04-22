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
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Object representing a block of tabular data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 04, 2014  3232     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class TabularBlock extends AbstractBlock {

    private static final int BLOCK_ID = 3;

    private static final int BLOCK_ID_STANDALONE = 7;

    public static int getBlockId() {
        return BLOCK_ID;
    }

    public static int getStandaloneBlockId() {
        return BLOCK_ID_STANDALONE;
    }

    @DynamicSerializeElement
    HashMap<RadarConstants.MapValues, String> map = new HashMap<RadarConstants.MapValues, String>();

    @DynamicSerializeElement
    protected List<List<String>> pages;

    @DynamicSerializeElement
    protected String string;

    /**
     * @param in
     * @throws IOException
     */
    public TabularBlock(DataInputStream in) throws IOException {
        super(in);
    }

    public TabularBlock() {

    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.common.dataplugin.radar.level3.AbstractBlock#init(java
     * .io. DataInputStream)
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        in.skip(122);
        int numPages = in.readUnsignedShort();
        pages = new ArrayList<List<String>>();
        for (int p = 0; p < numPages; p++) {
            List<String> page = new ArrayList<String>();
            int lineLen = in.readUnsignedShort();
            while (lineLen != 0xFFFF) {
                byte[] buf = new byte[lineLen];
                in.readFully(buf);
                page.add(new String(buf));
                lineLen = in.readUnsignedShort();
            }
            pages.add(page);
        }
    }

    /**
     * @return the map
     */
    public HashMap<RadarConstants.MapValues, String> getMap() {
        return map;
    }

    /**
     * @param map
     *            the map to set
     */
    public void setMap(HashMap<RadarConstants.MapValues, String> map) {
        this.map = map;
    }

    /**
     * @return the pages
     */
    public List<List<String>> getPages() {
        return pages;
    }

    /**
     * @param pages
     *            the pages to set
     */
    public void setPages(List<List<String>> pages) {
        this.pages = pages;
    }

    /**
     * @return the string
     */
    public String getString() {
        return string;
    }

    /**
     * @param string
     *            the string to set
     */
    public void setString(String string) {
        this.string = string;
    }

    @Override
    public String toString() {
        String s = "";
        if (pages != null) {
            int p = 1;
            for (List<String> page : pages) {
                s += "Page " + p++;
                for (String line : page) {
                    s += "\n\t" + line;
                }
                s += "\n";
            }
        }
        return s;
    }
}
