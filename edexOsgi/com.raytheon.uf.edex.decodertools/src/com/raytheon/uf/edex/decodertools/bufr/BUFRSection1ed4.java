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
import java.util.Calendar;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 20080204            862 jkorman     Changes due to BUFRSEparator refactor.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRSection1ed4 extends BUFRSection1 {
    private static final int OPTION_POS = 9;

    private static final int YEAR_POS = 15;

    private static final int MONTH_POS = 17;

    // private static final int DAY_POS = 18;

    private static final int HOUR_POS = 19;

    private static final int MINUTE_POS = 20;

    private static final int SECOND_POS = 21;

    // private boolean section2Present = false;

    private int internationalSubCatagory = -1;

    private int localSubCatagory = -1;

    private int second;

    BUFRSection1ed4(ByteBuffer dataBuffer) {
        super(dataBuffer);
        getSectionData();
    }

    void getSectionData() {
        masterTable = getSectionData(3) & 0xFF;
        subcenter = ((getSectionData(4) & 0xFF) << 8)
                + (getSectionData(5) & 0xFF);
        center = ((getSectionData(6) & 0xFF) << 8) + (getSectionData(7) & 0xFF);

        section2Present = (getSectionData(OPTION_POS) & 0x80) > 0;

        dataCatagory = getSectionData(OPTION_POS + 1) & 0xFF;
        internationalSubCatagory = getSectionData(OPTION_POS + 2) & 0xFF;
        localSubCatagory = getSectionData(OPTION_POS + 3) & 0xFF;
        masterTableVersion = getSectionData(OPTION_POS + 4) & 0xFF;
        localTableVersion = getSectionData(OPTION_POS + 5) & 0xFF;

        year = ((getSectionData(YEAR_POS) & 0xFF) << 8)
                + (getSectionData(YEAR_POS + 1) & 0xFF);
        month = getSectionData(MONTH_POS) & 0xFF;
        day = getSectionData(HOUR_POS) & 0xFF;
        hour = getSectionData(HOUR_POS) & 0xFF;
        minute = getSectionData(MINUTE_POS) & 0xFF;
        second = getSectionData(SECOND_POS) & 0xFF;
    }

    public StringBuilder getStringData(StringBuilder buffer) {
        buffer = super.getStringData(buffer);
        buffer.append(String.format(
                "\nEd =  4 : Master Table [%3d] Center [%3d]\n", masterTable,
                center));
        buffer.append(String.format("      4 : Data Catagory [%3d]\n",
                dataCatagory));
        buffer.append(String.format(
                "      4 : Intl Sub Catagory [%3d] local Sub Catagory [%3d]\n",
                internationalSubCatagory, localSubCatagory));
        buffer.append(String.format(
                "      4 : Master Table Vers [%3d] Local Table Vers [%3d]\n",
                masterTableVersion, localTableVersion));
        buffer
                .append(String
                        .format(
                                "      4 : YYYY/MM/DD HH:mm:ss = %4d/%02d/%02d %02d:%02d%02d\n",
                                year, month, day, hour, minute, second));

        return buffer;
    }

    public String toString() {
        return getStringData(null).toString();
    }

    /**
     * Get the seconds info from Section 3 data.
     * 
     * @return The seconds info.
     * @return
     */
    public int getSecond() {
        return second;
    }

    public Calendar getSectionDate() {
        Calendar cal = super.getSectionDate();

        cal.set(Calendar.SECOND, second);

        return cal;
    }
}
