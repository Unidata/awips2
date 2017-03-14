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

import java.nio.ByteBuffer;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080204            862 jkorman     Initial Coding.
 * 9/16/2014    #3628      mapeters    Moved from uf.edex.decodertools plugin.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRSection1ed2 extends BUFRSection1 {
    private static final int OPTION_POS = 7;

    private static final int YEAR_POS = 12;

    private static final int MONTH_POS = 13;

    // private static final int DAY_POS = 14;

    private static final int HOUR_POS = 15;

    private static final int MINUTE_POS = 16;

    // private int updateSubSeq = -1;

    // private int dataSubCatagory = -1;

    BUFRSection1ed2(ByteBuffer dataBuffer) {
        super(dataBuffer);
    }

    void getSectionData() {

        masterTable = getSectionData(OPTION_POS - 4) & 0xFF;
        subcenter = getSectionData(OPTION_POS - 3) & 0xFF;
        center = getSectionData(OPTION_POS - 2) & 0xFF;
        // updateSubSeq = getSectionData(OPTION_POS-1) & 0xFF;
        //
        section2Present = (getSectionData(OPTION_POS) & 0x80) > 0;
        //
        dataCatagory = getSectionData(OPTION_POS + 1) & 0xFF;
        // dataSubCatagory = getSectionData(OPTION_POS+2) & 0xFF;
        masterTableVersion = getSectionData(OPTION_POS + 3) & 0xFF;
        localTableVersion = getSectionData(OPTION_POS + 4) & 0xFF;

        // // Edition 3 only has the year of the century!
        year = (getSectionData(YEAR_POS) & 0xFF);
        // // TODO : Make sure of this constant!
        if (year < 50) {
            year += 2000;
        }
        month = getSectionData(MONTH_POS) & 0xFF;
        day = getSectionData(HOUR_POS) & 0xFF;
        hour = getSectionData(HOUR_POS) & 0xFF;
        minute = getSectionData(MINUTE_POS) & 0xFF;
    }

    public StringBuilder getStringData(StringBuilder buffer) {
        buffer = super.getStringData(buffer);
        buffer
                .append(String
                        .format(
                                "\nEd =  2 : Master Table [%3d] Center [%3d] Sub Center [%3d]\n",
                                masterTable, center, subcenter));
        buffer.append(String.format(
                "      2 : Master Table Vers [%3d] Local Table Vers [%3d]\n",
                masterTableVersion, localTableVersion));
        buffer.append(String.format(
                "      2 : YYYY MM DD HH mm = %4d/%02d/%02d %02d:%02d\n", year,
                month, day, hour, minute));
        buffer.append("Ed =  2 : ");

        return buffer;
    }

}
