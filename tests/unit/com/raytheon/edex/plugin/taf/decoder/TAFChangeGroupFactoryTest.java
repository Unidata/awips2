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
package com.raytheon.edex.plugin.taf.decoder;

import junit.framework.Assert;

import org.junit.Test;

import com.raytheon.uf.common.wmo.WMOHeader;



/**
 * TAF change group parsing test
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class TAFChangeGroupFactoryTest {


    /**
     * Test conversion from legacy valid time format to extended TAF format
     */
    @Test
    public void testValidTimeReformat() {
        final String input = "TAF SKCC 121000Z 121212 00000KT 9999 SCT018 SCT090\n"
                + " TEMPO 121213 8000 MIFG SCT017\n"
                + " TEMPO 300024 36007KT SCT023 PROB40 1219/1221 8000 DZRA\n"
                + " SCT018TCU\n" + " TEMPO 1300/1302 VCSH SCT017TCU\n"
                + " TEMPO 1310/1312 5000 BCFG SCT015 TX31/1218Z TN23/1311Z=";
        
        final String expected = "TAF SKCC 121000Z 1212/1312 00000KT 9999 SCT018 SCT090\n"
                + " TEMPO 1212/1213 8000 MIFG SCT017\n"
                + " TEMPO 3000/0100 36007KT SCT023 PROB40 1219/1221 8000 DZRA\n"
                + " SCT018TCU\n"
                + " TEMPO 1300/1302 VCSH SCT017TCU\n"
                + " TEMPO 1310/1312 5000 BCFG SCT015 TX31/1218Z TN23/1311Z=";

        TAFChangeGroupFactory factory = new TAFChangeGroupFactory();
        final String fileName = "/tmp/sbn/manual/nctext/20131112/11/FTXX99_KWBC_121100_20738883.2013111211";
        String result = factory.checkForLegacyFormat(new WMOHeader(
                "FTXX99 KWBC 121100".getBytes(), fileName), input);
        Assert.assertEquals(expected, result);
    }

}
