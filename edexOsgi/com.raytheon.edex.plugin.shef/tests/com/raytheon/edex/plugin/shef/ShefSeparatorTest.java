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
package com.raytheon.edex.plugin.shef;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.shef.ShefSeparator.ShefDecoderInput;
import com.raytheon.edex.plugin.shef.data.ShefRecord;
import com.raytheon.edex.plugin.shef.util.ShefParm;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * JUnit test class for ShefSeparator.java.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 8, 2018    6881     mduff       Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class ShefSeparatorTest {
    private AppsDefaults appsDefault = mock(AppsDefaults.class);

    private ShefParm shefParm = mock(ShefParm.class);

    @Before
    public void setup() {
        when(appsDefault.getBoolean(ShefConstants.SHEF_EMIT_SKIPPED, false))
                .thenReturn(false);
        when(shefParm.getSendCodeDurationDefaults("")).thenReturn(null);
    }

    @Test
    public void testFailureOnBRecordMalformedEnd() {
        ShefSeparator ss = new ShefSeparator();
        byte[] data = getBFileDataBadEnd();
        ss.setData(data, new Headers());

        ShefDecoderInput sdi = ss.next();
        SHEFParser parser = new SHEFParser(sdi, appsDefault, shefParm);
        ShefRecord sr = parser.decode();
        assertTrue(sr == null);
    }

    @Test
    public void testBRecordWithValidEnd() {
        ShefSeparator ss = new ShefSeparator();
        byte[] data = getBFileDataValidEnd();
        ss.setData(data, new Headers());

        ShefDecoderInput sdi = ss.next();
        SHEFParser parser = new SHEFParser(sdi, appsDefault, shefParm);
        ShefRecord sr = parser.decode();
        assertFalse(sr == null);
    }

    private byte[] getBFileDataBadEnd() {
        String shef = "SRUS53 KDMX 011210" + "\n";
        shef += "RR4DMX" + "\n\n";
        shef += ".BR DMX 1109 Z DH06/TAIRVX/DH12/TAIRVP/PPDRVZ/SFDRVZ/SDIRVZ"
                + "\n";
        shef += ": IOWA AWOS RTP FIRST GUESS PROCESSED BY THE IEM\n";
        shef += ":   06Z to 06Z HIGH TEMPERATURE FOR 05 FEB 2016\n";
        shef += ":   00Z TO 12Z TODAY LOW TEMPERATURE\n";
        shef += ":   12Z YESTERDAY TO 12Z TODAY RAINFALL\n";
        shef += ":   ...BASED ON REPORTED OBS...\n";
        shef += "DNS   :DENISON          :  28/  27 /    T  /    M /  M\n";
        shef += "AMW :DENISON : 28 / 27 / T / M / M\n";
        shef += "DMX :DENISON : T / 27 / T / M / M\n";
        shef += "DVN :ATLANTIC : 30 / 23 / 0.00 / M / T\n";
        shef += "ADU :AUDUBON : 30 / 28 / T / M / M\n";
        shef += "DUNN1 :WASHINGTON : 30 / 25 / T / M / M\n";
        shef += "END\n";

        return shef.getBytes();
    }

    private byte[] getBFileDataValidEnd() {
        String shef = "SRUS53 KDMX 011210" + "\n";
        shef += "RR4DMX" + "\n\n";
        shef += ".BR DMX 1109 Z DH06/TAIRVX/DH12/TAIRVP/PPDRVZ/SFDRVZ/SDIRVZ"
                + "\n";
        shef += ": IOWA AWOS RTP FIRST GUESS PROCESSED BY THE IEM\n";
        shef += ":   06Z to 06Z HIGH TEMPERATURE FOR 05 FEB 2016\n";
        shef += ":   00Z TO 12Z TODAY LOW TEMPERATURE\n";
        shef += ":   12Z YESTERDAY TO 12Z TODAY RAINFALL\n";
        shef += ":   ...BASED ON REPORTED OBS...\n";
        shef += "DNS   :DENISON          :  28/  27 /    T  /    M /  M\n";
        shef += "AMW :DENISON : 28 / 27 / T / M / M\n";
        shef += "DMX :DENISON : T / 27 / T / M / M\n";
        shef += "DVN :ATLANTIC : 30 / 23 / 0.00 / M / T\n";
        shef += "ADU :AUDUBON : 30 / 28 / T / M / M\n";
        shef += "DUNN1 :WASHINGTON : 30 / 25 / T / M / M\n";
        shef += ".END\n";

        return shef.getBytes();
    }
}
