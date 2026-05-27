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
package com.raytheon.edex.plugin.sfcobs.decoder;

import java.util.Set;
import com.raytheon.edex.plugin.sfcobs.SfcObsSeparator;
import com.raytheon.edex.plugin.sfcobs.common.AncCloud;
import com.raytheon.edex.plugin.sfcobs.common.ObsCommon;

import junit.framework.TestCase;

public class TestSynopticSect1 extends TestCase {

    public void testParseParts() {

        String [] testMessages = {
                String.valueOf((char) 1)
                + "\r\r\n886\r\r\nSMPR01 SPIM 281800\r\r\n"
                + "AAXX 28184\r\r\n"
                + "84390 31901 41611 10237 20077 30038 40137 70222 84100\r\r\n"
                + "333   56000 59009 84360=\r\r\n" + String.valueOf((char) 3),
                String.valueOf((char) 1)
                + "\r\r\n886\r\r\nSMPR01 SPIM 281800\r\r\n"
                + "AAXX 28184\r\r\n"
                + "84390 31965 41611 10237 20077 30038 40137 70222 84010\r\r\n"
                + "333   56000 59009 84360=\r\r\n" + String.valueOf((char) 3),
                String.valueOf((char) 1)
                + "\r\r\n886\r\r\nSMPR01 SPIM 281800\r\r\n"
                + "AAXX 28184\r\r\n"
                + "84390 319// 41611 10237 20077 30038 40137 70222 84135\r\r\n"
                + "333   56000 59009 84360=\r\r\n" + String.valueOf((char) 3),
        };
        int [] testRes = {
                2,3,4,
        };
        Integer [] testVis = {
                new Integer(1),new Integer(65),ISfcObsDecoder.VAL_MISSING,
        };
        
        int testIndex = 0;
        for(String s : testMessages) {
            SfcObsSeparator separator = new SfcObsSeparator();
            separator.setData(s.getBytes());

            ObsCommon report = null;
            while (separator.hasNext()) {
                ISfcObsDecoder decoder = SfcObsDecoderFactory
                        .getDecoderInstance(separator);

                if (!decoder.isNILObs()) {
                    try {

                        if ((report = (ObsCommon) decoder.decode()) != null) {
                            Set<AncCloud> clouds = report.getAncClouds();
                            assertNotNull(clouds);
                            assertEquals(testRes[testIndex],clouds.size());
                            assertEquals(testVis[testIndex],report.getHorzVisibility());
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                        fail();
                    }
                }
                testIndex++;
            } // end-while
        }
            
    }

}
