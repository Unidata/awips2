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

import java.util.List;

import com.raytheon.edex.db.maintenance.DbManager;
import com.raytheon.edex.plugin.IMessageDecoder;
import com.raytheon.edex.plugin.PluginFactory;
import com.raytheon.edex.plugin.sfcobs.SfcObsSeparator;
import com.raytheon.edex.plugin.sfcobs.common.SfcObsPart;
import com.raytheon.edex.plugin.sfcobs.decoder.buoy.DRIBUSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.metar.METARDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.CMANSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.LandSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.MobileSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.SHIPSynopticDecoder;
import com.raytheon.edex.tools.decoder.WMOHeader;
import com.raytheon.edex.util.PropertiesFactory;

import junit.framework.TestCase;

public class TestSfcObsSeparator extends TestCase {

    public void testSetMessage() {
        
        String testMessage = String.valueOf((char) 1) + "\r\r\n"
            + "256\r\r\nSSVX13 LFVW 241200 RRC\r\r\n"
            + "ZZYY 44723 24077 1200/ 762397 021367 6112/\r\r\n"
            + "11119 0//// 30067 40067 57003\r\r\n" + "22219 00141\r\r\n"
            + "444 20120 24077 1151/ 50101 80060 80006 9////=\r\r\n"
            + "ZZYY 44603 24077 1200/ 760891 039318 6111/\r\r\n"
            + "11119 0//// 30071 40071 52017\r\r\n" + "22219 00103\r\r\n"
            + "444 20110 24077 1207/ 50101 80145 80029 9/015=\r\r\n"
            + "ZZYY 64620 24077 1200/ 765836 029183 6113/\r\r\n"
            + "11119 0//// 30126 40126 52001\r\r\n" + "22219 00097\r\r\n"
            + "444 20130 24077 1203/ 50101 80138 80057 9/015=\r\r\n"
            + String.valueOf((char) 3);
        
        String r1Header = "SSVX13 LFVW 241200 RRC";

        // Result after clean and strip wmo header.
        String r1Message = "ZZYY 44723 24077 1200/ 762397 021367 6112/\r"
        + "11119 0//// 30067 40067 57003\r22219 00141\r"
        + "444 20120 24077 1151/ 50101 80060 80006 9////=\r"
        + "ZZYY 44603 24077 1200/ 760891 039318 6111/\r"
        + "11119 0//// 30071 40071 52017\r22219 00103\r"
        + "444 20110 24077 1207/ 50101 80145 80029 9/015=\r"
        + "ZZYY 64620 24077 1200/ 765836 029183 6113/\r"
        + "11119 0//// 30126 40126 52001\r22219 00097\r"
        + "444 20130 24077 1203/ 50101 80138 80057 9/015=\r"
        + String.valueOf((char) 3);
        
        SfcObsSeparator separator = new SfcObsSeparator();

        separator.setData(testMessage.getBytes());

        String d = new String(separator.getMessage());
        
        assertEquals(r1Message,d);
        
        WMOHeader h = separator.getWmoHeader();
        assertNotNull(h);
        assertEquals(r1Header,h.getWmoHeader());
    }
    
    public void testParseParts() {

        String [] testMessage = {
            String.valueOf((char) 1) + "\r\r\n"
        + "256\r\r\nSSVX13 LFVW 241200 RRC\r\r\n"
        + "ZZYY 44723 24077 1200/ 762397 021367 6112/\r\r\n"
        + "11119 0//// 30067 40067 57003\r\r\n" + "22219 00141\r\r\n"
        + "444 20120 24077 1151/ 50101 80060 80006 9////=\r\r\n"
        + "ZZYY 44603 24077 1200/ 760891 039318 6111/\r\r\n"
        + "11119 0//// 30071 40071 52017\r\r\n" + "22219 00103\r\r\n"
        + "444 20110 24077 1207/ 50101 80145 80029 9/015=\r\r\n"
        + "ZZYY 64620 24077 1200/ 765836 029183 6113/\r\r\n"
        + "11119 0//// 30126 40126 52001\r\r\n" + "22219 00097\r\r\n"
        + "444 20130 24077 1203/ 50101 80138 80057 9/015=\r\r\n"
        + String.valueOf((char) 3),
        
        String.valueOf((char) 1) + "\r\r\n886\r\r\nSMPR01 SPIM 281800\r\r\n"
        + "AAXX 28184\r\r\n"
        + "84455 31560 60902 10300 20216 39835 40189 70122 8323/\r\r\n"
        + "333   5600/ 58004 83825 83362=\r\r\n"
        + "81209 NIL=\r\r\n"
        + "84390 31965 41611 10237 20153 30038 40137 70222 84030\r\r\n"
        + "333   56000 59009 84360=\r\r\n" + String.valueOf((char) 3),
        
        String.valueOf((char) 1) + "\r\r\n886\r\r\nSMPR01 SPIM 281800\r\r\n"
        + "AAXX 28184\r\r\n"
        + "84400 31560 60902 10300 20216 39835 40189 70122 8323/\r\r\n"
        + "333   5600/ 58004 83825 83362=\r\r\n"
        + "84410 31965 41611 10237 20153 30038 40137 70222 84030\r\r\n"
        + "333   56000 59009 84360=\r\r\n"
        + "AAXX 28181\r\r\n"
        + "84420 31560 60902 10300 20216 39835 40189 70122 8323/\r\r\n"
        + "333   5600/ 58004 83825 83362=\r\r\n"
        + "84430 31965 41611 10237 20153 30038 40137 70222 84030\r\r\n"
        + "333   56000 59009 84360=\r\r\n" + String.valueOf((char) 3),

        String.valueOf((char) 1) + "\r\r\n511\r\r\nSMVE01 NZKL 281800\r\r\n"
        + "BBXX\r\r\n"
        + "ZMSOA  28184 99415 31748 46/// /2014 10097 20049 40089 52013 22213=\r\r\n"
        + "ZMFR   28184 99413 31748 43/// /1809 10099 20055 40094 52011 22200\r\r\n"
        + "00114=\r\r\n" + String.valueOf((char) 3),
        
        String.valueOf((char) 1) + "\r\r\n792\r\r\nSMVX90 KWBC 281803\r\r\n"
        + "BBXX PHBT 28183 99408 70635 41798 40711 10245 20200 40205 58012\r\r\n"
        + "70211 83026 22254 00230 20202 3//// 4//// 5//// 80214=\r\r\n"
        + "BBXX LAOO5 28183 99416 70413 41/// 92017 10214 20207 40160 56///\r\r\n"
        + "71122 8/600 22214 02108 20302 309// 40804 5//// 80209=\r\r\n"
        + "BBXX 4XIS 28183 99423 70333 43598 81526 10225 2020/ 40200 52020\r\r\n"
        + "881// 22225 20607 314// 40707 80205=\r\r\n" + String.valueOf((char) 3),
        
        String.valueOf((char) 1) + "\r\r\n953\r\r\nSXUS20 KWBC 281800 RRP\r\r\n"
        + "CMAN 28184\r\r\n"
        + "RPLV2 46/// /3610 1//// 40195 91736 333 91212=\r\r\n"
        + "SDHN4 46/// /1407 1//// 40226 91736 222// 00241 333 91209=\r\r\n"
        + "SJSN4 46/// /1910 1//// 40208 91736 333 91211=\r\r\n"
        + "SJSN4 46/// /1910 1//// 40208 91742 333 91211=\r\r\n",

        String.valueOf((char) 1) + "\r\r\n953\r\r\nSXUS20 KWBC 281800 RRP\r\r\n"
        + "CMAN 28184\r\r\n"
        + "RPLV2 46/// /3610 1//// 40195 91736 333 91212=\r\r\n"
        + "SDHN4 46/// /1407 1//// 40226 91736 222// 00241 333 91209=\r\r\n"
        + "SJSN4 46/// /1910 1//// 40208 91736 333 91211=\r\r\n"
        + "SJSN4 46/// /1910 1//// 40208 91742 333 91211=\r\r\n"
        + "CMAN 28181\r\r\n"
        + "RPLV2 46/// /3610 1//// 40195 91736 333 91212=\r\r\n"
        + "SDHN4 46/// /1407 1//// 40226 91736 222// 00241 333 91209=\r\r\n"
        + "SJSN4 46/// /1910 1//// 40208 91736 333 91211=\r\r\n"
        + "SJSN4 46/// /1910 1//// 40208 91742 333 91211=\r\r\n",
        
        String.valueOf((char) 1) + "\r\r\n992\r\r\nSAUS53 KWBC 261800\r\r\n"
        + "METAR\r\r\n"
        + "KAIA 261753Z AUTO 31012KT 10SM CLR 18/01 A3009=\r\r\n"
        + "KAID NIL=\r\r\n"
        + "KALO 261754Z 21005KT 10SM CLR 17/08 A3007=\r\r\n"
        + "KALS NIL=\r\r\n"
        + "KANW 261750Z AUTO 34013G16KT 10SM CLR 18/02 A3001=\r\r\n",
        
        String.valueOf((char) 1) + "\r\r\n635\r\r\nSPUS70 KWBC 261806\r\r\n"
        + "SPECI\r\r\n"
        + "KBMG 261802Z 20005KT 10SM -RA BKN035 21/19 A3001 RMK AO2 P0000=\r\r\n"
        + "KAID NIL=\r\r\n"
        + "KALO 261754Z 21005KT 10SM CLR 17/08 A3007=\r\r\n"
        + "KALS NIL=\r\r\n"
        + "KUCP 261802Z AUTO 00000KT VCTS OVC039 26/18 A3001 RMK AO2 TSE00B02"
        + "     $="
        + "KSDF 261803Z 24013G17KT 9SM -RA BKN023 OVC090 25/20 A3003 RMK AO2"
        +"     RAB00 P0000=",
        
        String.valueOf((char) 1) + "\r\r\n999\r\r\nSNCN19 CWAO 281802\r\r\n"
        + "OOXX\r\r\n"
        + "NOP49 28161 99782 10154 ///// 04551\r\r\n"
        + "     26/// ///// 1//// 29/// 3//// 91610 333 6///0=\r\r\n"
        + "NOP49 28161 99782 10154 ///// 04551\r\r\n"
        + "     26/// ///// 1//// 29/// 3//// 91620 333 6///0=\r\r\n"
        + "NOP49 28161 99782 10154 ///// 04551\r\r\n"
        + "     26/// ///// 1//// 29/// 3//// 91630 333 6///0=\r\r\n"
        + "NOP49 28161 99782 10154 ///// 04551\r\r\n"
        + "     26/// ///// 1//// 29/// 3//// 91640 333 6///0=\r\r\n",
        };

        Class [] classes = {
            DRIBUSynopticDecoder.class,
            DRIBUSynopticDecoder.class,
            DRIBUSynopticDecoder.class,
            LandSynopticDecoder.class,
            LandSynopticDecoder.class,
            LandSynopticDecoder.class,
            LandSynopticDecoder.class,
            LandSynopticDecoder.class,
            LandSynopticDecoder.class,
            LandSynopticDecoder.class,
            SHIPSynopticDecoder.class,
            SHIPSynopticDecoder.class,
            SHIPSynopticDecoder.class,
            SHIPSynopticDecoder.class,
            SHIPSynopticDecoder.class,
            CMANSynopticDecoder.class,
            CMANSynopticDecoder.class,
            CMANSynopticDecoder.class,
            CMANSynopticDecoder.class,
            CMANSynopticDecoder.class,
            CMANSynopticDecoder.class,
            CMANSynopticDecoder.class,
            CMANSynopticDecoder.class,
            CMANSynopticDecoder.class,
            CMANSynopticDecoder.class,
            CMANSynopticDecoder.class,
            CMANSynopticDecoder.class,
            METARDecoder.class,
            METARDecoder.class,
            METARDecoder.class,
            METARDecoder.class,
            METARDecoder.class,
            METARDecoder.class,
            METARDecoder.class,
            METARDecoder.class,
            METARDecoder.class,
            METARDecoder.class,
            METARDecoder.class,
            MobileSynopticDecoder.class,
            MobileSynopticDecoder.class,
            MobileSynopticDecoder.class,
            MobileSynopticDecoder.class,
        };
        
        int classIdx = 0;
        for(int i = 0;i < testMessage.length;i++) {
            
            SfcObsSeparator separator = new SfcObsSeparator();
            separator.setData(testMessage[i].getBytes());
            
            while(separator.hasNext()) {
                
                ISfcObsDecoder decoder =
                    SfcObsDecoderFactory.getDecoderInstance(separator);
                
                assertEquals(decoder.getClass(), classes[classIdx++]);
            }
        }
    }
    
}
