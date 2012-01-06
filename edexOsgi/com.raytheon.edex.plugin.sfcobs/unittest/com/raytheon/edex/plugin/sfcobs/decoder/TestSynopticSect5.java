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

import static org.junit.Assert.*;
import org.junit.Test;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.LandSynopticDecoder;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.SynopticSec5Decoder;
import com.raytheon.edex.tools.decoder.DefaultParserStrategy;
import com.raytheon.edex.tools.decoder.ReportParser;

import static com.raytheon.edex.tools.decoder.IDecoderConstants.*;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 20071010            391  jkorman     Initial coding.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1
 */
public class TestSynopticSect5 {

    private static String BLOCK_72_STATION = "72373";
    
    private static String BLOCK_71_STATION = "71950";
    /**
     * Test that passing a null ReportParser causes no exceptions.
     */
    @Test
    public void nullDecoderSection5() {
        LandSynopticDecoder parent = new LandSynopticDecoder();
        parent.setReportIdentifier(BLOCK_72_STATION);
        
        SynopticSec5Decoder decoder = new SynopticSec5Decoder(parent);
        ReportParser parser = null;

        try {
            decoder.decode(parser);
        } catch (DecoderException de) {
            de.printStackTrace();
            fail("This should never happen");
        } catch (Exception e) {
            e.printStackTrace();
            fail("General exception from decoder");
        }
    }

    /**
     * Ensure that internal data is cleared between calls to decode.
     */
    @Test
    public void clearOldValues() {
        LandSynopticDecoder parent = new LandSynopticDecoder();
        parent.setReportIdentifier(BLOCK_72_STATION);
        
        SynopticSec5Decoder decoder = new SynopticSec5Decoder(parent);
        // Data that should pass.
        ReportParser parser = new ReportParser("555 1045",new DefaultParserStrategy());
        try {
            decoder.decode(parser);
            
            // This parser has no section 5 data.
            parser = new ReportParser("333 12345 444 12345",new DefaultParserStrategy());
            decoder.decode(parser);
            // So all data should be nulled.
            
        } catch (DecoderException de) {
            de.printStackTrace();
            fail("This should never happen");
        } catch (Exception e) {
            e.printStackTrace();
            fail("General exception from decoder");
        }
    }

    /**
     * Ensure that the block 72 specific data does not decode when the station
     * id is not block 72.
     */
    @Test
    public void doNotDecodeCity() {
        LandSynopticDecoder parent = new LandSynopticDecoder();

        parent.setReportIdentifier(BLOCK_71_STATION);
        
        SynopticSec5Decoder decoder = new SynopticSec5Decoder(parent);
        // Data that should pass.
        ReportParser parser = new ReportParser("555 1045",new DefaultParserStrategy());
        try {
            decoder.decode(parser);
        } catch (DecoderException de) {
            de.printStackTrace();
            fail("This should never happen");
        } catch (Exception e) {
            e.printStackTrace();
            fail("General exception from decoder");
        }
    }

    /**
     * Test various mixtures of input data.
     */
    @Test
    public void decodeSection5() {

        final String[] testData = {
                "333 12345 12345 555 12345",
                "333 12345 12345 444 ///// 555 12345",
                "333 12345 12345 444 03004 555 12345",
                "333 12345 12345 444 13324 555 12345",
                "333 12345 12345 444 93984 555 12345",
                "333 12345 12345 444 /3994 555 12345",
                "333 12345 12345 444 1/99/ 555 12345",
                "333 12345 12345 444 1A994 555 12345", };

        final Integer[][] testResults = { {},};

        LandSynopticDecoder parent = new LandSynopticDecoder();
        parent.setReportIdentifier(BLOCK_72_STATION);
        
        for (int i = 0; i < testData.length; i++) {
            SynopticSec5Decoder decoder = new SynopticSec5Decoder(parent);
            ReportParser parser = new ReportParser(testData[i],new DefaultParserStrategy());

            try {
                decoder.decode(parser);
                String s = String.format("Item = %3d",i);
            } catch (DecoderException de) {
                de.printStackTrace();
                fail("This should never happen");
            } catch (Exception e) {
                e.printStackTrace();
                fail("General exception from decoder");
            }
        }
    }
}
