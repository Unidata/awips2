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

import java.util.Set;

import org.junit.Test;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.sfcobs.common.AncCloud;
import com.raytheon.edex.plugin.sfcobs.common.ObsCommon;
import com.raytheon.edex.plugin.sfcobs.decoder.synoptic.SynopticSec4Decoder;
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
public class TestSynopticSect4 {


    /**
     * Test that passing a null ReportParser causes no exceptions.
     */
    @Test
    public void nullDecoderSection4() {

        SynopticSec4Decoder decoder = new SynopticSec4Decoder();
        ReportParser parser = null;

        try {
            decoder.decode(parser);
            ObsCommon obs = new ObsCommon();
            decoder.getDecodedData(obs);
            Set<AncCloud> clouds = obs.getAncClouds();
            // should no entries
            assertEquals(0,clouds.size());
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
        SynopticSec4Decoder decoder = new SynopticSec4Decoder();
        // Data that should pass.
        ReportParser parser = new ReportParser("444 12345",new DefaultParserStrategy());

        try {
            decoder.decode(parser);

            // This parser has no section 4 data.
            parser = new ReportParser("333 12345 555 12345",new DefaultParserStrategy());
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
     * Test various mixtures of input data.
     */
    @Test
    public void decodeSection4() {

        final String[] testData = { "333 12345 12345 555 12345",
                "333 12345 12345 444 ///// 555 12345",
                "333 12345 12345 444 03004 555 12345",
                "333 12345 12345 444 13324 555 12345",
                "333 12345 12345 444 93984 555 12345",
                "333 12345 12345 444 /3994 555 12345",
                "333 12345 12345 444 1/99/ 555 12345",
                "333 12345 12345 444 1A994 555 12345", };

        final Integer[][] testResults = { { null, null, null, null },
                { VAL_MISSING, VAL_MISSING, VAL_MISSING, VAL_MISSING },
                { 0, 3, 0, 4 }, { 1, 3, 32, 4 }, { 9, 3, 98, 4 },
                { VAL_MISSING, 3, 99, 4 },
                { 1, VAL_MISSING, 99, VAL_MISSING },
                { null, null, null, null },};

        for (int i = 0; i < testData.length; i++) {
            SynopticSec4Decoder decoder = new SynopticSec4Decoder();
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
