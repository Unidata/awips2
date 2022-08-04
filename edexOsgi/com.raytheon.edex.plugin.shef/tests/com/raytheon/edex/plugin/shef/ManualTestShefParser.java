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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.shef.ShefSeparator.ShefDecoderInput;
import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.edex.plugin.shef.data.ShefRecord;
import com.raytheon.edex.plugin.shef.util.ParserToken;
import com.raytheon.edex.plugin.shef.util.ShefParm;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * This is a manual test class that can read in shef files and print out the
 * ParserToken objects. This allows for the checking of random shef file parsing
 * for testing and troubleshooting.
 * 
 * Set the file field to the test file and run the class as any other JUnit
 * test.
 * 
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 10, 2018   5049     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class ManualTestShefParser {

    private static final String TRACE_ID = "traceId";

    private String file = "/tmp/test.shef";

    private AppsDefaults appsDefault = mock(AppsDefaults.class);

    private ShefParm shefParm = mock(ShefParm.class);

    @Before
    public void setup() {
        when(appsDefault.getBoolean(ShefConstants.SHEF_EMIT_SKIPPED, false))
                .thenReturn(false);
        when(shefParm.getSendCodeDurationDefaults("")).thenReturn(null);
    }

    @Test
    public void testParserForFile() {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        ShefSeparator separator = ShefSeparator.separate(getFileData(),
                headers);

        while (separator.hasNext()) {
            ShefDecoderInput sdi = separator.next();
            SHEFParser parser = new SHEFParser(sdi, appsDefault, shefParm);
            System.out.println("\nRecord: [" + sdi.record + "]");
            List<ParserToken> tokens = parser.tokenize(sdi.record);
            for (ParserToken token : tokens) {
                System.out.println("Token: " + token);
            }
            ShefRecord shefRec = parser.decode();
            for (ShefData sd : shefRec.getDataValues()) {
                System.out.println(sd.toString());
            }
        }
    }

    private byte[] getFileData() {
        byte[] data = null;
        try {
            data = Files.readAllBytes(Paths.get(file));
        } catch (IOException e) {
            e.printStackTrace();
            System.out.println(
                    "** Must change file field to point to test file. **");

        }

        return data;
    }
}
