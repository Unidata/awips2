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
package com.raytheon.edex.plugin.redbook;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;

import org.junit.Ignore;
import org.junit.Test;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.redbook.RedbookRecord;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Regression Test RedbookRecords decoded by the RedbookDecoder against known
 * decoder output.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2013 1958       bgonzale    Initial creation
 * May 08, 2013 2000       djohnson    Ignore broken test.
 * Mar 13, 2014 2907       njensen     split edex.redbook plugin into common and
 *                                     edex redbook plugins
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
@Ignore("Test is broken")
public class RedbookDecoderTest {

    final static String DataDir = "./data/Redbook";

    private class RedbookTest {
        public final byte[] rawMessage;

        public final Headers headers;

        public final RedbookRecord result;

        public final String id;

        public RedbookTest(byte[] rawMessage, Headers headers,
                RedbookRecord result, byte[] redbookData, String id) {
            super();
            this.rawMessage = rawMessage;
            this.headers = headers;
            this.result = result;
            if (result != null && redbookData != null) {
                this.result.setRedBookData(redbookData);
            }
            this.id = id;
        }

        public boolean hasResults() {
            return result != null;
        }

    }

    private static class RedbookInput {

        private static final String DOT = ".";

        private static final String HEADERS_EXT = ".Headers";

        private static final String RESULT_EXT = ".Result";

        private static final String REDBOOKDATA_EXT = ".RedbookData";

        private static FilenameFilter inputExtensionFilter = new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.endsWith(".Input");
            }
        };

        public final File inputFile;

        public final File headersFile;

        public final File resultFile;

        public final File resultRedbookDataFile;

        public final String id;

        public RedbookInput(File inputFile) {
            String inputPath = inputFile.getAbsolutePath();
            int indexOfExtension = inputPath.lastIndexOf(DOT);
            String nameNoExtension = inputPath.substring(0, indexOfExtension);

            this.inputFile = inputFile;
            this.headersFile = new File(nameNoExtension + HEADERS_EXT);
            this.resultFile = new File(nameNoExtension + RESULT_EXT);
            this.resultRedbookDataFile = new File(nameNoExtension
                    + REDBOOKDATA_EXT);
            this.id = nameNoExtension.substring(nameNoExtension
                    .lastIndexOf(File.separator) + 1);
        }

        public boolean hasHeaders() {
            return this.headersFile != null && this.headersFile.exists();
        }

        public boolean hasResult() {
            return this.resultFile != null && this.resultFile.exists();
        }

        public static RedbookInput[] getInputs() {
            File dataDir = new File(DataDir);
            File[] inputFiles = dataDir.listFiles(inputExtensionFilter);
            RedbookInput[] inputs = new RedbookInput[inputFiles.length];
            int index = -1;
            for (File inputFile : inputFiles) {
                RedbookInput input = new RedbookInput(inputFile);
                inputs[++index] = input;
            }
            return inputs;
        }

    }

    private final Collection<RedbookTest> tests;

    public RedbookDecoderTest() throws IOException {
        PathManagerFactoryTest.initLocalization();
        tests = new ArrayList<RedbookDecoderTest.RedbookTest>();
        // load test byte arrays, test header data, and result objects.
        InputStream inStrm = null;

        for (RedbookInput redbookInput : RedbookInput.getInputs()) {
            byte[] rawMessage = null;
            Headers headers = null;
            RedbookRecord result = null;
            byte[] redbookData = null;

            // read rawMessage
            rawMessage = FileUtil.file2bytes(redbookInput.inputFile);

            // read header data
            if (redbookInput.hasHeaders()) {
                headers = new Headers();
                BufferedReader reader = null;
                try {
                    inStrm = new FileInputStream(redbookInput.headersFile);
                    reader = new BufferedReader(new InputStreamReader(inStrm));
                    String headerName = null;
                    String value = null;
                    while ((headerName = reader.readLine()) != null) {
                        value = reader.readLine();
                        if (value != null) {
                            headers.put(headerName, value);
                        }
                    }
                } finally {
                    try {
                        if (inStrm != null) {
                            inStrm.close();
                            inStrm = null;
                        }
                    } finally {
                        if (reader != null) {
                            reader.close();
                            reader = null;
                        }
                    }
                }
            }

            // read result
            if (redbookInput.hasResult()) {
                try {
                    JAXBContext con = JAXBContext
                            .newInstance(RedbookRecord.class);
                    Unmarshaller unmarshaller = con.createUnmarshaller();
                    result = (RedbookRecord) unmarshaller
                            .unmarshal(redbookInput.resultFile);
                    fixTimeObs(result);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            // read result redbook data
            try {
                redbookData = FileUtil
                        .file2bytes(redbookInput.resultRedbookDataFile);
            } catch (FileNotFoundException e) {
                // if a result file is not found then the input file does not
                // produce a result.
                redbookData = null;
            }

            RedbookTest test = new RedbookTest(rawMessage, headers, result,
                    redbookData,
                    redbookInput.id);
            tests.add(test);
        }
    }

    private static void fixTimeObs(RedbookRecord record) {
        if (record != null) {
            Calendar cal = record.getTimeObs();
            Calendar newCal = GregorianCalendar.getInstance(TimeZone
                    .getTimeZone("Zulu"));
            newCal.setTimeInMillis(cal.getTimeInMillis());
            record.setTimeObs(newCal);
        }
    }

    /**
     * Test method for
     * {@link com.raytheon.edex.plugin.redbook.RedbookDecoder#decode(byte[], com.raytheon.edex.esb.Headers)}
     * .
     * 
     * @throws DecoderException
     */
    @Test
    public void testDecode() throws DecoderException {
        RedbookDecoder decoder = new RedbookDecoder(this.getClass().getName());

        for (RedbookTest test : tests) {
            PluginDataObject[] result = decoder.decode(test.rawMessage,
                    test.headers);
            int expectedNumberOfResults = test.hasResults() ? 1 : 0;
            assertEquals(test.id
                    + " Failure, incorrect number of results returned for "
                    + test.id, expectedNumberOfResults, result.length);
            RedbookRecord expectedResult = (test.hasResults() ? test.result
                    : null);
            RedbookRecord actualResult = (RedbookRecord) (result.length > 0 ? result[0]
                    : null);

            // insert times have to match to perform comparison
            if (expectedResult != null && actualResult != null) {
                actualResult.setInsertTime(expectedResult.getInsertTime());
            }
            assertEquals(
                    test.id
                            + " Failure, decoder output does not match regression test value for "
                            + test.id, expectedResult, actualResult);
        }
    }

}
