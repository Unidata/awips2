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
package com.raytheon.uf.edex.dissemination;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.raytheon.edex.utility.EDEXLocalizationAdapter;
import com.raytheon.messaging.mhs.MhsMessage;
import com.raytheon.messaging.mhs.MhsSubmitException;
import com.raytheon.uf.common.dissemination.OUPDisseminatorObserver;
import com.raytheon.uf.common.dissemination.OUPResponse;
import com.raytheon.uf.common.dissemination.OfficialUserProduct;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.IMessageProducer;
import com.raytheon.uf.edex.plugin.manualIngest.MessageGenerator;

/**
 * Test suite for OUPDisseminator
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2025 2038247    tgurney     Initial creation
 * Apr 21, 2025 2038247    tgurney     Add disseminator observers
 *
 * </pre>
 *
 * @author tgurney
 */

//
// Only tests a few paths. A more thorough test would include:
//
// - Test with bad user date stamp
// - Check to see if archived file actually exists
// - Test with attached file
// - Test with target file creation failure and make sure it aborts the send.
// - Test sending to radar product generator
//
class TestOUPDisseminator {

    private static Path edexHome;

    /** OUPDisseminator with external side effects removed. */
    private static class FakeOUPDisseminator extends OUPDisseminator {
        private int sendMhsMessageCalls = 0;

        private boolean sentITOAlarm = false;

        public FakeOUPDisseminator() throws IOException {
            super();
        }

        public FakeOUPDisseminator(Set<OUPDisseminatorObserver> observers)
                throws IOException {
            super(observers);
        }

        @Override
        protected String sendMhsMessage(MhsMessage m)
                throws MhsSubmitException {
            sendMhsMessageCalls++;
            // meaningless string
            return "789012";
        }

        @Override
        protected void sendITOAlarm(OUPResponse resp,
                String messageIdToAcknowledge) {
            sentITOAlarm = true;
        }

        public int getSendMhsMessageCalls() {
            return sendMhsMessageCalls;
        }

        public boolean didSendITOAlarm() {
            return sentITOAlarm;
        }
    }

    /**
     * Attempts to create a somewhat believable EDEX environment.
     *
     * Creates and sets: edex.home, data.archive.root, manualIngest.dropBoxPath,
     * localization file tree, site identifier, message producer (that does
     * nothing), localization path manager.
     *
     * @param edexHomeDir
     *            location which will act as /awips2/edex (probably should be a
     *            tempdir)
     * @param siteId
     *            site identifier to set
     */
    private static void fakeEdex(Path edexHomeDir, String siteId)
            throws Exception {
        System.setProperty("edex.home", edexHomeDir.toString());
        System.setProperty("aw.site.identifier", siteId.toUpperCase());
        Path sbnDir = edexHomeDir.resolve("tmp/sbn");
        Files.createDirectories(sbnDir);
        System.setProperty("data.archive.root", sbnDir.toString());
        Path dropboxDir = sbnDir.resolve("dropbox");
        System.setProperty("manualIngest.dropBoxPath", dropboxDir.toString());
        Path dataDir = edexHomeDir.resolve("data");
        Files.createDirectories(dataDir);
        Path utilityDir = edexHomeDir.resolve("data/utility");
        Files.createDirectories(utilityDir);
        Path shareDir = edexHomeDir.resolve("data/share");
        Files.createDirectories(shareDir);
        Files.createDirectories(utilityDir.resolve("common_static/base"));
        Files.createDirectories(utilityDir.resolve("edex_static/base"));
        Files.createDirectories(utilityDir
                .resolve("common_static/site/" + siteId.toUpperCase()));
        IMessageProducer noOpMessageProducer = Mockito
                .mock(IMessageProducer.class);
        MessageGenerator.getInstance().setDropBoxPath(dropboxDir.toString());
        EDEXUtil.setMessageProducer(noOpMessageProducer);
        PathManagerFactory.setAdapter(new EDEXLocalizationAdapter());
    }

    /** Write localization file to the EDEX localization tree */
    private static void writeLocalizationFile(String path, String contents)
            throws IOException {
        String utilityDir = EDEXUtil.getEdexUtility();
        Path base = Paths.get(utilityDir);
        Path target = base.resolve(path);
        Files.createDirectories(target.getParent());
        Files.write(target, contents.getBytes(StandardCharsets.UTF_8));
    }

    /** Load localization file from current project */
    private static void loadLocalizationFile(String path) throws IOException {
        String utilityDir = EDEXUtil.getEdexUtility();
        Path base = Paths.get(utilityDir);
        Path target = base.resolve(path);
        Files.createDirectories(target.getParent());
        Path source = Paths.get("./utility").resolve(path);
        Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
    }

    @BeforeAll
    static void setUpBeforeClass(@TempDir
    Path tempDir) throws Exception {
        edexHome = tempDir;
        fakeEdex(edexHome, "xxx");
        loadLocalizationFile(
                "common_static/base/dissemination/rcv_action2codes.txt");
        loadLocalizationFile(
                "common_static/base/dissemination/awipsPriorities.txt");
        writeLocalizationFile(
                "common_static/site/XXX/dissemination/WAN_exclude_XXX.txt",
                """
                        # WAN product exclusion list. Products listed here will NOT be sent
                        # to the Wide Area Network.
                        #
                        # Entries can be in the form of AFOS id (cccnnnxxx), wmo id (ttaaii)
                        # or AWIPS id (ccccnnnxxx).
                        WBCTAFXXX
                        KXXXNOTWAN
                        WONW99
                        """);
        writeLocalizationFile(
                "common_static/site/XXX/dissemination/NWWS_exclude_XXX.txt",
                """
                        # NWWS product exclusion list. Products listed here will NOT be sent
                        # to the NOAA Weather Wire Service.
                        #
                        # Entries can be in the form of AFOS id (cccnnnxxx), wmo id (ttaaii)
                        # or AWIPS id (ccccnnnxxx).
                        ADMINISTR
                        WBCADMWBC
                        KXXXNOTNWW
                        WONN99
                        """);
    }

    @Test
    void testSlice() {
        String testString = "Test message";
        assertEquals("Test", OUPDisseminator.slice(testString, 0, 4));
        assertEquals("message", OUPDisseminator.slice(testString, 5, 12));
        assertEquals("st mes", OUPDisseminator.slice(testString, 2, 8));
        assertEquals("Test message",
                OUPDisseminator.slice(testString, 0, testString.length()));
        assertEquals("", OUPDisseminator.slice(testString, 5, 5));
        assertEquals("Test ", OUPDisseminator.slice(testString, 0, -7));
        assertEquals("Test me", OUPDisseminator.slice(testString, 0, -5));
        assertEquals("", OUPDisseminator.slice(testString, 0, -12));
        assertEquals("Test messag", OUPDisseminator.slice(testString, 0, -1));
        assertEquals("message", OUPDisseminator.slice(testString, 5, 9999));
        assertEquals("Test message",
                OUPDisseminator.slice(testString, 0, 999999));
        assertEquals("", OUPDisseminator.slice(testString, 8, 5));
        assertEquals("", OUPDisseminator.slice("", 0, 0));
        assertEquals("", OUPDisseminator.slice("", 0, 5));
        assertEquals("", OUPDisseminator.slice(testString, 20, 25));
        assertEquals("st mes", OUPDisseminator.slice(testString, 2, -4));
        assertThrows(IndexOutOfBoundsException.class,
                () -> OUPDisseminator.slice(testString, -1, 4));
    }

    @Test
    void testCreateTargetFile(@TempDir
    Path tempDir) throws Exception {
        Set<String> fileNamesUsed = new HashSet<>();
        Path testFile = tempDir.resolve("foo.txt");
        byte[] testData = "Test message\r\r\n".getBytes(StandardCharsets.UTF_8);
        for (int i = 0; i <= 10; i++) {
            String actualFile = OUPDisseminator.createTargetFile(testData,
                    testFile.toString());
            Path actualPath = Paths.get(actualFile);
            assertFalse(fileNamesUsed.contains(actualFile));
            fileNamesUsed.add(actualFile);
            assertTrue(Files.isRegularFile(actualPath));
            assertTrue(Files.isWritable(actualPath));
            assertTrue(Files.isReadable(actualPath));
            try (FileInputStream fis = new FileInputStream(
                    actualPath.toFile())) {
                assertArrayEquals(fis.readAllBytes(), testData);
            }
        }
    }

    private OfficialUserProduct makeOup(String address, String ttaaii,
            String wanPil, int num) {
        OfficialUserProduct oup = new OfficialUserProduct();
        oup.setAddress(address);
        oup.setAwipsWanPil(wanPil);
        oup.setFilename(String.format("%s.wan%010d", wanPil.substring(4), num));
        oup.setNeedsWmoHeader(false);
        oup.setPriority(0);
        oup.setProductText(String.format("""
                %s KXXX 123456\r\r
                \r\r
                THIS IS A TEST MESSAGE.\r\r
                """, ttaaii));
        oup.setSource("TextWS");
        oup.setUserDateTimeStamp("123456");
        oup.setWmoType("");
        return oup;
    }

    @Test
    void testConstructor() throws Exception {
        /* Uses the real OUPDisseminator */
        assertDoesNotThrow(() -> new OUPDisseminator(Set.of()));
        assertDoesNotThrow(() -> new OUPDisseminator());
    }

    @Test
    void testProcessTestFlag() throws Exception {
        // test=true flag passed to process method
        final boolean observerCalled[] = { false };
        OUPDisseminatorObserver observer = (oup) -> {
            observerCalled[0] = true;
        };
        FakeOUPDisseminator fake = new FakeOUPDisseminator(Set.of(observer));
        OUPResponse resp = new OUPResponse();
        resp.setAttempted(true);
        OfficialUserProduct oup = makeOup("ALL", "WOUS99", "KXXXTESXXX", 1);
        fake.process(oup, "TESXXX", resp, null, true);
        assertFalse(resp.hasFailure());
        assertEquals(fake.getSendMhsMessageCalls(), 0);
        assertFalse(observerCalled[0]);
    }

    @Test
    void testProcessProductWithObservers() throws Exception {
        final int observerCalls[] = { 0, 0 };
        OUPDisseminatorObserver observer0 = (oup) -> {
            observerCalls[0]++;
        };
        OUPDisseminatorObserver observer1 = (oup) -> {
            observerCalls[1]++;
        };
        FakeOUPDisseminator fake = new FakeOUPDisseminator(
                Set.of(observer0, observer1));
        OUPResponse resp = new OUPResponse();
        resp.setAttempted(true);
        OfficialUserProduct oup = makeOup("ALL", "WOUS99", "KXXXTESXXX", 2);
        fake.process(oup, "TESXXX", resp, null, false);
        assertFalse(resp.hasFailure());
        assertEquals(observerCalls[0], 1);
        assertEquals(observerCalls[1], 1);
    }

    @Test
    void testProcessNWWSProduct() throws Exception {
        FakeOUPDisseminator fake = new FakeOUPDisseminator();
        OUPResponse resp = new OUPResponse();
        resp.setAttempted(true);
        OfficialUserProduct oup = makeOup("ALL", "WOUS99", "KXXXTESXXX", 2);
        fake.process(oup, "TESXXX", resp, null, false);
        assertFalse(resp.hasFailure());
        // Transmit on both WAN and NWWS
        assertEquals(fake.getSendMhsMessageCalls(), 2);
    }

    @Test
    void testProcessProductExcludedFromNWWS() throws Exception {
        // is WAN product but not NWWS product because on NWWS exclude list
        FakeOUPDisseminator fake = new FakeOUPDisseminator();
        OUPResponse resp = new OUPResponse();
        resp.setAttempted(true);
        OfficialUserProduct oup = makeOup("ALL", "WONN99", "KXXXTESXXX", 3);
        fake.process(oup, "TESXXX", resp, null, false);
        assertFalse(resp.hasFailure());
        assertEquals(fake.getSendMhsMessageCalls(), 1);
    }

    @Test
    void testProcessProductExcludedFromWAN() throws Exception {
        // is not a WAN product because on WAN exclude list
        final boolean observerCalled[] = { false };
        OUPDisseminatorObserver observer = (oup) -> {
            observerCalled[0] = true;
        };
        FakeOUPDisseminator fake = new FakeOUPDisseminator(Set.of(observer));
        OUPResponse resp = new OUPResponse();
        resp.setAttempted(true);
        OfficialUserProduct oup = makeOup("ALL", "WOUS99", "KXXXNOTWAN", 4);
        fake.process(oup, "TESXXX", resp, null, false);
        assertFalse(resp.isSendWANSuccess());
        assertTrue(resp.isSendLocalSuccess());
        assertEquals(fake.getSendMhsMessageCalls(), 0);
        assertFalse(observerCalled[0]);
    }

    @Test
    void testProcessLocalOnlyProduct() throws Exception {
        // is not a WAN product because local
        final boolean observerCalled[] = { false };
        OUPDisseminatorObserver observer = (oup) -> {
            observerCalled[0] = true;
        };
        FakeOUPDisseminator fake = new FakeOUPDisseminator(Set.of(observer));
        OUPResponse resp = new OUPResponse();
        resp.setAttempted(true);
        OfficialUserProduct oup = makeOup("000", "WOUS99", "KXXXTESXXX", 5);
        fake.process(oup, "TESXXX", resp, null, false);
        assertFalse(resp.isSendWANSuccess());
        assertTrue(resp.isSendLocalSuccess());
        assertEquals(fake.getSendMhsMessageCalls(), 0);
        assertFalse(observerCalled[0]);
    }

    /*
     * we assume SVR product needs acknowledgement because priority == 2, check
     * awipsPriorities.txt.
     */
    @Test
    void testAcknowledgementSucceeded() throws Exception {
        FakeOUPDisseminator fake = new FakeOUPDisseminator();
        OUPResponse resp = new OUPResponse();
        OfficialUserProduct oup = makeOup("ALL", "WOUS99", "KXXXSVRXXX", 6);
        OUPAckManager fakeAckMgr = Mockito.mock(OUPAckManager.class);
        Answer<Void> answer = new Answer<>() {
            @Override
            public Void answer(InvocationOnMock invocation) throws Throwable {
                OUPResponse response = invocation.getArgument(2,
                        OUPResponse.class);
                response.setAcknowledged(true);
                return null;
            }
        };
        doAnswer(answer).when(fakeAckMgr).waitAck(anyString(), anyString(),
                eq(resp), anyString());
        resp.setAttempted(true);
        fake.process(oup, "SVRXXX", resp, fakeAckMgr, false);
        assertFalse(resp.hasFailure());
        assertEquals(fake.getSendMhsMessageCalls(), 2);
    }

    @Test
    void testAcknowledgmentNeededButNullAckMgrFailed() throws Exception {
        FakeOUPDisseminator fake = new FakeOUPDisseminator();
        OUPResponse resp = new OUPResponse();
        OfficialUserProduct oup = makeOup("ALL", "WOUS99", "KXXXSVRXXX", 7);
        resp.setAttempted(true);
        fake.process(oup, "SVRXXX", resp, null, false);
        assertTrue(resp.hasFailure());
        assertEquals(fake.getSendMhsMessageCalls(), 2);
    }

    @Test
    void testAcknowledgmentFailedDidSendITOAlarm() throws Exception {
        FakeOUPDisseminator fake = new FakeOUPDisseminator();
        OUPResponse resp = new OUPResponse();
        OfficialUserProduct oup = makeOup("ALL", "WOUS99", "KXXXSVRXXX", 8);
        OUPAckManager fakeAckMgr = Mockito.mock(OUPAckManager.class);
        Answer<Void> answer = new Answer<>() {
            @Override
            public Void answer(InvocationOnMock invocation) throws Throwable {
                OUPResponse response = invocation.getArgument(2,
                        OUPResponse.class);
                response.setAcknowledged(false);
                return null;
            }
        };
        doAnswer(answer).when(fakeAckMgr).waitAck(anyString(), anyString(),
                eq(resp), anyString());
        resp.setAttempted(true);
        fake.process(oup, "SVRXXX", resp, fakeAckMgr, false);
        assertTrue(resp.hasFailure());
        assertTrue(fake.didSendITOAlarm());
        assertEquals(fake.getSendMhsMessageCalls(), 2);
    }
}
