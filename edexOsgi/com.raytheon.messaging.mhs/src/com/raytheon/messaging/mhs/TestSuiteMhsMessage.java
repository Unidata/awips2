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

package com.raytheon.messaging.mhs;

import java.util.Date;

/**
 * Unit test suite for the <code>com.raytheon.message.mhs</code> package.
 * <p>
 * This program consists of more than 40 test cases for the Java MHS API that
 * can be executed as part of standard SWIT testing to exercise the various
 * methods, as well as giving an indication of whether MHS is functioning
 * overall.
 * <p>
 * Run this program from a properly configured host from the base directory of
 * all classes. For instance, if installed in <code>/awips/ops/bin</code>, run
 * it from there. The classes should be installed under the relative directory
 * <code>
 * com/raytheon/messaging/mhs</code>. A JVM and AWIPS baseline software later
 * than OB9.2 must be installed and the AWIPS environment must be functional.
 * <p>
 * To run use the following command:
 * 
 * <pre>
 * 	java com.raytheon.messaging.mhs.TestSuiteMhsMessage
 * </pre>
 * 
 * All output will be to <code>stdout</code> with an indication of whether each
 * test case passed or failed.
 * <p>
 * <b> THIS PROGRAM SHOULD ONLY BE EXECUTED FROM A TEST BED OR FROM THE TNCF. DO
 * NOT RUN THIS SUITE FROM A WFO, RFC, or from the ANCF or BNCF. </b>
 * <p>
 * 
 * @author brapp
 * 
 */
public class TestSuiteMhsMessage {

    boolean testsPassed;

    public static void main(String[] args) {
        TestSuiteMhsMessage tsm = new TestSuiteMhsMessage(); // Create a new
                                                             // message object
        tsm.testsPassed = true;

        tsm.testCase1();
        tsm.testCase2();
        tsm.testCase3();
        tsm.testCase4();
        tsm.testCase5();
        tsm.testCase6();
        tsm.testCase7();
        tsm.testCase8();
        tsm.testCase9();
        tsm.testCase10();
        tsm.testCase11();
        tsm.testCase12();
        tsm.testCase13();
        tsm.testCase14();
        tsm.testCase15();
        tsm.testCase16();
        tsm.testCase17();
        tsm.testCase18();
        tsm.testCase19();
        tsm.testCase20();
        tsm.testCase21();
        tsm.testCase22();
        tsm.testCase23();
        tsm.testCase24();
        tsm.testCase25();
        tsm.testCase26();
        tsm.testCase27();
        tsm.testCase28();
        tsm.testCase29();
        tsm.testCase30();
        tsm.testCase31();
        tsm.testCase32();
        tsm.testCase33();
        tsm.testCase34();
        tsm.testCase35();
        tsm.testCase36();
        tsm.testCase37();
        tsm.testCase38();
        tsm.testCase39();
        tsm.testCase40();
        tsm.testCase41();
        tsm.testCase42();
        tsm.testCase43();

        if (tsm.testsPassed) {
            System.out.println("All tests passed!!");
        } else {
            System.err
                    .println("************************************************************************");
            System.err
                    .println("************************************************************************");
            System.err
                    .println("Not all tests were passed - review output for details");
            System.err
                    .println("************************************************************************");
            System.err
                    .println("************************************************************************");
        }
        /*
         * MhsMessage msg = new MhsMessage(0); String bodyFile = "t.tmp"; String
         * prodID = "TEST";
         * 
         * msg.setSubject("This is a test message"); msg.addAddressee("tncf");
         * msg.addAddressee("tbw4"); msg.addAddressee("tbdr");
         * msg.addAckAddressee("tbdw");
         * msg.setPriority(MhsMessagePriority.High); // This is the default
         * value already msg.setType(MhsMessageType.Acknowledgement); // This is
         * the default msg.addEnclosure("nmc.000"); msg.addEnclosure("nmc.001");
         * msg.setBodyFile(bodyFile); msg.showTrace = true;
         * //msg.verifyAddressees = true; msg.setProductId(prodID);
         * msg.setUserId("fred"); msg.setRetryCount(2); msg.setValidTime(new
         * Date(System.currentTimeMillis() + 15 60 1000));
         * msg.setTimeoutTime(new Date(System.currentTimeMillis() - 10 60
         * 1000)); msg.send(); if (msg.getResultCode() ==
         * MhsMessageResult.Success) { System.out.println(msg.getMessageId()); }
         * else { System.err.println("Error " + msg.getResultCode() +
         * " sending message: " + msg.getResultText()); // Do some error
         * recovery stuff }
         */
    }

    private void testCase1() {
        // Invalid addressee

        MhsMessage msg = new MhsMessage(0);
        System.out.println("Case 1: Invalid Addressee");
        msg.addAckAddressee("blivit");
        msg.setValidTime(120);
        msg.setTimeoutTime(120);
        // msg.showTrace = true;

        try {
            msg.send();
            System.out
                    .println("\tCheck msgreq_svr log on sending MHS server for message "
                            + msg.getMessageId());
            System.out
                    .println("\tCheck mcError.log file on sending MHS server for error on "
                            + msg.getMessageId() + " in 5 minutes");
        } catch (MhsSubmitException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            System.out.println("Error: " + e);
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase2() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 2: Invalid body file path");
        msg.addAddressee("tbdw");
        // msg.showTrace = true;
        msg.setBodyFile("/home/brapp/test1.txt");
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase3() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 3: Non-existent body file");
        msg.addAddressee("tbdw");
        // msg.showTrace = true;
        msg.setBodyFile("/data/co/acq_nwstg/nmc/00/nmc.12345");
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase4() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 4: Non-existent enclosure file");
        msg.addEnclosure("/data/co/acq_nwstg/nmc/00/nmc.1234");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase5() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 5: Subject string too long");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg
                .setSubject("This subject string is long than it is supposed to be (max 129 characters) and it should cause the message to fail during validation");
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase6() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 6: body file size too big");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.setBodyFile("/awips/ops/sharedlib/libcoDDM_DAL.so");
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase7() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 7: Absolute message Valid Time before current time");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.setValidTime(-60);
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase8() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 8: Absolute message Time out Time before current time");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.setTimeoutTime(-60);
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase9() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 9: Absolute message Time out Time too far in the future (> 20 minutes)");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.setTimeoutTime(21 * 60);
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase10() {
        MhsMessage msg = new MhsMessage(999);

        System.out.println("Case 10: Message Action Code out of bounds");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase11() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 11: bodyfile string set to null");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.setBodyFile(null);
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase12() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 12: subject string set to null");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.setSubject(null);
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase13() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 13: product ID set to null");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.setProductId(null);
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase14() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 14: user ID set to null");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.setUserId(null);
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase15() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 15: enclosure file name set to null");
        msg.addEnclosure(null);
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase16() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 16: addressee set to null");
        msg.addAddressee(null);
        msg.showTrace = true;
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase17() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 17: message has no addressees");
        msg.showTrace = true;
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase18() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 18: verify addressees only");
        msg.addAddressee("DEFAULT");
        msg.setProductId("WOUS46");
        msg.showTrace = true;
        msg.verifyAddressees = true;
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase19() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 19: simple valid message to single addressee");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase20() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 20: simple valid message to multiple addressees");
        msg.addAddressee("tbdw");
        msg.addAddressee("tbdr");
        msg.addAddressee("tbw4");
        msg.showTrace = true;
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase21() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 21: valid message to single addressee with ack request");
        msg.addAckAddressee("tbdw");
        msg.showTrace = true;
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase22() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 22: valid message to multiple addressees, some with ack requests");
        msg.addAckAddressee("tbdw");
        msg.addAddressee("tbdr");
        msg.addAckAddressee("tbw4");
        msg.showTrace = true;
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase23() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 23: product ID too long");
        msg.addAddressee("tbdw");
        // msg.showTrace = true;
        msg.setProductId("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase24() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 24: simple valid message with a body file to single addressee");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.setBodyFile("/awips/ops/data/mhs/all_sites.data");
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase25() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 25: simple valid message with a body file to multiple addressees");
        msg.addAddressee("tbdw");
        msg.addAddressee("tbdr");
        msg.addAddressee("tbw4");
        msg.showTrace = true;
        msg.setBodyFile("/awips/ops/data/mhs/all_sites.data");
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase26() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 26: valid message to single addressee with a body file with ack request");
        msg.addAckAddressee("tbdw");
        msg.showTrace = true;
        msg.setBodyFile("/awips/ops/data/mhs/all_sites.data");
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase27() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 27: valid message to multiple addressees with a body file, some with ack requests");
        msg.addAckAddressee("tbdw");
        msg.addAddressee("tbdr");
        msg.addAckAddressee("tbw4");
        msg.showTrace = true;
        msg.setBodyFile("/awips/ops/data/mhs/all_sites.data");
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase28() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 28: simple valid message with an enclosure file to single addressee");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.addEnclosure("/awips/ops/data/mhs/all_sites.data");
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase29() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 29: simple valid message with an enclosure file to multiple addressees");
        msg.addAddressee("tbdw");
        msg.addAddressee("tbdr");
        msg.addAddressee("tbw4");
        msg.showTrace = true;
        msg.addEnclosure("/awips/ops/data/mhs/all_sites.data");
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase30() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 30: valid message to single addressee with an enclosure file with ack request");
        msg.addAckAddressee("tbdw");
        msg.showTrace = true;
        msg.addEnclosure("/awips/ops/data/mhs/all_sites.data");
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase31() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 31: valid message to multiple addressees with an enclosure file, some with ack requests");
        msg.addAckAddressee("tbdw");
        msg.addAddressee("tbdr");
        msg.addAckAddressee("tbw4");
        msg.showTrace = true;
        msg.addEnclosure("/awips/ops/data/mhs/all_sites.data");
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase32() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 32: simple valid message with an enclosure file and a body file to multiple addressees");
        msg.addAddressee("tbdw");
        msg.addAddressee("tbdr");
        msg.addAddressee("tbw4");
        msg.showTrace = true;
        msg.addEnclosure("/awips/ops/data/mhs/all_sites.data");
        msg.setBodyFile("/awips/ops/data/mhs/all_sites.data");
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase33() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 33: simple valid message with two enclosure files to multiple addressees");
        msg.addAddressee("tbdw");
        msg.addAddressee("tbdr");
        msg.addAddressee("tbw4");
        msg.showTrace = true;
        msg.addEnclosure("/awips/ops/data/mhs/all_sites.data");
        msg.addEnclosure("/awips/ops/data/mhs/default_addr.data");
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase34() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 34: Set Message Valid Time to 300 seconds");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.setValidTime(new Date(System.currentTimeMillis() + 5 * 60 * 1000));
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase35() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 35: High priority Administrative message to single addressee");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.setPriority(MhsMessagePriority.High);
        msg.setType(MhsMessageType.Administrative);
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase36() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 36: Disallow multiple submissions of the same message");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        try {
            msg.send();
            try {
                msg.send();
                System.out.println("test case passed.");
            } catch (MhsSubmitException e) {
                testsPassed = false;
                System.out.println("\t" + msg.getResultText());
                System.out.println("test case FAILED.");
            }
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase37() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 37: Relative valid time set");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        msg.setValidTime(60 * 60);
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase38() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 38: Relative timeout time set");
        msg.addAddressee("tbdw");
        msg.setTimeoutTime(10 * 60);
        msg.showTrace = true;
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase39() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 39: Relative message Valid Time before current time");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        System.out.println("Current time: " + new Date());
        msg.setValidTime(-60);
        System.out.println("Valid time: " + msg.getValidTime());
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase40() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 40: Relative message Time out Time before current time");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        System.out.println("Current time: " + new Date());
        msg.setTimeoutTime(-60);
        System.out.println("Timeout time: " + msg.getTimeoutTime());
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase41() {
        MhsMessage msg = new MhsMessage(0);

        System.out
                .println("Case 41: Relative message Time out Time too far in the future (> 20 minutes)");
        msg.addAddressee("tbdw");
        msg.showTrace = true;
        System.out.println("Current time: " + new Date());
        msg.setTimeoutTime(21 * 60);
        System.out.println("Timeout time: " + msg.getTimeoutTime());
        try {
            msg.send();
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        } catch (MhsSubmitException e) {
            System.out.println("test case passed.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase42() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 42: add 3 addressees, then remove one");
        msg.addAddressee("tbdw");
        msg.addAddressee("tbdr");
        msg.addAddressee("tbw4");
        System.out.println("Addressee count: " + msg.getAddresseeCount());
        msg.removeAddressee("tbdw");
        System.out.println("Addressee count: " + msg.getAddresseeCount());
        if (msg.getAddresseeCount() != 2) {
            testsPassed = false;
            System.out.println("Addressee count should be 2, but it is "
                    + msg.getAddresseeCount());
            System.out.println("test case FAILED");
        }
        msg.showTrace = true;
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }

    private void testCase43() {
        MhsMessage msg = new MhsMessage(0);

        System.out.println("Case 43: add 3 enclosures, then remove one");
        msg.addEnclosure("/data/co/acq_nwstg/nmc/00/nmc.000");
        msg.addEnclosure("/data/co/acq_nwstg/nmc/00/nmc.001");
        msg.addEnclosure("/data/co/acq_nwstg/nmc/00/nmc.002");
        msg.addAddressee("tbdw");
        System.out.println("Enclosure count: " + msg.getEnclosureCount());
        if (!msg.removeEnclosure("/data/co/acq_nwstg/nmc/00/nmc.001")) {
            System.out
                    .println("Error removing /data/co/acq_nwstg/nmc/00/nmc.001");
        }
        System.out.println("Enclosure count: " + msg.getEnclosureCount());
        if (msg.getEnclosureCount() != 2) {
            testsPassed = false;
            System.out.println("Enclosure count should be 2, but it is "
                    + msg.getEnclosureCount());
            System.out.println("test case FAILED");
        }
        msg.showTrace = true;
        try {
            msg.send();
            System.out.println("test case passed.");
        } catch (MhsSubmitException e) {
            testsPassed = false;
            System.out.println("\t" + msg.getResultText());
            System.out.println("test case FAILED.");
        }
        System.out
                .println("------------------------------------------------------------------------------");
    }
}
