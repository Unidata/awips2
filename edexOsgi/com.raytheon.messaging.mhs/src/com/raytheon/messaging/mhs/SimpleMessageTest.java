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


/**
 * A very simple message test class.
 * 
 * @author brapp
 * 
 */
@SuppressWarnings("unused")
public class SimpleMessageTest {

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub
        MhsMessage msg = new MhsMessage(0);
        String bodyFile = "t.tmp";
        String prodID = "TEST";

        msg.setSubject("This is a test message");
        msg.addAddressee("tbdw");
        // msg.setPriority(MhsMessagePriority.High); // This is the default
        // value already
        // msg.setType(MhsMessageType.Acknowledgement); // This is the default
        // msg.addEnclosure("nmc.000");
        // msg.addEnclosure("nmc.001");
        // msg.setBodyFile(bodyFile);
        msg.showTrace = true;
        // msg.verifyAddressees = true;
        // msg.setProductId(prodID);
        // msg.setUserId("fred");
        // msg.setRetryCount(2);
        // msg.setValidTime(15 * 60);
        // msg.setTimeoutTime(10 * 60);
        try {
            System.out.println(msg.send());
        } catch (MhsSubmitException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            System.out.println("Error: " + e);
        }
    }
}
