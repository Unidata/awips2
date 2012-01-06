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
 * Test class for submitting a test warning message message to TBDW using MHS.
 * <p>
 * <b> THIS PROGRAM SHOULD ONLY BE EXECUTED FROM A TEST BED OR FROM THE TNCF. DO
 * NOT RUN THIS SUITE FROM A WFO, RFC, or from the ANCF or BNCF. </b>
 * <p>
 * 
 * @author brapp
 * 
 */
public class SubmitMhsTestWarning {
    public static void main(String[] args) {
        // Create test message with Message code 134 addressed to DEFAULTNCF
        MhsMessage msg = new MhsMessage(131);

        msg.setSubject("WSWMLB");
        // msg.addAddressee("DEFAULTNCF");
        msg.addAddressee("tbdw");
        msg.setPriority(MhsMessagePriority.High); // This is the default value
                                                  // already
        msg.setType(MhsMessageType.Routine); // This is the default
        msg.addEnclosure("/root/WSW.txt");
        msg.showTrace = true;
        msg.setValidTime(new Date(System.currentTimeMillis() + 60 * 60 * 1000));
        msg.setTimeoutTime(10 * 60 * 1000);
        try {
            msg.send();
            System.out.println(msg.getMessageId());
        } catch (MhsSubmitException e) {
            System.out.println("\t" + msg.getResultText());
        }
    }
}
