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
 * The <code>MhsMessageType</code> class is used to represents message type
 * values.
 */

public enum MhsMessageType {
    Routine(0, "Routine"), Request(1, "Request"), Acknowledgement(2,
            "Acknowledgement"), Reply(3, "Reply"), Supplement(4, "Supplement"), Amendment(
            5, "Amendment"), Correction(6, "Correction"), Status(7, "Status"), Test(
            8, "Test"), Timing(9, "Timing"), Command(10, "Command"), Inhibit(
            11, "Inhibit"), Clear(12, "Clear"), WarningReceived(13,
            "Warning Received"), Special(14, "Special"), Administrative(15,
            "Administrative"), RoutineTransmissionDelayed(16,
            "Routine Transmission Delayed"), FileTransfer(17, "File Transfer"), RetransmitRequest(
            18, "Retransmit Request"), RetransmitReply(19, "Retransmit Reply"), NACK(
            20, "NACK");

    private final int value;

    private final String text;

    private MhsMessageType(int value, String text) {
        this.value = value;
        this.text = text;
    }

    public int value() {
        return value;
    }

    public String text() {
        return text;
    }

    public MhsMessageType findMessageType(int value) {
        for (MhsMessageType mt : MhsMessageType.values()) {
            if (mt.value == value) {
                return mt;
            }
        }
        return null;
    }
}
