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
 * Exception thrown by the submitMessage native method.
 * 
 * @author brapp
 * 
 */
public class MhsSubmitException extends Exception {

    private static final long serialVersionUID = 7380351334016777852L;

    private String errorText;

    /**
     * @param message
     *            Text to display when the toString method is called
     */
    public MhsSubmitException(String message) {
        errorText = message;
    }

    /**
     * Override for superclass toString method
     */
    public String toString() {
        return errorText;
    }
}
