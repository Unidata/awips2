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

package com.raytheon.uf.common.dataplugin.ffmp;

/**
 * FFMP Config exception class
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/13/2015   4722       dhladky     Initial check-in
 * 
 * </pre>
 *
 * @author dhladky
 * @version 1.0
 */

public class FFMPConfigurationException extends Exception {
    
    /**
     * Default serial verion UID
     */
    private static final long serialVersionUID = 1L;


    /**
     * Create an FFMPConfigurationException instance from only a message
     * @param message
     */
    public FFMPConfigurationException(String message) {
        super(message);
    }


    /**
     * Create an FFMPConfigurationException instance from both a message and a cause
     * @param message
     * @param cause
     */
    public FFMPConfigurationException(String message, Throwable cause) {
        super(message, cause);
    }

}
