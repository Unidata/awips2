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
package com.raytheon.uf.common.comm;

/**
 * Interface for collecting https Credentials.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 04, 2013    1786     mpduff      Initial creation
 * Feb 10, 2014    2704     njensen     Added credentialsFailed()
 * 
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public interface IHttpsCredentialsHandler {

    /**
     * Get the https credentials.
     * 
     * @param authValue
     *            The authorization message, typically returned from the server
     *            requesting authentication.
     * 
     * @return String Array, username and password
     */
    String[] getCredentials(String authValue);

    void credentialsFailed();
}
