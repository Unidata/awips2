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

import org.junit.Ignore;

/**
 * Test Https credentials handler. Returns valid credentials for HttpClient Test
 * Cases.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 6, 2013    1786     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@Ignore
public class TestHttpsCredentialsHandler implements IHttpsCredentialsHandler {

    @Override
    public String[] getCredentials(String message) {
        return new String[] { HttpTestConstants.USERNAME,
                HttpTestConstants.PASSWD };
    }

    @Override
    public void credentialsFailed() {
        // TODO Auto-generated method stub

    }
}
