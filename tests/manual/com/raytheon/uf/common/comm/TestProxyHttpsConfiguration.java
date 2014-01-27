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
 * Test implementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 9, 2013            mpduff     Initial creation
 * July 15, 2013  2180    dhladky    Updated for encryption
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@Ignore
public class TestProxyHttpsConfiguration implements IHttpsConfiguration {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.comm.IHttpsConfiguration#getHttpsPort()
     */
    @Override
    public int getHttpsPort() {
        return HttpProxyTestConstants.HTTPS_PORT;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.comm.IHttpsConfiguration#getHttpPort()
     */
    @Override
    public int getHttpPort() {
        return HttpProxyTestConstants.HTTP_PORT;
    }

}
