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
package com.raytheon.uf.edex.registry.ebxml;

import java.io.ByteArrayInputStream;

import com.raytheon.uf.edex.auth.RemoteRequestRouteWrapper;

/**
 * 
 * Registry specific wrapper for the remote request router
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 5/3/2013     1948        bphillip    Initial implementation
 * 7/26/2013    2232        mpduff      Don't override executeThrift.
 * 10/30/2013   1538        bphillip    Removed unused IRegistryRequestService interface
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryRemoteRequestWrapper extends RemoteRequestRouteWrapper {

    public byte[] request(byte[] data) {
        return executeThrift(new ByteArrayInputStream(data));
    }
}
