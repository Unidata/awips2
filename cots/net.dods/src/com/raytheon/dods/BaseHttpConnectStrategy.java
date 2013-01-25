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
package com.raytheon.dods;

import dods.dap.ServerVersion;

/**
 * Consolidates some common constants and attributes from
 * {@link HttpConnectStrategy} implementations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 6, 2012  634        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

abstract class BaseHttpConnectStrategy implements HttpConnectStrategy {
    static final String ACCEPT_ENCODING_HEADER = "Accept-Encoding";

    static final String DEFLATE = "deflate";

    static final String CONTENT_DESCRIPTION = "content-description";

    static final String XDODS_SERVER = "xdods-server";

    ServerVersion ver;

    @Override
    public ServerVersion getServerVersion() {
        return ver;
    }
}
