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
package com.raytheon.uf.edex.plugin.text.dbsrv;

import com.raytheon.uf.common.dataplugin.text.dbsrv.TextDBRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Handles requests from the textdb client
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2014 2926       bclement     Initial creation
 * Jan 18, 2016 4562       tjensen      Moved from edex.plugin.text to 
 *                                      edex.plugin.text.dbsrv
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class TextDBRequestHandler implements IRequestHandler<TextDBRequest> {

    private final TextDBSrv textdbSrv;

    /**
     * @param textdbSrv
     */
    public TextDBRequestHandler(TextDBSrv textdbSrv) {
        this.textdbSrv = textdbSrv;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(TextDBRequest request) throws Exception {
        return textdbSrv.processMessage(request.getMessage());
    }

}
