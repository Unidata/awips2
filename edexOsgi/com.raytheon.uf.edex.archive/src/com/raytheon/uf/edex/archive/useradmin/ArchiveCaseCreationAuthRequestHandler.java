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
package com.raytheon.uf.edex.archive.useradmin;

import com.raytheon.uf.common.archive.request.ArchiveAdminAuthRequest;
import com.raytheon.uf.common.archive.request.ArchiveCaseCreationAuthRequest;

/**
 * Handler for Case Creation dialog authorization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2014 2853       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class ArchiveCaseCreationAuthRequestHandler extends
        ArchiveAdminPrivilegedRequestHandler {

    private final String CASE_DIR_KEY = "archive.case.directory";

    private final String CASE_DIR_DEFAULT = "/data/archiver";

    @Override
    public ArchiveAdminAuthRequest handleRequest(ArchiveAdminAuthRequest request)
            throws Exception {
        super.handleRequest(request);
        if (request instanceof ArchiveCaseCreationAuthRequest) {
            ArchiveCaseCreationAuthRequest req = (ArchiveCaseCreationAuthRequest) request;
            req.setCaseDirectory(System.getProperty(CASE_DIR_KEY,
                    CASE_DIR_DEFAULT));
        }
        return request;
    }

}
