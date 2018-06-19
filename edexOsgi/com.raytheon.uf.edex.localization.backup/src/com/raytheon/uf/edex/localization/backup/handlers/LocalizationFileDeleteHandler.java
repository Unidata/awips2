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
package com.raytheon.uf.edex.localization.backup.handlers;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.backup.request.LocalizationFileDeleteRequest;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.serialization.comm.response.GenericResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Handler for LocalizationFileDeleteRequest
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 9, 2016  5937       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public class LocalizationFileDeleteHandler
        implements IRequestHandler<LocalizationFileDeleteRequest> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalizationFileDeleteHandler.class);

    @Override
    public GenericResponse handleRequest(LocalizationFileDeleteRequest request)
            throws Exception {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        ILocalizationFile file = pathManager
                .getLocalizationFile(request.getContext(), request.getPath());
        GenericResponse response = new GenericResponse();
        try {
            if (file != null) {
                file.delete();
            }
            response.setSuccess(true);
        } catch (LocalizationException e) {
            String msg = "Failed to delete localization file " + file;
            statusHandler.handle(Priority.PROBLEM, msg, e);
            response.setSuccess(false);
            response.setMessage(msg);
        }
        return response;
    }

}
