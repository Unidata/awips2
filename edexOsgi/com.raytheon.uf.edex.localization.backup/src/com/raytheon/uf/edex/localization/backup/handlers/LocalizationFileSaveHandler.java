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

import java.io.IOException;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.backup.request.LocalizationFileSaveRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.serialization.comm.response.GenericResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Handler for LocalizationFileSaveRequest
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

public class LocalizationFileSaveHandler
        implements IRequestHandler<LocalizationFileSaveRequest> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalizationFileSaveHandler.class);

    @Override
    public GenericResponse handleRequest(LocalizationFileSaveRequest request)
            throws Exception {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        ILocalizationFile file = pathManager
                .getLocalizationFile(request.getContext(), request.getPath());
        GenericResponse response = new GenericResponse();
        if (file == null) {
            response.setSuccess(false);
            response.setMessage(request.getContext() + IPathManager.SEPARATOR
                    + request.getPath() + ": Localization file is null");
            return response;
        }
        try (SaveableOutputStream outStream = file.openOutputStream()) {
            try {
                outStream.write(request.getBytes());
                outStream.save();
                response.setSuccess(true);
            } catch (IOException e) {
                String msg = "Failed to save localization file " + file;
                statusHandler.handle(Priority.PROBLEM, msg, e);
                response.setSuccess(false);
                response.setMessage(msg);

            }
        }
        return response;
    }

}
