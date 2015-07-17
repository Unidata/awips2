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
package com.raytheon.uf.edex.damagepath.handler;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.EnumSet;
import java.util.Set;

import com.raytheon.uf.common.damagepath.request.ExportToLdadRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.RunProcess;

/**
 * Request handler for {@code ExportToLdadRequest}. Sends given damage path tool
 * data to LDAD via scp.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 08, 2015  #4355     dgilling    Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class ExportToLdadHandler implements
        IRequestHandler<ExportToLdadRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ExportToLdadHandler.class);

    private static final String TEMP_FILE_NAME = "damage-path-tool";

    private static final String FILE_EXTENSION = ".js";

    private static final FileAttribute<Set<PosixFilePermission>> TEMP_FILE_PERMISSIONS = PosixFilePermissions
            .asFileAttribute(EnumSet.of(PosixFilePermission.OWNER_READ,
                    PosixFilePermission.OWNER_WRITE,
                    PosixFilePermission.GROUP_READ,
                    PosixFilePermission.GROUP_WRITE,
                    PosixFilePermission.OTHERS_READ));

    private static final String EXPORT_FILE_NAME_FMT = "%s.DPT.%2tY%<tm%<td.%<tH%<tM%<tS"
            + FILE_EXTENSION;

    private static final String ERROR_MSG_FORMAT = "Command [%s] returned non-zero exit code %d. Process output: %s";

    private final String scpHost;

    private final String scpUser;

    private final String scpDestination;

    public ExportToLdadHandler(final ExportToLdadJobConfig config) {
        this.scpHost = config.getHost();
        this.scpUser = config.getUser();
        this.scpDestination = config.getDestinationPath();
    }

    @Override
    public String handleRequest(ExportToLdadRequest request) throws Exception {
        String retVal = null;

        Path srcFile = writeTempFile(request.getData());
        String command = buildCommand(srcFile.toString(), request.getSiteId());

        RunProcess p = RunProcess.getRunProcess();
        p.exec(command);
        int returnCode = p.waitFor();
        if (returnCode != 0) {
            retVal = String.format(ERROR_MSG_FORMAT, command, returnCode,
                    p.getStderr());
            statusHandler.error(retVal);
        }

        try {
            Files.delete(srcFile);
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN,
                    "Could not delete temporary file.", e);
        }

        return retVal;
    }

    private Path writeTempFile(final byte[] contents) throws IOException {
        Path tempFile = Files.createTempFile(TEMP_FILE_NAME, FILE_EXTENSION,
                TEMP_FILE_PERMISSIONS);
        return Files.write(tempFile, contents);
    }

    private String buildCommand(final String sourceFile, final String siteID) {
        StringBuilder ldadCommand = new StringBuilder("scp -q");
        ldadCommand.append(' ');
        ldadCommand.append(sourceFile);
        ldadCommand.append(' ');
        ldadCommand.append(scpUser).append('@').append(scpHost);
        ldadCommand.append(':');
        ldadCommand.append(scpDestination);
        ldadCommand.append(String.format(EXPORT_FILE_NAME_FMT, siteID,
                TimeUtil.newDate()));
        return ldadCommand.toString();
    }
}
