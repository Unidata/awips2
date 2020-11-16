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
import java.util.Arrays;
import java.util.EnumSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;

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
 * Mar 10, 2016  #5288     dgilling    Add ability to run command on LDAD host
 *                                     via SSH after file has been copied.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class ExportToLdadHandler implements
        IRequestHandler<ExportToLdadRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String TEMP_FILE_NAME = "damage-path-tool";

    private static final String FILE_EXTENSION = ".json";

    private static final FileAttribute<Set<PosixFilePermission>> TEMP_FILE_PERMISSIONS = PosixFilePermissions
            .asFileAttribute(EnumSet.of(PosixFilePermission.OWNER_READ,
                    PosixFilePermission.OWNER_WRITE,
                    PosixFilePermission.GROUP_READ,
                    PosixFilePermission.GROUP_WRITE,
                    PosixFilePermission.OTHERS_READ));

    private static final String EXPORT_FILE_NAME_FMT = "%s.DPT.%2tY%<tm%<td.%<tH%<tM%<tS"
            + FILE_EXTENSION;

    private static final String ERROR_MSG_FORMAT = "Command %s returned non-zero exit code %d. Process output: %s";

    private final String host;

    private final String user;

    private final String scpDestination;

    private final String postProcessCommand;

    public ExportToLdadHandler(final ExportToLdadJobConfig config) {
        this.host = config.getHost();
        this.user = config.getUser();
        this.scpDestination = config.getDestinationPath();
        this.postProcessCommand = config.getPostProcessCommand();
    }

    @Override
    public String handleRequest(ExportToLdadRequest request) throws Exception {
        Path srcFile = null;

        try {
            srcFile = writeTempFile(request.getData());
            String destFileName = getDestFileName(request.getSiteId());

            String[] command = buildScpCommand(srcFile.toString(), destFileName);
            String errorMsg = runSystemCommand(command);
            if (!errorMsg.isEmpty()) {
                statusHandler.error(errorMsg);
                return errorMsg;
            }

            /*
             * Executing a remote command after sending the file to LDAD is
             * optional.
             */
            if (StringUtils.isNotBlank(postProcessCommand)) {
                command = buildSshCommand(destFileName);
                errorMsg = runSystemCommand(command);
                if (!errorMsg.isEmpty()) {
                    statusHandler.error(errorMsg);
                    return errorMsg;
                }
            }
        } finally {
            if (srcFile != null) {
                /*
                 * We want to catch all exceptions from deleting the temp file
                 * because it does not warrant informing the caller that it
                 * failed. we can simply log a warning.
                 */
                try {
                    Files.delete(srcFile);
                } catch (Exception e) {
                    statusHandler.handle(Priority.WARN,
                            "Could not delete temporary file.", e);
                }
            }
        }

        return StringUtils.EMPTY;
    }

    private Path writeTempFile(final byte[] contents) throws IOException {
        Path tempFile = Files.createTempFile(TEMP_FILE_NAME, FILE_EXTENSION,
                TEMP_FILE_PERMISSIONS);
        return Files.write(tempFile, contents);
    }

    private String getDestFileName(final String siteID) {
        return String.format(EXPORT_FILE_NAME_FMT, siteID, TimeUtil.newDate());
    }

    private String[] buildScpCommand(final String sourceFile,
            final String destFile) {
        StringBuilder remoteDestination = new StringBuilder();
        remoteDestination.append(user).append('@').append(host);
        remoteDestination.append(':');
        remoteDestination.append(scpDestination);
        remoteDestination.append(destFile);

        return new String[] { "scp", "-q", sourceFile,
                remoteDestination.toString() };
    }

    private String[] buildSshCommand(final String destFile) {
        StringBuilder connectionString = new StringBuilder();
        connectionString.append(user).append('@').append(host);

        StringBuilder remoteCommand = new StringBuilder();
        remoteCommand.append(postProcessCommand).append(' ')
                .append(scpDestination).append(destFile);

        return new String[] { "ssh", "-q", connectionString.toString(),
                remoteCommand.toString() };
    }

    private String runSystemCommand(String[] commandAndArgs) throws IOException {
        RunProcess p = RunProcess.getRunProcess();
        p.exec(commandAndArgs);
        int returnCode = p.waitFor();
        if (returnCode != 0) {
            return String.format(ERROR_MSG_FORMAT,
                    Arrays.toString(commandAndArgs), returnCode, p.getStderr());
        }

        return StringUtils.EMPTY;
    }
}
