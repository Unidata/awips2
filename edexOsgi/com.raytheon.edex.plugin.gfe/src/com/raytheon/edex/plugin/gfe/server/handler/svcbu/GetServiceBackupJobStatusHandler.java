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
package com.raytheon.edex.plugin.gfe.server.handler.svcbu;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitOption;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.GetServiceBackupJobStatusRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.dataplugin.gfe.svcbu.ServiceBackupJobStatus;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Request handler for {@code GetServiceBackupJobStatusRequest}. This handler
 * will return the current status of service backup for the requested site (or
 * alternatively all sites currently in service backup mode) by finding all
 * service backup lock files in the file system and getting their internal
 * status.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            bphillip     Initial creation
 * Feb 12, 2015  #4103     dgilling     Renamed from GetSbLockFilesRequestHandler,
 *                                      rewrite using Java7 nio.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public final class GetServiceBackupJobStatusHandler implements
        IRequestHandler<GetServiceBackupJobStatusRequest> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetServiceBackupJobStatusHandler.class);

    private static final class ListLockFiles extends SimpleFileVisitor<Path> {

        private final Map<String, Collection<ServiceBackupJobStatus>> locksBySite;

        private final Collection<Path> requestedLockDirs;

        public ListLockFiles(Collection<String> siteIds) {
            super();
            this.locksBySite = new HashMap<>();

            this.requestedLockDirs = new HashSet<>(siteIds.size());
            for (String siteId : siteIds) {
                this.requestedLockDirs.add(SvcBackupUtil.getLockDir(siteId));
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.nio.file.SimpleFileVisitor#visitFile(java.lang.Object,
         * java.nio.file.attribute.BasicFileAttributes)
         */
        @Override
        public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                throws IOException {
            super.visitFile(file, attrs);
            String siteId = file.getParent().getFileName().toString()
                    .toUpperCase();
            String lockName = file.getFileName().toString();

            Collection<ServiceBackupJobStatus> siteLocks = locksBySite
                    .get(siteId);
            if (siteLocks == null) {
                siteLocks = new ArrayList<>();
                locksBySite.put(siteId, siteLocks);
            }
            JobProgress status = getLockStatus(file);
            Date lastModifiedTime = new Date(attrs.lastModifiedTime()
                    .toMillis());
            siteLocks.add(new ServiceBackupJobStatus(lockName, status,
                    lastModifiedTime));

            return FileVisitResult.CONTINUE;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * java.nio.file.SimpleFileVisitor#preVisitDirectory(java.lang.Object,
         * java.nio.file.attribute.BasicFileAttributes)
         */
        @Override
        public FileVisitResult preVisitDirectory(Path dir,
                BasicFileAttributes attrs) throws IOException {
            super.preVisitDirectory(dir, attrs);
            if ((requestedLockDirs.contains(dir))
                    || (requestedLockDirs.isEmpty())
                    || (dir.equals(SvcBackupUtil.getLockDir()))) {
                return FileVisitResult.CONTINUE;
            } else {
                return FileVisitResult.SKIP_SUBTREE;
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * java.nio.file.SimpleFileVisitor#visitFileFailed(java.lang.Object,
         * java.io.IOException)
         */
        @Override
        public FileVisitResult visitFileFailed(Path file, IOException exc)
                throws IOException {
            statusHandler.error("Unable to read service backup lock file: "
                    + file.toString(), exc);
            return FileVisitResult.CONTINUE;
        }

        private JobProgress getLockStatus(final Path lockFile) {
            JobProgress retVal = JobProgress.UNKNOWN;
            try {
                List<String> fileContents = Files.readAllLines(lockFile,
                        StandardCharsets.US_ASCII);
                if (!fileContents.isEmpty()) {
                    retVal = JobProgress.valueOf(fileContents.get(0).trim());
                }
            } catch (IOException e) {
                statusHandler.error("Unable to read lock status from file ["
                        + lockFile.toString() + "].", e);
            } catch (IllegalArgumentException e) {
                statusHandler.error(
                        "Invalid status value specified in lock file ["
                                + lockFile.toString() + "].", e);
            }

            return retVal;
        }

        public Map<String, Collection<ServiceBackupJobStatus>> getLocksBySite() {
            return locksBySite;
        }
    }

    @Override
    public ServerResponse<Map<String, Collection<ServiceBackupJobStatus>>> handleRequest(
            final GetServiceBackupJobStatusRequest request) throws Exception {
        ServerResponse<Map<String, Collection<ServiceBackupJobStatus>>> sr = new ServerResponse<>();
        sr.setPayload(getLockFiles(request.getRequestedSiteIds()));
        return sr;
    }

    private static Map<String, Collection<ServiceBackupJobStatus>> getLockFiles(
            final Collection<String> siteIds) throws IOException {
        Path lockDir = SvcBackupUtil.getLockDir();
        ListLockFiles lockFileLister = new ListLockFiles(siteIds);
        Files.walkFileTree(lockDir, EnumSet.of(FileVisitOption.FOLLOW_LINKS),
                2, lockFileLister);
        return lockFileLister.getLocksBySite();
    }
}
