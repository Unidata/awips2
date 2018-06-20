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
package com.raytheon.uf.common.dataplugin.gfe.util;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileVisitOption;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.util.EnumSet;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * File Purger
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#      Engineer    Description
 * ------------------------------------ --------------------------
 * Aug 12, 2014  647 (17113)  randerso    Initial creation (code checked in by zhao on 12/16/2014 with slight modification)
 * Jul 18, 2016  5747         dgilling    Normalize provided path.
 * 
 * </pre>
 * 
 * @author randerso
 */

public class FilePurger {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FilePurger.class);

    final private Path rootPath;

    final private long purgeAge;

    final private boolean removeRoot;

    final private PathMatcher matcher;

    public FilePurger(String rootDirectory, long purgeAge) {
        this(rootDirectory, purgeAge, "**", false);
    }

    public FilePurger(String rootDirectory, long purgeAge,
            String filenamePattern) {
        this(rootDirectory, purgeAge, filenamePattern, false);
    }

    public FilePurger(String rootDirectory, long purgeAge,
            String filenamePattern, boolean removeRoot) {
        this.rootPath = Paths.get(rootDirectory).normalize();
        this.purgeAge = purgeAge;
        this.removeRoot = removeRoot;
        this.matcher = this.rootPath.getFileSystem().getPathMatcher(
                "glob:" + filenamePattern);
    }

    public void purge() {
        statusHandler.info("Purging files older than "
                + TimeUtil.prettyDuration(purgeAge) + " from " + rootPath);
        final FileTime purgeTime = FileTime.fromMillis(System
                .currentTimeMillis() - purgeAge);

        EnumSet<FileVisitOption> options = EnumSet
                .of(FileVisitOption.FOLLOW_LINKS);

        FileVisitor<Path> visitor = new SimpleFileVisitor<Path>() {
            /*
             * (non-Javadoc)
             * 
             * @see java.nio.file.SimpleFileVisitor#visitFile(java.lang .Object,
             * java.nio.file.attribute.BasicFileAttributes)
             */
            @Override
            public FileVisitResult visitFile(Path file,
                    BasicFileAttributes attrs) throws IOException {

                boolean matches = matcher.matches(file);
                boolean oldEnough = attrs.lastModifiedTime().compareTo(
                        purgeTime) < 0;

                if (!matches) {
                    statusHandler.debug(file + " doesn't match pattern");
                } else if (!oldEnough) {
                    statusHandler.debug(file + " not old enough");
                } else {
                    try {
                        Files.delete(file);
                        statusHandler.debug(file + " deleted");
                    } catch (Exception e) {
                        statusHandler.error("Unable to purge file: " + file, e);
                    }
                }

                return FileVisitResult.CONTINUE;
            }

            /*
             * (non-Javadoc)
             * 
             * @see java.nio.file.SimpleFileVisitor#visitFileFailed(java.lang
             * .Object, java.io.IOException)
             */
            @Override
            public FileVisitResult visitFileFailed(Path file, IOException exc)
                    throws IOException {
                statusHandler.error("Unable to purge file: " + file, exc);
                return FileVisitResult.CONTINUE;
            }

            /*
             * (non-Javadoc)
             * 
             * @see java.nio.file.SimpleFileVisitor#postVisitDirectory
             * (java.lang.Object, java.io.IOException)
             */
            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException exc)
                    throws IOException {
                try (DirectoryStream<Path> stream = Files
                        .newDirectoryStream(dir)) {
                    if (dir.equals(rootPath) && !removeRoot) {
                        statusHandler.debug(dir + " is root");
                    } else if (stream.iterator().hasNext()) {
                        statusHandler.debug(dir + " not empty");
                    } else {
                        Files.delete(dir);
                        statusHandler.debug(dir + " deleted");
                    }
                }
                return FileVisitResult.CONTINUE;
            }
        };

        try {
            Files.walkFileTree(rootPath, options, Integer.MAX_VALUE, visitor);
        } catch (Exception e) {
            statusHandler.error("Exception while purging " + rootPath, e);
        }
    }
}