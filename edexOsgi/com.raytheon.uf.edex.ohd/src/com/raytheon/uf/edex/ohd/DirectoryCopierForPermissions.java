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
package com.raytheon.uf.edex.ohd;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Set;
import java.io.File;

import com.raytheon.uf.common.util.file.IOPermissionsHelper;

/**
 * Custom implementation of {@link SimpleFileVisitor} designed/created to set
 * permissions on files and directories as they are copied.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2017  6334       bkowal      Initial creation
 * May 9, 2018  19240      jlinahan    Fix for DR Site Configuration Files Overwritten by Stop/Start EDEX
 *
 * </pre>
 *
 * @author bkowal
 */

public class DirectoryCopierForPermissions extends SimpleFileVisitor<Path> {

    private final Path oldRoot;

    private final Path newRoot;

    private final FileAttribute<Set<PosixFilePermission>> directoryPermissions;

    private final Set<PosixFilePermission> filePermissions;

    public DirectoryCopierForPermissions(Path oldRoot, Path newRoot,
            FileAttribute<Set<PosixFilePermission>> directoryPermissions,
            Set<PosixFilePermission> filePermissions) {
        this.oldRoot = oldRoot;
        this.newRoot = newRoot;
        this.directoryPermissions = directoryPermissions;
        this.filePermissions = filePermissions;
    }

    @Override
    public FileVisitResult preVisitDirectory(Path dir,
            BasicFileAttributes attrs) throws IOException {
        Path newDir = newRoot.resolve(oldRoot.relativize(dir));
        com.raytheon.uf.common.util.file.Files.createDirectories(newDir,
                directoryPermissions);
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
            throws IOException {
        Path newFile = newRoot.resolve(oldRoot.relativize(file));
        if (!new File(newFile.toString()).exists()) { // don't overwrite existing files
            Files.copy(file, newFile, StandardCopyOption.REPLACE_EXISTING);
        }
        IOPermissionsHelper.applyFilePermissions(newFile, filePermissions);
        return FileVisitResult.CONTINUE;
    }

}
