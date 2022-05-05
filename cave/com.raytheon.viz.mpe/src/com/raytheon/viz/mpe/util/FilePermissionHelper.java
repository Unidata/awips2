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
package com.raytheon.viz.mpe.util;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.EnumSet;
import java.util.Set;

/**
 * Utility class help change file permissions. This class is only usable on
 * Operating Systems that implement POSIX.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul16, 2018    7357       smanoj      Initial creation
 *
 * </pre>
 *
 * @author smanoj
 */

public class FilePermissionHelper {
    private static final String ATTRIBUTE_POSIX = "posix";

    public static final PosixFilePermission[] POSIX_FILE_PERMISSIONS = new PosixFilePermission[] {
            PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE,
            PosixFilePermission.GROUP_READ, PosixFilePermission.GROUP_WRITE };

    public static final PosixFilePermission[] POSIX_DIRECTORY_PERMISSIONS = new PosixFilePermission[] {
            PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE,
            PosixFilePermission.OWNER_EXECUTE, PosixFilePermission.GROUP_READ,
            PosixFilePermission.GROUP_WRITE,
            PosixFilePermission.GROUP_EXECUTE };

    public static final Set<PosixFilePermission> POSIX_FILE_SET = getPermissionsAsSet(
            POSIX_FILE_PERMISSIONS);

    public static final FileAttribute<Set<PosixFilePermission>> POSIX_DIRECTORY_ATTRIBUTES = getPermissionsAsAttributes(
            POSIX_DIRECTORY_PERMISSIONS);

    /**
     * Constructor.
     */
    private FilePermissionHelper() {

    }

    /**
     * Attempts to apply the specified {@link PosixFilePermission}s to the
     * specified file {@link Path}. Fails silently if the file is not owned by
     * the current user.
     *
     * @param filePath
     *            the specified file {@link Path}.
     * @param permissions
     *            the specified {@link PosixFilePermission}s
     * @throws IOException
     * @deprecated in 17.3.1+ they should be replaced with calls to the
     *             common.util version.
     */
    @Deprecated
    public static void applyFilePermissions(final Path filePath,
            final Set<PosixFilePermission> permissions) throws IOException {
        if (filePath == null) {
            throw new IllegalArgumentException(
                    "Required argument 'filePath' cannot be NULL.");
        }
        if (permissions == null) {
            throw new IllegalArgumentException(
                    "Required argument 'permissions' cannot be NULL.");
        }
        verifyPosixSupport(filePath);

        try {
            if (Files.getOwner(filePath).getName()
                    .equals(System.getProperty("user.name"))) {
                Files.setPosixFilePermissions(filePath, permissions);
            }
        } catch (IOException e) {
            throw new IOException(
                    "Failed to update the permissions for file: "
                            + filePath.toString() + " to: "
                            + PosixFilePermissions.toString(permissions) + ".",
                    e);
        }
    }

    /**
     * Determines if the Operating System implements the Portable Operating
     * System Interface (POSIX).
     *
     * @return {@code true} if the OS does implement POSIX; {@code false},
     *         otherwise.
     * @deprecated in 17.3.1+ they should be replaced with calls to the
     *             common.util version.
     */
    @Deprecated
    protected static boolean isPosixSupported(final Path path) {
        return path.getFileSystem().supportedFileAttributeViews()
                .contains(ATTRIBUTE_POSIX);
    }

    /**
     * Verifies that the Operating System implements the Portable Operating
     * System Interface (POSIX).
     * 
     * @deprecated in 17.3.1+ they should be replaced with calls to the
     *             common.util version.
     */
    @Deprecated
    public static void verifyPosixSupport(final Path path) {
        if (!isPosixSupported(path)) {
            /*
             * This OS does not support the POSIX filesystem; so, this class
             * will not work correctly.
             */
            throw new RuntimeException(
                    "As presently implemented, WriteDQCNetCDFGrids.java"
                            + " is not compatible with "
                            + path.getFileSystem().getClass().getName() + ".");
        }
    }

    /**
     * Converts the specified array of {@link PosixFilePermission}s to an
     * {@link EnumSet} of {@link PosixFilePermission}s.
     *
     * @param permissionsToConvert
     *            the specified array of {@link PosixFilePermission}s
     * @return the {@link EnumSet} of {@link PosixFilePermission}s.
     * @deprecated in 17.3.1+ they should be replaced with calls to the
     *             common.util version.
     */
    @Deprecated
    public static Set<PosixFilePermission> getPermissionsAsSet(
            final PosixFilePermission[] permissionsToConvert) {
        if (permissionsToConvert == null) {
            throw new IllegalArgumentException(
                    "Required argument 'permissionsToConvert' cannot be NULL.");
        }

        EnumSet<PosixFilePermission> permissions = EnumSet
                .noneOf(PosixFilePermission.class);
        for (PosixFilePermission permissionToConvert : permissionsToConvert) {
            permissions.add(permissionToConvert);
        }
        return permissions;
    }

    /**
     * Converts the specified array of {@link PosixFilePermission}s to
     * {@link FileAttribute}s.
     *
     * @param permissionsToConvert
     *            the specified array of {@link PosixFilePermission}s
     * @return {@link FileAttribute}s
     * @deprecated in 17.3.1+ they should be replaced with calls to the
     *             common.util version.
     */
    @Deprecated
    public static FileAttribute<Set<PosixFilePermission>> getPermissionsAsAttributes(
            final PosixFilePermission[] permissionsToConvert) {
        return PosixFilePermissions
                .asFileAttribute(getPermissionsAsSet(permissionsToConvert));
    }

}
