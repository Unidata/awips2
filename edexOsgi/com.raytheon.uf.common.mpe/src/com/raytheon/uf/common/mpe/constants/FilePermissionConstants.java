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
package com.raytheon.uf.common.mpe.constants;

import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Set;

import com.raytheon.uf.common.util.file.IOPermissionsHelper;

/**
 * Defines permissions constants to be used when writing to the hydroapps share
 * directory.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2017  6334       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public final class FilePermissionConstants {

    public static final PosixFilePermission[] POSIX_FILE_PERMISSIONS = new PosixFilePermission[] {
            PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE,
            PosixFilePermission.GROUP_READ, PosixFilePermission.GROUP_WRITE };

    public static final PosixFilePermission[] POSIX_DIRECTORY_PERMISSIONS = new PosixFilePermission[] {
            PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE,
            PosixFilePermission.OWNER_EXECUTE, PosixFilePermission.GROUP_READ,
            PosixFilePermission.GROUP_WRITE,
            PosixFilePermission.GROUP_EXECUTE };

    public static final Set<PosixFilePermission> POSIX_FILE_SET = IOPermissionsHelper
            .getPermissionsAsSet(POSIX_FILE_PERMISSIONS);

    public static final FileAttribute<Set<PosixFilePermission>> POSIX_DIRECTORY_ATTRIBUTES = IOPermissionsHelper
            .getPermissionsAsAttributes(POSIX_DIRECTORY_PERMISSIONS);

    private FilePermissionConstants() {
    }
}