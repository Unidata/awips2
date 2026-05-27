/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.edex.plugin.gfe.isc;

/**
 * Holds a singleton/static lock for Python ISC scripts ifpnetCDF and iscMosaic
 * to synchronize around to prevent multiple threads/sub-interpreters from
 * calling into the netCDF4 library at the same time, since the underlying
 * netCDF C library is not thread safe.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 04, 2024 2038100    njensen     Initial creation
 *
 *
 * </pre>
 *
 */
public class Netcdf4LibraryLock {

    private static final Object lock = new Object();

    private Netcdf4LibraryLock() {
        // don't allow instantiation
    }

    public static Object getLock() {
        return lock;
    }

}
