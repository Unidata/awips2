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
package com.raytheon.uf.common.mpe.util;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * Apps_Defaults utility to convert String-based Apps_Defaults properties to
 * usable Java objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2016  5614       bkowal      Initial creation
 * May 13, 2016 5576       bkowal      Updated to be more of a general utility for
 *                                     Apps_Defaults property conversions.
 * Jun 13, 2016 5576       bkowal      Added {@link #parallelExecEnabled()}.
 * Jun 29, 2016 5576       bkowal      Added a TODO explaning future steps.
 * Jul 12, 2016 4619       bkowal      Relocated to common.
 * Aug 10, 2016 4619       bkowal      The parallel write flag is now a constant to
 *                                     ensure it is not accidentally overwritten.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public final class AppsDefaultsConversionWrapper {

    private static final String PARALLEL_DIRECTORY = "-parallel";

    private static final boolean SUPPORT_PARALLEL_EXEC = true;

    protected AppsDefaultsConversionWrapper() {
    }

    /**
     * Returns a boolean indicating whether or not parallel MPE execution is
     * currently enabled. TODO: completely remove this method when the decision
     * is made to transition to the new MPE framework. Expect several errors
     * that will need to be resolved after removing this method.
     * 
     * @return {@code true}, if parallel execution is enabled; {@code false},
     *         otherwise
     */
    public static boolean parallelExecEnabled() {
        return SUPPORT_PARALLEL_EXEC;
    }

    /**
     * Retrieves a {@link Path} associated with an Apps_Defaults token. Will
     * also verify that that Path exists and make any required standard
     * modifications to the Path before returning it.
     * 
     * @param token
     *            the specified Apps_Defaults token.
     * @return the {@link Path} associated with the specified token.
     * @throws AppsDefaultsPathException
     */
    public static Path getPathForToken(final String token)
            throws AppsDefaultsPathException {
        final String directory = AppsDefaults.getInstance().getToken(token);
        if (directory == null) {
            throw new IllegalArgumentException("No " + AppsDefaults.NAME
                    + " path is associated with token: " + token + ".");
        }

        Path directoryPath = Paths.get(directory);
        if (SUPPORT_PARALLEL_EXEC) {
            String destinationRoot = directoryPath.getParent().toString();
            String destinationDirectory = directoryPath.getFileName()
                    .toString() + PARALLEL_DIRECTORY;
            directoryPath = Paths.get(destinationRoot, destinationDirectory);
        }
        if (!Files.exists(directoryPath)) {
            try {
                Files.createDirectories(directoryPath);
            } catch (IOException e) {
                throw new AppsDefaultsPathException(token, directoryPath, e);
            }
        }

        return directoryPath;
    }

    /**
     * Retrieves a {@link Boolean} for Apps_Defaults properties.
     * 
     * @param token
     *            the specified token identifying the property to retrieve.
     * @return {@code null} if the property is not found. True if the retrieved
     *         value is within the recognized set of values that should be
     *         interpreted as 'true'; False, otherwise.
     */
    public static Boolean getPropertyAsBoolean(final String token) {
        final String tokenValue = AppsDefaults.getInstance().getToken(token);
        if (tokenValue == null) {
            return null;
        }

        return AppsDefaults.getInstance().consideredTrue(tokenValue) ? Boolean.TRUE
                : Boolean.FALSE;
    }
}