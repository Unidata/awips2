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
package com.raytheon.uf.edex.plugin.mpe.conversion.data;

import java.nio.file.Files;
import java.nio.file.Path;

import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;

/**
 * Constants utilized by the mpe interceptors. TODO: the entire package that
 * this class is a part of should be removed when the decision has been made to
 * use the converted mpe applications.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2016 5699       bkowal      Initial creation
 * Jul 12, 2016 4619       bkowal      Moved {@link AppsDefaultsConversionWrapper} to common.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public final class InterceptConstants {

    public static final String APPS_DEFAULTS_APPS_DIR = "apps_dir";

    private static final String INTERCEPT_DIR = "data_intercept";

    protected InterceptConstants() {
    }

    public static final Path getInterceptDataDirectory() throws Exception {
        final Path appsDirPath = AppsDefaultsConversionWrapper
                .getPathForToken(APPS_DEFAULTS_APPS_DIR);
        final Path interceptDataPath = appsDirPath.resolve(INTERCEPT_DIR);
        if (!Files.exists(interceptDataPath)) {
            Files.createDirectories(interceptDataPath);
        }

        return interceptDataPath;
    }
}