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
package com.raytheon.uf.edex.plugin.mpe.conversion;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.edex.plugin.mpe.AppsDefaultsConversionWrapper;
import com.raytheon.uf.edex.plugin.mpe.biasmesgen.BiasmesgenConstants;

/**
 * Bias Table files are deleted as they are processed by the "receiver". Valid
 * files are not deleted by the receiver; however, there is something removing
 * them because they never seem to accumulate. This class will make one attempt
 * to intercept the legacy-generated Bias Table files and create a copy that can
 * be used for conversion validation. TODO: the entire package that this class
 * is a part of should be removed when the decision has been made to use the
 * converted mpe applications.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2016  5576       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BiasTableInterceptor {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    public static final String VALIDATE_EXT = ".validate";

    /**
     * Attempt to copy the specified {@link File} to an alternate location for
     * validation purposes. All errors that prevent a successful copy are
     * handled and recorded internally to ensure no interference with the
     * original application flow.
     * 
     * @param biasTableFile
     *            the specified {@link File}
     */
    public void intercept(File biasTableFile) {
        if (!AppsDefaultsConversionWrapper.parallelExecEnabled()) {
            /*
             * parallel execution of the legacy mpe code and the converted mpe
             * code is not currently enabled. So, nothing to do.
             */
            return;
        }

        /*
         * Determine the source and destination {@link Path}s.
         */
        final Path biasTableInterceptPath = biasTableFile.toPath();
        Path biasTableValidationRootPath = null;
        try {
            biasTableValidationRootPath = AppsDefaultsConversionWrapper
                    .getPathForToken(BiasmesgenConstants.AppsDefaults.BIAS_MSG_DIR);
        } catch (Exception e) {
            logger.error("Failed to intercept bias table file: "
                    + biasTableInterceptPath.toString() + ".", e);
        }
        final Path biasTableValidationPath = Paths.get(
                biasTableValidationRootPath.toString(), biasTableInterceptPath
                        .getFileName().toString() + VALIDATE_EXT);

        logger.info("Intercepting bias table file: {} ...",
                biasTableInterceptPath.toString());
        try {
            Files.copy(biasTableInterceptPath, biasTableValidationPath,
                    StandardCopyOption.REPLACE_EXISTING);
        } catch (Exception e) {
            logger.error("Failed to intercept bias table file: "
                    + biasTableInterceptPath.toString() + ".", e);
            return;
        }
        logger.info("Successfully intercepted bias table file: {} -> {}.",
                biasTableInterceptPath.toString(),
                biasTableValidationPath.toString());
    }
}