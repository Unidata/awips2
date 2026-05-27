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
package com.raytheon.uf.edex.plugin.mpe.grib;

import java.nio.file.Path;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.mpe.gribit2.AppsDefaultsOverrides;
import com.raytheon.uf.common.mpe.gribit2.XmrgToGribConversionException;
import com.raytheon.uf.common.mpe.gribit2.XmrgToGribConverter;
import com.raytheon.uf.common.mpe.gribit2.XmrgToGribInitializationException;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.mpe.util.AppsDefaultsPathException;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsConfigLoader;
import com.raytheon.uf.edex.plugin.mpe.apps.AppsDefaultsLoadException;
import com.raytheon.uf.edex.plugin.mpe.apps.RequiredTokenMissingException;

/**
 * Abstraction of the process grib files scripts.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public abstract class AbstractGribFilesRunner {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    protected final String xmrgInputName;

    protected final String gribOutputName;

    private final Class<? extends IProcessGribFilesConfig> configClass;

    /*
     * Temporary field to support parallel execution. TODO: this field should be
     * removed when parallel execution is no longer needed.
     */
    protected final String gribOutputPathProperty_tmp;

    public AbstractGribFilesRunner(final String xmrgInputFile,
            final String gribOutputName,
            final Class<? extends IProcessGribFilesConfig> configClass,
            final String gribOutputPathProperty_tmp) {
        this.xmrgInputName = xmrgInputFile;
        this.gribOutputName = gribOutputName;
        this.configClass = configClass;
        this.gribOutputPathProperty_tmp = gribOutputPathProperty_tmp;
    }

    public final void execute() throws ProcessGribException {
        ITimer timer = TimeUtil.getTimer();
        timer.start();

        logger.info("Process Grib File has started ...");

        IProcessGribFilesConfig config = null;
        try {
            config = configClass.newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new ProcessGribException(
                    "Failed to instantiate Grib Files Runner config: "
                            + configClass.getName() + ".",
                    e);
        }
        try {
            generateMpeGrib(config);
        } catch (Exception e) {
            timer.stop();
            logger.info("Process Grib File has failed to finished in "
                    + TimeUtil.prettyDuration(timer.getElapsedTime()) + ".", e);
            if (e instanceof ProcessGribException) {
                throw e;
            } else {
                throw new ProcessGribException(
                        "An unexpected Exception has occurred in Process Grib File.",
                        e);
            }
        }

        timer.stop();
        logger.info("Process Grib File has successfully finished in {}.",
                TimeUtil.prettyDuration(timer.getElapsedTime()));
    }

    /**
     * Generates the mpe grib file. The mpe grib file will always be generated.
     * 
     * @throws ProcessGribException
     */
    private void generateMpeGrib(IProcessGribFilesConfig config)
            throws ProcessGribException {
        /*
         * configure this instance of process grib file.
         */
        try {
            AppsDefaultsConfigLoader.populateFromAppsDefaults(config);
        } catch (RequiredTokenMissingException | AppsDefaultsLoadException e) {
            throw new ProcessGribException(
                    "Failed to configure the Process Grib Files Runner for: "
                            + xmrgInputName + " -> " + gribOutputName + ".");
        }

        final Path xmrgInputPath = config.getXmrgInputPath()
                .resolve(xmrgInputName);
        final Path gribOutputPath;
        if (AppsDefaultsConversionWrapper.parallelExecEnabled()) {
            final Path gribPath;
            try {
                gribPath = AppsDefaultsConversionWrapper
                        .getPathForToken(gribOutputPathProperty_tmp);
            } catch (AppsDefaultsPathException e) {
                throw new ProcessGribException(
                        "Failed to retrieve the directory path associated with "
                                + AppsDefaults.NAME + " property: "
                                + gribOutputPathProperty_tmp + ".",
                        e);
            }
            gribOutputPath = gribPath.resolve(gribOutputName);
        } else {
            gribOutputPath = config.getGribOutputPath().resolve(gribOutputName);
        }

        /*
         * Force the subcenter code to 0 in the created GRIB message.
         * Environment variables take precedence over Apps Default settings in
         * the legacy code. However, that will not work in this case because an
         * external application will not be started. So, override information
         * will be passed directly to the xmrg to grib converter.
         */
        final AppsDefaultsOverrides overrides = new AppsDefaultsOverrides();
        overrides.setSubCenter0(Boolean.TRUE);

        /*
         * Run gribit2 to generate the grib format file. input to gribit2 is
         * xmrg format file, output from gribit2 is grib 1 format file.
         */
        try {
            XmrgToGribConverter.getInstance().execute(xmrgInputPath,
                    gribOutputPath, overrides);
        } catch (XmrgToGribInitializationException
                | XmrgToGribConversionException e) {
            throw new ProcessGribException(
                    "Failed to complete the xmrg to grib conversion for: "
                            + xmrgInputPath.toString() + " -> "
                            + gribOutputPath.toString() + ".",
                    e);
        }

        postGribGeneration(config);
    }

    protected void postGribGeneration(IProcessGribFilesConfig config)
            throws ProcessGribException {
        /*
         * Default implementation does nothing.
         */
    }
}