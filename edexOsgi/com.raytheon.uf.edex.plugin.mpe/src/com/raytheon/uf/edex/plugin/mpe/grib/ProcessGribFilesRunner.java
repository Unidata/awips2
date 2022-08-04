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
import com.raytheon.uf.common.mpe.constants.MpeConstants;

/**
 * Converts a xmrg file to a GRIB 1 file in response to the saving of a Best
 * Estimate. The source xmrg files are read from the directory associated with
 * the 'rfcwide_xmrg_dir' Apps Defaults property. The generated grib files are
 * written to the directory associated with the 'mpe_grib_dir' Apps Defaults
 * property. Based on:
 * /files.native/awipsShare/hydroapps/precip_proc/bin/process_grib_files.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 31, 2016 4628       bkowal      Initial creation
 * Oct 19, 2016 5631       bkowal      Refactored to extend {@link AbstractGribFilesRunner}.
 *
 * </pre>
 *
 * @author bkowal
 */

public class ProcessGribFilesRunner extends AbstractGribFilesRunner {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    public ProcessGribFilesRunner(final String xmrgInputFile,
            final String gribOutputName) {
        super(xmrgInputFile, gribOutputName, ProcessGribFilesConfig.class,
                MpeConstants.AppsDefaults.MPE_GRIB_DIR);
    }

    @Override
    protected void postGribGeneration(IProcessGribFilesConfig config)
            throws ProcessGribException {
        sendQpeToSbn((ProcessGribFilesConfig) config);
    }

    /**
     * Generates the qpe sbn grib file. The qpe sbn grib file is only generated
     * when property 'mpe_send_qpe_to_sbn' is set to "ON" in Apps Defaults.
     * 
     * @param config
     *            configuraton parameters read from Apps Defaults to use during
     *            the generation.
     * @throws ProcessGribException
     */
    private void sendQpeToSbn(final ProcessGribFilesConfig config)
            throws ProcessGribException {
        /*
         * At RFCs, a second version of the grib file needs to be created for
         * transmission over the SBN. This version of the GRIB file will contain
         * the code of the sending office.
         */
        if (!config.isSendQpeToSbn()) {
            return;
        }

        logger.info("Producing GRIB file for transmission over the SBN.");
        /*
         * This is important. Must make sure thet sub_center code is set to the
         * sending RFC.
         */
        final AppsDefaultsOverrides overrides = new AppsDefaultsOverrides();
        overrides.setSubCenter0(Boolean.FALSE);
        logger.info(
                "The subcenter code will be set to represent the sending office.");

        final Path sbnXmrgInputPath = config.getQpeSbnPath()
                .resolve(xmrgInputName);
        final Path sbnGribOutputPath;
        if (AppsDefaultsConversionWrapper.parallelExecEnabled()) {
            final Path gribPath;
            try {
                gribPath = AppsDefaultsConversionWrapper.getPathForToken(
                        MpeConstants.AppsDefaults.MPE_QPE_GRIB_SBN_DIR);
            } catch (AppsDefaultsPathException e) {
                throw new ProcessGribException(
                        "Failed to retrieve the directory path associated with "
                                + AppsDefaults.NAME + " property: "
                                + MpeConstants.AppsDefaults.MPE_QPE_GRIB_SBN_DIR
                                + ".",
                        e);
            }
            sbnGribOutputPath = gribPath.resolve(gribOutputName);
        } else {
            sbnGribOutputPath = config.getQpeGribSbnDirPath()
                    .resolve(gribOutputName);
        }

        /*
         * Run gribit2 to generate the grib format file. input to gribit2 is the
         * sbn xmrg format file, output from gribit2 is the sbn grib 1 format
         * file.
         */
        try {
            XmrgToGribConverter.getInstance().execute(sbnXmrgInputPath,
                    sbnGribOutputPath, overrides);
        } catch (XmrgToGribInitializationException
                | XmrgToGribConversionException e) {
            throw new ProcessGribException(
                    "Failed to complete the xmrg to grib conversion for: "
                            + sbnXmrgInputPath.toString() + " -> "
                            + sbnGribOutputPath.toString() + ".",
                    e);
        }
    }
}