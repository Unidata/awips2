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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Performs setup operations for OHD services
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2014   2952     mpduff      Changed to use UFStatus for logging.
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */
public class SetupSrv implements ServiceInterface {
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(SetupSrv.class);

    /** set to true when setup service has succeeded */
    private static boolean isSetup = false;

    AppsDefaults appsDefaults = AppsDefaults.getInstance();

    public SetupSrv() {

        try {
            this.execute();
        } catch (Exception e) {
            logger.error("OHD Setup did not successfully complete: "
                    + e.getMessage());
        }

    }

    @Override
    public void execute() throws EdexException {

        isSetup = false;

        // any functions called below should throw an EdexException if any error
        // has occurred.

        copyLocalizationFiles();
        executeSetupProcedures();

        // if an exception occurs at any point prior isSetup will remain false.

        isSetup = true;
    }

    /**
     * Copies localizationFiles into a single location
     * 
     * @throws EdexException
     */
    private void copyLocalizationFiles() throws EdexException {
        IPathManager pm = PathManagerFactory.getPathManager();

        String apps_dir = AppsDefaults.getInstance().getToken("apps_dir");

        File[] hydroappsSourceDirectories = {
                pm.getFile(pm.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.BASE), OhdConstants.HYDROAPPS_DIR),
                pm.getFile(pm.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.SITE), OhdConstants.HYDROAPPS_DIR),
                pm.getFile(pm.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.USER), OhdConstants.HYDROAPPS_DIR) };

        File hydroappsDestinationDir = new File(apps_dir);

        for (File hydroappsSourceDir : hydroappsSourceDirectories) {

            if (hydroappsSourceDir.exists()) {
                try {
                    FileUtil.copyDirectory(hydroappsSourceDir,
                            hydroappsDestinationDir);
                } catch (IOException e) {
                    throw new EdexException(
                            "Error while copying localization files: "
                                    + e.getMessage());
                }
            }
        }

    }

    private void executeSetupProcedures() throws EdexException {

        String[] asciiFiles = { "rfc_boundary.dat", "state.dat" };

        List<String[]> setupScripts = new ArrayList<String[]>();

        String geo_util = appsDefaults.getToken("geo_util");
        String create_bas_bound = FileUtil.join(geo_util,
                "run_create_bas_bound");

        for (String asciiFile : asciiFiles) {
            setupScripts.add(new String[] { create_bas_bound, asciiFile,
                    asciiFile.replace(".dat", ".bin") });
        }

        String pproc_bin = appsDefaults.getToken("pproc_bin");

        // remove ruc.pl since run_create_mpe_station_lists calls a script that
        // halts if it exists
        new File(FileUtil.join(pproc_bin, "ruc.pl")).delete();

        if (!executeScripts(setupScripts)) {
            throw new EdexException("Error while executing setup procedures");
        }

    }

    /**
     * 
     * @param scripts
     *            a list of scripts and their arguments
     * @return true if the scripts exectuted succesfully
     */
    private boolean executeScripts(List<String[]> scripts) {

        boolean isSuccessful = true;

        for (String[] script : scripts) {
            if (MainMethod.runProgram(script) == 0) {
                logger.info(new File(script[0]).getName()
                        + " executed successfully");
            } else {
                isSuccessful = false;
            }
        }

        return isSuccessful;
    }

    /**
     * See if OHD Setup was a success.
     * 
     * @return true if setup was successful.
     */
    public static boolean isSetup() {
        return isSetup;
    }

}
