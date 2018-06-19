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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.grib;

import java.util.Map;

import com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.HPEException;
import com.raytheon.uf.edex.plugin.mpe.grib.HPEProcessGribFilesRunner;

import java.util.HashMap;;

/**
 * Determines, tracks, and constructs the {@link HPEProcessGribFilesRunner}
 * associated with a specified process flag and xmrg input name.
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

public class HPEGribFileRunnerFactory {

    private static HPEGribFileRunnerFactory INSTANCE;

    private final Map<HPEProcessGribFilesConfigLookupKey, HPEFieldgenGribConfigWrapper> configClassLookupMap = new HashMap<>();

    protected HPEGribFileRunnerFactory() {
        initialize();
    }

    public synchronized static HPEGribFileRunnerFactory getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new HPEGribFileRunnerFactory();
        }
        return INSTANCE;
    }

    public HPEProcessGribFilesRunner lookupRunner(final String gribOutputName,
            final String process, final String xmrgInputName)
                    throws HPEException {
        HPEFieldgenGribConfigWrapper configWrapper = lookupConfig(process,
                xmrgInputName);
        if (configWrapper == null) {
            throw new HPEException(
                    "Failed to prepare a runner. Unable to find configuration associated with process: "
                            + process + " and xmrg input: " + xmrgInputName
                            + ".");
        }
        return new HPEProcessGribFilesRunner(xmrgInputName, gribOutputName,
                configWrapper.getConfigClass(),
                configWrapper.getGribOutputProperty_tmp());
    }

    private HPEFieldgenGribConfigWrapper lookupConfig(final String process,
            final String xmrgInputName) {
        for (HPEProcessGribFilesConfigLookupKey lookupKey : configClassLookupMap
                .keySet()) {
            if (lookupKey.lookupKeyMatches(process, xmrgInputName)) {
                return configClassLookupMap.get(lookupKey);
            }
        }
        return null;
    }

    private void initialize() {
        /*
         * Register the various HPE Field Gen process grib file configuration
         * classes.
         */
        configClassLookupMap.put(
                new HPEProcessGribFilesConfigLookupKey(
                        DHRProcessGribFilesConfig.PROCESS_ID_MATCH,
                        DHRProcessGribFilesConfig.XMRG_MATCH),
                new HPEFieldgenGribConfigWrapper(
                        DHRProcessGribFilesConfig.class,
                        HPEFieldgenGribConstants.AppsDefaults.HPE_DHRMOSAIC_GRIB_DIR));
        configClassLookupMap.put(
                new HPEProcessGribFilesConfigLookupKey(
                        BDHRProcessGribFilesConfig.PROCESS_ID_MATCH,
                        BDHRProcessGribFilesConfig.XMRG_MATCH),
                new HPEFieldgenGribConfigWrapper(
                        BDHRProcessGribFilesConfig.class,
                        HPEFieldgenGribConstants.AppsDefaults.HPE_BDHRMOSAIC_GRIB_DIR));
        configClassLookupMap.put(
                new HPEProcessGribFilesConfigLookupKey(
                        BDSPProcessGribFilesConfig.PROCESS_ID_MATCH,
                        BDSPProcessGribFilesConfig.XMRG_MATCH),
                new HPEFieldgenGribConfigWrapper(
                        BDSPProcessGribFilesConfig.class,
                        HPEFieldgenGribConstants.AppsDefaults.HPE_EBMOSAIC_GRIB_DIR));
        configClassLookupMap.put(
                new HPEProcessGribFilesConfigLookupKey(
                        AbstractDSPProcessGribFilesConfig.PROCESS_ID_MATCH,
                        DSP_ERMOSAICProcessGribFilesConfig.XMRG_MATCH),
                new HPEFieldgenGribConfigWrapper(
                        DSP_ERMOSAICProcessGribFilesConfig.class,
                        HPEFieldgenGribConstants.AppsDefaults.HPE_ERMOSAIC_GRIB_DIR));
        configClassLookupMap.put(
                new HPEProcessGribFilesConfigLookupKey(
                        AbstractDSPProcessGribFilesConfig.PROCESS_ID_MATCH,
                        DSP_MAXRMOSAICProcessGribFilesConfig.XMRG_MATCH),
                new HPEFieldgenGribConfigWrapper(
                        DSP_MAXRMOSAICProcessGribFilesConfig.class,
                        HPEFieldgenGribConstants.AppsDefaults.HPE_ERMOSAIC_GRIB_DIR));
        configClassLookupMap.put(
                new HPEProcessGribFilesConfigLookupKey(
                        AbstractDSPProcessGribFilesConfig.PROCESS_ID_MATCH,
                        DSP_AVGRMOSAICProcessGribFilesConfig.XMRG_MATCH),
                new HPEFieldgenGribConfigWrapper(
                        DSP_AVGRMOSAICProcessGribFilesConfig.class,
                        HPEFieldgenGribConstants.AppsDefaults.HPE_ERMOSAIC_GRIB_DIR));
    }
}