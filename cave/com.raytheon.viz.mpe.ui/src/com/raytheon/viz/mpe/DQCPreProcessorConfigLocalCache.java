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
package com.raytheon.viz.mpe;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.apache.commons.collections.CollectionUtils;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.mpe.constants.MpeConstants;
import com.raytheon.uf.common.mpe.dqcpreprocessor.DQCPreProcRunConfiguration;
import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * Caches run configuration for DQC PreProcessor across a single CAVE session.
 * Also keeps track of the current MPE run area defined in Apps Defaults.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 5, 2018  7184       bkowal      Initial creation
 * Sep 10, 2018 7393       smanoj      Use mpe_dqc_num_days
 *                                     from AppsDefaults
 * </pre>
 *
 * @author bkowal
 */

public class DQCPreProcessorConfigLocalCache
        implements ILocalizationPathObserver {

    private static DQCPreProcessorConfigLocalCache INSTANCE;

    private final Object areaLock = new Object();

    private int numDays = AppsDefaults.getInstance().getInt("mpe_dqc_num_days",
            10);

    private boolean setOutputZero;

    private Map<String, Boolean> runAreas;

    public static synchronized DQCPreProcessorConfigLocalCache getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new DQCPreProcessorConfigLocalCache();
        }
        return INSTANCE;
    }

    private DQCPreProcessorConfigLocalCache() {
        /*
         * Populate the areas from Apps Defaults and watch for updates to the
         * base/site Apps Defaults file.
         */
        updateRunAreas();

        final String appsDefaultsLocalizationPath = "hydro"
                + IPathManager.SEPARATOR + AppsDefaults.NAME;
        final IPathManager pathManager = PathManagerFactory.getPathManager();
        pathManager.addLocalizationPathObserver(appsDefaultsLocalizationPath,
                this);
    }

    public void updateCache(final DQCPreProcRunConfiguration runConfig) {
        synchronized (areaLock) {
            this.numDays = runConfig.getNumDays();
            this.setOutputZero = runConfig.isSetZero();
            if (CollectionUtils.isNotEmpty(runConfig.getAreas())) {
                for (Entry<String, Boolean> entry : runAreas.entrySet()) {
                    final String areaName = entry.getKey();
                    if (runConfig.getAreas().contains(areaName)) {
                        runAreas.replace(areaName, Boolean.TRUE);
                    } else {
                        runAreas.replace(areaName, Boolean.FALSE);
                    }
                }
            }
        }
    }

    public int getNumDays() {
        return numDays;
    }

    public boolean isSetOutputZero() {
        return setOutputZero;
    }

    public Map<String, Boolean> getRunAreas() {
        synchronized (areaLock) {
            return new HashMap<>(runAreas);
        }
    }

    private void updateRunAreas() {
        synchronized (areaLock) {
            runAreas = new TreeMap<>();

            final AppsDefaults appsDefaults = AppsDefaults.getInstance();
            String mpeAreaNames = appsDefaults
                    .getToken(MpeConstants.AppsDefaults.MPE_AREA_NAMES, null);
            if (mpeAreaNames != null) {
                mpeAreaNames = mpeAreaNames.trim();
                if (!mpeAreaNames.isEmpty()) {
                    for (String mpeAreaName : mpeAreaNames.split(",")) {
                        runAreas.put(mpeAreaName.trim(), Boolean.TRUE);
                    }
                }
            }
            String mpeSiteId = appsDefaults
                    .getToken(MpeConstants.AppsDefaults.MPE_SITE_ID, null);
            if (mpeSiteId != null) {
                mpeSiteId = mpeSiteId.trim();
                if (!mpeSiteId.isEmpty()) {
                    runAreas.put(mpeSiteId, Boolean.TRUE);
                }
            }
        }
    }

    @Override
    public void fileChanged(ILocalizationFile file) {
        if (file.getContext()
                .getLocalizationLevel() != LocalizationContext.LocalizationLevel.BASE
                && file.getContext()
                        .getLocalizationLevel() != LocalizationContext.LocalizationLevel.SITE) {
            /*
             * Only the base and site level Apps Defaults files determine what
             * actions EDEX will take.
             */
            return;
        }
        updateRunAreas();
    }
}