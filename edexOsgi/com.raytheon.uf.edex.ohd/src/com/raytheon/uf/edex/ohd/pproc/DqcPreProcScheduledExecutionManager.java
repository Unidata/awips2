/**
 * 
 */
package com.raytheon.uf.edex.ohd.pproc;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.mpe.util.AppsDefaultsConversionWrapper;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.plugin.mpe.dqcpreprocessor.DQCPreProcessorConstants;

/**
 * Monitors the DqcPreProcSrv.run_dqc_preprocessor key/value pair in Apps
 * Defaults indicating whether or not the DQC PreProcessor should run
 * automatically per its cron settings.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 08, 2018 7184       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */
public class DqcPreProcScheduledExecutionManager
        implements ILocalizationPathObserver {

    private volatile Boolean enabled;

    public DqcPreProcScheduledExecutionManager() {
        determineScheduledExecution();

        final String appsDefaultsLocalizationPath = "hydro"
                + IPathManager.SEPARATOR + AppsDefaults.NAME;
        final IPathManager pathManager = PathManagerFactory.getPathManager();
        pathManager.addLocalizationPathObserver(appsDefaultsLocalizationPath,
                this);
    }

    public boolean isEnabled() {
        return Boolean.TRUE.equals(enabled);
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
        determineScheduledExecution();
    }

    private void determineScheduledExecution() {
        enabled = AppsDefaultsConversionWrapper.getPropertyAsBoolean(
                DQCPreProcessorConstants.AppsDefaults.DQCPREPROCSRV_RUN_DQC_PREPROCESSOR);
    }
}