package com.raytheon.uf.viz.radarapps.core;

import java.io.File;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.rcm.config.RcmResourceProvider;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;


/**
 * This is the CAVE implementation of RcmResourceProvider that gets files
 * from localization.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2015-09-08   DR 17944   D. Friedman Initial creation
 * </pre>
 */
public class LocalizationRcmResourceProvider extends RcmResourceProvider {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalizationRcmResourceProvider.class);

    private volatile Map<LocalizationFile, LocalizationFile> watchedLocalizationFiles;

    @Override
    public InputStream getResourceAsStream(String resource) {
        String localizationPath = "radar" + File.separator + resource;
        try {
            LocalizationFile lf = null;

            lf = PathManagerFactory.getPathManager().getStaticLocalizationFile(LocalizationType.COMMON_STATIC, localizationPath);
            synchronized (this) {
                if (watchedLocalizationFiles == null) {
                    watchedLocalizationFiles = new HashMap<LocalizationFile, LocalizationFile>();
                }
                LocalizationFile oldLf = watchedLocalizationFiles.get(lf);
                if (oldLf == null) {
                    watchedLocalizationFiles.put(lf, lf);
                    lf.addFileUpdatedObserver(new ILocalizationFileObserver() {
                        @Override
                        public void fileUpdated(FileUpdatedMessage message) {
                            notifyResourceChanged(LocalizationUtil
                                    .extractName(message.getFileName()));
                        }
                    });
                }
            }

            return lf.openInputStream();
        } catch (LocalizationException e) {
            statusHandler.error(String.format("Could not open localization file %s: %s", localizationPath, e.getMessage()), e);
            return null;
        }
    }

}
