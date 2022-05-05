package com.raytheon.uf.edex.plugin.text.handler;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.text.request.MixedCaseProductIdsRequest;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Mixed case product IDs request handler
 *
 * Returns a set of product IDs which are enabled for mixed case
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 27, 2020 8153       randerso    Initial creation
 *
 * </pre>
 *
 * @author randerso
 */
public class MixedCaseProductIdsHandler
        implements IRequestHandler<MixedCaseProductIdsRequest> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MixedCaseProductIdsHandler.class);

    private static final String MIXED_CASE_PIDS_FILE = LocalizationUtil
            .join("mixedCase", "mixedCaseProductIds.txt");

    private static final char COMMENT_DELIMITER = '#';

    private static Set<String> mixedCasePids;

    private static IPathManager pm = PathManagerFactory.getPathManager();

    private static ILocalizationPathObserver observer;

    @Override
    public Set<String> handleRequest(MixedCaseProductIdsRequest request)
            throws Exception {
        return getMixedCasePids();
    }

    /**
     * @return list of Product IDs enabled for mixed case transmission
     */
    public Set<String> getMixedCasePids() {
        // setup up the file updated observer
        synchronized (MixedCaseProductIdsHandler.class) {
            if (observer == null) {
                observer = new ILocalizationPathObserver() {
                    @Override
                    public void fileChanged(ILocalizationFile file) {
                        synchronized (MixedCaseProductIdsHandler.class) {
                            mixedCasePids = null;
                        }
                    }
                };

                pm.addLocalizationPathObserver(MIXED_CASE_PIDS_FILE, observer);
            }
        }

        synchronized (MixedCaseProductIdsHandler.class) {
            if (mixedCasePids == null) {

                // get all localization files in the hierarchy and merge them.
                Map<LocalizationLevel, LocalizationFile> fileHierarchy = pm
                        .getTieredLocalizationFile(
                                LocalizationType.COMMON_STATIC,
                                MIXED_CASE_PIDS_FILE);

                Set<String> newPids = new HashSet<>();
                for (LocalizationFile lf : fileHierarchy.values()) {
                    String filePath = lf.getPath();
                    try (BufferedReader in = new BufferedReader(
                            new InputStreamReader(lf.openInputStream()))) {

                        String line;
                        while ((line = in.readLine()) != null) {
                            int pos = line.indexOf(COMMENT_DELIMITER);
                            if (pos >= 0) {
                                line = line.substring(0, pos);
                            }
                            line = line.trim().toUpperCase();
                            String[] pids = line.split("[\\s,]+");
                            for (String pid : pids) {
                                if (pid.length() == 3) {
                                    newPids.add(pid);
                                } else if (pid.isEmpty()) {
                                    continue;
                                } else {
                                    statusHandler.warn("Invalid Product ID \""
                                            + pid + "\" found in " + filePath
                                            + ", ignored.");
                                }
                            }
                        }
                        mixedCasePids = newPids;
                    } catch (IOException e) {
                        statusHandler.error("Error reading " + filePath, e);
                    } catch (LocalizationException e) {
                        statusHandler.error("Error retrieving " + filePath, e);
                    }
                }

                mixedCasePids = newPids;
            }
        }

        return mixedCasePids;
    }
}
