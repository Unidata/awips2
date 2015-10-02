package com.raytheon.viz.hydro.timeseries.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.LinkedHashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * This class read the user preferred predefined PE-D-TS-EXT list from file:
 * hydro/preferred_order.txt File format: lines start with # is a comment One
 * line per LID LID PE1:TS1,TS2 PE2:TS2,TS3 ... Duplicated LID will replaced by
 * last one.
 * 
 * @author wkwock
 * 
 *         <pre>
 * SOFTWARE HISTORY 
 * Date       Ticket#    Engineer    Description 
 * ---------- ---------- ----------- -------------------------- 
 * 6/22/2015  DCS15102    wkwock      Initial creation.
 * </pre>
 */
public class PreferredOrderManager implements ILocalizationFileObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PreferredOrderManager.class);

    private static PreferredOrderManager pom = null;

    private Map<String, LinkedHashMap<String, String[]>> lidHm = new LinkedHashMap<String, LinkedHashMap<String, String[]>>();

    private boolean readFileFlag = true;

    private static final String COMMA_REGEX = "\\s*,\\s*";
    // For remove space around commas.

    private static final String COLON_REGEX = "\\s*:\\s*";
    // For remove spaces around colons.

    private static final String SPACE_REGEX = "\\s+";

    private PreferredOrderManager() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile file = pm
                .getStaticLocalizationFile(HydroConstants.PREFERRED_ORDER);
        if (file == null || !file.exists()) {
            return;
        }

        file.addFileUpdatedObserver(this);
    }

    private void readPreferredOrderFile() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile file = pm
                .getStaticLocalizationFile(HydroConstants.PREFERRED_ORDER);
        lidHm.clear();

        if (file == null || !file.exists()) {
            return;
        }

        StringBuilder message = new StringBuilder();

        BufferedReader in = null;
        try {
            in = new BufferedReader(new InputStreamReader(
                    file.openInputStream()));
            String line;
            inloop: while ((line = in.readLine()) != null) {
                String str = line.trim();
                if (str.startsWith("#") || str.length() == 0) {
                    continue;// This is a comment or blank line
                }

                str = str.replaceAll(COMMA_REGEX, ",").replaceAll(COLON_REGEX,
                        ":");
                String[] lineSplit = str.split(SPACE_REGEX);
                if (lineSplit.length < 2) {
                    message.append(line + "\n");
                    continue;
                }
                String lid = lineSplit[0].toUpperCase();
                LinkedHashMap<String, String[]> peHm = new LinkedHashMap<String, String[]>();
                for (int index = 1; index < lineSplit.length; index++) {
                    String[] peSplit = lineSplit[index].split(":");
                    if (peSplit.length > 2) {
                        message.append(line + "\n");
                        continue inloop;
                    }
                    String pe = peSplit[0].toUpperCase();

                    if (peSplit.length == 2) {
                        String[] tsSplit = peSplit[1].split(",");
                        peHm.put(pe, tsSplit);
                    } else {
                        peHm.put(pe, null);
                    }
                }
                lidHm.put(lid, peHm);
            }

            if (message.length() > 0) {
                message.insert(0,"Invalid line in file "+HydroConstants.PREFERRED_ORDER+":\n");
                message.append("Valid example: ACCM2 PP:RZ,RG TA:RZ\n");
            }
        } catch (IOException | LocalizationException e) {
            message.append("Failed to read file "+HydroConstants.PREFERRED_ORDER);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    message.append("Failed to close file "+HydroConstants.PREFERRED_ORDER);
                }
            }
        }

        if (message.length() > 0) {
            statusHandler.warn(message.toString());
        }
    }

    public final static synchronized PreferredOrderManager getInstance() {
        if (pom == null) {
            pom = new PreferredOrderManager();
        }

        return pom;
    }

    public Map<String, String[]> getPreferedOrder(String lid) {
        if (readFileFlag) {
            readPreferredOrderFile();
            readFileFlag = false;
        }
        
        Map<String, String[]> peMap = lidHm.get(lid);
        return peMap;
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        readFileFlag = true;
    }
}
