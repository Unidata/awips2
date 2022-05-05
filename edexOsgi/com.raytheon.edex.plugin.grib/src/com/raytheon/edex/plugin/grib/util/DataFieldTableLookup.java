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
package com.raytheon.edex.plugin.grib.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------------------------
 * Jun 04, 2012           bsteffen  Initial creation
 * Apr 11, 2016  5564     bsteffen  Move localization files to common_static
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class DataFieldTableLookup {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataFieldTableLookup.class);

    private static DataFieldTableLookup instance;

    private final Pattern DASH = Pattern.compile("-");

    private final Pattern PIPE = Pattern.compile("\\|");

    private final Map<String, String> data2name = new HashMap<>();

    public static synchronized DataFieldTableLookup getInstance() {
        if (instance == null) {
            instance = new DataFieldTableLookup();
        }
        return instance;
    }

    private DataFieldTableLookup() {
        Map<LocalizationLevel, ? extends ILocalizationFile> files = PathManagerFactory
                .getPathManager().getTieredLocalizationFile(
                        LocalizationType.COMMON_STATIC,
                        "grib/dataFieldTable.txt");
        loadDataFieldTable(files.get(LocalizationLevel.BASE));
        if (files.containsKey(LocalizationLevel.SITE)) {
            loadDataFieldTable(files.get(LocalizationLevel.SITE));
        }
    }

    private void loadDataFieldTable(ILocalizationFile file) {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(
                file.openInputStream()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.startsWith("//") || line.startsWith("#")) {
                    // ignore comments
                    continue;
                }
                String[] parts = PIPE.split(line);
                if (parts.length < 5) {
                    // invalid line
                    continue;
                }
                String data = parts[0].trim();
                if (data.isEmpty()) {
                    continue;
                }
                String name = parts[4].trim();
                if (!name.isEmpty()) {
                    data2name.put(data, name);
                }
            }
        } catch (IOException | LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    public String lookupName(String dataField) {
        String retVal = data2name.get(dataField);
        if (retVal == null) {
            retVal = data2name.get(DASH.matcher(dataField).replaceAll("_"));
        }
        return retVal;
    }
}
