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

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DataFieldTableLookup {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataFieldTableLookup.class);

    private static DataFieldTableLookup instance;

    private final Pattern DASH = Pattern.compile("-");

    private final Pattern UNDERSCORE = Pattern.compile("_");

    private final Pattern NEWLINE = Pattern.compile("\n");

    private final Pattern PIPE = Pattern.compile("\\|");

    private final Map<String, String> data2cdl = new HashMap<String, String>();

    private final Map<String, String> cdl2data = new HashMap<String, String>();

    private final Map<String, String> data2name = new HashMap<String, String>();

    public static synchronized DataFieldTableLookup getInstance() {
        if (instance == null) {
            instance = new DataFieldTableLookup();
        }
        return instance;
    }

    private DataFieldTableLookup() {
        Map<LocalizationLevel, LocalizationFile> files = PathManagerFactory
                .getPathManager()
                .getTieredLocalizationFile(LocalizationType.EDEX_STATIC,
                        "grib/dataFieldTable.txt");
        loadDataFieldTable(files.get(LocalizationLevel.BASE).getFile());
        if (files.containsKey(LocalizationLevel.SITE)) {
            loadDataFieldTable(files.get(LocalizationLevel.SITE).getFile());
        }
    }

    private void loadDataFieldTable(File file) {
        String contents = "";
        try {
            contents = FileUtil.file2String(file);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        for (String line : NEWLINE.split(contents)) {
            line = line.trim();
            if (line.startsWith("//") || line.startsWith("#")) {
                // ignore comments
                continue;
            }
            String[] parts = PIPE.split(line);
            if (parts.length < 2) {
                // invalid line
                continue;
            }
            String data = parts[0].trim();
            if (data.isEmpty()) {
                continue;
            }
            String cdl = parts[1].trim();
            if (!cdl.isEmpty()) {
                data2cdl.put(data, cdl);
                cdl2data.put(cdl, data);
            }
            if (parts.length < 5) {
                // invalid line
                continue;
            }
            String name = parts[4].trim();
            if (!name.isEmpty()) {
                data2name.put(data, name);
            }
        }
    }

    public String lookupCdlName(String dataField) {
        String retVal = data2cdl.get(dataField);
        if (retVal == null) {
            retVal = data2cdl.get(DASH.matcher(dataField).replaceAll("_"));
            if (retVal == null) {
                retVal = dataField;
            }
        }
        return retVal;
    }

    public String lookupDataName(String cdlField) {
        String retVal = cdl2data.get(cdlField);
        if (retVal != null) {
            retVal = UNDERSCORE.matcher(retVal).replaceAll("-");
        }
        return retVal;
    }

    public String lookupName(String dataField) {
        String retVal = data2name.get(dataField);
        if (retVal == null) {
            retVal = data2name.get(DASH.matcher(dataField).replaceAll("_"));
        }
        return retVal;
    }
}
