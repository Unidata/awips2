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

/**
 * Creating the combinations file for the TextFormatter
 * 
 * <pre>
 *    
 * SOFTWARE HISTORY
 *    
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *6/12/2008                 mnash       Initial creation
 *03/07/2013    15717       jzeng       Change CAVE_STATIC to COMMON_STATIC
 *     
 * </pre>
 * 
 * @author mnash
 * @version 1
 */
package com.raytheon.viz.gfe.textformatter;

import java.io.File;
import java.io.IOException;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.request.SaveCombinationsFileRequest;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.internal.IFPClient;

public class CombinationsFileGenerator {

    /**
     * Generates combinations files based on just running the formatter
     * 
     * @param zoneGroupList
     * @param filename
     * @throws IOException
     */
    public static void generateAutoCombinationsFile(
            List<List<String>> zoneGroupList, String filename) throws Exception {
        generateCombinationsFile(zoneGroupList, filename, "");
    }

    /**
     * Generates combinations files based on user wanting to save
     * 
     * @param zoneGroupList
     * @param filename
     * @throws IOException
     */
    public static void generateSavedCombinationsFile(
            List<List<String>> zoneGroupList, String filename) throws Exception {

        if (filename.endsWith(".py")) {
            generateCombinationsFile(zoneGroupList, filename, "saved"
                    + File.separator);
        } else {
            generateCombinationsFile(zoneGroupList, filename + ".py", "saved"
                    + File.separator);
        }
    }

    /**
     * Called by both auto and saved functions to actually write file
     * 
     * @param zoneGroupList
     * @param filename
     * @param loc
     * @throws Exception
     */
    public static void generateCombinationsFile(
            List<List<String>> zoneGroupList, String filename, String loc)
            throws Exception {
        IFPClient ifpc = DataManager.getCurrentInstance().getClient();
        SaveCombinationsFileRequest req = new SaveCombinationsFileRequest();
        req.setFileName(FileUtil.join(loc, filename));
        req.setCombos(zoneGroupList);
        ifpc.makeRequest(req);

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE);
        pm.getFile(ctx, FileUtil.join("gfe", "combinations", filename));
    }
}
