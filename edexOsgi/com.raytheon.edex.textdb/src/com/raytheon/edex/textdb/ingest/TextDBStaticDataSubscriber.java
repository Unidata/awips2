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
package com.raytheon.edex.textdb.ingest;

import java.io.File;
import java.io.IOException;

import com.raytheon.edex.textdb.dbapi.impl.TextDBStaticData;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * Text DB static data subscriber.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 25, 2011            bfarmer     Initial creation
 * Oct 18, 2011 10909      rferrel     notify() now saves a file.
 * Mar 06, 2014   2876     mpduff      New NDM plugin.
 * Mar 20, 2014   2915     dgilling    Code cleanup.
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class TextDBStaticDataSubscriber implements INationalDatasetSubscriber {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextDBStaticDataSubscriber.class);

    @Override
    public void notify(String fileName, File file) {
        // Assumes the fileName is the name of the file to place
        // in the BASE directory.
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        File outFile = pathMgr.getFile(lc, FileUtil.join("textdb", fileName));

        try {
            FileUtil.copyFile(file, outFile);
            TextDBStaticData.setDirty();
        } catch (IOException e) {
            statusHandler.error("Unable to copy textdb static data file ["
                    + file.getPath() + "] to destination [" + outFile.getPath()
                    + "].", e);
        }
    }
}
