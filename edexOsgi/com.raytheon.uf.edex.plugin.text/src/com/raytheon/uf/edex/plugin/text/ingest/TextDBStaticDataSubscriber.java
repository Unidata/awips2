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
package com.raytheon.uf.edex.plugin.text.ingest;

import java.io.File;
import java.io.IOException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;
import com.raytheon.uf.edex.plugin.text.impl.TextDBStaticData;

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
 * Mar 17, 2014 DR 16449   D. Friedman Send 'setDirty' notification to all nodes.
 * Mar 20, 2014   2915     dgilling    Code cleanup.
 * May 20, 2014 2536       bclement    moved from edex.textdb to edex.plugin.text
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class TextDBStaticDataSubscriber implements INationalDatasetSubscriber {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextDBStaticDataSubscriber.class);

    private String setDirtyURI;

    public TextDBStaticDataSubscriber(String setDirtyURI) {
        this.setDirtyURI = setDirtyURI;
    }

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
        try {
            if (setDirtyURI != null) {
                EDEXUtil.getMessageProducer().sendAsyncUri(setDirtyURI, "");
            } else {
                setDirty();
            }
        } catch (EdexException e) {
            statusHandler.error("Unable to notify that TextDB static files have changes", e);
        }
        } catch (IOException e) {
            statusHandler.error("Unable to copy textdb static data file ["
                    + file.getPath() + "] to destination [" + outFile.getPath()
                    + "].", e);
        }
    }
    public void setDirty() {
        TextDBStaticData.setDirty();
    }
}
