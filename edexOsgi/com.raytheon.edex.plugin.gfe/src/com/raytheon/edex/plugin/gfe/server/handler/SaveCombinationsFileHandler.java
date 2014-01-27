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
package com.raytheon.edex.plugin.gfe.server.handler;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.request.SaveCombinationsFileRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Request handler for <code>SaveCombinationsFileRequest</code>. Writes the
 * specified zone combinations to the specified site's combinations file
 * directory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2011            dgilling     Initial creation
 * Dec 02, 2013  #2591     dgilling     Only send notification after Writer is
 *                                      flushed/closed.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class SaveCombinationsFileHandler implements
        IRequestHandler<SaveCombinationsFileRequest> {

    private static final String COMBO_FILE_DIR = FileUtil.join("gfe",
            "combinations");

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public ServerResponse<Object> handleRequest(
            SaveCombinationsFileRequest request) throws Exception {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext localization = pm.getContextForSite(
                LocalizationType.CAVE_STATIC, request.getSiteID());

        File localFile = pm.getFile(localization,
                FileUtil.join(COMBO_FILE_DIR, request.getFileName()));
        boolean isAdded = (!localFile.exists());

        Writer outWriter = null;
        try {
            outWriter = new BufferedWriter(new FileWriter(localFile));
            String zoneComments = "\n# Automatically generated combinations file\n# "
                    + request.getFileName() + "\n\nCombinations = [\n";
            outWriter.write(zoneComments);

            NumberFormat df = new DecimalFormat("00");
            for (int i = 0; i < request.getCombos().size(); i++) {
                StringBuilder nextLineToWrite = new StringBuilder();
                List<String> modZGL = new ArrayList<String>(request.getCombos()
                        .get(i).size());
                for (String zone : request.getCombos().get(i)) {
                    modZGL.add("'" + zone + "'");
                }
                nextLineToWrite.append("\t([");
                nextLineToWrite.append(StringUtil.join(modZGL, ','));
                nextLineToWrite.append("], ");
                nextLineToWrite.append("'Region");
                nextLineToWrite.append(df.format(i + 1));
                nextLineToWrite.append("' ),\n");
                outWriter.write(nextLineToWrite.toString());
            }
            outWriter.write("]");
        } finally {
            if (outWriter != null) {
                outWriter.close();
            }
        }

        // placing the notification code here ensures we only send the
        // notification on a successful file write operation. Otherwise we would
        // have thrown an IOException and never gotten to this portion of the
        // request handler.
        FileChangeType changeType = isAdded ? FileChangeType.ADDED
                : FileChangeType.UPDATED;
        EDEXUtil.getMessageProducer().sendAsync(
                "utilityNotify",
                new FileUpdatedMessage(localization, FileUtil.join(
                        COMBO_FILE_DIR, request.getFileName()), changeType,
                        localFile.lastModified()));

        return new ServerResponse<Object>();
    }

}
