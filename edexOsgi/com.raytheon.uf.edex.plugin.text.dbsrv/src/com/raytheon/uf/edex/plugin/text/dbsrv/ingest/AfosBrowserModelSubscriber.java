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
package com.raytheon.uf.edex.plugin.text.dbsrv.ingest;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

/**
 * AFOS NDM Subscriber.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 12, 2011            bfarmer     Initial creation
 * Mar 06, 2014 2876       mpduff      New NDM plugin.
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.dbsrv
 * Nov 04, 2016 5975       rferrel     Create AWIPS master PIL from new AFOS PIL.
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class AfosBrowserModelSubscriber implements INationalDatasetSubscriber {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AfosBrowserModelSubscriber.class);

    private static final String CCC_HELP = "textdb/textCCChelp.txt";

    private static final String NNN_HELP = "textdb/textNNNhelp.txt";

    private static final String CATEGORY_CLASS = "textdb/textCategoryClass.txt";

    private static final String ORIGIN_TABLE = "textdb/textOriginTable.txt";

    private static final String AFOS_MASTER_PIL = "textdb/afosMasterPIL.txt";

    private static final String AWIPS_MASTER_PIL = "textdb/awipsMasterPIL.txt";

    private final static String COMMENT_DELIM = "#";

    /**
     * The XXX part of an AFOS PIL must have at least one character.
     */
    private static final int MIN_AFOS_PIL_LEN = 7;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.site.ingest.INationalDatasetSubscriber#notify(
     * java.lang.String, java.io.File)
     */
    @Override
    public void notify(String fileName, File file) {
        if ("textCCChelp.txt".equals(fileName)) {
            saveFile(file, createOutfile(CCC_HELP));
        } else if ("textNNNhelp.txt".equals(fileName)) {
            saveFile(file, createOutfile(NNN_HELP));
        } else if ("textCategoryClass.txt".equals(fileName)) {
            saveFile(file, createOutfile(CATEGORY_CLASS));
        } else if ("textOriginTable.txt".equals(fileName)) {
            saveFile(file, createOutfile(ORIGIN_TABLE));
        } else if ("afosMasterPIL.txt".equals(fileName)) {
            saveFile(file, createOutfile(AFOS_MASTER_PIL));
            convertAfosToAwipsMasterPIL(file);
        } else if ("awipsMasterPIL.txt".equals(fileName)) {
            saveFile(file, createOutfile(AWIPS_MASTER_PIL));
        }
    }

    private ILocalizationFile createOutfile(String filename) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        return pathMgr.getLocalizationFile(lc, filename);

    }

    private void saveFile(File file, ILocalizationFile outFile) {
        if ((file != null) && file.exists()) {
            try (BufferedReader fis = new BufferedReader(
                    new InputStreamReader(new FileInputStream(file)));
                    SaveableOutputStream so = outFile.openOutputStream();
                    BufferedWriter fos = new BufferedWriter(
                            new OutputStreamWriter(so));) {
                String line = null;
                while ((line = fis.readLine()) != null) {
                    fos.write(line);
                    fos.newLine();
                }
                fos.flush();
                so.save();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to copy file \"" + file.getAbsolutePath()
                                + "\" to \"" + outFile.getPath() + "\"",
                        e);
            }
        }
    }

    private void convertAfosToAwipsMasterPIL(File afosFile) {
        ILocalizationFile awipsFile = createOutfile(AWIPS_MASTER_PIL);

        int badLineCnt = 0;

        try (BufferedReader fis = new BufferedReader(
                new InputStreamReader(new FileInputStream(afosFile)));
                SaveableOutputStream so = awipsFile.openOutputStream();
                BufferedWriter fos = new BufferedWriter(
                        new OutputStreamWriter(so))) {

            String line = null;
            String outLine = null;
            String tLine = null;
            int lineCnt = 0;
            while ((line = fis.readLine()) != null) {
                tLine = line.trim();
                ++lineCnt;
                if ((tLine.isEmpty() == true)
                        || (tLine.startsWith(COMMENT_DELIM) == true)) {
                    outLine = line;
                } else if (tLine.length() >= MIN_AFOS_PIL_LEN) {
                    /*
                     * Strip off the CCC from AFOS line.
                     */
                    outLine = tLine.substring(3);
                } else {
                    /* Skip bad lines. */
                    statusHandler.handle(Priority.PROBLEM,
                            "Cannot convert line <" + lineCnt + ">: \"" + line
                                    + "\"");
                    badLineCnt++;
                    continue;
                }
                fos.write(outLine);
                fos.newLine();
            }
            fos.flush();
            so.save();
        } catch (IOException | LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not create AWIPS master PIL from AFOS master PIL ",
                    e);
            return;
        } finally {
            if (badLineCnt > 1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Found " + badLineCnt + " bad lines");
                return;
            }
        }
        statusHandler.handle(Priority.INFO,
                "Created awipsMasterPIL.txt from AfosMasterPIL.txt");
    }
}
