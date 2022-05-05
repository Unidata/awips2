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
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.file.Files;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
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
 * Feb 15, 2018 7158       dgilling    Code cleanup.
 *
 * </pre>
 *
 * @author bfarmer
 */

public class AfosBrowserModelSubscriber implements INationalDatasetSubscriber {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AfosBrowserModelSubscriber.class);

    private static final String TEXTDB = "textdb";

    private static final String CC_CHELP_FILENAME = "textCCChelp.txt";

    private static final String CCC_HELP = LocalizationUtil.join(TEXTDB,
            CC_CHELP_FILENAME);

    private static final String NNN_HELP_FILENAME = "textNNNhelp.txt";

    private static final String NNN_HELP = LocalizationUtil.join(TEXTDB,
            NNN_HELP_FILENAME);

    private static final String CATEGORY_CLASS_FILENAME = "textCategoryClass.txt";

    private static final String CATEGORY_CLASS = LocalizationUtil.join(TEXTDB,
            CATEGORY_CLASS_FILENAME);

    private static final String ORIGIN_TABLE_FILENAME = "textOriginTable.txt";

    private static final String ORIGIN_TABLE = LocalizationUtil.join(TEXTDB,
            ORIGIN_TABLE_FILENAME);

    private static final String AFOS_MASTER_PIL_FILENAME = "afosMasterPIL.txt";

    private static final String AFOS_MASTER_PIL = LocalizationUtil.join(TEXTDB,
            AFOS_MASTER_PIL_FILENAME);

    private static final String AWIPS_MASTER_PIL_FILENAME = "awipsMasterPIL.txt";

    private static final String AWIPS_MASTER_PIL = LocalizationUtil.join(TEXTDB,
            AWIPS_MASTER_PIL_FILENAME);

    private static final String COMMENT_DELIM = "#";

    /**
     * The XXX part of an AFOS PIL must have at least one character.
     */
    private static final int MIN_AFOS_PIL_LEN = 7;

    @Override
    public void notify(String fileName, File file) {
        if (CC_CHELP_FILENAME.equals(fileName)) {
            saveFile(file, createOutfile(CCC_HELP));
        } else if (NNN_HELP_FILENAME.equals(fileName)) {
            saveFile(file, createOutfile(NNN_HELP));
        } else if (CATEGORY_CLASS_FILENAME.equals(fileName)) {
            saveFile(file, createOutfile(CATEGORY_CLASS));
        } else if (ORIGIN_TABLE_FILENAME.equals(fileName)) {
            saveFile(file, createOutfile(ORIGIN_TABLE));
        } else if (AFOS_MASTER_PIL_FILENAME.equals(fileName)) {
            saveFile(file, createOutfile(AFOS_MASTER_PIL));
            convertAfosToAwipsMasterPIL(file);
        } else if (AWIPS_MASTER_PIL_FILENAME.equals(fileName)) {
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
            try (SaveableOutputStream fos = outFile.openOutputStream()) {
                Files.copy(file.toPath(), fos);
                fos.save();
            } catch (IOException e) {
                statusHandler.error("Error reading from " + file
                        + " or writing to file " + outFile, e);
            } catch (LocalizationException e) {
                statusHandler.error("Error writing to " + outFile, e);
            }
        }
    }

    private void convertAfosToAwipsMasterPIL(File afosFile) {
        ILocalizationFile awipsFile = createOutfile(AWIPS_MASTER_PIL);

        int badLineCnt = 0;

        try (BufferedReader fis = Files.newBufferedReader(afosFile.toPath());
                SaveableOutputStream los = awipsFile.openOutputStream();
                BufferedWriter fos = new BufferedWriter(
                        new OutputStreamWriter(los))) {

            String line = null;
            int lineCnt = 0;
            while ((line = fis.readLine()) != null) {
                line = line.trim();
                ++lineCnt;

                String outLine = null;
                if (line.isEmpty() || line.startsWith(COMMENT_DELIM)) {
                    outLine = line;
                } else if (line.length() >= MIN_AFOS_PIL_LEN) {
                    /*
                     * Strip off the CCC from AFOS line.
                     */
                    outLine = line.substring(3);
                } else {
                    /* Skip bad lines. */
                    statusHandler.error("Cannot convert line <" + lineCnt
                            + ">: \"" + line + "\"");
                    badLineCnt++;
                }

                if (outLine != null) {
                    fos.write(outLine);
                    fos.newLine();
                }
            }

            fos.flush();
            los.save();
        } catch (IOException | LocalizationException e) {
            String msg = String.format(
                    "Could not create AWIPS master PIL file [%s] from AFOS master PIL file [%s]",
                    awipsFile, afosFile);
            statusHandler.error(msg, e);
        } finally {
            if (badLineCnt > 1) {
                statusHandler.error("Found " + badLineCnt + " bad lines");
                return;
            }
        }

        statusHandler.info("Created awipsMasterPIL.txt from AfosMasterPIL.txt");
    }
}
