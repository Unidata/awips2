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
package com.raytheon.uf.edex.plugin.ffmp.common;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPException;
import com.raytheon.uf.common.dataplugin.ffmp.FFTIException;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Manages the in memory cache and reading/writing FFTI binary files. Code
 * extracted from FFMPGenerator.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2018 6560       njensen     Initial creation
 * Sep 10, 2018 6720       njensen     Better error handling
 *
 * </pre>
 *
 * @author njensen
 */
public class FFTIContainer {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFTIContainer.class);

    /** FFTI siteDatakey to accum/ratio/diff cache **/
    private ConcurrentMap<String, FFTIData> fftiData = new ConcurrentHashMap<>();

    /**
     * Get the FFTI names used to cache FFTI data in memory
     *
     * @return
     */
    public Set<String> getFFTINames() {
        return fftiData.keySet();
    }

    /**
     * Checks if the filesystem has the FFTI data
     *
     * @param fftiName
     * @return
     */
    public boolean fftiExists(String fftiName) {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        ILocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteFFTIFileName(fftiName));
        return f != null && f.exists();
    }

    /**
     * Clears out the in memory cache of FFTI Data
     */
    public void clear() {
        fftiData.clear();
    }

    /**
     * Get FFTI data cache from the filesystem while retaining the reset/dirty
     * state
     *
     * @param fftiName
     * @return
     * @throws FFTIException
     */
    public FFTIData getFFTIData(String fftiName) throws FFTIException {
        // preserve the state of the reset value
        boolean reset = true;
        if (fftiData.containsKey(fftiName)) {
            reset = fftiData.get(fftiName).isReset();
        }

        FFTIData ffti = readFFTIData(fftiName);

        if (fftiData != null) {
            ffti.setReset(reset);
            fftiData.put(fftiName, ffti);
        }
        return ffti;
    }

    /**
     * Puts the FFTI data in a memory cache and writes it to the filesystem
     *
     * @param ffti
     */
    public void storeFFTIData(String fftiName, FFTIData ffti) {
        fftiData.put(fftiName, ffti);
        writeFFTIFile(fftiName, ffti);
    }

    /**
     * Write out FFTI data files
     *
     * @param ffti
     * @param fftiName
     */
    private void writeFFTIFile(String fftiName, FFTIData ffti) {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);

        LocalizationFile lflist = pathManager.getLocalizationFile(lc,
                getAbsoluteFFTIFileName(fftiName));

        try {
            FileUtil.bytes2File(SerializationUtil.transformToThrift(ffti),
                    lflist.getFile(), true);

            lflist.save();
            statusHandler.handle(Priority.DEBUG,
                    "Wrote FFMP FFTI file: " + fftiName);
        } catch (Exception e) {
            statusHandler.error("Error writing FFTI file " + fftiName + " to "
                    + lflist.getPath(), e);
        }
    }

    /**
     * Read in the FFTI data files
     *
     * @param fftiName
     * @return
     * @throws FFMPException
     */
    private FFTIData readFFTIData(String fftiName) throws FFTIException {
        FFTIData ffti = null;
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        LocalizationFile f = pathManager.getLocalizationFile(lc,
                getAbsoluteFFTIFileName(fftiName));

        try {
            ffti = SerializationUtil.transformFromThrift(FFTIData.class,
                    FileUtil.readCompressedFileAsBytes(f.getFile()));
        } catch (Exception e) {
            throw new FFTIException("Error reading FFTI file " + f.getPath(),
                    e);
        }

        return ffti;
    }

    /**
     * Gets the full localization path to the file associated with this FFTI
     * name
     *
     * @return
     */
    private String getAbsoluteFFTIFileName(String fftiName) {
        return "ffmp" + IPathManager.SEPARATOR + "ffti" + IPathManager.SEPARATOR
                + fftiName + ".bin";
    }

}
