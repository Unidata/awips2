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
package com.raytheon.uf.edex.plugin.modelsounding.common;

import com.raytheon.uf.common.dataplugin.modelsounding.SoundingSite;
import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Path Provider for Model Sounding Data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 26, 2013           bkowal      Initial creation
 * Dec 02, 2013  2537     bsteffen    Remove fcstseconds from SoundingSite.
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class ModelSoundingPathProvider extends DefaultPathProvider {
    private static final String FILENAME_SEPARATOR = "-";

    private static final String FORECAST_HR_SPECIFIER = "FH";

    /**
     * 
     */
    public ModelSoundingPathProvider() {
    }

    @Override
    public String getHDFFileName(String pluginName, IPersistable persistable) {
        SoundingSite soundingSite = (SoundingSite) persistable;

        long forecastHour = soundingSite.getDataTime().getFcstTime()
                / TimeUtil.SECONDS_PER_HOUR;

        StringBuilder stringBuilder = new StringBuilder(pluginName);
        stringBuilder.append(FILENAME_SEPARATOR);
        stringBuilder.append(soundingSite.getReportType());
        stringBuilder.append(fileNameFormat.get().format(
                soundingSite.getDataTime().getRefTime()));
        stringBuilder.append(FILENAME_SEPARATOR);
        stringBuilder.append(FORECAST_HR_SPECIFIER);
        stringBuilder.append(FILENAME_SEPARATOR);
        stringBuilder.append(Long.toString(forecastHour));
        stringBuilder.append(DefaultPathProvider.HDF5_SUFFIX);

        return stringBuilder.toString();
    }
}