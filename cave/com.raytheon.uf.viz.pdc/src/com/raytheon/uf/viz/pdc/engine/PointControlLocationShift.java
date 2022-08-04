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
package com.raytheon.uf.viz.pdc.engine;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.pdc.data.PDCLocationShift;

/**
 * Handles the PDC location shift feature.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2010 2635       mpduff      Initial creation
 * Aug 07, 2018 7400       dgilling    Code cleanup.
 * Sep 21, 2018 7379       mduff       Moved for PDC Refactor.
 *
 * </pre>
 *
 * @author mpduff
 */

public class PointControlLocationShift {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointControlLocationShift.class);

    /** The Location Shift Config file */
    public static final String LOCATION_SHIFT_CONFIG = LocalizationUtil
            .join("hydro", "pdc_loc_shift.txt");

    private PointControlLocationShift() {
        throw new AssertionError();
    }

    /**
     * Loads the shift configuration data file.
     * 
     * @return List of PDCLocationShift objects, one per shift record
     */
    public static List<PDCLocationShift> loadShiftData() {
        List<PDCLocationShift> list = new ArrayList<>();

        ILocalizationFile cfgFile = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(LOCATION_SHIFT_CONFIG);

        if ((cfgFile == null) || (!cfgFile.exists())) {
            statusHandler
                    .warn("Error locating location shift configutation file ["
                            + LOCATION_SHIFT_CONFIG + "]");
            return Collections.emptyList();
        }

        try (BufferedReader in = new BufferedReader(
                new InputStreamReader(cfgFile.openInputStream()))) {
            String str = null;
            while ((str = in.readLine()) != null) {
                if ((!str.startsWith("#")) && (StringUtils.isNotBlank(str))) {
                    String[] parts = str.split("\\s+");

                    if (parts.length >= 4) {
                        PDCLocationShift shift = new PDCLocationShift();
                        shift.setLid(parts[0]);
                        shift.setParamCode(parts[1]);
                        shift.setXShift(Integer.parseInt(parts[2]));
                        shift.setYShift(Integer.parseInt(parts[3]));
                        list.add(shift);
                    }
                }
            }
        } catch (IOException | LocalizationException e) {
            statusHandler.error("Error reading from file [" + cfgFile + "]", e);
        }

        return list;
    }
}
