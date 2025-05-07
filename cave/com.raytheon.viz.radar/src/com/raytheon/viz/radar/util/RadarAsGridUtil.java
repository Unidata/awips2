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
package com.raytheon.viz.radar.util;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Utilities for displaying radar as grid
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2021 8652       njensen     Initial creation
 * Jun 24, 2022 8869       mapeters    Moved from com.raytheon.uf.viz.grid.radar,
 *                                     added more methods.
 * May 22, 2024 2037092    mapeters    Add virtual volume methods
 *
 * </pre>
 *
 * @author njensen
 */
public class RadarAsGridUtil {

    public static final String RCP = "RCP";

    public static final String RADAR_MODEL_PREFIX = "radar-";

    private static final String VIRTUAL_VOLUME = "virt";

    /**
     * Extracts the radar icao from a model name field
     *
     * @param modelName
     * @return the radar icao
     */
    public static String getIcaoFromModelName(String modelName) {
        if (modelName.startsWith(RADAR_MODEL_PREFIX)) {
            return modelName.substring(RADAR_MODEL_PREFIX.length());
        }

        return null;
    }

    /**
     * Builds a model name for the given radar icao.
     *
     * @param icao
     * @return the radar model name
     */
    public static String getModelNameForIcao(String icao) {
        return RADAR_MODEL_PREFIX + icao;
    }

    /**
     * Determine if the given model name is for radar data.
     *
     * @param modelName
     * @return true if the model name is for radar, false otherwise
     */
    public static boolean isRadarModelName(String modelName) {
        return modelName.startsWith(RADAR_MODEL_PREFIX);
    }

    /**
     * Get the virtual volume version of the given radar parameter (RR ->
     * RRvirt). If the parameter is already the virtual volume version, it is
     * returned as is.
     *
     * @param paramAbbrev
     *            parameter to convert
     * @return "virtual volume" version of parameter
     */
    public static String getVirtualVolumeParamAbbrev(String paramAbbrev) {
        if (!isVirtualVolume(paramAbbrev)) {
            return paramAbbrev + VIRTUAL_VOLUME;
        }
        return paramAbbrev;
    }

    /**
     * Get the standard version of the given radar parameter (RRvirt -> RR). If
     * the parameter is already the standard version, it is returned as is.
     *
     * @param virtualParamAbbrev
     *            parameter to convert
     * @return standard version of parameter
     */
    public static String getStandardParamAbbrev(String virtualParamAbbrev) {
        if (isVirtualVolume(virtualParamAbbrev)) {
            return virtualParamAbbrev.substring(0,
                    virtualParamAbbrev.length() - VIRTUAL_VOLUME.length());
        }
        return virtualParamAbbrev;
    }

    /**
     * Determine if the given parameter is for "virtual volume" radar data
     * (RRvirt) or standard radar data (RR).
     *
     * @param paramAbbrev
     *            the parameter to check
     * @return true for virtual volume parameter, false for standard parameter
     */
    public static boolean isVirtualVolume(String paramAbbrev) {
        return paramAbbrev.endsWith(VIRTUAL_VOLUME);
    }

    /**
     * Return a set containing the given radar parameters, with their virtual
     * volume parameters added ([RR] -> [RR, RRvirt]).
     *
     * @param paramAbbrevs
     *            the parameter abbreviations to add the virtual volume versions
     *            to
     * @return set containing given parameters and their virtual volume
     *         counterparts
     */
    public static Set<String> addVirtualVolumeParamAbbrevs(
            Set<String> paramAbbrevs) {
        Set<String> rval = new HashSet<>(paramAbbrevs);
        for (String paramAbbrev : paramAbbrevs) {
            rval.add(getVirtualVolumeParamAbbrev(paramAbbrev));
        }
        return Collections.unmodifiableSet(rval);
    }
}
