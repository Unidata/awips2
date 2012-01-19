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
package com.raytheon.uf.common.sounding;

import java.util.HashMap;
import java.util.Map;

/**
 * Enumeration constants for various upper air sounding levels identification.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06 Nov 2006             jkorman     Initial Coding
 * 20071127            382 jkorman     Moved from Cave graphing.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public enum LayerType {
    GENERIC(0, "generic"), SURFACE(1000, "surface"), MAN_PRESSURE(1010,
            "manPressure"), SIG_PRESSURE(1011, "sigPressure"), SIG_WIND(1020,
            "sigWind"), MAX_WIND(1021, "maxWind"), TROPOPAUSE(1012, "trop"), ACFT_FLT_LVL(
            2000, "flightLevel"), MODEL_DATA(3000, "modelData");

    public static final double[] MAN_LEVELS = { 1000, 925, 850, 700, 500, 400,
            300, 250, 200, 150, 100, };

    public static final double[] MAN_LEVELS_SHORT = { 0, 92, 85, 70, 50, 40,
            30, 25, 20, 15, 10, };

    private static final Map<Integer, LayerType> LayerTypeMap = new HashMap<Integer, LayerType>();
    static {
        LayerTypeMap.put(GENERIC.getLayerOrdinal(), GENERIC);
        LayerTypeMap.put(SURFACE.getLayerOrdinal(), SURFACE);
        LayerTypeMap.put(MAN_PRESSURE.getLayerOrdinal(), MAN_PRESSURE);
        LayerTypeMap.put(SIG_PRESSURE.getLayerOrdinal(), SIG_PRESSURE);
        LayerTypeMap.put(SIG_WIND.getLayerOrdinal(), SIG_WIND);
        LayerTypeMap.put(MAX_WIND.getLayerOrdinal(), MAX_WIND);
        LayerTypeMap.put(TROPOPAUSE.getLayerOrdinal(), TROPOPAUSE);
        LayerTypeMap.put(ACFT_FLT_LVL.getLayerOrdinal(), ACFT_FLT_LVL);
        LayerTypeMap.put(MODEL_DATA.getLayerOrdinal(), MODEL_DATA);
    }

    private final int layerOrdinal;

    private final String layerName;

    /**
     * Construct an Enum with a given ordinal value.
     * 
     * @param ordinalValue
     */
    private LayerType(int ordinalValue, String name) {
        layerOrdinal = ordinalValue;
        layerName = name;
    }

    /**
     * Get the ordinal value for this enum.
     * 
     * @return The ordinal value.
     */
    public int getLayerOrdinal() {
        return layerOrdinal;
    }

    /**
     * Get the LayerType enum corresponding to a specified data type name. If
     * the data type is not defined, a null is returned.
     * 
     * @param typeName
     *            ordinal LayerType data type name to find.
     * @return The LayerType if defined, null otherwise.
     */
    public static LayerType getCorrespondingEnum(Integer ordinal) {
        return LayerTypeMap.get(ordinal);
    }

    public String toString() {
        return this.layerName;
    }
}
