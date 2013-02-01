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

import java.util.Arrays;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.VizStyleException;
import com.raytheon.viz.core.style.image.ImagePreferences;

/**
 * provides static utility method for getting the correct units for radar data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2010 #4473      rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class DataUtilities {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataUtilities.class);

    /**
     * Returns dataUnit with scale/offset accounted for.
     * 
     * @param record
     * @return
     */
    @Deprecated
    public static Unit<?> getDataUnit(RadarRecord record) {
        return record.getDataUnit();
    }

    /**
     * Returns dataUnit but first checks style rules for alternative units for
     * different display modes. Used for VIL displayed as reflectivity.
     * 
     * @param record
     * @param mode
     * @return
     */
    public static Unit<?> getDataUnit(RadarRecord record, String mode) {
        // The purpose of this block is to find a new data Unit in the
        // style rules, a style rule with a creating entity is assumed
        // to specify different data units to use.
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(null);
        match.setParameterName(Arrays.asList(record.getProductCode() + ""));
        match.setCreatingEntityNames(Arrays.asList(mode));
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.IMAGERY, match);
        } catch (VizStyleException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error processing radar data Unit from Style Rules", e);

        }
        if (sr != null
                && (((ParamLevelMatchCriteria) sr.getMatchCriteria()))
                        .getCreatingEntityNames().contains(mode)) {
            ImagePreferences prefs = (ImagePreferences) sr.getPreferences();
            record.setUnit(prefs.getDisplayUnits().toString());
            if (prefs.getDataMapping() != null) {
                Unit<?> unit = prefs.getDataMapping().getImageUnit(
                        prefs.getDisplayUnits());
                UnitConverter converter = unit.getConverterTo(record
                        .getUnitObject());
                Object[] thresholds = record.getDecodedThresholds();
                for (int i = 1; i < 16; i++) {
                    thresholds[i] = ((Double) converter.convert(i))
                            .floatValue();
                }
                return unit;
            } else {
                return prefs.getDisplayUnits();
            }
        }
        return record.getDataUnit();
    }
}
