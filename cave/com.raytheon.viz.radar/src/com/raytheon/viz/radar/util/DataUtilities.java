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

import java.util.ArrayList;
import java.util.Arrays;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.DataLevelThreshold;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.VizStyleException;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.raytheon.viz.core.units.PiecewisePixel;
import com.raytheon.viz.radar.units.DigitalVilUnit;

/**
 * TODO Add Description
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
    public static Unit<?> getDataUnit(RadarRecord record) {
        Unit<?> rval = null;
        int numLevels = record.getNumLevels();
        Object[] thresholds = record.getDecodedThresholds();
        if (numLevels <= 16) {
            ArrayList<Integer> pixel = new ArrayList<Integer>();
            ArrayList<Float> real = new ArrayList<Float>();
            if ("V".equals(record.getDisplayModes())) {
                // V does this weird thing at zero, they have a data value of -1
                // at index 7 which just symbolizes that the data goes from -10
                // - 0, which seems pointless, and A1 also throws it out
                int p = 1;
                for (int i = 0; i < numLevels; i++) {
                    if (i == 7) {
                        continue;
                    }
                    if (thresholds[i] instanceof Float) {
                        pixel.add(p);
                        real.add((Float) thresholds[i]);
                    }
                    p++;
                }
            } else {
                for (int i = 0; i < numLevels; i++) {
                    if (thresholds[i] instanceof Float) {
                        if (real.contains(thresholds[i])) {
                            // Try to determine if we can treat one of these
                            // different
                            Float fVal = (Float) thresholds[i];
                            Integer prevI = pixel.get(real
                                    .indexOf(thresholds[i]));
                            DataLevelThreshold prevThresh = new DataLevelThreshold(
                                    record.getThreshold(prevI));
                            DataLevelThreshold currThresh = new DataLevelThreshold(
                                    record.getThreshold(i));
                            if (prevThresh.isLessThan()
                                    || prevThresh.isGtrThan()) {
                                if (prevThresh.isLessThan()) {
                                    record.getDecodedThresholds()[prevI] = "<"
                                            + fVal.intValue();
                                } else {
                                    record.getDecodedThresholds()[prevI] = ">"
                                            + fVal.intValue();
                                }
                                real.remove(fVal);
                                real.add(fVal);
                                pixel.remove(prevI);
                                pixel.add(i);
                                continue;
                            } else if (currThresh.isLessThan()) {
                                record.getDecodedThresholds()[i] = "<"
                                        + fVal.intValue();
                                continue;
                            } else if (currThresh.isGtrThan()) {
                                record.getDecodedThresholds()[i] = ">"
                                        + fVal.intValue();
                                continue;
                            }
                        }
                        pixel.add(i);
                        real.add((Float) thresholds[i]);

                    }
                }
            }

            if (pixel.size() == 0) {
                return record.getUnitObject();
            }

            double[] pix = new double[pixel.size()];
            int i = 0;
            for (Integer p : pixel) {
                pix[i++] = p;
            }

            double[] std = new double[real.size()];

            boolean allZeroes = true;

            i = 0;
            for (Float r : real) {
                allZeroes = allZeroes && (r == 0);
                std[i++] = r;
            }
            if (allZeroes) {
                // allZeroes is not a valid unit and is basically disgarded,
                // this check is done for CFC
                rval = record.getUnitObject();
            } else {
                rval = new PiecewisePixel(record.getUnitObject(), pix, std);
            }
        } else if (record.getProductCode() == 134) {
            // Digital Vil is all messy in the spec.
            rval = new DigitalVilUnit(record.getThresholds());
        } else if (record.getThreshold(5) == 0) {
            // The offset and scale are set as ints
            double offset = record.getThreshold(0);
            double scale = record.getThreshold(1);
            int nLevels = record.getThreshold(2);
            // I believe all products have at least one flag value and DHR is
            // reporting 256 levels
            if (nLevels > 255) {
                nLevels = 255;
            }
            double[] pix = { 256 - nLevels, 255 };
            if (record.getProductCode() == 155) {
                pix = new double[] { 129, 149 };
            }

            double[] data = { offset, offset + (nLevels - 1) * scale };
            rval = new PiecewisePixel(record.getUnitObject(), pix, data);

        } else {
            // The offset and scale are set as floats
            double scale = Float.intBitsToFloat((record.getThreshold(0) << 16)
                    + record.getThreshold(1));
            if (scale == 0.0) {
                // 0.0 is sometimes used by HC and leads to a massively invalid
                // data range.
                scale = 1.0;
            }
            double offset = Float.intBitsToFloat((record.getThreshold(2) << 16)
                    + record.getThreshold(3));
            int nLevels = record.getThreshold(5);
            if (nLevels < 0) {
                // Only for DPR, the 65536 can't be encoded in a short
                nLevels = record.getNumLevels();
            }
            int nFlags = record.getThreshold(6);

            double[] pix = { nFlags, nLevels };
            double[] data = { (nFlags - offset) / scale,
                    (nLevels - offset) / scale };

            rval = new PiecewisePixel(record.getUnitObject(), pix, data);

        }
        return rval;
    }

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
        return DataUtilities.getDataUnit(record);
    }
}
