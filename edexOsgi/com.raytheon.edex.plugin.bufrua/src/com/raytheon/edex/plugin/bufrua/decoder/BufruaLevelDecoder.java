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
package com.raytheon.edex.plugin.bufrua.decoder;

import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.plugin.bufrua.util.SigWindHeightConversionManager;
import com.raytheon.uf.common.dataplugin.bufrua.LayerTools;
import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.pointdata.PointDataView;

/**
 * 
 * Decodes specific levels within a bufrua file. The {@link BufrStructure} for
 * the level must contain a valid value for "Vertical sounding significance" or
 * "Extended vertical sounding significance". The significance is used to
 * determine what data to expect on the level.
 * 
 * This can handle many different BUFR Templates, for example [3 03 050], [3 03
 * 052], [3 03 054], [3 03 055], [3 03 003], [3 03 11], [3 03 012], [3 03 14]
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jul 06, 2016  5736     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class BufruaLevelDecoder {

    protected static Logger logger = LoggerFactory
            .getLogger(BufruaLevelDecoder.class);

    /* BUFR descriptor 0 08 001 */
    private static final String BUFR_VERTICAL_SOUNDING_SIGNIFICANCE = "Vertical sounding significance";

    /* BUFR descriptor 0 08 042 */
    private static final String BUFR_EXTENDED_VERTICAL_SOUNDING_SIGNIFICANCE = "Extended vertical sounding significance";

    /* BUFR descriptor 0 07 004 */
    private static final String BUFR_PRESSURE = "Pressure";

    /* BUFR descriptor 0 10 009 */
    private static final String BUFR_GEOPOTENTIAL_HEIGHT = "Geopotential Height";

    /* BUFR descriptor 0 07 003 */
    private static final String BUFR_GEOPOTENTIAL = "Geopotential";

    /* BUFR descriptor 0 12 101 */
    private static final String BUFR_TEMPERATURE = "Temperature/dry-bulb temperature";

    /* BUFR descriptor 0 12 103 */
    private static final String BUFR_DEWPOINT = "Dew-point temperature";

    /* BUFR descriptor 0 11 001 */
    private static final String BUFR_WIND_DIRECTION = "Wind direction";

    /* BUFR descriptor 0 11 002 */
    private static final String BUFR_WIND_SPEED = "Wind speed";

    /* BUFR descriptor 0 11 042 */
    private static final String BUFR_MAX_WIND_SPEED = "Maximum wind speed (10-minute mean wind)";

    public static void decodeLevel(UAObs obsData, BufrStructure levelStructure) {
        Number significance = levelStructure
                .lookupNumericValue(BUFR_VERTICAL_SOUNDING_SIGNIFICANCE);
        if (significance != null) {
            decodeNarrow(obsData, levelStructure, significance.intValue());
        } else {
            significance = levelStructure
                    .lookupNumericValue(BUFR_EXTENDED_VERTICAL_SOUNDING_SIGNIFICANCE);
            if (significance != null) {
                decodeExtended(obsData, levelStructure, significance.intValue());
            }
        }
    }

    /**
     * Decode a level that indicates significance using
     * {@link #BUFR_VERTICAL_SOUNDING_SIGNIFICANCE}.
     */
    protected static void decodeNarrow(UAObs obsData,
            BufrStructure levelStructure, int significance) {
        PointDataView view = obsData.getPointDataView();

        Set<VerticalSoundingSignificance> sigSet = VerticalSoundingSignificance
                .decode(significance);
        if (sigSet.isEmpty()) {
            logger.warn("Ignoring bufrua level with no vertical significance.");
            return;
        } else if (sigSet.size() > 1) {
            logger.warn("Ignoring bufrua level with more than one vertical significance: "
                    + sigSet);
            return;
        }
        VerticalSoundingSignificance sig = sigSet.iterator().next();
        switch (sig) {
        case SIG_WIND:
            decodeSigWindLevel(obsData, view, levelStructure);
            break;
        case SIG_TEMP:
            decodeSigTempLevel(view, levelStructure);
            break;
        case MAX_WIND:
            decodeMaxWindLevel(view, levelStructure);
            break;
        case TROPOPAUSE:
            decodeTropopauseLevel(view, levelStructure);
            break;
        case SURFACE:
            decodeSurfaceLevel(view, levelStructure);
            break;
        case STANDARD:
            decodeMandatoryLevel(view, levelStructure);
            break;
        default:
            logger.warn("Ignoring bufrua level with unexpected vertical significance: "
                    + sig);

        }
    }

    /**
     * Decode a level that indicates significance using
     * {@link #BUFR_EXTENDED_VERTICAL_SOUNDING_SIGNIFICANCE}.
     */
    protected static void decodeExtended(UAObs obsData,
            BufrStructure levelStructure, int significance) {
        PointDataView view = obsData.getPointDataView();
        Set<ExtendedVerticalSoundingSignificance> sigSet = ExtendedVerticalSoundingSignificance
                .decode(significance);
        if (sigSet.isEmpty()) {
            /*
             * From B/C25 "All bits set to 0 indicate a level determined by
             * national decision or a level of no significance that has been
             * included when high resolution data are reported."
             */
            decodeMandatoryLevel(view, levelStructure);
            return;
        }

        for (ExtendedVerticalSoundingSignificance sig : sigSet) {
            switch (sig) {
            case SIG_WIND:
                decodeSigWindLevel(obsData, view, levelStructure);
                break;
            case SIG_TEMP:
                decodeSigTempLevel(view, levelStructure);
                break;
            case SIG_HUM:
                decodeSigTempLevel(view, levelStructure);
                break;
            case MAX_WIND:
                decodeMaxWindLevel(view, levelStructure);
                break;
            case TROPOPAUSE:
                decodeTropopauseLevel(view, levelStructure);
                break;
            case SURFACE:
                decodeSurfaceLevel(view, levelStructure);
                break;
            case STANDARD:
                decodeMandatoryLevel(view, levelStructure);
                break;
            default:
                logger.warn("Ignoring bufrua level with unexpected vertical significance: "
                        + sig);
            }
        }
    }

    protected static void decodeSigWindLevel(UAObs obsData, PointDataView view,
            BufrStructure levelStructure) {
        int windIdx = view.getInt(LayerTools.NUM_SIGW);
        Number height = levelStructure.lookupNumericValue(BUFR_GEOPOTENTIAL);
        if (height != null) {
            /*
             * Only older TUA based records use Geopotential and only those
             * records encode height wrong. Newer B/C25 formatted data uses
             * Geopotential height and encodes the height correctly.
             */
            height = SigWindHeightConversionManager.convertHeight(obsData,
                    height.doubleValue());
        } else {
            height = levelStructure
                    .lookupNumericValue(BUFR_GEOPOTENTIAL_HEIGHT);
        }
        Number pressure = levelStructure.lookupNumericValue(BUFR_PRESSURE);
        if (pressure == null && height == null) {
            return;
        }
        if (height != null) {
            if (height.intValue() >= 30000) {
                return;
            }
            if (height.floatValue() == 0) {
                /*
                 * Only allow one level with height == 0, discard any additional
                 * levels.
                 */
                boolean haveSfc = false;
                for (int i = 0; i < windIdx; i += 1) {
                    if (0 == view.getFloat(LayerTools.HT_SIGW, i)) {
                        haveSfc = true;
                        break;
                    }
                }
                if (haveSfc) {
                    return;
                }
            }
            view.setFloat(LayerTools.HT_SIGW, height.floatValue(), windIdx);
        }

        if (pressure != null) {
            view.setFloat(LayerTools.PR_SIGW, pressure.floatValue(), windIdx);
        }

        copyValue(levelStructure, view, BUFR_WIND_DIRECTION,
                LayerTools.WD_SIGW, windIdx);
        copyValue(levelStructure, view, BUFR_WIND_SPEED, LayerTools.WS_SIGW,
                windIdx);

        view.setInt(LayerTools.NUM_SIGW, windIdx + 1);
    }

    protected static void decodeSigTempLevel(PointDataView view,
            BufrStructure levelStructure) {
        int tempIdx = view.getInt(LayerTools.NUM_SIGT);
        Number pressure = levelStructure.lookupNumericValue(BUFR_PRESSURE);
        if (pressure == null) {
            return;
        }
        Number temperature = levelStructure
                .lookupNumericValue(BUFR_TEMPERATURE);
        Number dewpoint = levelStructure.lookupNumericValue(BUFR_DEWPOINT);
        if (temperature == null && dewpoint == null) {
            return;
        }

        view.setFloat(LayerTools.PR_SIGT, pressure.floatValue(), tempIdx);
        if (temperature != null) {
            view.setFloat(LayerTools.TP_SIGT, temperature.floatValue(), tempIdx);
        }
        if (dewpoint != null) {
            view.setFloat(LayerTools.TD_SIGT, dewpoint.floatValue(), tempIdx);
        }
        view.setInt(LayerTools.NUM_SIGT, tempIdx + 1);
    }

    protected static void decodeMaxWindLevel(PointDataView view,
            BufrStructure levelStructure) {
        int maxWindIdx = view.getInt(LayerTools.NUM_MWND);
        if (isParameterFull(view, LayerTools.PR_MAXW, maxWindIdx)) {
            return;
        }
        if (copyValue(levelStructure, view, BUFR_PRESSURE, LayerTools.PR_MAXW,
                maxWindIdx)) {
            copyValue(levelStructure, view, BUFR_WIND_DIRECTION,
                    LayerTools.WD_MAXW, maxWindIdx);
            if (!copyValue(levelStructure, view, BUFR_WIND_SPEED,
                    LayerTools.WS_MAXW, maxWindIdx)) {
                copyValue(levelStructure, view, BUFR_MAX_WIND_SPEED,
                        LayerTools.WS_MAXW, maxWindIdx);
            }
            view.setInt(LayerTools.NUM_MWND, maxWindIdx + 1);
        }
    }

    protected static void decodeTropopauseLevel(PointDataView view,
            BufrStructure levelStructure) {
        int tropIdx = view.getInt(LayerTools.NUM_TROP);
        if (isParameterFull(view, LayerTools.PR_TROP, tropIdx)) {
            return;
        }
        Number pressure = levelStructure.lookupNumericValue(BUFR_PRESSURE);
        if (pressure != null && pressure.intValue() != 99900) {
            view.setFloat(LayerTools.PR_TROP, pressure.floatValue(), tropIdx);
            copyValue(levelStructure, view, BUFR_TEMPERATURE,
                    LayerTools.TP_TROP, tropIdx);
            copyValue(levelStructure, view, BUFR_DEWPOINT, LayerTools.TD_TROP,
                    tropIdx);
            copyValue(levelStructure, view, BUFR_WIND_DIRECTION,
                    LayerTools.WD_TROP, tropIdx);
            copyValue(levelStructure, view, BUFR_WIND_SPEED,
                    LayerTools.WS_TROP, tropIdx);
            view.setInt(LayerTools.NUM_TROP, tropIdx + 1);
        }
    }

    protected static void decodeSurfaceLevel(PointDataView view,
            BufrStructure levelStructure) {
        copyValue(levelStructure, view, BUFR_PRESSURE, LayerTools.SFC_PRESSURE,
                0);
        decodeMandatoryLevel(view, levelStructure);
    }

    protected static void decodeMandatoryLevel(PointDataView view,
            BufrStructure levelStructure) {
        int manIdx = view.getInt(LayerTools.NUM_MAND);
        if (isParameterFull(view, LayerTools.PR_MAN, manIdx)) {
            return;
        }
        if (copyValue(levelStructure, view, BUFR_PRESSURE, LayerTools.PR_MAN,
                manIdx)) {
            if (!copyValue(levelStructure, view, BUFR_GEOPOTENTIAL_HEIGHT,
                    LayerTools.HT_MAN, manIdx)) {
                copyValue(levelStructure, view, BUFR_GEOPOTENTIAL,
                        LayerTools.HT_MAN, manIdx);
            }
            copyValue(levelStructure, view, BUFR_TEMPERATURE,
                    LayerTools.TP_MAN, manIdx);
            copyValue(levelStructure, view, BUFR_DEWPOINT, LayerTools.TD_MAN,
                    manIdx);
            copyValue(levelStructure, view, BUFR_WIND_DIRECTION,
                    LayerTools.WD_MAN, manIdx);
            copyValue(levelStructure, view, BUFR_WIND_SPEED, LayerTools.WS_MAN,
                    manIdx);
            view.setInt(LayerTools.NUM_MAND, manIdx + 1);

        }
    }

    /**
     * Determine if a specified index is beyond the maximum index allowed by a
     * parameter.
     * 
     * @param view
     *            the view where a value will be stored
     * @param testparameter
     *            a parameter whose dimension should be checked against index
     * @param index
     *            the index where a new value will be stored.
     * @return true if the parameter is full and no new data can be stored,
     *         falso otherwise.
     */
    protected static boolean isParameterFull(PointDataView view,
            String testparameter, int index) {
        int max = view.getContainer().getDescription(testparameter)
                .getDimensionAsInt();
        return index >= max;
    }

    /**
     * Copy a value from a {@link BufrStructure} to a {@link PointDataView}.
     * 
     * @param levelStructure
     *            the structure to get the data from
     * @param view
     *            the view to put the data in
     * @param structureName
     *            the name of the parameter in the structure
     * @param viewName
     *            the name of the parameter in the view
     * @param index
     *            the index in the view to copy the parameter to.
     * @return true if the structure contains a valid value that was copied into
     *         the view. False if the value was missing and the structure is
     *         unmodified.
     */
    protected static boolean copyValue(BufrStructure levelStructure,
            PointDataView view, String structureName, String viewName, int index) {
        Number number = levelStructure.lookupNumericValue(structureName);
        if (number != null) {
            view.setFloat(viewName, number.floatValue(), index);
            return true;
        }
        return false;
    }

}
