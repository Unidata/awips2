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
package com.raytheon.uf.edex.plugin.vil;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.vil.VILRecord;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.edex.plugin.vil.common.VILConfig;

/**
 *
 * Vil, do the VIL processing
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 14, 2009  2037     dhladky   Initial Creation.
 * Jul 09, 2018  6706     randerso  Refactored getRasterizedData() to better
 *                                  match A1 behavior. Code cleanup.
 *
 * </pre>
 *
 * @author dhladky
 */

public class VIL {

    private VILConfig vil_config = null;

    private Map<String, float[]> vils = null;

    /**
     * public constructor
     *
     * @param vil_config
     */
    public VIL(VILConfig vil_config) {
        this.vil_config = vil_config;
    }

    /**
     * Generate the VIL arrays as directed by Amburn and Wolf(1997)
     */
    public void genVIL() {
        vils = new HashMap<>();
        // Low Resolution Grids
        float[] vil = getRasterizedData(vil_config.getVil(),
                ScanUtils.SCAN_GRID_DIM, ScanUtils.SCAN_GRID_DIM_RESOLUTION);
        float[] et = getRasterizedData(vil_config.getEt(),
                ScanUtils.SCAN_GRID_DIM, ScanUtils.SCAN_GRID_DIM_RESOLUTION);
        // High Resolution Grids
        float[] dvil = getRasterizedData(vil_config.getDVil(),
                ScanUtils.SCAN_GRID_DIM_HALFKM,
                ScanUtils.SCAN_GRID_HALFK_DIM_RESOLUTION);
        float[] eet = getRasterizedData(vil_config.getEet(),
                ScanUtils.SCAN_GRID_DIM_HALFKM,
                ScanUtils.SCAN_GRID_HALFK_DIM_RESOLUTION);

        vils.put(VILRecord.DATA_TYPE.VILD.name(), processVilGrid(vil, et));
        vils.put(VILRecord.DATA_TYPE.DVILD.name(), processVilGrid(dvil, eet));
        // Morphology on EET grid before running
        vils.put(VILRecord.DATA_TYPE.EDVILD.name(),
                processVilGrid(dvil, morphEETgrid(eet)));
    }

    /**
     * Gets the VIL data sets
     *
     * @return
     */
    public Map<String, float[]> getFloatArrays() {
        return vils;
    }

    /**
     * Process the grids
     *
     * @param vils
     * @param ets
     * @return
     */
    private float[] processVilGrid(float[] vilfs, float[] ets) {

        float[] vilds = null;
        int grid_res = 0;
        // low res
        if (vilfs.length == ScanUtils.SCAN_GRID_DIM_SQ) {
            vilds = new float[ScanUtils.SCAN_GRID_DIM_SQ];
            grid_res = ScanUtils.SCAN_GRID_DIM;
        }
        // high res
        else if (vilfs.length == ScanUtils.SCAN_GRID_DIM_HALFKM_SQ) {
            vilds = new float[ScanUtils.SCAN_GRID_DIM_HALFKM_SQ];
            grid_res = ScanUtils.SCAN_GRID_DIM_HALFKM;
        }

        for (int i = 0; i < grid_res; i++) {
            for (int j = 0; j < grid_res; j++) {

                // make sure we don't divide by zero
                if (ets[(grid_res * i) + j] > 0.0f
                        && vilfs[(grid_res * i) + j] > 0.0f) {
                    float VD = (float) (3.28 * (vilfs[(grid_res * i) + j]
                            / ets[(grid_res * i) + j]));
                    if (VD < 0) {
                        vilds[(grid_res * i) + j] = 0.0f;
                    } else {
                        vilds[(grid_res * i) + j] = VD;
                    }
                } else {
                    vilds[(grid_res * i) + j] = 0.0f;
                }
            }
        }

        return vilds;
    }

    /**
     * Morph the EET grid to find nearby types
     *
     * @param eet
     * @return
     */
    private float[] morphEETgrid(float[] eet) {

        for (int i = 0; i < ScanUtils.SCAN_GRID_DIM_HALFKM; i++) {
            for (int j = 0; j < ScanUtils.SCAN_GRID_DIM_HALFKM; j++) {
                // search the surrounding pixels for a higher EET value
                // the variables(s[1],s[2]...) below mean surrounding 1,2
                // +---+---+---+
                // | 0 | 1 | 2 |
                // +---+---+---+
                // | 7 | i | 3 |
                // +---+---+---+
                // | 6 | 5 | 4 |
                // +---+---+---+
                // iteratively search the 8-connected surrounding pixels
                // value at center
                float value = eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * i) + j];
                int iplus = i + 1;
                int jplus = j + 1;
                int iminus = i - 1;
                int jminus = j - 1;

                if (i == 0) {
                    iminus = i;
                }

                if (i == ScanUtils.SCAN_GRID_DIM_HALFKM - 1) {
                    iplus = i;
                }

                if (j == 0) {
                    jminus = j;
                }

                if (j == ScanUtils.SCAN_GRID_DIM_HALFKM - 1) {
                    jplus = j;
                }

                // 0 test
                if (eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * iminus)
                        + jminus] > value) {
                    value = eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * iminus)
                            + jminus];
                }
                // 1 test
                if (eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * i)
                        + jminus] > value) {
                    value = eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * i) + jminus];
                }
                // 2 test
                if (eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * iplus)
                        + jminus] > value) {
                    value = eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * iplus)
                            + jminus];
                }
                // 3 test
                if (eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * iplus) + j] > value) {
                    value = eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * iplus) + j];
                }
                // 4 test
                if (eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * iplus)
                        + jplus] > value) {
                    value = eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * iplus)
                            + jplus];
                }
                // 5 test
                if (eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * i) + jplus] > value) {
                    value = eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * i) + jplus];
                }
                // 6 test
                if (eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * iminus)
                        + jplus] > value) {
                    value = eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * iminus)
                            + jplus];
                }
                // 7 test
                if (eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * iminus)
                        + j] > value) {
                    value = eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * iminus) + j];
                }

                eet[(ScanUtils.SCAN_GRID_DIM_HALFKM * i) + j] = value;
            }
        }

        return eet;
    }

    private float[] getRasterizedData(RadarRecord radarRecord, int gridDim,
            float gridRes) {
        /*
         * Algorithm ported from A1 D-2D/src/scan/processor/mapRadialsToGrid.C
         */

        float[] rasterData = new float[gridDim * gridDim];

        byte[] rawData = radarRecord.getRawData();

        Object[] thresholds = radarRecord.getDecodedThresholds();

        if ("Raster".equals(radarRecord.getFormat())) {
            int rawRows = radarRecord.getNumRadials();
            int rawCols = radarRecord.getNumBins();
            for (int aa = 0; aa < rasterData.length; aa++) {
                int x = aa % gridDim;
                int y = aa / gridDim;

                int xRaw = x * rawCols / gridDim;
                int yRaw = y * rawRows / gridDim;
                int rawValue = rawData[(yRaw * rawCols) + xRaw] & 0xFF;

                float value = 0;
                if (radarRecord.getProductCode() == 41
                        || radarRecord.getProductCode() == 57) {
                    Object th = thresholds[rawValue];
                    if (th instanceof Float) {
                        value = ((Float) th).floatValue();
                    }
                }
                if (radarRecord.getProductCode() == 134) {
                    value = ScanUtils.getDecodedDvil(radarRecord, rawValue);
                }
                if (radarRecord.getProductCode() == 135) {
                    value = ScanUtils.getDecodedEET(rawValue);
                }

                rasterData[aa] = value;
            }

        } else if ("Radial".equals(radarRecord.getFormat())) {
            float[] angleData = radarRecord.getAngleData();

            for (int aa = 0; aa < rasterData.length; aa++) {

                // compute nearest radial point
                int iwebox = aa % gridDim;
                int insbox = gridDim - 1 - (aa / gridDim);

                float rwedisp = gridRes * (iwebox - (gridDim / 2));
                float rnsdisp = gridRes * (insbox - (gridDim / 2));

                double range = Math.hypot(rwedisp, rnsdisp) / Math.cos(
                        Math.toRadians(radarRecord.getTrueElevationAngle()));
                int nbin = (int) (range / radarRecord.getGateResolution()
                        + 0.5);

                double azm = Math.toDegrees(Math.atan2(rwedisp, rnsdisp));
                if (azm > 360) {
                    azm -= 360;
                }
                if (azm < 0) {
                    azm += 360;
                }

                int nrad;
                for (nrad = 0; nrad < angleData.length - 1; nrad++) {
                    if (azm >= angleData[nrad] && azm < angleData[nrad + 1]) {
                        break;
                    }
                }

                // assign radial value to cartesian grid value
                int rawValue = rawData[(nrad * radarRecord.getNumBins()) + nbin]
                        & 0xFF;

                float value = 0;
                if (radarRecord.getProductCode() == 41
                        || radarRecord.getProductCode() == 57) {
                    value = rawValue;
                }
                if (radarRecord.getProductCode() == 134) {
                    value = ScanUtils.getDecodedDvil(radarRecord, rawValue);
                }
                if (radarRecord.getProductCode() == 135) {
                    value = ScanUtils.getDecodedEET(rawValue);
                }

                rasterData[aa] = value;
            }
        }

        return rasterData;
    }

}
