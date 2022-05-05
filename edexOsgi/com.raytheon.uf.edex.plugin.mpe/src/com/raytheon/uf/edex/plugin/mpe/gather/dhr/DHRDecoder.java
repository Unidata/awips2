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
package com.raytheon.uf.edex.plugin.mpe.gather.dhr;

import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import com.google.common.io.LittleEndianDataOutputStream;
import com.raytheon.uf.common.mpe.util.AppsDefaultsPathException;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DhradaptDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DhrradarDao;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarDataRecord;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarDecodeConstants;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarDecoder;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarInputException;

/**
 * Decodes a Digital Hybrid Scan Reflectivity (DHR) Product and outputs the rain
 * rate in a quarterly HRAP grid.
 *
 * Ported from decode_dhr_dsp/TEXT/write_decoded_dhr.c.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2016 5588       nabowle     Initial creation
 * Dec 14, 2016 5588       nabowle     Cleanup.
 *
 * </pre>
 *
 * @author nabowle
 */
public class DHRDecoder extends MpeRadarDecoder<DHRRadarFile> {

    private DhrradarDao radarDao = new DhrradarDao();

    private DhradaptDao adaptDao = new DhradaptDao();

    public DHRDecoder() {
        super(DHRRadarFile.DHR_PRODUCT_TYPE,
                MpeRadarDecodeConstants.AppsDefaults.DHR_PROD_DIR,
                MpeRadarDecodeConstants.AppsDefaults.DHR_GRID_DIR);
    }

    @Override
    public void writeRadarRecords(DHRRadarFile radarFile) {
        radarDao.saveOrUpdate(radarFile.getRadar());
        adaptDao.saveOrUpdate(radarFile.getAdapt());
    }

    /**
     * Normalize the DHR data into to rainrate in mm/hr.
     */
    @Override
    protected float[][] normalizeData(DHRRadarFile file) {

        float[][] ratePol = new float[MpeRadarDecodeConstants.MAX_AZIMUTH][MpeRadarDecodeConstants.MAX_RANGE];
        float[] dbzToRate = new float[MpeRadarDecodeConstants.NUM_LEVEL_DBZ];

        float zrmult = file.getAdapt().getMltZrcoef();
        float zrexp = file.getAdapt().getPwrZrcoef();
        DHRProductDescription productDesc = file.getDescription();
        float dbzMin = productDesc.getDbzMin();
        float dbzInc = productDesc.getDataLevelInc();
        float bias = productDesc.getBias();
        int dbzCnt = productDesc.getDataLevelCount();
        float mxpra = file.getAdapt().getMaxPrate();

        List<MpeRadarDataRecord> dbzData = file.getSymbologyData().getData();

        float minRate = MpeRadarDecodeConstants.MAX_RATE;
        float maxRate = MpeRadarDecodeConstants.MIN_RATE;

        /*
         * Generate lookup table for conversion of byte-compressed dBZs to
         * rainrate (mm/hr) based on Z-R parameter and maximum rain rate etc.
         */

        float logmult = (float) Math.log10(zrmult);

        dbzToRate[0] = 0.0F; // below SNR
        dbzToRate[1] = 0.0F; // missing data

        /*
         * Compute byte-compressed dBZ corresponding to minimum resolvable
         * rainrate of 0.05 mm/hr (which rounds to 0.1 mm/hr) and maximum
         * rainrate of 'mxpra' mm/hr.
         *
         * For z=300r^1.4, bdbz_min is 79 and bdbz_max is 171 (for mxpra=103.8
         * mm/hr)
         */

        float dbz = (float) (10.0 * Math.log10(zrmult * Math.pow(0.05, zrexp)));
        float bdbzMin = (int) ((dbz - dbzMin) / dbzInc + 2.0);
        dbz = (float) (10.0 * Math.log10(zrmult * Math.pow(mxpra, zrexp)));
        float bdbzMax = (int) ((dbz - dbzMin) / dbzInc + 2.0);

        for (int i = 2; i < dbzCnt; i++) {
            if (i < bdbzMin) {
                dbzToRate[i] = 0.0F;
            } else if (i > bdbzMax) {
                dbzToRate[i] = mxpra;
            } else {
                dbz = (float) (((i - 2.0) * dbzInc) + dbzMin);
                dbzToRate[i] = (float) Math.pow(10.0,
                        (dbz - 10.0 * logmult) / (10.0 * zrexp));
            }
        }

        /*
         * For MPE do not apply bias even if bias flag is set.
         *
         * Make sure bias is not equal to 0 due to known problems in writing
         * bias info from PPS into DHR product
         */

        if (!file.getSymbologyData().isBiasApplied() || bias == 0.0F) {
            bias = 1.0F;
        }

        for (int az = 0; az < MpeRadarDecodeConstants.MAX_AZIMUTH; az++) {
            byte[] data = dbzData.get(az).getData();
            for (int ra = 0; ra < MpeRadarDecodeConstants.MAX_RANGE; ra++) {

                /*
                 * Transform byte-compressed reflectivity to unbiased rainrate;
                 * Utilize data from 2nd range gate in 1st gate due to missing
                 * data there
                 *
                 * Bias is not applied here because the bias will be applied in
                 * mpe bmosaic.
                 */

                if (ra != 0) {
                    ratePol[az][ra] = dbzToRate[data[ra] & 0xFF];
                } else {
                    ratePol[az][ra] = dbzToRate[data[1] & 0xFF];
                }

                if (ratePol[az][ra] < minRate) {
                    minRate = ratePol[az][ra];
                }

                if (ratePol[az][ra] > maxRate) {
                    maxRate = ratePol[az][ra];
                }
            }
        }

        return ratePol;
    }

    /**
     * Writes the converted grid data to the grid directory. The file is written
     * in little endian order.
     */
    @Override
    public void writeRadarGridFile(DHRRadarFile file, float[][] data)
            throws IOException, AppsDefaultsPathException {
        DHRProductDescription productDesc = file.getDescription();
        short endDate = productDesc.getjDate();
        short endTime = productDesc.getjTime();
        short opermode = productDesc.getOperationalMode();
        try (FileOutputStream fos = new FileOutputStream(getGridFile(file));
                LittleEndianDataOutputStream ledos = new LittleEndianDataOutputStream(
                        fos)) {
            ledos.writeShort(endDate);
            ledos.writeShort(endTime);
            ledos.writeShort(opermode);

            for (int j = 0; j < MpeRadarDecodeConstants.MAX_JHRAP; j++) {
                for (int i = 0; i < MpeRadarDecodeConstants.MAX_IHRAP; i++) {
                    ledos.writeFloat(data[j][i]);
                }
            }
        }
    }

    @Override
    protected DHRRadarFile loadRadarFile(Path path, int buildVersion)
            throws MpeRadarInputException {
        return new DHRRadarFile(path, buildVersion);
    }
}
