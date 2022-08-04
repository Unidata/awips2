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
package com.raytheon.uf.edex.plugin.mpe.gather.dsp;

import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import com.google.common.io.LittleEndianDataOutputStream;
import com.raytheon.uf.common.mpe.util.AppsDefaultsPathException;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DspadaptDao;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.DspradarDao;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarDataRecord;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarDecodeConstants;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarDecoder;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarInputException;

/**
 * Decodes a DSP product and outputs the storm-total rainfall in a quarterly
 * HRAP grid.
 *
 * Ported from decode_dhr_dsp/TEXT/write_decoded_dsp.c.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2016 5588       nabowle     Initial creation
 * Dec 14, 2016 5588       nabowle     Fix negative values. Round to 3 decimals
 *                                     to match legacy.
 * Jul 19, 2018 5588       mapeters    Change rounding to truncating to match
 *                                     legacy
 *
 * </pre>
 *
 * @author nabowle
 */

public class DSPDecoder extends MpeRadarDecoder<DSPRadarFile> {

    /**
     * The factor that determines the number of decimal places to truncate the
     * data values to (1000 = 3 decimal places).
     */
    private static final float TRUNCATE_FACTOR = 1000F;

    private DspradarDao radarDao = new DspradarDao();

    private DspadaptDao adaptDao = new DspadaptDao();

    public DSPDecoder() {
        super(DSPRadarFile.DSP_PRODUCT_TYPE,
                MpeRadarDecodeConstants.AppsDefaults.DSP_PROD_DIR,
                MpeRadarDecodeConstants.AppsDefaults.DSP_GRID_DIR);
    }

    /**
     * Normalize the DSP data to storm-total rainfall in mm.
     */
    @Override
    protected float[][] normalizeData(DSPRadarFile file) {
        float[][] precip = new float[MpeRadarDecodeConstants.MAX_AZIMUTH][MpeRadarDecodeConstants.MAX_RANGE];
        DSPProductDescription productDesc = file.getDescription();
        short scaleFactor = productDesc.getDataLevelScaleFactor();
        List<MpeRadarDataRecord> dbzData = file.getSymbologyData().getData();

        for (int i = 0; i < MpeRadarDecodeConstants.MAX_AZIMUTH; i++) {
            byte[] data = dbzData.get(i).getData();
            for (int j = 0; j < MpeRadarDecodeConstants.MAX_RANGE; j += 2) {
                /*
                 * DSP data is 116 bytes. Convert this to 230 values by
                 * duplicating the first 115 values, and dropping the last one.
                 *
                 * based on decodeDSP.c lines 459-469
                 */
                float val = (data[j / 2] & 0xFF) * scaleFactor * 0.254F;
                precip[i][j] = val;
                precip[i][j + 1] = val;
            }
        }
        return precip;
    }

    /**
     * Writes the converted grid data to the grid directory. The file is written
     * in little endian order.
     */
    @Override
    public void writeRadarGridFile(DSPRadarFile file, float[][] data)
            throws IOException, AppsDefaultsPathException {
        DSPProductDescription productDesc = file.getDescription();
        short endDate = productDesc.getjDate();
        short endTime = productDesc.getjTime();
        short opermode = productDesc.getOperationalMode();
        short startDate = productDesc.getJulianBeginDate();
        short startTime = productDesc.getJulianBeginTime();
        try (FileOutputStream fos = new FileOutputStream(getGridFile(file));
                LittleEndianDataOutputStream ledos = new LittleEndianDataOutputStream(
                        fos)) {
            ledos.writeShort(startDate);
            ledos.writeShort(startTime);
            ledos.writeShort(opermode);
            ledos.writeShort(endDate);
            ledos.writeShort(endTime);
            ledos.writeShort(opermode);

            for (int j = 0; j < MpeRadarDecodeConstants.MAX_JHRAP; j++) {
                for (int i = 0; i < MpeRadarDecodeConstants.MAX_IHRAP; i++) {
                    ledos.writeFloat(truncateFloat(data[j][i]));
                }
            }
        }
    }

    private float truncateFloat(float f) {
        return (int) (f * TRUNCATE_FACTOR) / TRUNCATE_FACTOR;
    }

    @Override
    protected DSPRadarFile loadRadarFile(Path path, int buildVersion)
            throws MpeRadarInputException {
        return new DSPRadarFile(path, buildVersion);
    }

    @Override
    protected void writeRadarRecords(DSPRadarFile radarFile) {
        radarDao.saveOrUpdate(radarFile.getRadar());
        adaptDao.saveOrUpdate(radarFile.getAdapt());
    }

}
