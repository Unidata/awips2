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

import java.nio.ByteBuffer;

import com.raytheon.uf.edex.plugin.mpe.gather.radar.InvalidMpeRadarException;
import com.raytheon.uf.edex.plugin.mpe.gather.radar.MpeRadarProductDescription;

/**
 * Reads and represents the Product Description block for a DHR Radar File.
 *
 * Based on decode_dhr_dsp/TEXT/decodeDHR.c
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 22, 2016 5588      nabowle     Initial creation
 *
 * </pre>
 *
 * @author nabowle
 */
public class DHRProductDescription extends MpeRadarProductDescription {

    private static final float DBZ_DIVIDEND = 10.0f;

    private static final float BIAS_DIVIDEND = 100.0f;

    private static final class DHRDescriptionIndices {
        public static final int DBZMIN_INDEX = 21;

        public static final int DBZINC_INDEX = 22;
    }

    private float dataLevelInc;

    private float bias;

    private float dbzMin;

    public DHRProductDescription(ByteBuffer buf)
            throws InvalidMpeRadarException {
        super(buf);
    }

    @Override
    protected void parseData(short[] data) {
        super.parseData(data);
        bias = getMeanFieldBias() / BIAS_DIVIDEND;
        dbzMin = data[DHRDescriptionIndices.DBZMIN_INDEX] / DBZ_DIVIDEND;
        dataLevelInc = data[DHRDescriptionIndices.DBZINC_INDEX] / DBZ_DIVIDEND;
    }

    /**
     * @return the dataLevelInc
     */
    public float getDataLevelInc() {
        return dataLevelInc;
    }

    /**
     * @return the bias
     */
    public float getBias() {
        return bias;
    }

    /**
     * @return the dbzMin
     */
    public float getDbzMin() {
        return dbzMin;
    }
}
