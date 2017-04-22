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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * POJO representation of DHR Radar parameters. DHR parameters consist of an
 * array of adaptable params that can contain up to 45 values. There is also an
 * additional adaptable flag parameter that is stored as a boolean.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2016 4625       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class DHRParameters {

    private static final class DHRParamIndices {
        public static final int ZRMULT_INDEX = 9;

        public static final int ZREXP_INDEX = 10;

        public static final int MXPRA_V5_INDEX = 25;

        public static final int MXPRA_V8_INDEX = 19;
    }

    private final Logger logger = LoggerFactory.getLogger(getClass());

    /*
     * Default of 45-float parameters.
     */
    public static final int DEFAULT_PARAMS_COUNT = 45;

    public static final float DEFAULT_PARAM_VALUE = -99.0f;

    /*
     * Two build versions were recognized by the legacy code. A build version of
     * five and a build version of 8 or greater.
     */
    private static final int BUILD_VERSION_5 = 5;

    private static final int BUILD_VERSION_8 = 8;

    /*
     * The exact number of parameters that were read from the input file.
     */
    private final int parameterCount;

    private final float[] adaptableParams;

    /*
     * In the legacy code, this was referred to as param 38 in one place and
     * param 46 in another place.
     */
    private final Boolean adaptableFlag;

    /*
     * TODO: document once it is seen how the following variables are actually
     * used (or eliminate if no longer necessary). Based on the {@link
     * DHRParameters} that are read.
     */
    private final float zrmult;

    private final float zrexp;

    private float mxpra;

    public DHRParameters(final int parameterCount,
            final float[] adaptableParams, Boolean adaptableFlag,
            final int version) throws InvalidDHRException {
        this.parameterCount = parameterCount;
        this.adaptableParams = adaptableParams;
        this.adaptableFlag = adaptableFlag;

        this.zrmult = adaptableParams[DHRParamIndices.ZRMULT_INDEX];
        this.zrexp = adaptableParams[DHRParamIndices.ZREXP_INDEX];
        if (version == BUILD_VERSION_5) {
            mxpra = DHRParamIndices.MXPRA_V5_INDEX;
            logger.info("A Version 5 DHR Product has been detected.");
        } else if (version >= BUILD_VERSION_8) {
            mxpra = DHRParamIndices.MXPRA_V8_INDEX;
        }
    }

    public int getParameterCount() {
        return parameterCount;
    }

    public float[] getAdaptableParams() {
        return adaptableParams;
    }

    public Boolean isAdaptableFlag() {
        return adaptableFlag;
    }

    public float getMxpra() {
        return mxpra;
    }

    public void setMxpra(float mxpra) {
        this.mxpra = mxpra;
    }

    public float getZrmult() {
        return zrmult;
    }

    public float getZrexp() {
        return zrexp;
    }
}