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
package com.raytheon.uf.common.dataplugin.shef.data;

/**
 * Defines a common set of parameters that should be utilized within the HQL
 * that has been written to retrieve {@link Observation}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2016 5699       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public final class ObsHqlConstants {

    public static final String START_OBS_TIME_PARAM = "startObsTime";

    public static final String HQL_START_OBS_TIME_PARAM = ":"
            + START_OBS_TIME_PARAM;

    public static final String END_OBS_TIME_PARAM = "endObsTime";

    public static final String HQL_END_OBS_TIME_PARAM = ":"
            + END_OBS_TIME_PARAM;

    public static final String QUALITY_CODE_PARAM = "qualityCode";

    public static final String HQL_QUALITY_CODE_PARAM = ":"
            + QUALITY_CODE_PARAM;

    public static final String LID_PARAM = "lid";

    public static final String HQL_LID_PARAM = ":" + LID_PARAM;

    public static final String PE_PARAM = "pe";

    public static final String HQL_PE_PARAM = ":" + PE_PARAM;

    public static final String VALUE_PARAM = "value";

    public static final String HQL_VALUE_PARAM = ":" + VALUE_PARAM;

    protected ObsHqlConstants() {
    }
}