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
package com.raytheon.viz.hydrocommon.data;

import java.util.Map;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * This class contains data for the locdatalimits tables.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 09, 2008  1744      askripsk     Initial Creation
 * Apr 18, 2018  DCS19644  jwu          Add column 'ts' (Type-Source) in locdatalimits.
 * Jul 10, 2018  #7211     dgilling     Remove unnecessary functions.
 *
 * </pre>
 *
 * @author askripsk
 */
public class LocationDataLimitData extends DataLimitData {

    protected String ts;

    protected String lid;

    public LocationDataLimitData() {
        super();
    }

    public LocationDataLimitData(QueryResultRow data,
            Map<String, Integer> dataMap) {
        super(data, dataMap);
        setLid(getDBValue(HydroConstants.LID, data, dataMap,
                StringUtils.EMPTY));
        setTs(getDBValue(HydroConstants.TS, data, dataMap,
                HydroConstants.DEFAULT_TS));
    }

    public String getTs() {
        return ts;

    }

    public void setTs(String ts) {
        this.ts = ts;
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }
}