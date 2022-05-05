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
package com.raytheon.viz.hydrobase.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;

/**
 * Data object for SHEF durations.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 06, 2018  #7211     dgilling     Initial creation
 *
 * </pre>
 *
 * @author dgilling
 */

public class ShefDur {

    private final short duration;

    private final String durationCode;

    private final String name;

    public ShefDur(short duration, String durationCode, String name) {
        this.duration = duration;
        this.durationCode = durationCode;
        this.name = name;
    }

    public short getDuration() {
        return duration;
    }

    public String getDurationCode() {
        return durationCode;
    }

    public String getName() {
        return name;
    }

    public static List<ShefDur> getShefDur(final String where)
            throws VizException {
        String selectStatement = "SELECT dur, durcode, name FROM ShefDur ";
        StringBuilder query = new StringBuilder(selectStatement);
        if (StringUtils.isNotEmpty(where)) {
            query.append(where);
        }

        QueryResult data = HydroDBDataManager.getInstance()
                .runMappedQuery(query.toString());

        if (data != null) {
            List<ShefDur> rval = new ArrayList<>();
            for (QueryResultRow entry : data.getRows()) {
                short dur = ((Number) entry
                        .getColumn(data.getColumnNames().get("dur")))
                                .shortValue();
                Object code = entry
                        .getColumn(data.getColumnNames().get("durcode"));
                String durCode = (code != null) ? code.toString()
                        : StringUtils.EMPTY;
                Object name = entry
                        .getColumn(data.getColumnNames().get("name"));
                String nameString = (name != null) ? name.toString()
                        : StringUtils.EMPTY;
                rval.add(new ShefDur(dur, durCode, nameString));
            }

            return rval;
        }

        return Collections.emptyList();
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("ShefDur [duration=").append(duration)
                .append(", durationCode=").append(durationCode)
                .append(", name=").append(name).append("]");
        return builder.toString();
    }
}
