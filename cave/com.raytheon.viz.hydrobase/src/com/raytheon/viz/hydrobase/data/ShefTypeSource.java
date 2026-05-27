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
 * Data object for SHEF type sources.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2018  #7211     dgilling     Initial creation
 *
 * </pre>
 *
 * @author dgilling
 */

public class ShefTypeSource {

    private final String code;

    private final String name;

    public ShefTypeSource(String code, String name) {
        this.code = code;
        this.name = name;
    }

    public String getCode() {
        return code;
    }

    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("ShefTypeSource [code=").append(code).append(", name=")
                .append(name).append("]");
        return builder.toString();
    };

    public static List<ShefTypeSource> getShefTs(final String where)
            throws VizException {
        String selectStatement = "SELECT ts, name FROM ShefTS ";
        StringBuilder query = new StringBuilder(selectStatement);
        if (StringUtils.isNotEmpty(where)) {
            query.append(where);
        }

        QueryResult data = HydroDBDataManager.getInstance()
                .runMappedQuery(query.toString());

        if (data != null) {
            List<ShefTypeSource> rval = new ArrayList<>();
            for (QueryResultRow entry : data.getRows()) {
                Object ts = entry.getColumn(data.getColumnNames().get("ts"));
                String tsString = (ts != null) ? ts.toString()
                        : StringUtils.EMPTY;
                Object name = entry
                        .getColumn(data.getColumnNames().get("name"));
                String nameString = (name != null) ? name.toString()
                        : StringUtils.EMPTY;
                rval.add(new ShefTypeSource(tsString, nameString));
            }

            return rval;
        }

        return Collections.emptyList();
    }
}
