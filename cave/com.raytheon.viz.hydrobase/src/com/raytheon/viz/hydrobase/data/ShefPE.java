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
 * Data object for SHEF physical elements.
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

public class ShefPE {

    private final String pe;

    private final String name;

    private final String englishUnit;

    private final String metricUnit;

    public ShefPE(String pe, String name, String englishUnit,
            String metricUnit) {
        this.pe = pe;
        this.name = name;
        this.englishUnit = englishUnit;
        this.metricUnit = metricUnit;
    }

    public String getPe() {
        return pe;
    }

    public String getName() {
        return name;
    }

    public String getEnglishUnit() {
        return englishUnit;
    }

    public String getMetricUnit() {
        return metricUnit;
    }

    public static List<ShefPE> getShefPe(final String where)
            throws VizException {
        String selectStatement = "SELECT pe, name, eng_unit, met_unit FROM ShefPe ";
        StringBuilder query = new StringBuilder(selectStatement);
        if (StringUtils.isNotEmpty(where)) {
            query.append(where);
        }

        QueryResult data = HydroDBDataManager.getInstance()
                .runMappedQuery(query.toString());

        if (data != null) {
            List<ShefPE> rval = new ArrayList<>();
            for (QueryResultRow entry : data.getRows()) {
                Object pe = entry.getColumn(data.getColumnNames().get("pe"));
                String peString = (pe != null) ? pe.toString()
                        : StringUtils.EMPTY;
                Object name = entry
                        .getColumn(data.getColumnNames().get("name"));
                String nameString = (name != null) ? name.toString()
                        : StringUtils.EMPTY;
                Object unit = entry
                        .getColumn(data.getColumnNames().get("eng_unit"));
                String engUnit = (unit != null) ? unit.toString()
                        : StringUtils.EMPTY;
                unit = entry.getColumn(data.getColumnNames().get("met_unit"));
                String metUnit = (unit != null) ? unit.toString()
                        : StringUtils.EMPTY;
                rval.add(new ShefPE(peString, nameString, engUnit, metUnit));
            }

            return rval;
        }

        return Collections.emptyList();
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("ShefPE [pe=").append(pe).append(", name=").append(name)
                .append(", englishUnit=").append(englishUnit)
                .append(", metricUnit=").append(metricUnit).append("]");
        return builder.toString();
    }
}
