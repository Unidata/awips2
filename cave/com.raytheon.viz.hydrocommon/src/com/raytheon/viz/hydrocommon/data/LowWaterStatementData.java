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

import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * this class contains the Low Water Statement data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Nov 14, 2008	1697		askripsky	Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */
public class LowWaterStatementData {

    /**
     * Lid that the data refers to
     */
    private String lid;

    /**
     * PE.
     */
    private String pe;

    /**
     * Lower Value.
     */
    private double lowerValue;

    /**
     * Minor Stage.
     */
    private double upperValue;

    /**
     * Criteria Rank.
     */
    private int criteriaRank;

    /**
     * Statement.
     */
    private String statement;

    /**
     * Low Water Criteria.
     */
    private String lowWaterCriteria;

    /**
     * Low Water Source.
     */
    private String lowWaterSource;

    /**
     * Constructor
     */
    public LowWaterStatementData() {
        lid = "";
        pe = "";
        lowerValue = HydroConstants.MISSING_VALUE;
        upperValue = HydroConstants.MISSING_VALUE;
        criteriaRank = HydroConstants.MISSING_VALUE;
        statement = "";
        lowWaterCriteria = "";
        lowWaterSource = "";
    }

    /**
     * Constructor that accepts the object array from a db call
     * 
     * @param data
     *            The raw data from the database.
     */
    public LowWaterStatementData(QueryResultRow data,
            Map<String, Integer> dataMap) {
        this();

        setLid((String) data.getColumn(dataMap.get("lid")));
        setPe((String) data.getColumn(dataMap.get("pe")));
        setLowerValue((Double) data.getColumn(dataMap.get("lower_value")));

        if (data.getColumn(dataMap.get("upper_value")) != null) {
            setUpperValue((Double) data.getColumn(dataMap.get("upper_value")));
        }

        setCriteriaRank((Integer) data.getColumn(dataMap.get("criteria_rank")));

        if (data.getColumn(dataMap.get("statement")) != null) {
            setStatement((String) data.getColumn(dataMap.get("statement")));
        }

        if (data.getColumn(dataMap.get("lw_criteria")) != null) {
            setLowWaterCriteria((String) data.getColumn(dataMap
                    .get("lw_criteria")));
        }

        if (data.getColumn(dataMap.get("lw_source")) != null) {
            setLowWaterSource((String) data.getColumn(dataMap.get("lw_source")));
        }
    }

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    public String getPe() {
        return pe;
    }

    public void setPe(String pe) {
        this.pe = pe;
    }

    public double getLowerValue() {
        return lowerValue;
    }

    public String getLowerValueString() {
        return (lowerValue != HydroConstants.MISSING_VALUE) ? String.format(
                "%9.2f", lowerValue) : "";
    }

    public void setLowerValue(double lowerValue) {
        this.lowerValue = lowerValue;
    }

    public double getUpperValue() {
        return upperValue;
    }

    public String getUpperValueString() {
        return (upperValue != HydroConstants.MISSING_VALUE) ? String.format(
                "%9.2f", upperValue) : "";
    }

    public String getUpperValueDBString() {
        String rval = getUpperValueString();

        return (rval.compareTo("") != 0) ? rval : "null";
    }

    public void setUpperValue(double upperValue) {
        this.upperValue = upperValue;
    }

    public int getCriteriaRank() {
        return criteriaRank;
    }

    public void setCriteriaRank(int criteriaRank) {
        this.criteriaRank = criteriaRank;
    }

    public String getStatement() {
        return statement;
    }

    public void setStatement(String statement) {
        this.statement = statement;
    }

    public String getLowWaterCriteria() {
        return lowWaterCriteria;
    }

    public void setLowWaterCriteria(String lowWaterCriteria) {
        this.lowWaterCriteria = lowWaterCriteria;
    }

    public String getLowWaterSource() {
        return lowWaterSource;
    }

    public void setLowWaterSource(String lowWaterSource) {
        this.lowWaterSource = lowWaterSource;
    }

    @Override
    public String toString() {
        return String.format(
                " %-11.11s    %-11.11s            %2.2s            %4d",
                getLowerValueString(), getUpperValueString(), pe, criteriaRank);
    }
}
