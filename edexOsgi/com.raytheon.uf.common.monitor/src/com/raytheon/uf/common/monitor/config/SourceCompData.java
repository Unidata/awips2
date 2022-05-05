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
package com.raytheon.uf.common.monitor.config;

import java.util.ArrayList;
import java.util.List;

/**
 * Data for a SourceComp
 * 
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                                 Initial creation
 * Jul 2, 2018  6641       njensen     Use List instead of ArrayList, cleanup
 *
 * </pre>
 *
 * @author njensen
 */
public class SourceCompData {

    private String sourceName;

    private String areaFFGValue;

    private List<ValueNameIdData> countyBasinData = new ArrayList<>();

    public SourceCompData() {
    }

    public String getSourceName() {
        return sourceName;
    }

    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
    }

    public String getAreaFFGValue() {
        return areaFFGValue;
    }

    public void setAreaFFGValue(String areaFFGValue) {
        this.areaFFGValue = areaFFGValue;
    }

    public List<ValueNameIdData> getCountyBasinData() {
        return countyBasinData;
    }

    public void setCountyBasinData(List<ValueNameIdData> countyBasinData) {
        this.countyBasinData = countyBasinData;
    }

    public void mergeCountyBasinData(List<ValueNameIdData> data) {
        boolean matchFound = false;
        List<ValueNameIdData> dataToMerge = new ArrayList<>();

        /*
         * Loop over the data to be merged.
         */
        for (ValueNameIdData dataVnid : data) {
            matchFound = false;

            /*
             * Loop over the existing data and see if there is data that matches
             * the new data element.
             */
            for (ValueNameIdData cbdVnid : countyBasinData) {
                if (dataVnid.getUniqueId().equals(cbdVnid.getUniqueId())) {
                    matchFound = true;
                    break;
                }
            }

            // Add the data because there was not match found.
            if (!matchFound) {
                dataToMerge.add(dataVnid);
            }
        }

        // Add all of the new data to the county/basin array.
        countyBasinData.addAll(dataToMerge);
    }

    public void mergeOverwriteCountyBasinData(List<ValueNameIdData> data) {
        boolean matchFound = false;
        List<ValueNameIdData> dataToMerge = new ArrayList<>();
        ValueNameIdData cbdVnid;

        /*
         * Loop over the data to be merged/overwritten.
         */
        for (ValueNameIdData dataVnid : data) {
            matchFound = false;

            /*
             * Loop over the existing data and see if there is data that matches
             * the new data element.
             */
            for (int i = 0; i < countyBasinData.size(); i++) {
                cbdVnid = countyBasinData.get(i);

                // If a match is found then overwrite the entry in the
                // county/basin array.
                if (dataVnid.getUniqueId().equals(cbdVnid.getUniqueId())) {
                    matchFound = true;
                    countyBasinData.set(i, dataVnid);
                    break;
                }
            }

            // Add the data because there was not match found.
            if (!matchFound) {
                dataToMerge.add(dataVnid);
            }
        }

        // Add all of the new data to the county/basin array.
        countyBasinData.addAll(dataToMerge);
    }

    public void mergeAreaFFG(String areaFfg) {
        if ((this.getAreaFFGValue() == null)
                || (this.getAreaFFGValue().length() == 0)) {
            this.setAreaFFGValue(areaFfg);
        }
    }

    public void printData() {
        System.out.println("*******************************************");

        System.out.println("areaFFGValue = " + this.areaFFGValue);
        System.out.println("sourceName   = " + this.sourceName);

        if (countyBasinData == null) {
            System.out.println("Null data for County & Basin data...");
        }

        for (ValueNameIdData data : countyBasinData) {
            if (data != null) {
                System.out.println("--- type = " + data.getType());
                System.out.println("--- name = " + data.getName());
                System.out.println("--- id   = " + data.getId());
                System.out.println("--- val  = " + data.getValue());
            }
        }
    }
}
