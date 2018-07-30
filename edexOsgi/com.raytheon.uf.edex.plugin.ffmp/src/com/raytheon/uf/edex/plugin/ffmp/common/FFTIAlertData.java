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
package com.raytheon.uf.edex.plugin.ffmp.common;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;

/**
 * This class holds the String data for the FFTI alert message.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2011            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class FFTIAlertData {

    private String attributeName = "";

    private Map<SOURCE_TYPE, String> sourceMap = new HashMap<SOURCE_TYPE, String>(
            3);

    private Map<SOURCE_TYPE, Double> durationMap = new HashMap<SOURCE_TYPE, Double>(
            3);

    private String displayName = "";

    private String unit = "";

    private String value = "";

    private String gap = "";

    private String priority = "";

    private Map<SOURCE_TYPE, String> sourceDisplayMap = new HashMap<SOURCE_TYPE, String>();

    public FFTIAlertData() {

    }

    public String getAttributeName() {
        return attributeName;
    }

    public void setAttributeName(String attributeName) {
        this.attributeName = attributeName;
    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    public String getUnit() {
        return unit;
    }

    public void setUnit(String unit) {
        this.unit = unit;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getGap() {
        return gap;
    }

    public void setGap(String gap) {
        this.gap = gap;
    }

    public String getPriority() {
        return priority;
    }

    public void setPriority(String priority) {
        this.priority = priority;
    }

    public void addSource(SOURCE_TYPE field, String source) {
        sourceMap.put(field, source);
    }

    public String getSource(SOURCE_TYPE field) {
        return sourceMap.get(field);
    }

    public void addDuration(SOURCE_TYPE field, Double dur) {
        durationMap.put(field, dur);
    }

    public double getDuration(SOURCE_TYPE field) {
        return durationMap.get(field);
    }

    public void addSourceDisplay(SOURCE_TYPE field, String displayStr) {
        sourceDisplayMap.put(field, displayStr);
    }

    public String getSourceDisplay(SOURCE_TYPE field) {
        return sourceDisplayMap.get(field);
    }
}
