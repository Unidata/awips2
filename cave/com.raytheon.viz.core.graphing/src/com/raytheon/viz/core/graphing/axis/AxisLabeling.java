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
package com.raytheon.viz.core.graphing.axis;

import java.util.HashMap;

/**
 * Contains data for the labeling of an axis
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 23, 2007            njensen     Initial creation	
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class AxisLabeling {

    /**
     * The format for sampling the data as used by String.format(fmt, args...)
     */
    protected String sampleFormat;

    /**
     * The labels for the data
     */
    protected HashMap<Double, String> labels = new HashMap<Double, String>();

    /**
     * @return the sampleFormat
     */
    public String getSampleFormat() {
        return sampleFormat;
    }

    /**
     * @param sampleFormat
     *            the sampleFormat to set
     */
    public void setSampleFormat(String sampleFormat) {
        this.sampleFormat = sampleFormat;
    }

    /**
     * @return the labels
     */
    public HashMap<Double, String> getLabels() {
        return labels;
    }

    /**
     * @param labels
     *            the labels to set
     */
    public void setLabels(HashMap<Double, String> labels) {
        this.labels = labels;
    }

    public String getLabel(double aValue) {
        return labels.get(aValue);
    }

}
