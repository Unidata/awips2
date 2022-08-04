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
package com.raytheon.viz.radar.textcontributors;

import java.text.DecimalFormat;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;

/**
 * 
 * Do some funky math to the Max value for Digital Vil
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DigitalVilMaxTextContributor implements IRadarTextContributor {

    @Override
    public String contributeText(RadarRecord record) {
        if (record.getProductDependentValues() == null) {
            return "Loading";
        }
        double val = record.getProductDependentValue(3);
        if (val <= 20) {
            val = 0.05 + (val - 2) * (0.189 - 0.05) / 18;
        } else {
            val = Math.exp(Math.log(0.189) + (val - 20) * Math.log(80 / 0.189)
                    / 234);
        }
        DecimalFormat df;
        if (val >= 9.5) {
            df = new DecimalFormat("#");
        } else if (val >= 0.95) {
            df = new DecimalFormat("#.#");
        } else {
            df = new DecimalFormat("#.##");
        }
        return "MX: " + df.format(val) + "kg/m² ";
    }
}