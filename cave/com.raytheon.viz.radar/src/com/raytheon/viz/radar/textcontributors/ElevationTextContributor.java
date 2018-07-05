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

import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;

/**
 * 
 * Format elevation angle based on the given format
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2010            bsteffen     Initial creation
 * Oct 18,2017  DCS20303  jdynina      Use true elevation angle
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ElevationTextContributor implements IRadarTextContributor {

    @XmlAttribute(required = true)
    private String format;

    @Override
    public String contributeText(RadarRecord record) {
        return String.format(format != null ? format : "%.1f", record.getTrueElevationAngle());
    }
}