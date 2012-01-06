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

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlTransient;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;

/**
 * 
 * Take two product dependent parameters and format them using a
 * SimpleDataFormat object based on the format specified
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
@XmlAccessorType(XmlAccessType.NONE)
public class ProdDepDateTextContributor implements IRadarTextContributor {

    @XmlAttribute(required = true)
    private int dayIndex = -1;

    @XmlAttribute(required = true)
    private int minuteIndex;

    @XmlAttribute(required = true)
    private String format;

    @XmlTransient
    private SimpleDateFormat sdf;

    @Override
    public String contributeText(RadarRecord record) {
        if (record.getProductDependentValues() == null) {
            return "Loading";
        }
        if (dayIndex >= record.getProductDependentValues().length) {
            return "Invalid Index: " + dayIndex;
        } else if (minuteIndex >= record.getProductDependentValues().length) {
            return "Invalid Index: " + minuteIndex;
        }
        Calendar t = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        if (dayIndex < 0) {
            long nsecs = record.getProductDependentValue(minuteIndex) * 60;
            t.setTimeInMillis(nsecs * 1000);
        } else {
            long nsecs = (record.getProductDependentValue(dayIndex) - 1)
                    * 86400l + record.getProductDependentValue(minuteIndex)
                    * 60;
            t.setTimeInMillis(nsecs * 1000);
        }
        if (sdf == null) {
            sdf = new SimpleDateFormat(this.format);
            sdf.setCalendar(t);
        }
        return sdf.format(t.getTime());
    }

}