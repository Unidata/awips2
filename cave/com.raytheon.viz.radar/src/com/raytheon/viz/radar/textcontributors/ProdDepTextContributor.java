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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;

/**
 * 
 * Take a product dependent parameter and apply the given format
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
public class ProdDepTextContributor implements IRadarTextContributor {

    @XmlAccessorType(XmlAccessType.NONE)
    public static class FlagValue {

        @XmlAttribute(required = true)
        private double value;

        @XmlAttribute(required = true)
        private String label;
    }

    @XmlElement(name = "flag")
    private List<ProdDepTextContributor.FlagValue> flags = new ArrayList<ProdDepTextContributor.FlagValue>();

    @XmlAttribute(required = true)
    private String format;

    @XmlAttribute(required = true)
    private int index;

    @XmlAttribute
    private float multiplier = 1;

    @Override
    public String contributeText(RadarRecord record) {
        if (record.getProductDependentValues() == null) {
            return "Loading";
        }
        if (index >= record.getProductDependentValues().length) {
            return "Invalid Index: " + index;
        }
        for (ProdDepTextContributor.FlagValue flag : flags) {
            if (record.getProductDependentValue(index) == flag.value) {
                return flag.label;
            }
        }
        Float value = record.getProductDependentValue(index) * multiplier;
        if (multiplier == 1) {
            return String.format(format, value.intValue());
        } else {
            return String.format(format, value);
        }
    }

}