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
package com.raytheon.viz.core.style.arrow;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.style.AbstractStylePreferences;

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "arrowStyle")
public class ArrowPreferences extends AbstractStylePreferences {

    @XmlElement
    private double scale = 1.0;

    public void setScale(double scale) {
        this.scale = scale;
    }

    public double getScale() {
        return scale;
    }

    // TODO this should contain some other preferences
    // 1. This units string replaces whatever units string is in the legend text
    // in the depictable info table. If blank the default is left alone.
    // 2. The data that come from the virtual data server are multiplied by this
    // to get the display units. If this is a negative number then logarithmic
    // scaling will be used for the arrow lengths.
    // 3. This is the magnitude value, in display units, that is scaled to the
    // default length, which is 25 pixels times the character magnification. Two
    // values separated by a greater than sign will cause this to vary with the
    // vertical coordinate value. Presence of the item `log' in the rule
    // (delimited by commas like everything else) will cause this to vary with
    // the natural log of the vertical coordinate value.
    // 4. This is the magnitude value, in display units, that is the smallest
    // value for which to show an arrow. Defaults to zero.
    // 5. This is the magnitude value, in display units, that is the largest
    // value for which to show an arrow. Defaults to an arbitrarily large
    // number.
}
