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
package com.raytheon.viz.pointdata.def;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

/**
 * Maps a condition to a color. Used in plots to color the plot fields (Say
 * Temp) differently based on their individual value (condition. Say Temp less
 * than 50 is red)
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- -----------   --------------------------
 * Jan 07, 2020  73083    ksunil         Initial Creation
 *
 * </pre>
 *
 * @author ksunil
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class ColorCondition extends Condition {

    @XmlElement
    @XmlJavaTypeAdapter(value = RgbAdapter.class)
    private RGB color;

    public ColorCondition() {
        super();
    }

    public ColorCondition(RGB color) {
        super();
        this.color = color;
    }

    public ColorCondition(ColorCondition other) {
        super(other);
        color = ConditionalColor.cloneRgb(other.color);
    }

    public ColorCondition(List<ParamConstraint> paramConstraints, RGB color) {
        super(paramConstraints);
        this.color = color;
    }

    public RGB getColor() {
        return color;
    }

    public void setColor(RGB color) {
        this.color = color;
    }
}