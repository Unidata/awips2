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

package com.raytheon.viz.core.style.graph;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.core.style.AbstractStylePreferences;
import com.raytheon.uf.viz.core.style.LabelingPreferences;

/**
 * TODO class description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2007             njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
@XmlRootElement(name = "graphStyle")
@XmlAccessorType(XmlAccessType.NONE)
public class GraphPreferences extends AbstractStylePreferences {

    @XmlElement(name = "range")
    private AxisScale axisScale;

    /**
     * Dotted lines are drawn at these values if they're within the range on the
     * axis
     */
    @XmlElement
    private LabelingPreferences dottedLines;

    /**
     * @return the axisScale
     */
    public AxisScale getAxisScale() {
        return axisScale;
    }

    /**
     * @param axisScale
     *            the axisScale to set
     */
    public void setAxisScale(AxisScale axisScale) {
        this.axisScale = axisScale;
    }

    /**
     * @return the dottedLines
     */
    public LabelingPreferences getDottedLines() {
        return dottedLines;
    }

    /**
     * @param dottedLines
     *            the dottedLines to set
     */
    public void setDottedLines(LabelingPreferences dottedLines) {
        this.dottedLines = dottedLines;
    }

}
