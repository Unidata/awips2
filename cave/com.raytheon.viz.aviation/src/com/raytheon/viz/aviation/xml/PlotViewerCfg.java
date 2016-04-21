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
package com.raytheon.viz.aviation.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * Contains information for the initializing the plot viewer check box controls.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 11, 2009            lvenable     Initial creation
 * Nov 18, 2010 6701       rferrel      Created enum of valid class names.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class PlotViewerCfg implements ISerializableObject {

    /**
     * The Xml configuration ClassName's tags valid values.
     */
    static public enum ClassNames {
        TAFS("dcdTafs"), METARS("dcdMtrs"), NAM_MOS("namMosData"), GFS_MOS(
                "gfsMosData"), GFSLAMP("gfsLampData"), NAM_WRF("namWrfData"), ;

        private String name;

        ClassNames(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }
    }

    /**
     * Check box label name.
     */
    @XmlElement(name = "LabelName")
    private String labelName;

    /**
     * Class name of the class to invoke.
     */
    @XmlElement(name = "ClassName")
    private String className;

    /**
     * Background color of the check box.
     */
    @XmlElement(name = "ColorName")
    private String colorName;

    /**
     * Flag indicating if the check box is selected or not.
     */
    @XmlElement(name = "Selected")
    private boolean selected;

    /**
     * Constructor.
     */
    public PlotViewerCfg() {
    }

    /**
     * Get the label name.
     * 
     * @return The label name.
     */
    public String getLabelName() {
        return labelName;
    }

    /**
     * Set the label name.
     * 
     * @param labelName
     *            The label name.
     */
    public void setLabelName(String labelName) {
        this.labelName = labelName;
    }

    /**
     * Get the class name.
     * 
     * @return The class name.
     */
    public String getClassName() {
        return className;
    }

    /**
     * Set the class name.
     * 
     * @param className
     *            The class name.
     */
    public void setClassName(String className) {
        this.className = className;
    }

    /**
     * Get the color name.
     * 
     * @return The color name.
     */
    public String getColorName() {
        return colorName;
    }

    /**
     * Set the color name.
     * 
     * @param colorName
     *            The color name.
     */
    public void setColorName(String colorName) {
        this.colorName = colorName;
    }

    /**
     * Get the selection flag.
     * 
     * @return True if the item is selected, false otherwise.
     */
    public boolean getSelected() {
        return selected;
    }

    /**
     * Set the selection.
     * 
     * @param selected
     *            True for selected, false for unselected.
     */
    public void setSelected(boolean selected) {
        this.selected = selected;
    }
}
