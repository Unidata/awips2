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
package com.raytheon.viz.texteditor.dialogs;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * Wrap button configuration
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 09, 2011           rferrel   Initial creation
 * May 01, 2019  7831     randerso  Removed unnecessary wrapEnabled field.
 *                                  Changes to support making AutoWrapCfg.xml an
 *                                  incremental override.
 *
 * </pre>
 *
 * @author rferrel
 */
@XmlAccessorType(XmlAccessType.NONE)
public class WrapButtonCfg {
    @XmlElement(name = "LabelName", required = false)
    private String labelName;

    @XmlElement(name = "WrapCol", required = true)
    private int wrapCol;

    @XmlElement(name = "Selected", required = false)
    private boolean selected = false;

    /**
     * default constructor
     *
     * defaults to wrap disabled
     */
    public WrapButtonCfg() {
        this(-1, false);
    }

    /**
     * @param wrapCol
     *            wrap column or -1 to disable wrapping
     * @param selected
     */
    public WrapButtonCfg(int wrapCol, boolean selected) {
        this.wrapCol = wrapCol;
        this.selected = selected;
    }

    /**
     * @return the labelName
     */
    public String getLabelName() {
        if (labelName != null) {
            return labelName;
        }

        return String.format("%d chars", wrapCol);
    }

    /**
     * @return the wrapCol
     */
    public int getWrapCol() {
        return wrapCol;
    }

    /**
     * @param wrapCol
     *            the wrapCol to set
     */
    public void setWrapCol(int wrapCol) {
        this.wrapCol = wrapCol;
    }

    /**
     * @return the selected
     */
    public boolean isSelected() {
        return selected;
    }

    /**
     * @param selected
     *            the selected to set
     */
    public void setSelected(boolean selected) {
        this.selected = selected;
    }

    /**
     * @return true if wrap is enabled (wrapCol > -1)
     */
    public boolean isWrapEnabled() {
        return wrapCol > -1;
    }

    @Override
    public String toString() {
        return getLabelName() + (isSelected() ? "<-Selected" : "");
    }
}
