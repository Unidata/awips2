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
package com.raytheon.viz.mpe.ui.rsc;

import java.util.HashSet;
import java.util.Set;

import javax.measure.Unit;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay2;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.DisplayMode;

/**
 * Generic resource data for MPE plot resources
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 06, 2012  ?        mschenke  Initial creation
 * Mar 01, 2017  6160     bkowal    Added {@link #getDisplayString()}.
 * Jun 20, 2019  7137     bhurley   Changed data type to allow for accumulation
 *                                  values greater than 13 inches.
 * 
 * </pre>
 * 
 * @author mschenke
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractMPEGriddedResourceData
        extends AbstractResourceData {

    public static class Frame {
        public final int[] data;

        public GriddedImageDisplay2 imageDisplay;

        public GriddedContourDisplay contourDisplay;

        public Frame(int[] data) {
            this.data = data;
        }

        public synchronized void dispose() {
            disposeImage();
            disposeContour();
        }

        public synchronized void disposeImage() {
            if (imageDisplay != null) {
                imageDisplay.dispose();
                imageDisplay = null;
            }
        }

        public synchronized void disposeContour() {
            if (contourDisplay != null) {
                contourDisplay.dispose();
                contourDisplay = null;
            }
        }
    }

    @XmlElement(name = "displayMode")
    private Set<DisplayMode> displayModes = new HashSet<>();

    @Override
    public void update(Object updateData) {
        fireChangeListeners(ChangeType.DATA_UPDATE, updateData);
    }

    /**
     * @return the displayModes
     */
    public Set<DisplayMode> getDisplayModes() {
        return displayModes;
    }

    /**
     * @param displayModes
     *            the displayModes to set
     */
    public void setDisplayModes(Set<DisplayMode> displayModes) {
        this.displayModes = displayModes;
    }

    /**
     * Toggles visibilty of specified display mode
     * 
     * @param displayMode
     */
    public void toggleDisplayMode(DisplayMode displayMode) {
        if (!displayModes.remove(displayMode)) {
            displayModes.add(displayMode);
        }
    }

    /**
     * Checks if the DisplayMode is displayed
     * 
     * @param displayMode
     * @return
     */
    public boolean isDisplayed(DisplayMode displayMode) {
        return displayModes.contains(displayMode);
    }

    /**
     * Get the duration of the gridded data
     * 
     * @return
     */
    public abstract int getDurationInHours();

    /**
     * Gets the cv_use name for the gridded data
     * 
     * @return
     */
    public abstract String getCvUseString();

    /**
     * Used to retrieve the display name/string of the gridded data.
     * 
     * @return the display name/string of the gridded data
     */
    public String getDisplayString() {
        return null;
    }

    /**
     * Get the data units for this type
     * 
     * @return
     */
    public abstract Unit<?> getDataUnits();

    /**
     * TODO: Replace with style rules?
     * 
     * Get the display units for this type
     * 
     * @return
     */
    public abstract Unit<?> getDisplayUnits();

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        String cvUse = getCvUseString();
        result = prime * result + ((cvUse == null) ? 0 : cvUse.hashCode());
        result = prime * result + getDurationInHours();
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        AbstractMPEGriddedResourceData other = (AbstractMPEGriddedResourceData) obj;
        String cvUseString = getCvUseString();
        String otherCvUseString = other.getCvUseString();
        if (cvUseString == null) {
            if (otherCvUseString != null) {
                return false;
            }
        } else if (!cvUseString.equals(otherCvUseString)) {
            return false;
        }
        if (getDurationInHours() != other.getDurationInHours()) {
            return false;
        }
        return true;
    }

}
