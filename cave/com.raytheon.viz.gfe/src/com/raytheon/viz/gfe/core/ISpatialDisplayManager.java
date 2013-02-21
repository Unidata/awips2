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
package com.raytheon.viz.gfe.core;

import java.util.Date;

import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener;
import com.raytheon.viz.gfe.core.msgs.IDisplayModeChangedListener;
import com.raytheon.viz.gfe.core.msgs.IGlobalSelectionTRChangedListener;
import com.raytheon.viz.gfe.core.msgs.IGridVisibilityChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISpatialEditorTimeChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.edittool.AbstractGFEEditTool;
import com.raytheon.viz.gfe.rsc.GFESystemResource;

/**
 * Defines interactions with the spatial display
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/20/2008              chammack    Initial Creation.
 * 07/03/2008       #1160  randerso    Added makeVisible method
 * 04/09/2009        1288  rjpeter     Added toggleVisibility and removed refresh.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public interface ISpatialDisplayManager {

    /**
     * Get the spatial editor time
     * 
     * @return the time of the spatial editor
     */
    public Date getSpatialEditorTime();

    /**
     * Set the spatial editor time
     * 
     * @param date
     *            the date to set the spatial editor to
     */
    public void setSpatialEditorTime(Date date);

    /**
     * Activate the specified parm for the display manager.
     * 
     * @param parmToActivate
     *            the parm to activate
     * @throws GFEOperationFailedException
     *             if unable to activate the parm
     * 
     */
    public void activateParm(Parm parmToActivate)
            throws GFEOperationFailedException;

    /**
     * Set the global time range
     * 
     * @param timeRange
     *            the global time range
     */
    public void setGlobalTimeRange(TimeRange timeRange);

    /**
     * Get the global time range
     * 
     * @return the global time range
     */
    public TimeRange getGlobalTimeRange();

    /**
     * Get the activated parm
     * 
     * @return the activated parm
     */
    public Parm getActivatedParm();

    /**
     * Get the most recently activated parm
     * 
     * @return the most recently activated parm
     */
    public Parm getLastActivatedParm();

    /**
     * Add a listener for when the activated (editable) parm changes
     * 
     * @param parmChangeListener
     *            the listener
     */
    public void addActivatedParmChangedListener(
            IActivatedParmChangedListener parmChangeListener);

    /**
     * Removes a listener for when the activated (editable) parm changes
     * 
     * @param parmChangeListener
     *            the listener
     */
    public void removeActivatedParmChangedListener(
            IActivatedParmChangedListener parmChangeListener);

    /**
     * Add a listener for when the activated (editable) parm changes
     * 
     * @param parmChangeListener
     *            the listener
     */
    public void addGridVisibilityChangedListener(
            IGridVisibilityChangedListener parmChangeListener);

    /**
     * Removes a listener for when the activated (editable) parm changes
     * 
     * @param parmChangeListener
     *            the listener
     */
    public void removeGridVisibilityChangedListener(
            IGridVisibilityChangedListener parmChangeListener);

    /**
     * Add a listener for when the global selection time range changes
     * 
     * @param listener
     */
    public void addGlobalSelectionTRChangedListener(
            IGlobalSelectionTRChangedListener listener);

    /**
     * Remove a listener for when the global selection time range changes
     * 
     * @param listener
     */
    public void removeGlobalSelectionTRChangedListener(
            IGlobalSelectionTRChangedListener listener);

    /**
     * Add a listener for when the display mode changes
     * 
     * @param listener
     */
    public void addDisplayModeChangedListener(
            IDisplayModeChangedListener listener);

    /**
     * Remove a listener for when the display mode changes
     * 
     * @param listener
     */
    public void removeDisplayModeChangedListener(
            IDisplayModeChangedListener listener);

    /**
     * Add a listener for when the spatial editor time changes
     * 
     * @param listener
     */
    public void addSpatialEditorTimeChangedListener(
            ISpatialEditorTimeChangedListener listener);

    /**
     * Remove a listener for when the spatial editor time changes
     * 
     * @param listener
     */
    public void removeSpatialEditorTimeChangedListener(
            ISpatialEditorTimeChangedListener listener);

    /**
     * Add an edit tool to the display
     * 
     * @param editTool
     *            the edit tool
     * @throws VizException
     */
    public void addEditTool(AbstractGFEEditTool editTool) throws VizException;

    /**
     * Remove an edit tool from the display
     * 
     * @param editTool
     *            the edit tool
     * @throws VizException
     */
    public void removeEditTool(AbstractGFEEditTool editTool)
            throws VizException;

    /**
     * Return the list of parms that the user currently has visible on the map.
     * This is different than the Visible parms in the IParmManager, which is a
     * list of parms that are visible to the grid manager.
     * 
     * @return the parms currently shown on screen
     */
    public Parm[] getCurrentlyEnabledParms();

    /**
     * Return the GFE System Resource
     * 
     * @return the system resource
     * @throws VizException
     */
    public GFESystemResource getSystemResource() throws VizException;

    /**
     * Toggles the visibility of the specified parm.
     * 
     * @param parm
     */
    public void toggleVisibility(Parm parm);

    /**
     * Make the specified parm visible
     * 
     * @param parm
     * @param visible
     *            true to make visible, false to make invisible
     * @param makeOnlyVisible
     *            true to make only this parm visible, false to add this parm to
     *            set of visible parms
     */
    void makeVisible(Parm parm, boolean visible, boolean makeOnlyVisible);

    /**
     * Set the display mode of the specified parm
     * 
     * @param parm
     * @param mode
     *            display mode
     * 
     */
    void setDisplayMode(Parm parm, VisMode mode);

    /**
     * Returns the resource pair for a specific Parm and null if the resource
     * pair cannot be found.
     * 
     * @param parm
     *            The parm that is associated to the resource pair
     * @return The resource pair that matches the parm or null if the resource
     *         pair is not found
     */
    public ResourcePair getResourcePair(Parm parm);

    /**
     * If true the Wx/Discrete description will be displayed in the pickup value
     * dialog
     * 
     * @return true is show description set
     */
    public boolean getShowDescription();

    /**
     * Set to true to display the Wx/Discrete description in the pickup value
     * dialog
     * 
     * @param showDescription
     */
    public void setShowDescription(boolean showDescription);

    public boolean isShowISCMarkers();

    public void setShowISCMarkers(boolean showIscMarkers);

    public boolean isShowISCUpdateTimeMarker();

    public void setShowISCUpdateTimeMarker(boolean showIscUpdateTime);

    public boolean isShowISCSiteIdMarker();

    public void setShowISCSiteIDMarker(boolean showIscSiteId);

    public boolean isShowISCOfficialSymbolMarker();

    public void setShowISCOfficialSymbolMarker(boolean showIscOfficial);

    public boolean isShowIscSampleUpdateTime();

    public void setShowISCUpdateTime(boolean showIscSampleUpdateTime);

    public boolean isShowISCSiteID();

    public void setShowISCSiteID(boolean showIscSampleSite);

    public boolean isShowISCOfficialSymbol();

    public void setShowISCOfficialSymbol(boolean showIscSampleOfficial);

    // /**
    // * Add a parm to the display and optionally make it visible
    // *
    // * @param parm
    // * @param visible
    // * @throws VizException
    // */
    // void addParm(Parm parm, boolean visible) throws
    // GFEOperationFailedException;
}