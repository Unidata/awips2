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
package com.raytheon.uf.viz.datadelivery.subscription.subset;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.viz.ui.widgets.duallist.DualList;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;
import com.raytheon.viz.ui.widgets.duallist.IUpdate;

/**
 * Level/Parameter selection widgets.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2012            mpduff     Initial creation.
 * Aug 08, 2012    863     jpiatt     Added clean & dirty checks.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class LevelParameterSelection extends Composite implements IUpdate {
    
    private final List<String> levelList;

    private final List<String> paramList;

    /** Dual list for levels */
    private DualList dualLevelList;

    /** Dual list for parameters */
    private DualList dualParamList;

    private final ISubset callback;

    private final String id;
     
    /** Flag to determine if tab has changed */
    private boolean isDirty = false;

    /**
     * Constructor
     * 
     * @param parent
     * @param style
     * @param levelList
     * @param paramList
     * @param callback
     * @param id
     */
    public LevelParameterSelection(Composite parent, int style,
            List<String> levelList, List<String> paramList, ISubset callback,
            String id) {
        super(parent, style);
        this.levelList = levelList;
        this.paramList = paramList;
        this.callback = callback;
        this.id = id;
        init();
    }

    private void init() {
        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        if (levelList != null && levelList.size() > 0) {
            DualListConfig levelConfig = new DualListConfig();
            levelConfig.setAvailableListLabel("Available Levels:");
            levelConfig.setSelectedListLabel("Selected Levels:");
            levelConfig.setListHeight(75);
            levelConfig.setListWidth(100);
            levelConfig.setShowUpDownBtns(false);
            levelConfig.setFullList(levelList);

            dualLevelList = new DualList(this, SWT.NONE, levelConfig, this);
        }

        DualListConfig paramConfig = new DualListConfig();
        paramConfig.setAvailableListLabel("Available Parameters:");
        paramConfig.setSelectedListLabel("Selected Parameters:");
        paramConfig.setListHeight(75);
        paramConfig.setListWidth(100);
        paramConfig.setShowUpDownBtns(false);
        paramConfig.setFullList(paramList);
        
        dualParamList = new DualList(this, SWT.NONE, paramConfig, this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.common.ui.IUpdate#hasEntries(boolean)
     */
    @Override
    public void hasEntries(boolean entries) {
        if (dualParamList != null && dualLevelList != null) {
            if (dualParamList.getItemCount() > 0 && this.dualLevelList.getItemCount() > 0) {
                callback.updateSelectionState(true, id);
            } else {
                callback.updateSelectionState(false, id);
            }
        } else {
            if (dualParamList != null && dualParamList.getItemCount() > 0) {
                callback.updateSelectionState(true, id);
            } else {
                callback.updateSelectionState(false, id);
            }
        }

    }

    /**
     * Get the number of items in the level selected list.
     * 
     * @return The number of items in the selected list
     */
    public int getLevelSelectListCount() {
        return dualLevelList.getItemCount();
    }

    /**
     * Get the number of items in the parameter selected list.
     * 
     * @return The number of items in the selected list
     */
    public int getParamSelectListCount() {
        return dualParamList.getItemCount();
    }

    /**
     * Get the items in the selected parameter list.
     * 
     * @return String[] of items
     */
    public String[] getSelectedParameters() {
        return dualParamList.getSelectedListItems();
    }

    /**
     * Get the items in the selected level list.
     * 
     * @return String[] of items
     */
    public String[] getSelectedLevels() {
        if (dualLevelList != null) {
            return dualLevelList.getSelectedListItems();
        }

        return new String[0];
    }

    /**
     * If items are selected in the list then can expand
     * 
     * @return true if item can expand
     */
    public boolean shouldExpand() {
        if (dualParamList != null && dualLevelList != null) {
            if (dualParamList.getItemCount() > 0 && this.dualLevelList.getItemCount() > 0) {
                return true;
            }
        } else {
            if (dualParamList.getItemCount() > 0) {
                return true;
            }
        }

        return false;
    }
    
    /**
     * Action when selecting Parameters on the vertical tab.
     * 
     * @param levelList
     */
    public void selectLevels(ArrayList<String> levelList) {
        dualLevelList.selectItems(levelList.toArray(new String[levelList.size()]));
    }
    
    /**
     * Action when selecting Parameters on the vertical tab.
     * 
     * @param paramList
     */
    public void selectParameters(ArrayList<String> paramList) {
        dualParamList.selectItems(paramList.toArray(new String[paramList.size()]));
    }
    
    /**
     * Set isDirty flag.
     * 
     * @return isDirty
     */
    public boolean isDirty() {
        return isDirty;
    }

    /**
     * Set parameters to clean.
     */
    public void setClean() {
       isDirty = false;
    }

    @Override
    public void selectionChanged() {
        isDirty = true;
    }
}
