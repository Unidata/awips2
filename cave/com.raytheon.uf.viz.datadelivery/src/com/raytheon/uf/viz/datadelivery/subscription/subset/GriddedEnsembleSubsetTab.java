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
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

import com.raytheon.uf.common.datadelivery.registry.Ensemble;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.SubsetXML;
import com.raytheon.viz.ui.widgets.duallist.DualList;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;
import com.raytheon.viz.ui.widgets.duallist.IUpdate;

/**
 * Gridded ensemble subset tab.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 03, 2013            bsteffen     Initial creation
 * Oct 11, 2013  2386      mpduff       Refactor DD Front end.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GriddedEnsembleSubsetTab {

    private final String NAME = "Ensemble Members";

    private final Set<IDataSize> listeners = new HashSet<IDataSize>();

    private final Ensemble ensemble;

    private DualList dualList;

    private boolean modified;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            Parent composite
     * @param ensemble
     *            Ensemble object
     */
    public GriddedEnsembleSubsetTab(Composite parentComp, Ensemble ensemble) {
        this.ensemble = ensemble;
        init(parentComp);
    }

    private void init(Composite parentComp) {
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        gl.horizontalSpacing = 0;
        gl.verticalSpacing = 0;
        gl.marginWidth = 0;
        gl.marginHeight = 0;

        Group group = new Group(parentComp, SWT.NONE);
        group.setText(getName());
        group.setLayout(gl);
        group.setLayoutData(gd);

        DualListConfig dualListConfig = new DualListConfig();
        dualListConfig.setAvailableListLabel("Available Members:");
        dualListConfig.setSelectedListLabel("Selected Memebers:");
        dualListConfig.setListHeight(125);
        dualListConfig.setListWidth(175);
        dualListConfig.setShowUpDownBtns(false);
        dualListConfig.setFullList(ensemble.getMembers());
        dualList = new DualList(group, SWT.NONE, dualListConfig, new IUpdate() {
            @Override
            public void selectionChanged() {
                modified = true;
                notifyListeners();
            }

            @Override
            public void hasEntries(boolean entries) {

            }
        });
    }

    /**
     * Get the tab name.
     * 
     * @return the tab name
     */
    public String getName() {
        return NAME;
    }

    /**
     * Get the ensemble object representing the selections in this tab.
     * 
     * @return The populated ensemble object
     */
    public Ensemble getEnsembleWithSelection() {
        Ensemble ensemble = new Ensemble(this.ensemble);
        ensemble.setSelectedMembers(Arrays.asList(dualList
                .getSelectedListItems()));
        return ensemble;
    }

    private void loadFromEnsemble(Ensemble ensemble) {
        dualList.clearSelection();
        if (ensemble != null && ensemble.getSelectedMembers() != null) {
            dualList.selectItems(ensemble.getSelectedMembers().toArray(
                    new String[0]));
        }
    }

    /**
     * Add the ensemble data to the provided subscription.
     * 
     * @param subscription
     *            The subscription getting the ensemble data
     */
    public void populateSubscription(Subscription subscription) {
        subscription.setEnsemble(getEnsembleWithSelection());
    }

    /**
     * Load ensemble data from the provided subscription.
     * 
     * @param subscription
     *            The subscription with ensemble data
     */
    public void loadFromSubscription(Subscription subscription) {
        loadFromEnsemble(subscription.getEnsemble());
    }

    /**
     * Load subset object with the ensemble data from this tab.
     * 
     * @param subsetXml
     *            The subset object
     */
    public void populateSubsetXML(SubsetXML subsetXml) {
        subsetXml.setEnsemble(getEnsembleWithSelection());
    }

    /**
     * Load ensemble data from the provided subset object.
     * 
     * @param subsetXml
     */
    public void loadFromSubsetXML(SubsetXML subsetXml) {
        loadFromEnsemble(subsetXml.getEnsemble());
    }

    /**
     * Does a valid selection exist?
     * 
     * @return true if a valid selection
     */
    public boolean isValid() {
        return !CollectionUtil.isNullOrEmpty(dualList.getSelectedListItems());
    }

    /**
     * Get the modified flag
     * 
     * @return the modified flag
     */
    public boolean isModified() {
        return modified;
    }

    /**
     * Set the modified flag
     * 
     * @param modified
     *            true for modified
     */
    public void setModified(boolean modified) {
        this.modified = modified;
    }

    /**
     * Add a listener.
     * 
     * @param listener
     *            The listener
     */
    public void addListener(IDataSize listener) {
        synchronized (this.listeners) {
            listeners.add(listener);
        }
    }

    /**
     * Notifiy the listeners.
     */
    protected void notifyListeners() {
        Collection<IDataSize> listeners;
        synchronized (this.listeners) {
            listeners = new ArrayList<IDataSize>(this.listeners);
        }
        for (IDataSize listener : listeners) {
            listener.updateDataSize();
        }
    }

}
