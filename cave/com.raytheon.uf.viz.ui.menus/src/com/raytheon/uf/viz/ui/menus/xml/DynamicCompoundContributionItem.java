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
package com.raytheon.uf.viz.ui.menus.xml;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.ui.actions.CompoundContributionItem;

import com.raytheon.uf.common.menus.xml.VariableSubstitution;

/**
 * Class to allow making dynamic contribution item list.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2012 875        rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public abstract class DynamicCompoundContributionItem extends
        CompoundContributionItem {
    protected VariableSubstitution[] substitutions;

    public IContributionItem[] getItems() {
        return getContributionItems();
    }

    public VariableSubstitution[] getSubstitutions() {
        return substitutions;
    }

    public void setSubstitutions(VariableSubstitution[] substitutions) {
        this.substitutions = substitutions;
    }

}
