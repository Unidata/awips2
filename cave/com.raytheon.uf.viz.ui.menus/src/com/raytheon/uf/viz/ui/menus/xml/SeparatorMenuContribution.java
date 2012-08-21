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

import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.Separator;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonSeparatorMenuContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Describes a separator contribution
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2009             chammack    Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SeparatorMenuContribution extends
        AbstractMenuContributionItem<CommonSeparatorMenuContribution> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.ui.menus.xml.IContribItemProvider#getContributionItems
     * (com.raytheon.uf.viz.ui.menus.xml.VariableSubstitution[], java.util.Set)
     */
    @Override
    public IContributionItem[] getContributionItems(
            CommonAbstractMenuContribution items, VariableSubstitution[] subs,
            Set<String> removals) throws VizException {
        CommonSeparatorMenuContribution item = (CommonSeparatorMenuContribution) items;
        if (removals.contains(item.id))
            return new IContributionItem[0];

        Separator s = new Separator(item.id);
        s.setVisible(item.visible);
        return new IContributionItem[] { s };
    }

}
