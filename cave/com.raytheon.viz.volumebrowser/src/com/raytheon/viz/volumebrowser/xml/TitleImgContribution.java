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
package com.raytheon.viz.volumebrowser.xml;

import java.util.Set;

import org.eclipse.jface.action.IContributionItem;

import com.raytheon.uf.common.menus.xml.CommonAbstractMenuContribution;
import com.raytheon.uf.common.menus.xml.CommonTitleImgContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.ui.menus.xml.AbstractMenuContributionItem;
import com.raytheon.uf.viz.ui.menus.xml.IContribItemProvider;
import com.raytheon.viz.volumebrowser.widget.TitleImgContributionItem;

public class TitleImgContribution extends
        AbstractMenuContributionItem<CommonTitleImgContribution> implements
        IContribItemProvider {

    public TitleImgContribution() {
        this.xml = new CommonTitleImgContribution();
    }

    @Override
    public IContributionItem[] getContributionItems(
            CommonAbstractMenuContribution items, VariableSubstitution[] subs,
            Set<String> removals) throws VizException {
        TitleImgContribution contrib = new TitleImgContribution();
        contrib.xml = (CommonTitleImgContribution) items;
        return new IContributionItem[] { new TitleImgContributionItem(contrib) };

    }
}
