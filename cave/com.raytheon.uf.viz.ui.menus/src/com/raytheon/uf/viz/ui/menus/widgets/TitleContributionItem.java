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
package com.raytheon.uf.viz.ui.menus.widgets;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

import com.raytheon.uf.common.menus.xml.CommonTitleContribution;
import com.raytheon.uf.common.menus.xml.VariableSubstitution;
import com.raytheon.uf.viz.core.VariableSubstitutionUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.ui.menus.xml.TitleContribution;

public class TitleContributionItem extends ContributionItem {

    protected MenuItem widget;

    protected TitleContribution titleContribution;

    protected Map<String, String> substitutions;

    protected CommonTitleContribution commonTitleContribution;

    public TitleContributionItem(TitleContribution titleContribution,
            CommonTitleContribution commonTitleContribution,
            VariableSubstitution[] substitutions) throws VizException {
        this.substitutions = VariableSubstitution.toMap(substitutions);
        HashMap<String, String> includeSubstitutionsMap = VariableSubstitution
                .toMap(substitutions);
        commonTitleContribution.titleText = VariableSubstitutionUtil
                .processVariables(commonTitleContribution.titleText,
                        includeSubstitutionsMap);
        this.commonTitleContribution = commonTitleContribution;
        this.titleContribution = titleContribution;
    }

    @Override
    public void fill(Menu parent, int index) {
        if (this.commonTitleContribution == null) {
            return;
        }

        if (parent == null) {
            return;
        }

        if (widget != null && !widget.isDisposed()) {
            return;
        }

        MenuItem item = null;
        if (index >= 0) {
            item = new MenuItem(parent, SWT.PUSH, index);
        } else {
            item = new MenuItem(parent, SWT.PUSH);
        }
        item.setEnabled(false);
        item.setData(this);
        item.setText(this.commonTitleContribution.titleText);

        widget = item;
        update(null);
    }
}
