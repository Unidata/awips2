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

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

public class ToolbarSubmenuContributionItem extends ContributionItem
{
    private IContributionItem[] contribs;

    private String name;
    
    private MenuItem widget;
    
    protected Menu menu;

    public ToolbarSubmenuContributionItem(String name, IContributionItem[] ci, String id) {
        super(id);
        this.contribs = ci;
        this.name = name;        
    }
    
    public IContributionItem[] getContributions()
    {
        return contribs;
    }

    @Override
    public void fill(Menu parent, int index) {

        if (widget != null || parent == null) {
            return;
        }

        MenuItem item = null;
        if (index >= 0) {
            item = new MenuItem(parent, SWT.CASCADE, index);
        } else {
            item = new MenuItem(parent, SWT.CASCADE);
        }

        item.setData(this);
        
        item.setText(this.name);        

        widget = item;
        
        createMenu();
        
        update(null);
    }

    
    @Override
    public void dispose() {
        super.dispose();
        widget.dispose();
        widget = null;
        menu.dispose();
        menu = null;
        for (int i = 0; i < contribs.length; i++)
        {
            contribs[i].dispose();
        }
    }

    private void createMenu()
    {
        menu = new Menu(widget.getParent().getShell(), SWT.DROP_DOWN);
        
        for (int i = 0; i < contribs.length; i++)
        {
            contribs[i].fill(menu, -1);   
        }
        
        widget.setMenu(menu);
    }
}

