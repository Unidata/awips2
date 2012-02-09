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
package com.raytheon.viz.volumebrowser.widget;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

import com.raytheon.uf.viz.core.level.LevelMapping;
import com.raytheon.uf.viz.core.level.LevelMappingFactory;
import com.raytheon.viz.volumebrowser.vbui.MenuItemManager;
import com.raytheon.viz.volumebrowser.xml.MenuContribution;

public class MenuContributionItem extends ContributionItem {

    protected MenuItem widget;

    protected MenuContribution menuContribution;

    protected Image image;

    public MenuContributionItem(MenuContribution contribution) {
        this.menuContribution = contribution;
    }

    @Override
    public void fill(Menu parent, int index) {
        if (this.menuContribution == null) {
            return;
        }

        if (widget != null || parent == null) {
            return;
        }

        MenuItem item = null;
        if (index >= 0) {
            item = new MenuItem(parent, SWT.PUSH, index);
        } else {
            item = new MenuItem(parent, SWT.PUSH);
        }

        item.setData(this);

        if (menuContribution.xml.textLookup.equals("LevelMapping")) {
            LevelMapping mapping = LevelMappingFactory.getInstance()
                    .getLevelMappingForKey(menuContribution.xml.key);
            if (mapping != null) {
                menuContribution.xml.menuText = mapping.getDisplayName();
            }
        }

        if (menuContribution.xml.indentText == true) {
            item.setText("     " + menuContribution.xml.menuText);
        } else {
            item.setText(menuContribution.xml.menuText);
        }

        item.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleWidgetSelected();
            }
        });

        widget = item;

        update(null);

        MenuItemManager menuItemMgr = MenuItemManager.getInstance();
        menuItemMgr.addMenuContribItem(menuContribution.xml.key, this);
    }

    public void handleWidgetSelected() {
        widget.setEnabled(false);

        MenuItemManager menuItemMgr = MenuItemManager.getInstance();
        menuItemMgr.menuItemSelectedAction(widget.getText().trim(),
                menuContribution);
    }

    public MenuContribution getMenuContribution() {
        return menuContribution;
    }

    public void enableMenu(boolean flag) {
        widget.setEnabled(flag);
    }

    @Override
    public boolean isEnabled() {
        return widget.getEnabled();
    }

    /**
     * 
     * @return true is data is available, false otherwise
     */
    public boolean isAvailable() {
        return (widget.getImage() != null);
    }

    public String getMenuItemText() {
        return widget.getText().trim();
    }

    @Override
    public void dispose() {
        super.dispose();
        if (widget != null) {
            widget.dispose();
            widget = null;
        }
        if (image != null) {
            image.dispose();
            image = null;
        }
    }

    public void markDataAvailable(boolean available) {
        if (available == true) {
            if (image == null) {
                createMenuImage();
            }
            widget.setImage(image);
        } else {
            widget.setImage(null);
        }
    }

    private void createMenuImage() {
        int imgWidth = 10;
        int imgHeight = 10;

        image = new Image(widget.getDisplay(), imgWidth, imgHeight);

        GC gc = new GC(image);
        drawImage(gc, imgWidth, imgHeight);

        gc.dispose();
    }

    private void drawImage(GC gc, int imgWidth, int imgHeight) {
        gc.setAntialias(SWT.ON);

        // Draw a solid rectangle.
        gc.setBackground(widget.getDisplay().getSystemColor(SWT.COLOR_GREEN));
        gc.fillRectangle(0, 0, imgWidth, imgHeight);

        // Draw a black outline.
        gc.setBackground(widget.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(0, 0, imgWidth - 1, imgHeight - 1);
    }
}
