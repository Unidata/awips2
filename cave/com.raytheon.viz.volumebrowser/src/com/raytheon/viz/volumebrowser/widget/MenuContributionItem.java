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

import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapping;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.viz.volumebrowser.vbui.MenuItemManager;
import com.raytheon.viz.volumebrowser.xml.MenuContribution;

/**
 * This class handles putting an instance of MenuContribution into a parent menu
 * and the displaying of the availability status.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Oct 03, 2012 #1235      rferrel     Checks for disposed widget.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class MenuContributionItem extends ContributionItem {

    protected MenuItem widget;

    protected MenuContribution menuContribution;

    protected Image image;

    /**
     * @param contribution
     */
    public MenuContributionItem(MenuContribution contribution) {
        this.menuContribution = contribution;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.action.ContributionItem#fill(org.eclipse.swt.widgets
     * .Menu, int)
     */
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
            LevelMapping mapping = LevelMappingFactory.getInstance(
                    LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
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
        if (!widget.isDisposed()) {
            widget.setEnabled(flag);
        }
    }

    @Override
    public boolean isEnabled() {
        if (!widget.isDisposed()) {
            return widget.getEnabled();
        }
        return false;
    }

    /**
     * 
     * @return true if data is available, false otherwise
     */
    public boolean isAvailable() {
        if (!widget.isDisposed()) {
            return (widget.getImage() != null);
        }
        return false;
    }

    public String getMenuItemText() {
        if (!widget.isDisposed()) {
            return widget.getText().trim();
        }
        return "";
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
        if (!widget.isDisposed()) {
            if (available == true) {
                if (image == null) {
                    createMenuImage();
                }
                widget.setImage(image);
            } else {
                widget.setImage(null);
            }
        }
    }

    private void createMenuImage() {
        if (!widget.isDisposed()) {
            int imgWidth = 10;
            int imgHeight = 10;

            image = new Image(widget.getDisplay(), imgWidth, imgHeight);

            GC gc = new GC(image);
            drawImage(gc, imgWidth, imgHeight);

            gc.dispose();
        }
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
