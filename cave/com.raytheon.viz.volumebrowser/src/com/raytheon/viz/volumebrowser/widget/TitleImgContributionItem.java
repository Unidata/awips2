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
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

import com.raytheon.viz.volumebrowser.xml.TitleImgContribution;

public class TitleImgContributionItem extends ContributionItem {

    protected MenuItem widget;

    protected TitleImgContribution titleImgContribution;

    public TitleImgContributionItem(TitleImgContribution titleImgContribution) {

        this.titleImgContribution = titleImgContribution;
    }

    @Override
    public void fill(Menu parent, int index) {
        if (this.titleImgContribution == null) {
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
        widget = item;

        if (titleImgContribution.xml.displayDashes == true) {
            // item.setText(titleImgContribution.titleText);
            widget.setText("--- " + titleImgContribution.xml.titleText + " ---");
        } else {
            widget.setText(titleImgContribution.xml.titleText);
        }

        if (titleImgContribution.xml.displayImage == true) {
            createTitleImage(widget);
        }

        update(null);
    }

    private void createTitleImage(MenuItem mi) {
        int imgWidth = 10;
        int imgHeight = 10;

        Display display = mi.getDisplay();

        Image menuImg = new Image(display, imgWidth, imgHeight);

        GC gc = new GC(menuImg);
        drawImage(gc, imgWidth, imgHeight, display);

        gc.dispose();

        mi.setImage(menuImg);

        menuImg.dispose();
    }

    private void drawImage(GC gc, int imgWidth, int imgHeight, Display display) {
        gc.setAntialias(SWT.ON);

        // "Erase" the canvas by filling it in with the widget color.
        gc.setBackground(display.getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, imgWidth, imgHeight);

        // Draw a solid rectangle.
        gc.setBackground(display.getSystemColor(SWT.COLOR_BLUE));
        gc.fillRectangle(0, 0, imgWidth, imgHeight);
        // gc.fillOval(0, 0, imgWidth, imgHeight);

        // Draw a black outline.
        gc.setBackground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.drawRectangle(0, 0, imgWidth - 1, imgHeight - 1);
        // gc.drawOval(0, 0, imgWidth - 1, imgHeight - 1);
    }

    @Override
    public void dispose() {
        super.dispose();
        if (widget != null) {
            widget.dispose();
            widget = null;
        }
    }
}
