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

package com.raytheon.viz.hydro.flashfloodguidance;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

import com.raytheon.uf.viz.core.RGBColors;

/**
 * This class is the container that hold the color legend canvas.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation 
 * 12 Oct 2009  2256       mpduff      Implmented the code.
 * 07 Feb 2012  1578       rferrel     Code clean up non-blocking dialogs.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ColorLegendComp extends Composite {
    /**
     * Group title.
     */
    private String groupTitle;

    /**
     * Group composite.
     */
    private Group mainGroupComposite;

    /**
     * Legend canvas.
     */
    private Canvas legendCanvas;

    /**
     * parent composite.
     */
    private Composite parent;

    /**
     * Flash Flood Guidance text.
     */
    private String ffgLineText = "FFG Grid";

    /**
     * Product text.
     */
    private String productText = "";

    /**
     * Canvas font.
     */
    private Font font;

    /**
     * Canvas width.
     */
    private final int CANVAS_WIDTH = 250;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 250;

    /**
     * Color bar width.
     */
    private final int BAR_WIDTH = 80;

    /**
     * Color bar height.
     */
    private final int BAR_HEIGHT = 20;

    /**
     * Current color.
     */
    private Color currentColor;

    /**
     * Array of legend color bar data.
     */
    private List<ColorLegendBarData> barDataArray;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param barDataArray
     *            Array of legend color bar data.
     */
    public ColorLegendComp(Composite parent,
            List<ColorLegendBarData> barDataArray) {
        super(parent, SWT.NONE);

        this.parent = parent;
        this.barDataArray = barDataArray;

        groupTitle = "Color Legend";

        init();
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param title
     *            Group title
     * @param barDataArray
     *            Array of legend color bar data.
     */
    public ColorLegendComp(Composite parent, String title,
            List<ColorLegendBarData> barDataArray) {
        super(parent, SWT.NONE);

        this.parent = parent;
        groupTitle = title;
        this.barDataArray = barDataArray;

        init();
    }

    /**
     * Initialize the class.
     */
    private void init() {
        setLayout(new FillLayout());

        // Setup the main composite.
        mainGroupComposite = new Group(this, SWT.NONE);
        mainGroupComposite.setText(groupTitle);
        GridLayout gl = new GridLayout(3, false);
        mainGroupComposite.setLayout(gl);

        setupCanvas();
    }

    /**
     * Setup the canvas that will display the color bar data.
     */
    private void setupCanvas() {
        font = new Font(parent.getDisplay(), "Courier", 10, SWT.BOLD);

        legendCanvas = new Canvas(mainGroupComposite, SWT.DOUBLE_BUFFERED);
        legendCanvas.setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));
        legendCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawColorLegend(e);
            }
        });

        legendCanvas.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent arg0) {
                if ((font != null) && (font.isDisposed() == false)) {
                    font.dispose();
                }
                if ((currentColor != null)
                        && (currentColor.isDisposed() == false)) {
                    currentColor.dispose();
                }
            }
        });
    }

    /**
     * Draw the color bar legend data on the canvas.
     * 
     * @param e
     *            Paint event.
     */
    private void drawColorLegend(PaintEvent e) {
        e.gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));
        e.gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        e.gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_WHITE));
        e.gc.setFont(font);

        if ((productText == null) || (productText.length() < 1)) {
            return;
        }

        // Draw the FFG line text
        e.gc.drawString(ffgLineText, 3, 0, true);

        // Draw the product line text
        e.gc.drawString(productText.trim(), 3, 15, true);

        ColorLegendBarData barData;
        int barYOffset = 0;

        double lowestValue = 999999999;

        for (int i = 0; i < barDataArray.size(); ++i) {
            barData = barDataArray.get(i);
            if ((barData.getDuration() != -9999)
                    && (barData.getDuration() != -8888)
                    && (barData.getDuration() < lowestValue)) {
                lowestValue = barData.getDuration();
            }

            if ((currentColor != null) && (currentColor.isDisposed() == false)) {
                currentColor.dispose();
            }

            RGB rgb = RGBColors.getRGBColor(barData.getColorName());

            if (rgb == null) {
                System.out.println("-RGB color name cannot be found -- "
                        + barData.getColorName()
                        + "-- setting color to white...");
                currentColor = new Color(parent.getDisplay(), 255, 255, 255);
            } else {
                currentColor = new Color(parent.getDisplay(),
                        RGBColors.getRGBColor(barData.getColorName()));
            }

            e.gc.setBackground(currentColor);

            if (barData.getDuration() == -9999) {
                // We need to move the Y coord down the height of 1 color
                // bar so there is an empty space between the lowest value
                // color bar and the MISSING (-9999) color bar.
                barYOffset = 50 + BAR_HEIGHT * (i + 1);
            } else {
                barYOffset = 50 + BAR_HEIGHT * i;
            }

            e.gc.fillRectangle(130, barYOffset, BAR_WIDTH, BAR_HEIGHT);

            int textY = barYOffset;
            String barText;

            if (i == 0) {
                barText = String.format(">=      %5.2f---",
                        barData.getDuration());
                textY += 11;
            } else if (barData.getDuration() == -8888) {
                double duration = lowestValue;// - 0.01;
                barText = String.format(" <      %5.2f---", duration);
                textY += 11;
            } else if (barData.getDuration() == -9999) {
                barText = "       MISSING";
                textY += 2;
            } else {
                barText = String.format("        %5.2f---",
                        barData.getDuration());
                textY += 11;
            }

            e.gc.drawString(barText, 3, textY, true);
        }
    }

    /**
     * Set the text to display above the legend color bar.
     * 
     * @param line1
     *            The first line of text
     * @param line2
     *            The second line of text
     */
    public void setDisplayText(String line1, String line2) {
        if ((line1 != null) && (line1.length() > 0)) {
            ffgLineText = line1;
        }

        if ((line2 != null) && (line2.length() > 0)) {
            productText = line2;
        }

        legendCanvas.redraw();
    }
}
