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
package com.raytheon.viz.hydro.riversummary;

/**
 * draw the staff gage.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10 NOV 2008  1628       dhladky      Initial creation 
 * 15 JAN 2008  1802       askripsk     Added missing data handling
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 *
 */

import java.text.DecimalFormat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;

import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.RiverDataPoint;

public class RiverGage {

    private static int GAGE_WIDTH = 51;

    private static int STAGE_RECT_WIDTH = 40;

    private static int PIXELS_PER_TICK = 10;

    private static int TICK_LENGTH = 3;

    private static int CENTER_OFFSET = 14;

    private static Color MAJOR_COLOR = new Color(null, 255, 0, 0);

    private static Color MODERATE_COLOR = new Color(null, 255, 255, 0);

    private static Color NO_FLOOD_COLOR = new Color(null, 0, 255, 0);

    private static Color STAGE_COLOR = new Color(null, 0, 0, 255);

    private static Color OUTLINE_COLOR = new Color(null, 255, 255, 255);

    private static DecimalFormat df = new DecimalFormat();

    /**
     * Gets the Font height offset
     * 
     * @param gc
     * @return
     */
    private static int getFontOffset(GC gc) {
        return (gc.getFontMetrics().getHeight() / 2);
    }

    /**
     * Draw the staff gage information on the canvas.
     * 
     * @param e
     *            Paint event.
     */
    public static void drawRiverGage(GC gc, RiverDataPoint rdp, int xStart,
            int yStart, double stageValue, double minStage, double maxStage, MaxMin maxMin) {
        // calculate the total height above and below the flood stage line
        double totalIncBelow = Math.round(rdp.getFloodStage());
        double totalIncAbove = maxStage - rdp.getFloodStage();
        
        if (maxStage > 50) {
            PIXELS_PER_TICK = 3;
        } else if (maxStage > 30) {
            PIXELS_PER_TICK = 6;
        } else {
            PIXELS_PER_TICK = 10;
        }
        
        
        int aboveGageHeight = (int) totalIncAbove * PIXELS_PER_TICK;
        int belowGageHeight = (int) totalIncBelow * PIXELS_PER_TICK;

        int MINX = xStart;
        int MAXX = xStart + GAGE_WIDTH;
        int MIDY = yStart;
        int MINY = yStart - aboveGageHeight;
        int MAXY = yStart + belowGageHeight;
        int ACTIONY = MAXY - (int) (rdp.getActionStage() * PIXELS_PER_TICK);
        int STAGEY = MAXY - (int) (stageValue * PIXELS_PER_TICK);
        
        df.setMinimumIntegerDigits(1);
        df.setMaximumFractionDigits(2);
        df.setMinimumFractionDigits(1);
        gc.setLineStyle(SWT.LINE_SOLID);

        // ------------------------------------------
        // Create Below Flood Stage, GREEN area on gage
        // ------------------------------------------
        if (Double.compare(rdp.getMinorStage(), HydroConstants.MISSING_VALUE) != 0) {
            gc.setBackground(NO_FLOOD_COLOR);
            gc.setForeground(NO_FLOOD_COLOR);
            gc.fillRectangle(MINX, MINY, GAGE_WIDTH, MAXY - MINY);
//            gc.drawString(df.format(rdp.getActionStage()), MINX
//                    + (STAGE_RECT_WIDTH / 2), ACTIONY - (getFontOffset(gc) * 2),
//                    true);
        }

        // -------------------------------------
        // Create Moderate Flood Stage, YELLOW areas on gage
        // -------------------------------------
        if (Double
                .compare(rdp.getModerateStage(), HydroConstants.MISSING_VALUE) != 0) {
            if (rdp.getActionStage() > 0) {
            gc.setBackground(MODERATE_COLOR);
            gc.setForeground(MODERATE_COLOR);
            gc.fillRectangle(MINX, MIDY, GAGE_WIDTH, ACTIONY - MIDY);
//            gc.drawString(df.format(rdp.getMinorStage()), MINX
//                    + (STAGE_RECT_WIDTH / 2), MIDY
//                    - (getFontOffset(gc) * 2), true);
            }
        }

        // -------------------------------------
        // Create Major Flood Stage, RED area on GAGE
        // -------------------------------------
        if (Double.compare(rdp.getFloodStage(), HydroConstants.MISSING_VALUE) != 0) {
            gc.setBackground(MAJOR_COLOR);
            gc.setForeground(MAJOR_COLOR);
            gc.fillRectangle(MINX, MINY, GAGE_WIDTH, MIDY - MINY);
            gc.setForeground(OUTLINE_COLOR);
            gc.drawString(df.format(rdp.getFloodStage()), MAXX + TICK_LENGTH + 4, (int) (MAXY
                    - ((PIXELS_PER_TICK * rdp.getFloodStage()) + getFontOffset(gc)*2)),
                    true);
//            gc.drawString(df.format(i), MAXX + TICK_LENGTH + 4, MAXY
//                    - ((PIXELS_PER_TICK * i) + getFontOffset(gc)), true);
        }

        // ------------------------------------------
        // Create Current Stage, Blue area on gage
        // ------------------------------------------
        if (Double.compare(stageValue, HydroConstants.MISSING_VALUE) != 0) {
            gc.setBackground(STAGE_COLOR);
            gc.setForeground(STAGE_COLOR);
            gc.fillRectangle(MINX + (STAGE_RECT_WIDTH / 2) - CENTER_OFFSET,
                    STAGEY, STAGE_RECT_WIDTH, MAXY - STAGEY);
            gc.setForeground(OUTLINE_COLOR);
            gc.drawString(df.format(stageValue), MINX + (STAGE_RECT_WIDTH / 2) - (CENTER_OFFSET/2),
                    STAGEY, true);
        }

        // ----------------------------------------
        // Draw rectangle around the gage
        // ----------------------------------------
        gc.setForeground(OUTLINE_COLOR);
        gc.drawRectangle(MINX, MINY, GAGE_WIDTH, aboveGageHeight
                + belowGageHeight);
        // ----------------------------------------
        // Draw the tick marks and labels on the below portion of gage
        int i = 0;
        while (i <= (int) totalIncBelow + totalIncAbove) {
            if (i % 5 == 0) {
                gc.drawString(df.format(i), MAXX + TICK_LENGTH + 4, MAXY
                        - ((PIXELS_PER_TICK * i) + getFontOffset(gc)), true);                
                gc.drawLine(MAXX, MAXY - (PIXELS_PER_TICK * i), MAXX + TICK_LENGTH + 3,
                        MAXY - (PIXELS_PER_TICK * i));
            } else {
                gc.drawLine(MAXX, MAXY - (PIXELS_PER_TICK * i), MAXX + 3,
                        MAXY - (PIXELS_PER_TICK * i));
            }
            
            i++;
        }

        // draw the stage line
        gc.setLineStyle(SWT.LINE_DOT);
        gc.drawLine(MINX, MIDY, MAXX, MIDY);
    }
}
