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

package com.raytheon.viz.gfe.gridmanager;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.msgs.IGlobalSelectionTRChangedListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.msgs.SelectTimeRangesChangedMsg;
import com.raytheon.viz.gfe.preferences.TimeScaleDisplayedPeriodsPreference;
import com.raytheon.viz.gfe.rsc.GFEFonts;
import com.raytheon.viz.gfe.ui.SelectTRMenu;

/**
 * Displays the time range.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/19/2008              dfitch      Initial creation.
 * Apr 7, 2009       #2212 randerso    Reimplemented
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */

public class TimeScale extends Canvas implements IMessageClient, Runnable {
    private static final int MINOR_TICK = 5;

    private static final int MAJOR_TICK = 10;

    public static final int HEIGHT = 60;

    // Font height in pixels, before config adjustment
    private static final int PERIOD_FONT_HEIGHT = 14;

    // Hour font height in pixels, before config adjustment
    private static final int HOUR_FONT_HEIGHT = 18;

    private Color CurrentSystemTime_color;

    private static final SimpleDateFormat[] dateFormat;
    static {
        dateFormat = new SimpleDateFormat[] { //
        new SimpleDateFormat("MMM dd (EEE)"), //
                new SimpleDateFormat("MMM dd"), //
                new SimpleDateFormat("M/d"), //
                new SimpleDateFormat("d"), //
        };
        for (SimpleDateFormat sdf : dateFormat) {
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        }
    }

    private final GridManager gridManager;

    private String[] displayedPeriods;

    private IPropertyChangeListener periodListener;

    private IGlobalSelectionTRChangedListener globalTRListener;

    private int periodFontNum;

    private int hourFontNum;

    private Font periodFont;

    private Font hourFont;

    private MenuManager menuMgr;

    @SuppressWarnings("unchecked")
    TimeScale(final Composite aParent, final GridManager aGridManager) {
        super(aParent, SWT.NONE);
        gridManager = aGridManager;

        displayedPeriods = TimeScaleDisplayedPeriodsPreference
                .getTimeScaleDisplayedPeriods();
        Arrays.sort(displayedPeriods);

        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();
        String color;
        if ((color = prefs.getString("CurrentSystemTime_color")).isEmpty()) {
            color = "Green";
        }
        CurrentSystemTime_color = new Color(Display.getDefault(),
                RGBColors.getRGBColor(color));

        periodListener = new IPropertyChangeListener() {

            @Override
            public void propertyChange(PropertyChangeEvent event) {
                if (event.getProperty().equals(
                        TimeScaleDisplayedPeriodsPreference.getPreferenceKey())) {
                    displayedPeriods = TimeScaleDisplayedPeriodsPreference
                            .getTimeScaleDisplayedPeriods();
                    Arrays.sort(displayedPeriods);
                    refresh();
                }
            }
        };

        Activator.getDefault().getPreferenceStore()
                .addPropertyChangeListener(periodListener);

        hourFontNum = 2;
        if (GFEPreference.contains("TimeScale_font")) {
            hourFontNum = GFEPreference.getIntPreference("TimeScale_font");
        }
        periodFontNum = Math.max(hourFontNum - 1, 0);
        hourFont = GFEFonts.getFont(getDisplay(), hourFontNum);
        periodFont = GFEFonts.getFont(getDisplay(), periodFontNum);

        globalTRListener = new IGlobalSelectionTRChangedListener() {

            @Override
            public void globalSelectionTRChanged(TimeRange globalSelectionTR) {
                refresh();
            }

        };
        gridManager.getDataManager().getSpatialDisplayManager()
                .addGlobalSelectionTRChangedListener(globalTRListener);

        addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {

                if (periodFont != null) {
                    periodFont.dispose();
                    periodFont = null;
                }

                if (hourFont != null) {
                    hourFont.dispose();
                    hourFont = null;
                }

                if (CurrentSystemTime_color != null) {
                    CurrentSystemTime_color.dispose();
                    CurrentSystemTime_color = null;
                }

                Activator.getDefault().getPreferenceStore()
                        .removePropertyChangeListener(periodListener);

                gridManager
                        .getDataManager()
                        .getSpatialDisplayManager()
                        .removeGlobalSelectionTRChangedListener(
                                globalTRListener);

                Message.unregisterInterest(TimeScale.this,
                        SelectTimeRangesChangedMsg.class);
            }

        });

        addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                paint(e);
            }
        });

        MouseHandler mouseHandler = new MouseHandler() {

            @Override
            public void mouseClick(MouseEvent e) {
                super.mouseClick(e);

                if (e.stateMask == SWT.BUTTON1) {
                    gridManager.setSelectedTime(gridManager.getUtil()
                            .pixelToDate(e.x));
                } else if (e.stateMask == SWT.BUTTON2) {
                    TimeRange tr = gridManager.getUtil().pixelToHour(e.x);
                    gridManager.getDataManager().getParmOp()
                            .setSelectionTimeRange(tr);
                }
            }

            /*
             * (non-Javadoc)
             * 
             * @see
             * com.raytheon.viz.gfe.gridmanager.MouseHandler#displayContextMenu
             * (org.eclipse.swt.events.MouseEvent)
             */
            @Override
            public void displayContextMenu(MouseEvent e)
                    throws GFEServerException {
                super.displayContextMenu(e);

                if (menuMgr != null) {
                    menuMgr.dispose();
                }
                menuMgr = new MenuManager("#PopupMenu");

                // Select Grids By Time
                MenuManager subMenu = new MenuManager("Select Grids By Time");
                subMenu.add(new SelectTRMenu());
                menuMgr.add(subMenu);

                // Select All Weather Elements
                menuMgr.add(new CommandContributionItem(
                        new CommandContributionItemParameter(
                                PlatformUI.getWorkbench(),
                                null,
                                "com.raytheon.viz.gfe.actions.SelectAllWeatherElements",
                                null, null, null, null,
                                "Select All Weather Elements", null, null,
                                CommandContributionItem.STYLE_PUSH, null, true)));

                // Deselect All
                menuMgr.add(new CommandContributionItem(
                        new CommandContributionItemParameter(PlatformUI
                                .getWorkbench(), null,
                                "com.raytheon.viz.gfe.actions.ClearSelection",
                                null, null, null, null, "Deselect All", null,
                                null, CommandContributionItem.STYLE_PUSH, null,
                                true)));

                Menu menu = menuMgr.createContextMenu(TimeScale.this);
                menu.setVisible(true);
                setMenu(menu);
            }
        };
        addMouseListener(mouseHandler);

        Message.registerInterest(this, SelectTimeRangesChangedMsg.class);
    }

    protected void paint(PaintEvent event) {
        Rectangle rect = getClientArea();
        gridManager.getUtil().paintBackground(event, rect);
        gridManager.getUtil().paintTimeScaleLines(event, rect);

        Rectangle selection = gridManager
                .getUtil()
                .timeRangeToPixels(
                        gridManager.getDataManager().getSpatialDisplayManager()
                                .getGlobalTimeRange()).intersection(rect);
        gridManager.getUtil().paintSelectionTimeRange(event, selection);

        paintTimeNow(event, rect);
        paintTicks(event, rect);
        paintWhiteLines(event, rect);
        paintPeriods(event, rect);

        gridManager.getUtil().paintSelected(event, rect);
    }

    private void paintPeriods(PaintEvent event, Rectangle bounds) {
        String[] periods = gridManager.getDataManager()
                .getSelectTimeRangeManager().inventory();

        GC gc = event.gc;
        Font origFont = gc.getFont();
        gc.setFont(periodFont);

        TimeRange visibleRange = gridManager.getVisibleTimeRange();

        for (String name : periods) {

            // if period is selected for display
            if (Arrays.binarySearch(displayedPeriods, name) >= 0) {
                TimeRange timeRange = gridManager.getDataManager()
                        .getSelectTimeRangeManager().getRange(name)
                        .toTimeRange();

                // if period overlaps the visible range
                if (visibleRange.overlaps(timeRange)) {
                    paintPeriod(event, timeRange, name, bounds);
                }
            }

        }
        gc.setFont(origFont);
    }

    private void paintPeriod(PaintEvent event, TimeRange timeRange,
            String label, Rectangle bounds) {
        GC gc = event.gc;
        // figure out the screen coordinate of the start/end times of the
        // input time range.
        Rectangle rect = gridManager.getUtil().timeRangeToPixels(timeRange);

        int x1 = rect.x;
        int x2 = x1 + rect.width;
        int y1 = bounds.y;
        int y2 = y1 + PERIOD_FONT_HEIGHT;
        int d = PERIOD_FONT_HEIGHT / 2;

        // paint the two tick marks
        gc.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_WHITE));
        gc.setLineStyle(SWT.LINE_SOLID);
        gc.drawLine(x1, y1, x1, y2);
        gc.drawLine(x2, y1, x2, y2);

        // label length in pixels
        Point labelSize = gc.stringExtent(label);

        // special case for left/right ends of the screen
        int screenWidth = bounds.x + bounds.width;
        int labelXLoc, availableWidth;
        if (x1 < bounds.x) {
            labelXLoc = x2 / 2;
            availableWidth = x2 - 2;
        } else if (x2 > screenWidth) {
            labelXLoc = (screenWidth + x1) / 2;
            availableWidth = screenWidth - x1 - 2;
        } else {
            labelXLoc = (x1 + x2) / 2;
            availableWidth = x2 - x1 - 2;
        }

        // if room, then paint label, and then leader lines
        gc.setLineStyle(SWT.LINE_DOT);
        if (availableWidth >= labelSize.x) {
            gc.drawString(label, labelXLoc - labelSize.x / 2, y1, true);

            // left line and arrow
            gc.drawLine(x1, y1 + d, labelXLoc - labelSize.x / 2, y1 + d);
            gc.drawLine(x1, y1 + d, x1 + d, y1);
            gc.drawLine(x1, y1 + d, x1 + d, y2);

            // right line and arrow
            gc.drawLine(x2, y1 + d, labelXLoc + labelSize.x / 2, y1 + d);
            gc.drawLine(x2, y1 + d, x2 - d, y1);
            gc.drawLine(x2, y1 + d, x2 - d, y2);
        }

    }

    /**
     * Paints the white tick marks for the hours on the TimeScale
     * 
     * @param gc
     */
    public void paintTicks(PaintEvent event, Rectangle rect) {
        GC gc = event.gc;

        TimeRange tr = gridManager.getVisibleTimeRange();
        Date startTime = tr.getStart();
        Date stopTime = tr.getEnd();

        Calendar currentTickCal = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        currentTickCal.setTime(startTime);
        currentTickCal.set(Calendar.MINUTE, 0);
        currentTickCal.set(Calendar.SECOND, 0);
        currentTickCal.set(Calendar.MILLISECOND, 0);
        currentTickCal.add(Calendar.HOUR_OF_DAY, 1);
        Date currentTickDate = currentTickCal.getTime();

        int top = rect.y;
        int bottom = top + rect.height;

        Font origFont = gc.getFont();
        gc.setFont(hourFont);

        int pixelsPerDay = gridManager.getUtil().durationToPixels(
                GridManagerUtil.MILLIS_PER_HOUR * 24);
        int xMax = getClientArea().width;

        gc.setLineWidth(0);
        gc.setLineStyle(SWT.LINE_SOLID);
        gc.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_WHITE));

        int hour = currentTickCal.get(Calendar.HOUR_OF_DAY);
        if (hour > 0) {
            int width = gridManager.getUtil().durationToPixels(
                    (24 - hour) * GridManagerUtil.MILLIS_PER_HOUR);
            paintDate(gc, getClientArea().x, top + PERIOD_FONT_HEIGHT, width,
                    currentTickDate);
        }

        while (currentTickCal.getTime().before(stopTime)) {
            int x = gridManager.getUtil().dateToPixel(currentTickDate);

            hour = currentTickCal.get(Calendar.HOUR_OF_DAY);

            if (hour == 0) {
                // Draw the day lines
                gc.drawLine(x, top + PERIOD_FONT_HEIGHT, x, bottom);
                int width = Math.min(xMax - x, pixelsPerDay);
                paintDate(gc, x, top + PERIOD_FONT_HEIGHT, width,
                        currentTickDate);
            } else if (hour % gridManager.getUtil().getHourIncrement() == 0) {
                // draw major tick
                gc.drawLine(x, bottom - MAJOR_TICK, x, bottom);

                String s = String.format("%02d", hour);
                Point p = gc.stringExtent(s);
                gc.drawString(s, x - p.x / 2, top + PERIOD_FONT_HEIGHT
                        + HOUR_FONT_HEIGHT, true);
            } else {
                // draw minor tick
                gc.drawLine(x, bottom - MINOR_TICK, x, bottom);
            }

            currentTickCal.add(Calendar.HOUR_OF_DAY, 1);
            currentTickDate = currentTickCal.getTime();
        }

        gc.setFont(origFont);
    }

    /**
     * Paints the horizontal white lines
     * 
     * @param gc
     */
    public void paintWhiteLines(PaintEvent event, Rectangle bounds) {
        GC gc = event.gc;

        gc.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_WHITE));
        gc.setLineStyle(SWT.LINE_SOLID);
        gc.setLineWidth(0);
        int y = bounds.y + PERIOD_FONT_HEIGHT;
        gc.drawLine(bounds.x, y, bounds.x + bounds.width, y);

        y += HOUR_FONT_HEIGHT;
        gc.drawLine(bounds.x, y, bounds.x + bounds.width, y);

    }

    private void paintDate(GC gc, int x, int y, int width, Date date) {
        Point p;
        int i = 0;
        String s;
        do {
            s = dateFormat[i++].format(date);
            p = gc.stringExtent(s);
        } while (i < dateFormat.length && p.x > width);

        if (p.x <= width) {
            gc.drawString(s, x + (width - p.x) / 2, y, true);
        }
    }

    public void paintTimeNow(PaintEvent event, Rectangle rect) {
        GC gc = event.gc;

        Date now = SimulatedTime.getSystemTime().getTime();
        int x = gridManager.getUtil().dateToPixel(now);
        int y = PERIOD_FONT_HEIGHT - 2;
        gc.setLineStyle(SWT.LINE_SOLID);
        gc.setLineWidth(0);
        gc.setForeground(CurrentSystemTime_color);
        gc.drawPolyline(new int[] { x, 0, x, y, x - y / 2, 0, x + y / 2, 0, x,
                y });
    }

    /**
     * 
     */
    private void refresh() {
        if (this.getDisplay().getThread() == Thread.currentThread()) {
            run();
        } else {
            VizApp.runAsync(this);
        }
    }

    @Override
    public void run() {
        if (!isDisposed()) {
            redraw();
        }
    }

    @Override
    public void receiveMessage(Message message) {
        if (message instanceof SelectTimeRangesChangedMsg) {
            refresh();
        }
    }

}
