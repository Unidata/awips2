/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.fin
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
package com.raytheon.uf.viz.datadelivery.utils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;

/**
 * This is the data delivery utility class for GUIs. This class is intended to
 * be extended so common classes can be created and shared.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2012   702      jpiatt     Initial creation.
 * Sep 19, 2012   730      jpiatt     Added enums.
 * Oct  4, 2012  1245      jpiatt     Added pattern matching.
 * Dec 03, 2012  1269      mpduff     Added colors to subscription priorities.
 * Dec 12, 2012  1391      bgonzale   Added methods to manage when shells become busy for user interaction.
 * Dec 17, 2012  1435      mpduff     Fix ThreadLocal implementation.
 * Dec 18, 2012  1439      mpduff     Change Regex to match invalid chars.
 * Jan 04, 2013  1420      mpduff     Change default priority to normal priority.
 * Jan 25, 2013  1528      djohnson   Subscription priority has moved up in the world to the Subscription class.
 * May 17, 2013  1040      mpduff     Fixed JavaDoc and added tooltip.
 * May 20, 2013  2000      djohnson   Add methods for managing widget listeners.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class DataDeliveryGUIUtils {

    /** Status Handler */
    private final static IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataDeliveryGUIUtils.class);

    /** Tooltip */
    private static Shell tooltip;

    /** Subscription start/end date format */
    private final static ThreadLocal<SimpleDateFormat> subscriptionFormat = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sTemp = new SimpleDateFormat("MM/dd/yyyy HH");
            sTemp.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sTemp;
        }
    };

    /** Active period start/end date format */
    private final static ThreadLocal<SimpleDateFormat> activeFormat = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sTemp = new SimpleDateFormat("MM/dd");
            sTemp.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sTemp;
        }
    };

    /**
     * Regex pattern to match on any character except those listed.
     */
    public static final Pattern INVALID_CHAR_PATTERN = Pattern
            .compile("[^a-zA-Z0-9-]+");

    /** Pattern digit */
    public static final Pattern DIGIT_PATTERN = Pattern.compile("[0-9]+");

    /** Invalid Character Message Title */
    public static final String INVALID_CHARS_TITLE = "Invalid Characters";

    /** Invalid Character Message */
    public static final String INVALID_CHARS_MESSAGE = "Invalid characters.\nThe Subset Name may only contain letters/numbers/dashes.";

    /** Name Required Message Title */
    public static final String NAME_REQUIRED_TITLE = "Name Required";

    /** Name Required Message */
    public static final String NAME_REQUIRED_MESSAGE = "Name required.\nA Subscription Name must be entered.";

    /**
     * Constructor.
     */
    public DataDeliveryGUIUtils() {

    }

    /**
     * Validate the date fields
     * 
     * @param isRelative
     *            Is this a relative date
     * @param date
     *            The date
     * @return true if the date is valid
     */
    public static boolean validateDate(boolean isRelative, String date) {
        boolean valid = false;
        SimpleDateFormat formatToUse = (isRelative) ? subscriptionFormat.get()
                : activeFormat.get();

        try {

            if (date != null && date.length() > 0) {
                formatToUse.parse(date.trim());
                valid = true;
            }

        } catch (ParseException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid date format - Use the Select Date "
                            + "buttons for the correct format ", e);
        }

        return valid;
    }

    /**
     * Get the active start date.
     * 
     * @param selText
     *            selected text.
     * 
     * @return Date
     */
    public static Date getSelectedActDate(String selText) {
        Date d = null;

        try {
            d = activeFormat.get().parse(selText);
        } catch (ParseException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid date format - Format should be MM/dd", e);
        }

        return d;
    }

    /**
     * Get the subscription start date.
     * 
     * @param selText
     *            selected text
     * 
     * @return Date
     */
    public static Date getSelectedSubDate(String selText) {
        Date d = null;

        try {
            d = subscriptionFormat.get().parse(selText);
        } catch (ParseException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid date format - Format should be MM/dd/yyyy HH", e);
        }

        return d;
    }

    /**
     * Get the Subscription format.
     * 
     * @return subscriptionFormat
     */
    public static SimpleDateFormat getSubscriptionFormat() {
        return subscriptionFormat.get();
    }

    /**
     * Get the Active period format.
     * 
     * @return activeFormat
     */
    public static SimpleDateFormat getActiveFormat() {
        return activeFormat.get();
    }

    /**
     * Check the date ordering.
     * 
     * @param start
     *            The starting date as a string
     * @param end
     *            The ending date as a string
     * @param subscriptionDuration
     *            true if for subscription duration, false for active period
     * @return true if the starting date is before the ending date
     */
    public static boolean checkDateOrder(String start, String end,
            boolean subscriptionDuration) {
        boolean valid = false;

        SimpleDateFormat formatToUse = (subscriptionDuration) ? subscriptionFormat
                .get() : activeFormat.get();

        try {

            Date startDate = formatToUse.parse(start.trim());
            Date endDate = formatToUse.parse(end.trim());

            if (startDate.before(endDate) || startDate.equals(endDate)) {
                valid = true;
            }
        } catch (ParseException e) {
            valid = false;
        }

        return valid;
    }

    /**
     * Check text for null.
     * 
     * @param text
     *            The text to check
     * @return true if text not null
     */
    public static boolean hasText(final Text text) {
        if (text == null) {
            throw new NullPointerException(
                    "passed in text instance cannot be null!");
        }

        String stringValue = text.getText();
        return stringValue != null && !stringValue.trim().isEmpty();
    }

    /**
     * Display the settings have changed popup.
     * 
     * @param shell
     *            the shell
     * @return SWT.YES if yes, SWT.NO if no
     */
    public static int showSettingsHaveChangedPopup(final Shell shell) {
        return DataDeliveryUtils
                .showMessage(shell, SWT.YES | SWT.NO, "Save Changes?",
                        "Settings have changed.\nAre you sure you want to exit without saving?");
    }

    /**
     * Identify the shell as busy by disabling actions on it and setting the
     * mouse cursor over it to the wait cursor. To undo this call the
     * corresponding method markNotBusyInUIThread(Shell).
     * 
     * @see #DataDeliveryGUIUtils.markNotBusyInUIThread(Shell)
     * 
     * @param shell
     *            The Shell to mark busy
     */
    public static void markBusyInUIThread(final Shell shell) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (!shell.isDisposed()) {
                    shell.setEnabled(false);
                    shell.setCursor(shell.getDisplay().getSystemCursor(
                            SWT.CURSOR_WAIT));
                }
            }
        });
    }

    /**
     * Turns off busy status for this shell by re-enabling it and setting the
     * mouse cursor to normal. This call is the followup to
     * markBusyInUIThread(Shell).
     * 
     * This should be placed in a finally block so when exceptions occur this
     * code is still called.
     * 
     * @see #DataDeliveryGUIUtils.markBusyInUIThread(Shell)
     * 
     * @param shell
     *            the Shell used in markBusyInUIThread
     */
    public static void markNotBusyInUIThread(final Shell shell) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (!shell.isDisposed()) {
                    shell.setEnabled(true);
                    shell.setCursor(null);
                }
            }
        });
    }

    /**
     * Check the user's latency value.
     * 
     * @param latency
     *            the latency value
     * @param maxLatency
     *            the max latency value
     * 
     * @return true if valid
     */
    public static boolean latencyValidChk(int latency, int maxLatency) {
        if (latency > -1 && latency <= maxLatency) {
            return true;
        }

        return false;
    }

    /**
     * Remove all listeners of the specified types from the widget.
     * 
     * @param widget
     *            the widget
     * @param listenerTypes
     *            the listener types
     */
    public static void removeListeners(Widget widget, int... listenerTypes) {
        // Remove any current listeners
        for (int listenerType : listenerTypes) {
            Listener[] listeners = widget.getListeners(listenerType);
            for (Listener listener : listeners) {
                widget.removeListener(listenerType, listener);
            }
        }
    }

    /**
     * Creates a selection listener that will run the specified runnable if the
     * current value does not equal the initial value.
     * 
     * @param initialValue
     *            the initial value
     * @param spinner
     *            the spinner
     * @param runnable
     *            the runnable to run
     * @return the selection listener
     */
    public static SelectionListener addValueChangedSelectionListener(
            final int initialValue, final Spinner spinner,
            final Runnable runnable) {
        return new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (spinner.getSelection() != initialValue) {
                    runnable.run();
                }
            }
        };
    }

    /**
     * Creates a selection listener that will run the specified runnable if the
     * current value does not equal the initial value.
     * 
     * @param initialSelectionIndex
     *            the initial selection index
     * @param combo
     *            the combo
     * @param runnable
     *            the runnable to run
     * @return
     */
    public static SelectionListener addValueChangedSelectionListener(
            final int initialSelectionIndex, final Combo combo,
            final Runnable runnable) {
        return new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (combo.getSelectionIndex() != initialSelectionIndex) {
                    runnable.run();
                }
            }
        };
    }

    /**
     * Get a tooltip display. Caller is responsible for disposing the tooltip by
     * calling {@link #hideToolTip}.
     * 
     * @param parent
     *            the parent shell
     * @param x
     *            the x location
     * @param y
     *            the y location
     * @param text
     *            the text
     */
    public static void showTooltip(Shell parent, int x, int y, String text) {
        if (tooltip != null && !tooltip.isDisposed()) {
            tooltip.dispose();
        }
        tooltip = new Shell(parent, SWT.TOOL | SWT.ON_TOP);
        tooltip.setLayout(new GridLayout());

        tooltip.setBackground(tooltip.getDisplay().getSystemColor(
                SWT.COLOR_INFO_BACKGROUND));
        tooltip.setBackgroundMode(SWT.INHERIT_FORCE);

        Label lbContent = new Label(tooltip, SWT.NONE);
        lbContent.setText(text);

        Point lbContentSize = lbContent.computeSize(SWT.DEFAULT, SWT.DEFAULT);

        int width = lbContentSize.x + 10;
        int height = lbContentSize.y + 10;

        tooltip.setBounds(x, y, width, height);
        tooltip.setVisible(true);
    }

    /**
     * Dispose the tooltip created by {@link #showTooltip}
     */
    public static void hideToolTip() {
        if (tooltip != null && !tooltip.isDisposed()) {
            tooltip.dispose();
        }
    }
}
