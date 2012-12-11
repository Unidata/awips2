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
package com.raytheon.viz.ui.widgets;

import java.util.Calendar;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;

/**
 * Time of day entry field. Heavily borrowed from {@link DateTime}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 10, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TimeEntry extends Composite {

    private static Point[] fieldIndices = new Point[] { new Point(0, 2),
            new Point(3, 5), new Point(6, 8) };

    private static int[] fieldNames = new int[] { Calendar.HOUR_OF_DAY,
            Calendar.MINUTE, Calendar.SECOND };

    private static String[] formatStrings = new String[] { "%02d", "%02d:%02d",
            "%02d:%02d:%02d" };

    private static String[] computeSizeStrings = new String[] { "99", "99:99",
            "99:99:99" };

    private Text text;

    private Button up;

    private Button down;

    private int fieldCount;

    private int characterCount = 0;

    private int currentField = 0;

    private Calendar calendar;

    private boolean ignoreVerify;

    /**
     * Constructor
     * 
     * @param parent
     * @param fieldCount
     *            1 = HH, 2 = HH:MM 3 = HH:MM:SS
     * 
     */
    public TimeEntry(Composite parent, int fieldCount) {
        super(parent, SWT.NONE);
        if (fieldCount < 1 || fieldCount > 3) {
            throw new IllegalArgumentException("fieldCount must be 1, 2, or 3");
        }
        this.fieldCount = fieldCount;
        calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

        text = new Text(this, SWT.SINGLE | SWT.READ_ONLY | SWT.BORDER);
        Listener listener = new Listener() {
            @Override
            public void handleEvent(Event event) {
                switch (event.type) {
                case SWT.KeyDown:
                    onKeyDown(event);
                    break;
                case SWT.FocusIn:
                    onFocusIn(event);
                    break;
                case SWT.FocusOut:
                    onFocusOut(event);
                    break;
                case SWT.MouseDown:
                    onMouseClick(event);
                    break;
                case SWT.MouseUp:
                    onMouseClick(event);
                    break;
                case SWT.Verify:
                    onVerify(event);
                    break;
                }
            }
        };

        text.addListener(SWT.KeyDown, listener);
        text.addListener(SWT.FocusIn, listener);
        text.addListener(SWT.FocusOut, listener);
        text.addListener(SWT.MouseDown, listener);
        text.addListener(SWT.MouseUp, listener);
        text.addListener(SWT.Verify, listener);
        up = new Button(this, SWT.ARROW | SWT.UP);
        down = new Button(this, SWT.ARROW | SWT.DOWN);
        up.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                incrementField(+1);
                text.setFocus();
            }
        });
        down.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event event) {
                incrementField(-1);
                text.setFocus();
            }
        });
        addListener(SWT.Resize, new Listener() {
            @Override
            public void handleEvent(Event event) {
                onResize(event);
            }
        });
    }

    @Override
    public Point computeSize(int wHint, int hHint, boolean changed) {
        checkWidget();
        int width = 0, height = 0;
        if (wHint == SWT.DEFAULT || hHint == SWT.DEFAULT) {
            /* SWT.DATE and SWT.TIME */
            GC gc = new GC(text);
            Point textSize = gc
                    .stringExtent(computeSizeStrings[fieldCount - 1]);
            gc.dispose();
            Rectangle trim = text.computeTrim(0, 0, textSize.x, textSize.y);
            Point buttonSize = up
                    .computeSize(SWT.DEFAULT, SWT.DEFAULT, changed);
            width = trim.width + buttonSize.x;
            height = Math.max(trim.height, buttonSize.y);
        }
        if (width == 0) {
            width = SWT.DEFAULT;
        }
        if (height == 0) {
            height = SWT.DEFAULT;
        }
        if (wHint != SWT.DEFAULT) {
            width = wHint;
        }
        if (hHint != SWT.DEFAULT) {
            height = hHint;
        }
        int border = getBorderWidth();
        width += border * 2;
        height += border * 2;
        return new Point(width, height);
    }

    protected void incrementField(int amount) {
        int fieldName = fieldNames[currentField];
        int value = calendar.get(fieldName);
        setTextField(fieldName, value + amount, true);
    }

    void setTextField(int fieldName, int value, boolean commit) {
        if (commit) {
            int max = calendar.getActualMaximum(fieldName);
            int min = calendar.getActualMinimum(fieldName);
            if (value > max) {
                value = min; // wrap
            }
            if (value < min) {
                value = max; // wrap
            }
        }
        int start = fieldIndices[currentField].x;
        int end = fieldIndices[currentField].y;
        text.setSelection(start, end);
        String newValue = String.format("%02d", value);
        ignoreVerify = true;
        text.insert(newValue);
        ignoreVerify = false;
        selectField(currentField);
        if (commit) {
            setField(fieldName, value);
        }
    }

    void selectField(int index) {
        if (index != currentField) {
            commitCurrentField();
        }
        final int start = fieldIndices[index].x;
        final int end = fieldIndices[index].y;
        Point pt = text.getSelection();
        if (index == currentField && start == pt.x && end == pt.y) {
            return;
        }
        currentField = index;
        getDisplay().asyncExec(new Runnable() {
            @Override
            public void run() {
                if (!text.isDisposed()) {
                    String value = text.getText(start, end - 1);
                    int s = value.lastIndexOf(' ');
                    if (s == -1) {
                        s = start;
                    } else {
                        s = start + s + 1;
                    }
                    text.setSelection(s, end);
                }
            }
        });
    }

    void setField(int fieldName, int value) {
        if (calendar.get(fieldName) == value) {
            return;
        }
        calendar.set(fieldName, value);
        // TODO
        // sendSelectionEvent(SWT.Selection);
    }

    void commitCurrentField() {
        if (characterCount > 0) {
            characterCount = 0;
            int fieldName = fieldNames[currentField];
            int start = fieldIndices[currentField].x;
            int end = fieldIndices[currentField].y;
            String value = text.getText(start, end - 1);
            int s = value.lastIndexOf(' ');
            if (s != -1) {
                value = value.substring(s + 1);
            }
            int newValue = unformattedIntValue(fieldName, value,
                    calendar.getActualMaximum(fieldName));
            if (newValue != -1) {
                setTextField(fieldName, newValue, true);
            }
        }
    }

    int unformattedIntValue(int fieldName, String newText, int max) {
        int newValue;
        try {
            newValue = Integer.parseInt(newText);
        } catch (NumberFormatException ex) {
            return -1;
        }
        return newValue;
    }

    protected void onResize(Event event) {
        Rectangle rect = getClientArea();
        int width = rect.width;
        int height = rect.height;
        Point buttonSize = up.computeSize(SWT.DEFAULT, height);
        int buttonHeight = buttonSize.y / 2;
        text.setBounds(0, 0, width - buttonSize.x, height);
        up.setBounds(width - buttonSize.x, 0, buttonSize.x, buttonHeight);
        down.setBounds(width - buttonSize.x, buttonHeight, buttonSize.x,
                buttonHeight);
    }

    protected void onKeyDown(Event event) {
        int fieldName;
        switch (event.keyCode) {
        case SWT.ARROW_RIGHT:
            // a right arrow or a valid separator navigates to the field on the
            // right, with wraping
            selectField((currentField + 1) % fieldCount);
            break;
        case SWT.ARROW_LEFT:
            // navigate to the field on the left, with wrapping
            int index = currentField - 1;
            selectField(index < 0 ? fieldCount - 1 : index);
            break;
        case SWT.ARROW_UP:
        case SWT.KEYPAD_ADD:
            // set the value of the current field to value + 1, with wrapping
            commitCurrentField();
            incrementField(+1);
            break;
        case SWT.ARROW_DOWN:
        case SWT.KEYPAD_SUBTRACT:
            // set the value of the current field to value - 1, with wrapping
            commitCurrentField();
            incrementField(-1);
            break;
        case SWT.HOME:
            // set the value of the current field to its minimum
            fieldName = fieldNames[currentField];
            setTextField(fieldName, calendar.getActualMinimum(fieldName), true);
            break;
        case SWT.END:
            // set the value of the current field to its maximum
            fieldName = fieldNames[currentField];
            setTextField(fieldName, calendar.getActualMaximum(fieldName), true);
            break;
        case SWT.CR:
            // TODO
            // sendSelectionEvent(SWT.DefaultSelection);
            break;
        default:
            switch (event.character) {
            case '/':
            case ':':
            case '-':
            case '.':
                // a valid separator navigates to the field on the right, with
                // wrapping
                selectField((currentField + 1) % fieldCount);
                break;
            }
        }
    }

    protected void onFocusIn(Event event) {
        selectField(currentField);
    }

    protected void onFocusOut(Event event) {
        commitCurrentField();
    }

    protected void onMouseClick(Event event) {
        if (event.button != 1) {
            return;
        }
        Point sel = text.getSelection();
        for (int i = 0; i < fieldCount; i++) {
            if (fieldIndices[i].x <= sel.x && sel.x <= fieldIndices[i].y) {
                selectField(i);
                break;
            }
        }
    }

    protected void onVerify(Event event) {
        if (ignoreVerify) {
            return;
        }
        event.doit = false;
        int fieldName = fieldNames[currentField];
        int start = fieldIndices[currentField].x;
        int end = fieldIndices[currentField].y;
        int length = end - start;
        String newText = event.text;
        if (characterCount > 0) {
            try {
                Integer.parseInt(newText);
            } catch (NumberFormatException ex) {
                return;
            }
            String value = text.getText(start, end - 1);
            int s = value.lastIndexOf(' ');
            if (s != -1) {
                value = value.substring(s + 1);
            }
            newText = "" + value + newText;
        }
        int newTextLength = newText.length();
        boolean first = characterCount == 0;
        characterCount = (newTextLength < length) ? newTextLength : 0;
        int max = calendar.getActualMaximum(fieldName);
        int min = calendar.getActualMinimum(fieldName);
        int newValue = unformattedIntValue(fieldName, newText, max);
        if (newValue == -1) {
            characterCount = 0;
            return;
        }
        if (first && newValue == 0 && length > 1) {
            setTextField(fieldName, newValue, false);
        } else if (min <= newValue && newValue <= max) {
            setTextField(fieldName, newValue, characterCount == 0);
        } else {
            if (newTextLength >= length) {
                newText = newText.substring(newTextLength - length + 1);
                newValue = unformattedIntValue(fieldName, newText, max);
                if (newValue != -1) {
                    characterCount = length - 1;
                    if (min <= newValue && newValue <= max) {
                        setTextField(fieldName, newValue, characterCount == 0);
                    }
                }
            }
        }
    }

    private boolean isValidTime(int fieldName, int value) {
        int min = calendar.getActualMinimum(fieldName);
        int max = calendar.getActualMaximum(fieldName);
        return value >= min && value <= max;
    }

    String getFormattedString() {
        int h = calendar.get(Calendar.HOUR_OF_DAY);
        int m = calendar.get(Calendar.MINUTE);
        int s = calendar.get(Calendar.SECOND);

        return String.format(formatStrings[fieldCount - 1], h, m, s);
    }

    private void updateControl() {
        if (text != null) {
            String string = getFormattedString();
            ignoreVerify = true;
            text.setText(string);
            ignoreVerify = false;
        }
        redraw();
    }

    /**
     * Sets the receiver's hours.
     * <p>
     * Hours is an integer between 0 and 23.
     * </p>
     * 
     * @param hours
     *            an integer between 0 and 23
     * 
     * @exception SWTException
     *                <ul>
     *                <li>ERROR_WIDGET_DISPOSED - if the receiver has been
     *                disposed</li>
     *                <li>ERROR_THREAD_INVALID_ACCESS - if not called from the
     *                thread that created the receiver</li>
     *                </ul>
     */
    public void setHours(int hours) {
        checkWidget();
        if (!isValidTime(Calendar.HOUR_OF_DAY, hours)) {
            return;
        }
        calendar.set(Calendar.HOUR_OF_DAY, hours);
        updateControl();
    }

    /**
     * Sets the receiver's minutes.
     * <p>
     * Minutes is an integer between 0 and 59.
     * </p>
     * 
     * @param minutes
     *            an integer between 0 and 59
     * 
     * @exception SWTException
     *                <ul>
     *                <li>ERROR_WIDGET_DISPOSED - if the receiver has been
     *                disposed</li>
     *                <li>ERROR_THREAD_INVALID_ACCESS - if not called from the
     *                thread that created the receiver</li>
     *                </ul>
     */
    public void setMinutes(int minutes) {
        checkWidget();
        if (!isValidTime(Calendar.MINUTE, minutes)) {
            return;
        }
        calendar.set(Calendar.MINUTE, minutes);
        updateControl();
    }

    /**
     * Sets the receiver's seconds.
     * <p>
     * Seconds is an integer between 0 and 59.
     * </p>
     * 
     * @param seconds
     *            an integer between 0 and 59
     * 
     * @exception SWTException
     *                <ul>
     *                <li>ERROR_WIDGET_DISPOSED - if the receiver has been
     *                disposed</li>
     *                <li>ERROR_THREAD_INVALID_ACCESS - if not called from the
     *                thread that created the receiver</li>
     *                </ul>
     */
    public void setSeconds(int seconds) {
        checkWidget();
        if (!isValidTime(Calendar.SECOND, seconds)) {
            return;
        }
        calendar.set(Calendar.SECOND, seconds);
        updateControl();
    }

    /**
     * Sets the receiver's hours, minutes, and seconds in a single operation.
     * 
     * @param hours
     *            an integer between 0 and 23
     * @param minutes
     *            an integer between 0 and 59
     * @param seconds
     *            an integer between 0 and 59
     * 
     * @exception SWTException
     *                <ul>
     *                <li>ERROR_WIDGET_DISPOSED - if the receiver has been
     *                disposed</li>
     *                <li>ERROR_THREAD_INVALID_ACCESS - if not called from the
     *                thread that created the receiver</li>
     *                </ul>
     * 
     * @since 3.4
     */
    public void setTime(int hours, int minutes, int seconds) {
        checkWidget();
        if (!isValidTime(Calendar.HOUR_OF_DAY, hours)) {
            return;
        }
        if (!isValidTime(Calendar.MINUTE, minutes)) {
            return;
        }
        if (!isValidTime(Calendar.SECOND, seconds)) {
            return;
        }
        calendar.set(Calendar.HOUR_OF_DAY, hours);
        calendar.set(Calendar.MINUTE, minutes);
        calendar.set(Calendar.SECOND, seconds);
        updateControl();
    }

    /**
     * Returns the receiver's hours.
     * <p>
     * Hours is an integer between 0 and 23.
     * </p>
     * 
     * @return an integer between 0 and 23
     * 
     * @exception SWTException
     *                <ul>
     *                <li>ERROR_WIDGET_DISPOSED - if the receiver has been
     *                disposed</li>
     *                <li>ERROR_THREAD_INVALID_ACCESS - if not called from the
     *                thread that created the receiver</li>
     *                </ul>
     */
    public int getHours() {
        checkWidget();
        return calendar.get(Calendar.HOUR_OF_DAY);
    }

    /**
     * Returns the receiver's minutes.
     * <p>
     * Minutes is an integer between 0 and 59.
     * </p>
     * 
     * @return an integer between 0 and 59
     * 
     * @exception SWTException
     *                <ul>
     *                <li>ERROR_WIDGET_DISPOSED - if the receiver has been
     *                disposed</li>
     *                <li>ERROR_THREAD_INVALID_ACCESS - if not called from the
     *                thread that created the receiver</li>
     *                </ul>
     */
    public int getMinutes() {
        checkWidget();
        return calendar.get(Calendar.MINUTE);
    }

    /**
     * Returns the receiver's seconds.
     * <p>
     * Seconds is an integer between 0 and 59.
     * </p>
     * 
     * @return an integer between 0 and 59
     * 
     * @exception SWTException
     *                <ul>
     *                <li>ERROR_WIDGET_DISPOSED - if the receiver has been
     *                disposed</li>
     *                <li>ERROR_THREAD_INVALID_ACCESS - if not called from the
     *                thread that created the receiver</li>
     *                </ul>
     */
    public int getSeconds() {
        checkWidget();
        return calendar.get(Calendar.SECOND);
    }
}
