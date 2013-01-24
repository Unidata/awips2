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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.ui.widgets.DateTimeEntry.IUpdateListener;

/**
 * TimeRange entry widget with read-only text and button to pop up an editable
 * dialog to enter the start and end date and time. End time must always be
 * greater than or equal the start time.
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

public class TimeRangeEntry extends Composite {

    private DateTimeEntry start;

    private DateTimeEntry end;

    public TimeRangeEntry(Composite parent, int style) {
        super(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, true);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        layout.horizontalSpacing = 0;
        if ((style & SWT.HORIZONTAL) != 0) {
            layout.numColumns = 2;
        }
        setLayout(layout);

        start = new DateTimeEntry(this);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        start.setLayoutData(layoutData);

        end = new DateTimeEntry(this);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        end.setLayoutData(layoutData);

        start.addUpdateListener(new IUpdateListener() {
            @Override
            public void dateTimeUpdated(Date date) {
                if (end.getDate().before(date)) {
                    end.setDate(date);
                }
            }
        });

        end.addUpdateListener(new IUpdateListener() {
            @Override
            public void dateTimeUpdated(Date date) {
                if (date.before(start.getDate())) {
                    start.setDate(date);
                }
            }
        });
    }

    /**
     * Sets the date format string used to format the date/time strings for
     * display
     * 
     * @param dateFormat
     *            the date format string see {@link SimpleDateFormat} for more
     *            information.
     */
    public void setDateFormat(String dateFormat) {
        start.setDateFormat(dateFormat);
        end.setDateFormat(dateFormat);
    }

    /**
     * Sets the time zone used to format the date/time strings for display
     * 
     * @param tz
     *            the time zone
     */
    public void setTimeZone(TimeZone tz) {
        start.setTimeZone(tz);
        end.setTimeZone(tz);
    }

    /**
     * Sets the contents of the receiver to the given time range
     * 
     * @param timeRange
     *            the time range
     */
    public void setTimeRange(TimeRange timeRange) {
        this.start.setDate(timeRange.getStart());
        if (timeRange.getEnd().before(timeRange.getStart())) {
            end.setDate(timeRange.getStart());
        } else {
            end.setDate(timeRange.getEnd());
        }
    }

    /**
     * Get the time range
     * 
     * @return the time range
     */
    public TimeRange getTimeRange() {
        Date startDate = start.getDate();
        Date endDate = end.getDate();

        return new TimeRange(startDate, endDate);
    }

    @Override
    public void setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        for (Control c : getChildren()) {
            c.setEnabled(enabled);
        }
    }
}
