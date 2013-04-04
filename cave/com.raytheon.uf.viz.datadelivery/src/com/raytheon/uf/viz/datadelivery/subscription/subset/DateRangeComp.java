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
package com.raytheon.uf.viz.datadelivery.subscription.subset;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.viz.datadelivery.subscription.AwipsCalendar;
import com.raytheon.uf.viz.datadelivery.subscription.subset.xml.DateRangeTimeXML;

/**
 * Date Range Composite.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2012   223      mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class DateRangeComp extends Composite {
    /** Default group text. */
    private String groupText = " Date Range ";
    
    /** Get Latest Data check box */
    private Button latestDataChk;

    /** From button */
    private Button fromBtn;

    /** To button */
    private Button toBtn;

    /** Select From Date button */
    private Date fromDate;

    /** Select To Date button */
    private Date toDate;

    /** Flag for date range dirty. */
    private boolean dateRangeDirty = false;

    /** Date Format object */
    private final ThreadLocal<SimpleDateFormat> sdf = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sTemp = new SimpleDateFormat("MM/dd/yyyy");
            sTemp.setTimeZone(TimeZone.getTimeZone("GMT"));  
            return sTemp;
        }
        
    };    
    
    /**
     * Constructor. 
     * 
     * @param parent
     */
    public DateRangeComp(Composite parent) {
        this(parent, null);
    }
    
    /**
     * Constructor. 
     * 
     * @param parent
     * @param groupTxt
     */
    public DateRangeComp(Composite parent, String groupTxt) {
        super(parent, SWT.NONE);
        
        if (groupTxt != null) {
            this.groupText = groupTxt;
        }

        init();
    }

    /**
     * Initialize components
     */
    private void init() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        Group dateRangeGrp = new Group(this, SWT.NONE);
        dateRangeGrp.setText(groupText);
        dateRangeGrp.setLayout(gl);
        dateRangeGrp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        gl.horizontalSpacing = 2;
        gl.verticalSpacing = 2;
        gl.marginWidth = 2;
        gl.marginHeight = 2;

        latestDataChk = new Button(dateRangeGrp, SWT.CHECK);
        latestDataChk.setText("Get Latest Date");
        latestDataChk.setSelection(true);
        latestDataChk.setToolTipText("Use the latest time");
        latestDataChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setDateRangeDirty(true);
                fromBtn.setEnabled(!latestDataChk.getSelection());
                toBtn.setEnabled(!latestDataChk.getSelection());   
            }
        });

        gl = new GridLayout(4, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gl.horizontalSpacing = 2;
        gl.verticalSpacing = 2;
        gl.marginWidth = 2;
        gl.marginHeight = 2;
        Composite rangeComp = new Composite(dateRangeGrp, SWT.NONE);
        rangeComp.setLayout(gl);
        rangeComp.setLayoutData(gd);

        Label fromLbl = new Label(rangeComp, SWT.LEFT);
        fromLbl.setText("From: ");

        int width = 135;
        GridData btnData = new GridData(width, SWT.DEFAULT);
        fromBtn = new Button(rangeComp, SWT.PUSH);
        fromBtn.setLayoutData(btnData);
        fromBtn.setText("Select \"From\" Date");
        fromBtn.setEnabled(false);
        fromBtn.setToolTipText("Click to select begin date");
        fromBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                fromDate = getDate();
                if (fromDate != null) {
                    String fromDateStr = formatDate(fromDate);
                    if (fromDateStr != null) {
                        fromBtn.setText(fromDateStr);
                        if (toDate != null) {
                            if (fromDate.after(toDate)) {
                                toBtn.setText(formatDate(fromDate));
                            }
                        }
                    }
                    
                    setDateRangeDirty(true);
                }
            }
        });

        Label toLbl = new Label(rangeComp, SWT.LEFT);
        toLbl.setText("      To: ");

        btnData = new GridData(width, SWT.DEFAULT);
        toBtn = new Button(rangeComp, SWT.PUSH);
        toBtn.setLayoutData(btnData);
        toBtn.setEnabled(false);
        toBtn.setText("Select \"To\" Date");
        toBtn.setToolTipText("Click to select end date");
        toBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                toDate = getDate();
                if (toDate != null) {
                    String toDateStr = formatDate(toDate);
                    if (toDateStr != null) {
                        toBtn.setText(toDateStr);
                        toBtn.setData(toDate);
                    }

                    if (fromDate != null) {
                        if (toDate.before(fromDate)) {
                            fromBtn.setText(formatDate(toDate));
                        }
                    }
                    setDateRangeDirty(true);
                }
            }
        });
    }

    /**
     * Get the date from the Calendar widget
     * 
     * @return The selected Date
     */
    private Date getDate() {
        AwipsCalendar ac = new AwipsCalendar(this.getShell(), false);
        Object obj = ac.open();
        if ((obj != null) && (obj instanceof Calendar)) {
            return ((Calendar) obj).getTime();
        }

        return null;
    }

    private String formatDate(Date d) {
        if (d != null) {
            SimpleDateFormat format = sdf.get();
            String date = ((DateFormat) format).format(d);

            if (date != null) {
                return date;
            }
        }
        return null;
    }
    
    private Date parseDate(String date) throws ParseException {
        Date d = null; 
        if (date != null && !date.isEmpty()) {
            SimpleDateFormat format = sdf.get();
            d = format.parse(date);
        }
        
        return d;
    }
    
    public void populate(DateRangeTimeXML time) {
        latestDataChk.setSelection(time.isLatestData());
        
        if (time.getRangeStart() != null) {
            fromBtn.setText(time.getRangeStart());
        } else {
            fromBtn.setText("Select \"From\" Date");
        }

        if (time.getRangeEnd() != null) {
            toBtn.setText(time.getRangeEnd());
        } else {
            toBtn.setText("Select \"To\" Date");
        }
    }

    public DateRangeTimeXML getSaveInfo() {
        DateRangeTimeXML time = new DateRangeTimeXML();
        time.setLatestData(this.latestDataChk.getSelection());
        if (!fromBtn.getText().startsWith("Select")) {
            time.setRangeStart(fromBtn.getText());
        }
        if (!toBtn.getText().startsWith("Select")) {
            time.setRangeEnd(toBtn.getText());
        }
        
        return time;
    }
    
    /**
     * Set boolean to whether or not date selections have changed.
     * 
     * @param dateRangeDirty
     */
    public void setDateRangeDirty(boolean dateRangeDirty) {
        this.dateRangeDirty = dateRangeDirty;
    }

    /**
     * Get the boolean if any date selections have changed.
     * 
     * @return dateRangeDirty
     */
    public boolean isDateCycleDirty() {
        return dateRangeDirty;
    }
    
    public boolean isLatestDateChecked() {
        return latestDataChk.getSelection();
    }
    
    public void setLatestDateSelection(boolean selection) {
        this.latestDataChk.setSelection(selection);
    }

    /**
     * @return the fromDate
     */
    public Date getFromDate() {
        return fromDate;
    }

    /**
     * @param fromDate the fromDate to set
     */
    public void setFromDate(Date fromDate) {
        this.fromDate = fromDate;
    }

    /**
     * @return the toDate
     */
    public Date getToDate() {
        return toDate;
    }

    /**
     * @param toDate the toDate to set
     */
    public void setToDate(Date toDate) {
        this.toDate = toDate;
    }
    
    /**
     * Set the To Date.
     * 
     * @param dateStr
     * @throws ParseException
     */
    public void setToDate(String dateStr) throws ParseException {
        this.toDate = this.parseDate(dateStr);
    }
    
    /**
     * Set the From date.
     * 
     * @param dateStr
     * @throws ParseException
     */
    public void setFromDate(String dateStr) throws ParseException {
        this.fromDate = this.parseDate(dateStr);
    }
    
    public String getFromDateString() {
        String fromStr = null;
        if (fromDate != null) {
            SimpleDateFormat format = sdf.get();
            fromStr = format.format(fromDate);
        }
        
        return fromStr;
    }
    
    public String getToDateString() {
        String toStr = null;
        if (toDate != null) {
            SimpleDateFormat format = sdf.get();
            toStr = format.format(toDate);
        }
        
        return toStr;
    }
}
