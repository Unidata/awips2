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
package com.raytheon.uf.viz.datadelivery.common.ui;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.datadelivery.subscription.AwipsCalendar;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryGUIUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * This is the subscription duration composite. This class is intended to be 
 * extended so common classes can be created and shared.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012   702      jpiatt     Initial creation.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class DurationComp extends Composite {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DurationComp.class);
    
    /** TableItem object. */
    private final Composite parentComp;
    
    /** The indefinite option radio button */
    private Button indefiniteChk;
    
    /** The start date text field */
    private Text startText;

    /** Subscription start date button */
    private Button startDateBtn;

    /** Subscription end date button */
    private Button endDateBtn;

    /** The end date text field */
    private Text endText;
    
    /** The starting date */
    private Date startDate;

    /** The ending date */
    private Date endDate;
    
    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public DurationComp(Composite parent) {
        super(parent, SWT.NONE);
        this.parentComp = parent;
        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        /*
         * Setup the layout for the composite
         */
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, true);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createDurationGroup();

    }
    
    /**
     * Create the Subscription Duration Group
     */
    private void createDurationGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        Group durationGrp = new Group(this, SWT.NONE);
        durationGrp.setLayout(gl);
        durationGrp.setLayoutData(gd);
        durationGrp.setText("  Subscription Duration  ");

        indefiniteChk = new Button(durationGrp, SWT.CHECK);
        indefiniteChk.setText("No Expiration Date");
        indefiniteChk.setSelection(true);
        indefiniteChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                boolean inDef = !indefiniteChk.getSelection();
                startText.setEnabled(inDef);
                startDateBtn.setEnabled(inDef);
                endText.setEnabled(inDef);
                endDateBtn.setEnabled(inDef);

            } 
        });

        int buttonWidth = 85;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);
        int textWidth = 100;
        GridData textData = new GridData(textWidth, SWT.DEFAULT);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite periodComp = new Composite(durationGrp, SWT.NONE);
        periodComp.setLayout(new GridLayout(3, false));
        periodComp.setLayoutData(gd);
        
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        
        Label startLbl = new Label(periodComp, SWT.LEFT);
        startLbl.setLayoutData(gd);
        startLbl.setText("Start Date: ");

        startText = new Text(periodComp, SWT.BORDER);
        startText.setLayoutData(textData);
        startText.setEnabled(false);
        startText.addFocusListener(new FocusAdapter() {

            @Override
            public void focusLost(FocusEvent e) {
                if (!DataDeliveryGUIUtils.validateDate(false, startText.getText())) {
                    DataDeliveryUtils.showMessage(getShell(), SWT.ERROR, "Invalid Date/Time",
                            "Invalid Starting Date/Time entered.\n\n" + "Please use the Select Date button\n"
                                    + "to select the date/time.");
                }
            }
        });

        startDateBtn = new Button(periodComp, SWT.PUSH);
        startDateBtn.setText("Select Date");
        startDateBtn.setLayoutData(btnData);
        startDateBtn.setEnabled(false);
        startDateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getStartDate();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        
        Label endLbl = new Label(periodComp, SWT.LEFT);
        endLbl.setLayoutData(gd);
        endLbl.setText("Expiration Date: ");

        textData = new GridData(textWidth, SWT.DEFAULT);
        endText = new Text(periodComp, SWT.BORDER);
        endText.setLayoutData(textData);
        endText.setEnabled(false);
        endText.addFocusListener(new FocusAdapter() {

            @Override
            public void focusLost(FocusEvent e) {
                if (!DataDeliveryGUIUtils.validateDate(false, endText.getText())) {
                    DataDeliveryUtils.showMessage(getShell(), SWT.ERROR, "Invalid Date/Time",
                            "Invalid End Date/Time entered.\n\n" + "Please use the Select Date button\n"
                                    + "to select the date/time.");
                }
            }
        });

        btnData = new GridData(buttonWidth, SWT.DEFAULT);
        endDateBtn = new Button(periodComp, SWT.PUSH);
        endDateBtn.setText("Select Date");
        endDateBtn.setLayoutData(btnData);
        endDateBtn.setEnabled(false);
        endDateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                getEndDate();
            }
        });
    }
    
    /**
     * Get the start date selected by the user
     */
    private void getStartDate() {
        Date d = getDate(startDate, true);
        if (d != null) {
            startDate = d;
            try {
                startText.setText(DataDeliveryGUIUtils.getSubscriptionFormat().format(startDate) + "Z");
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Invalid date format - format needs to be " +
                        "MM/dd/yyyy HH", e);
            }
        }

    }
    
    /**
     * Get the end date selected by the user
     */
    private void getEndDate() {
        Date d = getDate(endDate, true);
        if (d != null) {
            endDate = d;
            try {
                endText.setText(DataDeliveryGUIUtils.getSubscriptionFormat().format(endDate) + "Z");
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Invalid date format - format needs to be " +
                		"MM/dd/yyyy HH", e);
            }
        }
    }

    /**
     * Get the date from the Calendar widget
     * 
     * @return The selected Date
     */
    private Date getDate(Date date, boolean showHour) {

        if (date == null) {
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            date = cal.getTime();
        }

        AwipsCalendar ac = new AwipsCalendar(getShell(), date, showHour);
        Object obj = ac.open();

        if ((obj != null) && (obj instanceof Calendar)) {
            return ((Calendar)obj).getTime();
        }

        return null;
    }
    
    /**
     * Get the does not expire radio button selection.
     * 
     * @return boolean
     *            true if checked
     */
    public boolean isIndefiniteChk() {
		return indefiniteChk.getSelection();
	}

	/**
	 * Set the does not expire check box.
	 * 
	 * @param flag
	 */
    public void setNoExpiration(boolean flag) {
    	indefiniteChk.setSelection(flag);
    }

    /**
     * Get the start date text value.
     * 
     * @return start date
     */
    public String getStartText() {
        if (startText.isEnabled()) {
            return startText.getText().trim();
        } else {
            return null;
        }
        
    }
    
    /**
     * Enable or disable text boxes.
     * 
     * @param flag 
     */
    public void resetTextBoxes(boolean flag) {
        this.startText.setEnabled(flag);
        this.endText.setEnabled(flag);
    }

    /**
     * Get the end date text.
     * 
     * @return end date
     */
    public String getEndText() {
        if (endText.isEnabled()) {
            return endText.getText().trim();
        } else {
            return null;
        }
    }

    /**
     * Set the end date text box value.
     * 
     * @param endText
     */
    public void setEndText(String endText) {
        this.endText.setText(endText);
    }

    /**
     * Set the start btn.
     * 
     * @param flag
     */
    public void setStartBtnEnabled(boolean flag) {
        startDateBtn.setEnabled(flag); 
    }

    /**
     * Set the start btn.
     * 
     * @param flag
     */
    public void setEndBtnEnabled(boolean flag) {
        endDateBtn.setEnabled(flag); 
    }
	
	/**
     * Set the start date.
     * 
     * @param startDate
     */
    public void setStartDate(Date startDate) {
        startText.setText(DataDeliveryGUIUtils.getSubscriptionFormat().format(startDate) + "Z");
        startText.setEnabled(true);
        startDateBtn.setEnabled(true); 
        this.startDate = startDate;
    }

	/**
     * Set the end date.
     * 
     * @param endDate
     */	
	public void setEndDate(Date endDate) {
        endText.setText(DataDeliveryGUIUtils.getSubscriptionFormat().format(endDate) + "Z");
        endText.setEnabled(true);
        endDateBtn.setEnabled(true); 
        this.endDate = endDate;
	}

	/**
	 * Check if dates are valid. 
	 * @return true if dates are valid
	 */
	public boolean isValidChk() {
	    boolean datesValid = false;
	    boolean dateOrderValid = false;

	    if (!isIndefiniteChk()) {
	        boolean validateDur = DataDeliveryGUIUtils.validateDate(false, getStartText());
	        if (validateDur) {

	                validateDur = DataDeliveryGUIUtils.validateDate(false, getEndText());
	                if (validateDur) {
	                    datesValid = true;
	                    dateOrderValid = DataDeliveryGUIUtils.checkDateOrder(getStartText(), getEndText(), true);
	                }
	        }
	    }
	    else {
	        datesValid = true;
	        dateOrderValid = true;
	    }

	    // Display error message
	    if (!datesValid) {
	        DataDeliveryUtils.showMessage(parentComp.getShell(), SWT.ERROR, "Invalid Date/Time",
	                "Invalid Subscription Duration values entered.\n\n" + "Please use the Select Date button\n"
	                + "to select the date/time.");
	    }
	    else if (!dateOrderValid) {
	        DataDeliveryUtils.showMessage(parentComp.getShell(), SWT.ERROR, "Invalid Start/End Dates",
	                "Invalid Start or Expiration Duration Date entered.\n\n"
	                + "The expiration date is before the start date.");
	    }

	    return datesValid && dateOrderValid;
	}

}
