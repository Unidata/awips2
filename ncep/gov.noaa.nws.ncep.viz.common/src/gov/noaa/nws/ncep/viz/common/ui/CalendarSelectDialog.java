package gov.noaa.nws.ncep.viz.common.ui;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.time.DataTime;


/**
 * General purpose Dialog for selecting a date/time. 
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    02/16/11       #408       Greg Hull   Created.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class CalendarSelectDialog extends Dialog {

	private Display display = null;

	private Shell shell = null;
	
	private Calendar initCal; // use mainly to save the time zone
	
	private Button[] dayBtns = new Button[42];
	
	private Integer seldDay = null;
	private Integer seldMonth = null;
	private Integer seldYear = null;
	private Integer seldHour = null;
	private Integer seldMin  = null;
	
	private Text  seldDateTxt = null;

	private DataTime seldDataTime = null;


	public CalendarSelectDialog(Shell parent, int style) {
		super(parent, style);
	}

	public CalendarSelectDialog(Shell parent) {
		this(parent, 0);
	}

	// calMonth is 0-based
	private int getLastDayOfMonth(int year, int calMonth) {
		int month = calMonth+1;
		
		if( month == 1 || month == 3 || month == 5 || month == 7 ||
			month == 8 ||month == 10 || month == 12 ) {
			return 31;
		}
		else if( month == 4 || month == 6 || month == 9 || month == 11) {
			return 30;
		}
		else if( month == 2) {
			if (isLeapYear(year)) {
				return 29;
			} else {
				return 28;
			}
		}
		return 0;
	}

	public boolean isLeapYear(int year) {
		return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
	}

	public DataTime open() {
		Calendar current = Calendar.getInstance();
		current.setTimeZone( TimeZone.getTimeZone( "GMT") );
		
		return open( new DataTime( new Date( current.getTimeInMillis() ) ) );
	}

	// return the selected DataTime or null on Cancel.
	public DataTime open( DataTime initDateTime ) {

		if( initDateTime == null ) {
			initCal = Calendar.getInstance();
			initCal.setTimeZone( TimeZone.getTimeZone( "GMT") );
		}
		else {
			initCal = initDateTime.getValidTime();		
		}
		
		seldYear = initCal.get( Calendar.YEAR );
		seldMonth= initCal.get( Calendar.MONTH );
		seldDay  = initCal.get( Calendar.DAY_OF_MONTH );
		seldHour = initCal.get( Calendar.HOUR_OF_DAY );
		seldMin  = initCal.get( Calendar.MINUTE );

		Shell parent = getParent();
		
		display = Display.getDefault();
		shell = new Shell(parent,  SWT.TITLE |SWT.PRIMARY_MODAL);
		shell.setText("Select Date/Time ("+ initCal.getTimeZone().getDisplayName(false, TimeZone.SHORT) +")" );
//		shell.setSize(300, 401);

		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 1;
		shell.setLayout(gridLayout);
		
		Composite yrMonComp = new Composite( shell, SWT.NONE );
		yrMonComp.setLayoutData( new GridData(GridData.FILL_HORIZONTAL ));
		yrMonComp.setLayout( new FormLayout() );
		
		
		Combo monCombo = new Combo( yrMonComp, SWT.DROP_DOWN | SWT.READ_ONLY );
		GridData  gridData = new GridData( GridData.CENTER);
		FormData fd = new FormData();
    	fd.top = new FormAttachment( 0, 10 );
    	fd.left = new FormAttachment( 0, 45 );
		monCombo.setLayoutData(fd);

		monCombo.setItems( new String[] { 
	             "Jan", "Feb", "Mar", "Apr", "May", "Jun",
	             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" } );
		monCombo.select( seldMonth );

		monCombo.addSelectionListener(new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				seldMonth = ((Combo)e.widget).getSelectionIndex();
   				updateDayOfMonthBtns();
   				updateSelectedDate();
   			}
		});

		Spinner yrSpnr = new Spinner( yrMonComp, SWT.BORDER | SWT.READ_ONLY); 
		yrSpnr.setMinimum(1950);
		yrSpnr.setMaximum(2050); 
		yrSpnr.setDigits(0);
		yrSpnr.setTextLimit(4);
		fd = new FormData();
    	fd.top = new FormAttachment( 0, 10 );
    	fd.right = new FormAttachment( 100, -45 );
    	yrSpnr.setLayoutData(fd);
		
		yrSpnr.setSelection( seldYear );

		yrSpnr.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				seldYear = ((Spinner)e.widget).getSelection();
   				updateDayOfMonthBtns();
   				updateSelectedDate();   				
   			}
		});

		Composite daysComp = new Composite( shell, SWT.NONE );
		gridLayout = new GridLayout(7, true);
		gridLayout.numColumns = 7;
		daysComp.setLayout(gridLayout);
		gridData = new GridData(GridData.FILL_HORIZONTAL);
		daysComp.setLayoutData(gridData);

		Label sunday = new Label(daysComp, SWT.None );
		gridData = new GridData(GridData.FILL_HORIZONTAL |
				GridData.FILL_VERTICAL);
		gridData.widthHint = 30;
		gridData.heightHint = 20;
		sunday.setLayoutData(gridData);
		sunday.setText("Sun");

		Label monday = new Label(daysComp, SWT.None );
		gridData = new GridData(GridData.FILL_HORIZONTAL |
				GridData.FILL_VERTICAL);
		gridData.widthHint = 30;
		gridData.heightHint = 20;
		monday.setLayoutData(gridData);
		monday.setText("Mon");

		Label tuesday = new Label(daysComp, SWT.None );
		gridData = new GridData(GridData.FILL_HORIZONTAL |
				GridData.FILL_VERTICAL);
		gridData.widthHint = 30;
		gridData.heightHint = 20;
		tuesday.setLayoutData(gridData);
		tuesday.setText("Tue");

		Label wednesday = new Label(daysComp, SWT.None );
		gridData = new GridData(GridData.FILL_HORIZONTAL |
				GridData.FILL_VERTICAL);
		gridData.widthHint = 30;
		gridData.heightHint = 20;
		wednesday.setLayoutData(gridData);
		wednesday.setText("Wed");

		Label thursday = new Label(daysComp, SWT.None );
		gridData = new GridData(GridData.FILL_HORIZONTAL |
				GridData.FILL_VERTICAL);
		gridData.widthHint = 30;
		gridData.heightHint = 20;
		thursday.setLayoutData(gridData);
		thursday.setText("Thu");

		Label friday = new Label(daysComp, SWT.None );
		gridData = new GridData(GridData.FILL_HORIZONTAL |
				GridData.FILL_VERTICAL);
		gridData.widthHint = 30;
		gridData.heightHint = 20;
		friday.setLayoutData(gridData);
		friday.setText("Fri");

		Label saturday = new Label(daysComp, SWT.None );
		gridData = new GridData(GridData.FILL_HORIZONTAL |
				GridData.FILL_VERTICAL);
		gridData.widthHint = 30;
		gridData.heightHint = 20;
		saturday.setLayoutData(gridData);
		saturday.setText("Sat");

		for (int i = 0; i < 42; i++) {
			dayBtns[i] = new Button( daysComp, SWT.TOGGLE );
			dayBtns[i].setSize(30, 22);
			gridData = new GridData(GridData.FILL_HORIZONTAL |
					GridData.FILL_VERTICAL);
			dayBtns[i].setLayoutData(gridData);
			
			dayBtns[i].addSelectionListener( new SelectionAdapter() {
	    		public void widgetSelected(SelectionEvent e) {
	    			String dayStr = ((Button)e.widget).getText();
	    			
	    			if( !dayStr.equals("") ) {	    				
	    				seldDay = Integer.parseInt( dayStr );
	    				updateSelectedDate( );
	    			}
	    			for( int i=0 ; i<42 ; i++ ) {
	    				dayBtns[i].setSelection( false );
	    			}
	    			((Button)e.widget).setSelection(true);
	    		}
			});
		}

		// next add the next/prev month and year buttons
		Composite minHrsComp = new Composite( shell, SWT.NONE );
		gridData = new GridData( GridData.FILL_BOTH );
		minHrsComp.setLayoutData( gridData );

		minHrsComp.setLayout( new FormLayout() );

    	
		Spinner hrsSpnr = new Spinner( minHrsComp, SWT.BORDER | SWT.READ_ONLY); 
		fd = new FormData();
    	fd.top = new FormAttachment( 0, 0 );
    	fd.left = new FormAttachment( 17, 0 );
		hrsSpnr.setLayoutData(fd);
		
		hrsSpnr.setMinimum(0);
		hrsSpnr.setMaximum(23); 
		hrsSpnr.setDigits(0);
		hrsSpnr.setTextLimit(2);
		
		hrsSpnr.setSelection( seldHour );
		
		hrsSpnr.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				seldHour = ((Spinner)e.widget).getSelection();
   				updateSelectedDate();   				
   			}
		});

		Label hrsLbl = new Label( minHrsComp, SWT.NONE );
		hrsLbl.setText("Hours");
		fd = new FormData();
    	fd.top = new FormAttachment( hrsSpnr, 3, SWT.TOP );
    	fd.left = new FormAttachment( hrsSpnr, 3, SWT.RIGHT );
    	hrsLbl.setLayoutData(fd);
		
		Spinner minsSpnr = new Spinner( minHrsComp, SWT.BORDER | SWT.READ_ONLY); 
		fd = new FormData();
    	fd.top = new FormAttachment( hrsSpnr, 0, SWT.TOP );
    	fd.left = new FormAttachment( 60, 0 );
    	minsSpnr.setLayoutData(fd);

		minsSpnr.setMinimum(0);
		minsSpnr.setMaximum(59);
		minsSpnr.setDigits(0);
		minsSpnr.setTextLimit(2);
		
		minsSpnr.setSelection( seldMin );
		
		minsSpnr.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				seldMin = ((Spinner)e.widget).getSelection();
   				updateSelectedDate();   				
   			}
		});
	
		Label minsLbl = new Label( minHrsComp, SWT.NONE );
		minsLbl.setText("Mins");
		fd = new FormData();
    	fd.top = new FormAttachment( minsSpnr, 3, SWT.TOP );
    	fd.left = new FormAttachment( minsSpnr, 3, SWT.RIGHT );
    	minsLbl.setLayoutData(fd);

		seldDateTxt = new Text( minHrsComp, SWT.CENTER | SWT.BORDER );
		fd = new FormData();
		fd.width = 90;
    	fd.top = new FormAttachment( hrsSpnr, 12 );
    	fd.left = new FormAttachment( 50, -45 ); // center
		seldDateTxt.setLayoutData(fd);
		seldDateTxt.setBackground( seldDateTxt.getParent().getBackground() );
		
		Label sep = new Label( minHrsComp, SWT.SEPARATOR | SWT.HORIZONTAL );
		fd = new FormData();
    	fd.top = new FormAttachment( seldDateTxt, 10, SWT.BOTTOM );
    	fd.left = new FormAttachment( 0, 5 );
    	fd.right = new FormAttachment( 100, -5 );
    	fd.bottom = new FormAttachment( 100, -47 );
		sep.setLayoutData(fd);
		
		
		Button canBtn = new Button( minHrsComp, SWT.PUSH );
		canBtn.setText("Cancel");
		fd = new FormData();
		fd.width = 70;
    	fd.bottom = new FormAttachment( 100, -5 );
    	fd.left = new FormAttachment( 33, -35 ); // center
    	canBtn.setLayoutData(fd);
    	
		canBtn.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				seldDataTime = null;
   				shell.dispose();   				
   			}
		});

		
		Button okBtn = new Button( minHrsComp, SWT.PUSH );
		okBtn.setText("  Ok  ");
		fd = new FormData();
		fd.width = 70;
    	fd.top = new FormAttachment( canBtn, 0, SWT.TOP );
    	fd.left = new FormAttachment( 67, -35 ); // center
    	okBtn.setLayoutData(fd);
    	
		okBtn.addSelectionListener( new SelectionAdapter() {
   			public void widgetSelected(SelectionEvent e) {
   				shell.dispose();   				
   			}
		});

		updateDayOfMonthBtns();
		updateSelectedDate();

		shell.pack();
		
		shell.open();
		
		Display display = parent.getDisplay();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}
		
		return seldDataTime;
	}
		
	public void updateDayOfMonthBtns() {		
		Calendar cal = Calendar.getInstance();
		cal.clear();
		cal.set( seldYear, seldMonth, 1 );		
		cal.getTimeInMillis();
	    int startIndex = cal.get(Calendar.DAY_OF_WEEK) - 1; //		
		int lastDayOfMonth = getLastDayOfMonth(seldYear, seldMonth ); //
		int endIndex = startIndex + lastDayOfMonth - 1; //
		int dayOfMonth = 1;
		
		if( seldDay > lastDayOfMonth ) {
			seldDay = lastDayOfMonth;
		}		

		for (int i = 0; i < 42; i++) {
			dayBtns[i].setSelection( ( seldDay == dayOfMonth ) );			

			if (i >= startIndex && i <= endIndex) {
				dayBtns[i].setEnabled(true);
				dayBtns[i].setText("" + dayOfMonth);
				dayOfMonth++;
			} else {
				dayBtns[i].setEnabled(false);
				dayBtns[i].setText("");
			}
			
			if( i > 34 ) {
				dayBtns[i].setVisible( dayBtns[i].getEnabled() );
			}
		}		
	}
	
	public void updateSelectedDate( ) {
		Calendar seldCal = initCal; 

		seldCal.set(seldYear, seldMonth, seldDay, seldHour, seldMin);

		seldDataTime = new DataTime( seldCal.getTime() );

		seldDateTxt.setText( NmapCommon.getTimeStringFromDataTime( seldDataTime, "/" ) );
	}
}