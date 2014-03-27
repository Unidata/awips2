/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.TcaAttrDlg
 * 
 * 23 December 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.util.Calendar;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;

/**
 * Implementation of a text widget for UTC time
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/13		TTR800		B. Yin		Initial Creation.
 * </pre>
 * 
 * @author B. Yin
 */
public class UTCTimeText {

	private Text swtText;
	
	public UTCTimeText(Composite parent, int style) {
		swtText = new Text(parent, style);
	}
	
	/**
	 *		Create a UTC time text widget
	 */
	public UTCTimeText setUTCTimeTextField( Composite parent, Calendar cal, Control topWidget, int offset, boolean addLabel ){
		
		swtText.setTextLimit(4);
		swtText.setText( getInitialTime(cal ));
		swtText.addVerifyListener( new VerifyListener(){

			@Override
			public void verifyText(VerifyEvent ve) {
				final char BACKSPACE = 0x08;
				final char DELETE = 0x7F;
				
				if ( Character.isDigit(ve.character) || Character.UNASSIGNED == ve.character ||
					  ve.character == BACKSPACE || ve.character == DELETE ) ve.doit = true;
				else {
					ve.doit = false;
					Display.getCurrent().beep();
				}
			}} );
		
		swtText.addModifyListener( new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				if ( isTimeValid() )
					swtText.setBackground( Display.getCurrent().getSystemColor( SWT.COLOR_WHITE));
				else
					swtText.setBackground( Display.getCurrent().getSystemColor( SWT.COLOR_RED));
			}
			
		});
		 
		if ( addLabel ){
			org.eclipse.swt.widgets.Label utcLabel = new org.eclipse.swt.widgets.Label(parent,SWT.NONE);
			utcLabel.setText("UTC");


			FormData fd = new FormData();
			parent.setLayout( new FormLayout());
			if ( topWidget != null ) fd.top = new FormAttachment(topWidget,offset, SWT.BOTTOM);
			fd.left = new FormAttachment(swtText, 5, SWT.RIGHT);
			utcLabel.setLayoutData(fd);
		}
		
		return this;
	}
	
	/**
 	 *  check if the input string is a valid time
     */
	public boolean isTimeValid() {
		String text = swtText.getText();
		int time = Integer.parseInt(text);
		int hour = time / 100;
		int minute = time % 100;
		
		if ( hour >= 0 && hour <= 23 &&
				minute >= 00 && minute <=59 ) return true;
		
		return false;
	}
	
	/**
     *	Get the time string in the format HH00 from the input calendar time.
	 */
	public String getInitialTime(Calendar now ) {
		
		int minute = now.get(Calendar.MINUTE);
		if ( minute >= 15 ) now.add(Calendar.HOUR_OF_DAY, 1);
		int hour = now.get(Calendar.HOUR_OF_DAY);

		return String.format("%02d00", hour);
	}
	
	/**
 	 * Get the expiration hour from the validTime text widget
     */
	public int getHours(){
		int ret =0;
		try {
			String hm = swtText.getText();
			ret = Integer.parseInt(hm.substring(0, hm.length()== 4 ? 2:1 ));
		}
		catch (Exception e ){
			
		}
		return ret;
	}
	
	/**
 	 * Get the expiration minute from the validTime text widget
     */
	public int getMinutes(){
		int ret =0;
		try {
			String hm = swtText.getText();
			ret = Integer.parseInt(hm.substring(hm.length()== 4 ? 2:1 ), hm.length()-1);
		}
		catch (Exception e ){
			
		}
		return ret;
	}

	public String getText(){
		return swtText.getText();
	}
	
	public void setText(String str){
		swtText.setText(str);
	}
	
	public void setTime(int hh, int mm){
		swtText.setText( String.format("%1$02d%2$02d", hh,mm));
	}
	
	public void setLayoutData(Object layoutData){
		swtText.setLayoutData(layoutData);
	}
	
	public Text getTextWidget(){
		return swtText;
	}
}
