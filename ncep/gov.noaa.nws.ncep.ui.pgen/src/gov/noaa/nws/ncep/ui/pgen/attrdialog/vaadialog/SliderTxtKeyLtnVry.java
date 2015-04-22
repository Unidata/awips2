/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.vaaDialog.SliderTxtKeyLtnVry
 * 
 * 23 November 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog;


import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Text;


/**
 * Listener class for SymbolAttrDlg and OutlookAttrDlg's
 * Slider related Text Widget.
 * 
 * This class could be moved into AttrDlg class in future.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 11/10		341			G. Zhang	Initial Creation. 
 * </pre>
 * 
 * @author	G. Zhang
 */

public class SliderTxtKeyLtnVry  extends KeyAdapter implements VerifyListener{
	
	Slider widthSlider;
	Text   widthText;
	int MIN_LINE_WIDTH = 0, MAX_LINE_WIDTH = 0;
	double MIN_SIZE = 0.0,	MAX_SIZE = 10.0,DEFAULT_SIZE = 2.0;
	boolean isDouble = false;
	String iniValue = "";
	
	public SliderTxtKeyLtnVry(Slider ws, Text wt, int minw, int maxw){
		
		widthSlider = ws;
		widthText = wt;
		MIN_LINE_WIDTH = minw;
		MAX_LINE_WIDTH = maxw;
		
		widthSlider.addListener(SWT.Selection, new Listener(){

			@Override
			public void handleEvent(Event event) {
				
				widthText.setText(String.valueOf(widthSlider.getSelection()));
				
			}
			
		});
	}
	
	public SliderTxtKeyLtnVry(Slider ws, Text wt, double minw, double maxw){
		
		widthSlider = ws;
		widthText = wt;
		MIN_SIZE = minw;	
		MAX_SIZE = maxw;
		isDouble = true;
		
		widthSlider.addListener(SWT.Selection, new Listener(){

			@Override
			public void handleEvent(Event event) {
				
				widthText.setText( String.format("%1$4.1f", getTxtFrmSlider(widthSlider)) );
				
			}
			
		});
	}
	
	@Override
	public void keyReleased( KeyEvent e ) {
		
		if( isDouble ){
			
			double value = 0;
			String in = widthText.getText();
			
			try {
				
				value = Double.parseDouble( in );
				if ( value >= MIN_SIZE && value <= MAX_SIZE ) {
					
					widthSlider.setSelection( getIntFrmText(value) );
					widthText.setToolTipText( "" );
					
				}else if( value > MAX_SIZE){ 
					// handles the case with x.0: 1). highlight x; 2). type-in a number like 8; 3). widthText will show 8x.0. 
					widthText.setText(String.format("%1$4.1f", getFrstAsNum(in)));
					widthSlider.setSelection( getIntFrmText( getFrstAsNum(in) ));
				}
				
			} catch ( NumberFormatException e1 ) {
				// handles the case with _x.0(_ is space): 1). highlight _x; 2). type-in a number like 8; 3). widthText will show 8_x.0. 
				widthText.setText(String.format("%1$4.1f", getFrstAsNum(in)));
				widthSlider.setSelection( getIntFrmText( getFrstAsNum(in) ));
			}
			
		}else{			
		
			int value = 0;
			try {
				value = Integer.parseInt( widthText.getText() );
				if ( value >= MIN_LINE_WIDTH && value <= MAX_LINE_WIDTH ) {
					widthSlider.setSelection( value );
					widthText.setToolTipText( "" );
				}
			} catch ( NumberFormatException e1 ) {
			}
		}
	}

	@Override
	public void verifyText(VerifyEvent e) {
		e.doit = validateLineWidth(e);
		if ( ! e.doit ) Display.getCurrent().beep();
	}
	
	/**
	 * Validate the line width text field.
	 * copied from LineAttrDlg.java
	 * 
	 * @param ve:	VerifyEvent
	 * @return:		true valid text, false otherwise.
	 */
	protected boolean validateLineWidth(VerifyEvent ve ){
		
		if( isDouble ){
			
			boolean stat = false;
			
			if ( ve.widget instanceof Text ) {
				Text wText = (Text)ve.widget;
				StringBuffer str = new StringBuffer(wText.getText());
				str.replace(ve.start, ve.end, ve.text);
	
				if ( str.toString().isEmpty() ) return true;
				else {
					try {
						double value = Double.parseDouble( str.toString() );	
						if ( value >= MIN_SIZE && value <= MAX_SIZE ) {			
							stat = true;
						}
						else 
							stat = false;
					} catch ( NumberFormatException e1 ) {
						stat = false;
					}
				}
			}
	
			return stat;
			
		}else{			
		
			boolean stat = false;
	
			if ( ve.widget instanceof Text ) {
				Text wText = (Text)ve.widget;
				StringBuffer str = new StringBuffer(wText.getText());
				str.replace(ve.start, ve.end, ve.text);
	
				if ( str.toString().isEmpty() ) return true;
				else {
					try {
						int value = Integer.parseInt( str.toString() );
						if ( value >= MIN_LINE_WIDTH && value <= MAX_LINE_WIDTH ) {
							stat = true;
						}
						else 
							stat = false;
					} catch ( NumberFormatException e1 ) {
						stat = false;
					}
				}
			}
	
			return stat;
		}
	}
	
	/**
	 * map 1-100 to 0.1-10.0.
	 * @param s: 	the Slider Widget.
	 * @return:		String value of 0.1-10.0.
	 */
	public static double getTxtFrmSlider(Slider s){
		
		int i = s.getSelection();
		
		int ii = ( i==0 ? 1 : i );
		
		return ii/10.0;
	}
	
	/**
	 * map 0.1-10.0 to 1-100.
	 * @param d:	the double value to be mapped.
	 * @return:		the value range 1-20 for the Slider.
	 */
	public static int getIntFrmText(double d){
		
		int i = (int)( ( d==0.0 ) ? 1 : d*10 );
		
		return i;
	}
	
	/**
	 * return the first number of the user input.
	 * 
	 * @param t:	the user input in String.
	 * @return:		the first number in double.
	 */
	public static double getFrstAsNum(String t){
		double d = 10.0;
		
		if(t == null || t.isEmpty())
			return d;
		
		try{
			
			String s = t.trim().substring(0,1);
			d = Double.parseDouble(s);
			
		}catch(Exception e){
			System.out.println("______Error: getFrstAsNum()"+e.getMessage());
		}
		
		return d==0.0 ? 1.0 : d;
	}


}
