/*
 * SpenesFormatMsgDlg
 * 
 * Date created: June 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import java.io.File;
import java.io.FileWriter;

import org.eclipse.jface.dialogs.IDialogConstants;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

import gov.noaa.nws.ncep.ui.pgen.elements.Spenes;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
/**
* Singleton for a Spenes format message dialog.
* 
* <pre>
* SOFTWARE HISTORY
* Date       	Ticket#		Engineer	Description
* ------------	----------	-----------	--------------------------
* 05/12		    #734		J. Zeng   	Initial Creation.
* </pre>
* 
* @author	J. Zeng
*/
public class SpenesFormatMsgDlg extends CaveJFACEDialog{
	
	//top level container for all widgets
	private Composite top;
	
	//text message to display
	private String spenesMsg;
	
	private Spenes spenes;
	
	private SpenesFormatDlg sfDlg;
	private Text txtFileLabel;
	
	//dialog size
	private final int NUM_LINES = 25;
	private final int NUM_COLUMNS = 68;

	
	/**
	 * constructor
	 */
	protected SpenesFormatMsgDlg(Shell parentShell, SpenesFormatDlg sfd){
		super(parentShell);
		sfDlg = sfd;
		spenes = sfDlg.getSpenes();
	}
	
	/**
	 * get the file name
	 * @return
	 */
	private String getTxtFileName() {
		String connector = "_";
		StringBuilder sb = new StringBuilder();
		sb.append(PgenUtil.getPgenActivityTextProdPath()+ File.separator);
		sb.append( spenes.getName());
		sb.append(connector);
        String initDataTime = spenes.getInitDateTime().replace(" ", connector).replace("/", connector);
		sb.append(initDataTime);
		sb.append(".txt");
		String spenesTxtFileName = sb.toString();
		return spenesTxtFileName;
	}
	
	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent){
		getShell().setText("SPENES TEXT");
		
		top = (Composite) super.createDialogArea(parent);
		
		/**
		 *  Create the main layout for the dialog area.
	     */
		GridLayout mainLayout = new GridLayout(1, true);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;
		top.setLayout(mainLayout);

		/*
		 *  Create a text box for the WCL message
		 */
		Text messageBox = new Text(top, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.WRAP);
		messageBox.setFont(new Font(messageBox.getDisplay(),"Courier",12, SWT.NORMAL) );
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
			
		//  Calculate approximate size of text box to display 25 lines at 80 characters each
		gd.heightHint = NUM_LINES * messageBox.getLineHeight();       
		GC gc = new GC (messageBox);
		FontMetrics fm = gc.getFontMetrics ();
		gd.widthHint = NUM_COLUMNS * fm.getAverageCharWidth ();

		messageBox.setLayoutData(gd);
		if (spenes != null ){
			setMessage( generateSpenesText(spenes));
		}
		messageBox.setText(spenesMsg);
	        
		//  Make sure to dispose of font
		messageBox.addDisposeListener(new DisposeListener() {

			@Override
			public void widgetDisposed(DisposeEvent e) {
				Text w = (Text)e.widget;
				w.getFont().dispose();
			}
	        	
		});
	        
		/*
		 * Add horizontal separator
		 */
		Label sep = new Label(top, SWT.HORIZONTAL | SWT.SEPARATOR);
		sep.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		/*
		 * Set label to display output file name
		 */
		Label filelabel = new Label(top, SWT.NONE );
		filelabel.setText("SPENES File Name:  " );

		txtFileLabel = new Text(top, SWT.LEFT | SWT.BORDER | SWT.WRAP );
		GridData gData1 = new GridData(400,16);
		gData1.horizontalAlignment = 7;
		txtFileLabel.setLayoutData(gData1);
		txtFileLabel.setText(getTxtFileName());
		txtFileLabel.addModifyListener(new TxtModifyListener());
		
		return top;
	}
		
	/**
	 * To listen text modification
	 */	
	private class TxtModifyListener implements ModifyListener{
		@Override
		public void modifyText(ModifyEvent e){
			//String txt = "";
			if(e.widget instanceof Text){
				//txt =((Text)e.widget).getText();
			}else if(e.widget instanceof Combo){
			}
			
		}
	}
	
	/*
	 * 
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void okPressed() {
		String txtFileName = txtFileLabel.getText();
		//System.out.println("File saved: " + txtFileName);
		/*
		 * Save SPENES button pressed.  Save SPENES Message to a file
		 */
		FileTools.writeFile(txtFileName, spenesMsg);
		
		super.okPressed();
	}
				
	/**
	 * Set spenes text
	 * @param spenes message
	 */
	public void setMessage(String spenesMessage) {
		spenesMsg = spenesMessage;
	}
	
	@Override
	/**
	 * Set the location of the dialog
	 */
	public int open(){
		
		if ( this.getShell() == null ){
			this.create();
		}
		// this.getShell().setLocation(this.getShell().getParent().getLocation());
		this.getButton(IDialogConstants.OK_ID).setText("Save");
		this.getButtonBar().pack();
		return super.open();
			
	}
	
	/**
	 * generate spenes text message
	 * @param sp
	 * @return
	 */
	private String generateSpenesText( Spenes sp){
		
		sp.generateStatesWfosRfcs();
		
		StringBuilder sb = new StringBuilder();
		sb.append("ZCZC NFDSPENES ALL\n\nSPENES\n\n");

		sb.append(sp.getStateZ000());
		//issue time
		sb.append("\n.\n\nSATELLITE PRECIPITATION ESTIMATES..DATE/TIME ");
		//sb.append(String.format("%1$tm/%1$td/%1$ty %1$tH%1$tMZ", getInitTime()));
		sb.append(sp.getInitDateTime());
		
		sb.append("\n\nSATELLITE ANALYSIS BRANCH/NESDIS---NPPU---TEL.301-763-8678\n\nLATEST DATA USED:");
		sb.append(sp.getLatestDataUsed());
		sb.append("   " + sp.getObsHr() + "00Z" ); 
		sb.append("   " + sp.getForecasters());
		sb.append("\n\n.\n\nLOCATION...");
		sb.append(sp.getLocation());
		
		sb.append("\n\n.\n\nATTN WFOS...");
		sb.append(sp.getAttnWFOs());
		sb.append("\n\n.\n\nATTN RFCS...");
		sb.append(sp.getAttnRFCs());
		
		sb.append("\n\n.\nEVENT...");
		sb.append(sp.getEvent());
		
		sb.append("\n\n.\n\nSATELLITE ANALYSIS AND TRENDS..." );
		sb.append(" " + sp.getSatAnalysisTrend() );
		sb.append("\n\n.\n\n");
		sb.append("AN ANNOTATED SATELLITE GRAPHIC SHOWING THE HEAV PERCIPITATION THREAT AREA\n\n");
		sb.append("SHOULD BE AVAILABLE ON THE INTERNET ADDRESS LISTED BELOW IN APPROXIMATELY\n\n10-15 MINUTES.\n\n.\n\n");
		sb.append("SHORT TERM OUTLOOK VALID ");
		sb.append(sp.getShortTermBegin() +"00-"+ sp.getShortTermEnd()+"00Z");
		sb.append("... " + sp.getOutlookLevel());
		sb.append(" CONFIDENCE FACTOR IN SHORT TERM OUTLOOK...");
		sb.append(sp.getAddlInfo());
		sb.append("\n\n.\n\n");
		sb.append("SEE NCEP HPC DISCUSSION AND QPF/S FOR FORECAST.\n\n....NESDIS IS A MEMBER OF 12 PLANET....\n\n.\n\n");
		sb.append("SSD\\SAB WEB ADDRESS FOR PRECIP ESTIMATES:\n\nHTTP://WWW.SSD.NOAA.GOV/PS/PCPN/\n\n");
		sb.append("...ALL LOWER CASE EXCEPT /PS/PCPN/\n\n.\n\n");
		
		//lat/lon
		sb.append("LAT...LON ");
		sb.append(sp.getLatLon());
		sb.append("\n\n.\n\nNNNN");
		
		return sb.toString();
	}

}
