package gov.noaa.nws.ncep.ui.nsharp.view;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpPrintHandle
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._lplvalues;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._parcel;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Transform;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.WGraphics;
import com.sun.jna.ptr.FloatByReference;

public class NsharpPrintHandle {
	private Printer printer;
	private StringBuffer wordBuffer;
	private int lineHeight = 0;
	private int tabWidth = 0;
	private int leftMargin, rightMargin, topMargin, bottomMargin;
	private GC gc;
	private NsharpNative nsharpNative=null;
	private static int SKEWT_X_ORIG= 0;
	private static int SKEWT_HEIGHT= 375;
	private static int SKEWT_WIDTH= 420;
	private static int HODO_X_ORIG= SKEWT_X_ORIG + SKEWT_WIDTH-130;
	private static int HODO_HEIGHT= 130;
	private static int HODO_WIDTH= HODO_HEIGHT;
	private Font printerFont;
    private Color printerForegroundColor, printerBackgroundColor;
	private Transform transform;
    
	private static  NsharpPrintHandle printHandle=null;
	public static NsharpPrintHandle getPrintHandle() {
		if(printHandle==null)
			printHandle = new NsharpPrintHandle();
		return printHandle;
	}
	public void handlePrint(String intext) {
		/* Do the printing in a background thread so that spooling does not freeze the UI. */
		/*
		 * Chin:NOTE:::the following is from http://www.eclipse.org/swt/faq.php
		 * To allow background threads to perform operations on objects belonging to the UI-thread, 
		 * the methods syncExec(Runnable runnable) and asyncExec(Runnable runnable) of Display are used. 
		 * These are the only methods in SWT that can be called from any thread. 
		 * They allow a runnable to be executed by the UI-thread, either synchronously, 
		 * causing the background thread to wait for the runnable to finish, 
		 * or asynchronously allowing the background thread to continue execution without waiting for the result. 
		 * A runnable that is executed using syncExec() most closely matches the equivalent direct call to the UI 
		 * operation because a Java method call always waits for the result before proceeding, 
		 * just like syncExec().
		 */
		
		/*Chin:NOTE::: This code does not work when run in 5th floor FIT lab. CAVE display will freeze after hit "Print"
		 * button.
		Display display = Display.getCurrent();
		display.asyncExec(
				new Runnable() {
					public void run(){
						Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
						PrintDialog dialog = new PrintDialog(shell, SWT.NULL);
						PrinterData data = dialog.open();
						
						if (data == null) return;
						if (data.printToFile) {
							
							//System.out.println("Prnit to file with file name="+data.fileName);
							String fileName = data.fileName;
							int i = fileName.indexOf("///");
							if(i!=-1) //to fix a bug that add extra "//" at front of file path
								data.fileName = fileName.substring(0, i)+fileName.substring(i+2);
							//System.out.println("Prnit to file with file name="+data.fileName);
						}

						// Do the printing in a background thread so that spooling does not freeze the UI. 
						printer = new Printer(data);
						
						print(printer);
						printer.dispose();
					}
				});
*/
		
		
		
		//Chin: Note: now try use UIJOB to see if it works in 5th floor fit lab..
		Job uijob = new UIJob("clear source selection"){ //$NON-NLS-1$
			public IStatus runInUIThread(
					IProgressMonitor monitor) {
				Shell shell =PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
				PrintDialog dialog = new PrintDialog(shell, SWT.NULL);
				PrinterData data = dialog.open();

				if (data == null) return Status.CANCEL_STATUS;
				if (data.printToFile) {

					//System.out.println("Prnit to file with file name="+data.fileName);
					String fileName = data.fileName;
					int i = fileName.indexOf("///");
					if(i!=-1) //to fix a bug that add extra "//" at front of file path
						data.fileName = fileName.substring(0, i)+fileName.substring(i+2);
					//System.out.println("Prnit to file with file name="+data.fileName);
				}
				try{
					// Do the printing in a background thread so that spooling does not freeze the UI. 
					createPrinter(data);			        
			        if (startJob()) {
			            printPage();
			            endJob();
			        }
			        disposePrinter();
				}
				catch (Exception e) {

					e.printStackTrace();
				}
				return Status.OK_STATUS;
			}

		};
		uijob.setSystem(true);
		uijob.schedule();
		
	  
	}

	public void createPrinter(PrinterData data){
	    this.printer = new Printer(data);
	}
	
	public boolean startJob(){
	    String tabs;
	    if (printer.startJob("NSHARP")) {   // the string is the job name - shows up in the printer's job list
            Rectangle clientArea = printer.getClientArea();
            Rectangle trim = printer.computeTrim(0, 0, 0, 0);
            Point dpi = printer.getDPI();
            
            float dpiScaleX = dpi.x/72f;
            float dpiScaleY = dpi.y/72f;
            
            transform = new Transform(printer);
            transform.scale(dpiScaleX, dpiScaleY);
            
            leftMargin = 72 + trim.x; // one inch from left side of paper
            rightMargin = clientArea.width - 72 + trim.x + trim.width; // one inch from right side of paper
            topMargin = 72 + trim.y; // one inch from top edge of paper
            bottomMargin = clientArea.height - 72 + trim.y + trim.height; // one inch from bottom edge of paper
            //System.out.println("leftMargin="+leftMargin+"rightMargin="+rightMargin+"topMargin"+topMargin+"bottomMargin"+bottomMargin);
            //leftMargin=54rightMargin=521topMargin54bottomMargin701
            /* Create a buffer for computing tab width. */
            int tabSize = 4; // is tab width a user setting in your UI?
            StringBuffer tabBuffer = new StringBuffer(tabSize);
            for (int i = 0; i < tabSize; i++) tabBuffer.append(' ');
            tabs = tabBuffer.toString();
            
            /* Create printer GC, and create and set the printer font & foreground color. */
            gc = new GC(printer);
            int fontSize = (int) Math.round(5/dpiScaleY);
            fontSize = Math.max(1, fontSize);
            printerFont = new Font(printer, "Courier", fontSize, SWT.NORMAL);
            gc.setFont(printerFont);
            tabWidth = gc.stringExtent(tabs).x;
            lineHeight = gc.getFontMetrics().getHeight();

            RGB rgb = new RGB(0,0,0);//Black
            printerForegroundColor = new Color(printer, rgb);
            gc.setForeground(printerForegroundColor);
            rgb = new RGB(255,255,255);//white
            printerBackgroundColor = new Color(printer, rgb);
            gc.setBackground(printerBackgroundColor);
            gc.setTransform(transform);
            return true;
	    }
	    return false;
	}
	
	public void printPage(){
		NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
    	if( editor != null) {
    		NsharpResourceHandler rsc = editor.getRscHandler();
    		if(rsc != null){
	    printer.startPage();
        // Print SkewT square
        gc.drawRectangle(leftMargin+SKEWT_X_ORIG, topMargin, SKEWT_WIDTH, SKEWT_HEIGHT);
        // set view dimension
        WGraphics world = new WGraphics(leftMargin+SKEWT_X_ORIG, topMargin, leftMargin+SKEWT_X_ORIG+SKEWT_WIDTH, topMargin+SKEWT_HEIGHT);
        //set SKEWT virtual world coordinate. 
        world.setWorldCoordinates(NsharpConstants.left, NsharpConstants.top,
                NsharpConstants.right, NsharpConstants.bottom);
        gc.setLineWidth(1);
        
        try {
        	
        			gc.setClipping(leftMargin-30, topMargin-30, rightMargin+30, bottomMargin);
        			gc.drawString(rsc.getPickedStnInfoStr(),leftMargin+SKEWT_X_ORIG, topMargin-20);
        			rsc.printNsharpPressureLinesNumber(world, gc);
        			rsc.printNsharpTempNumber(world, gc);
        			rsc.printHeightMark(world, gc);
        			rsc.printNsharpWind(world, gc);
        			//set clipping 
        			gc.setClipping(leftMargin+SKEWT_X_ORIG, topMargin, SKEWT_WIDTH, SKEWT_HEIGHT);
        			//print skewt background
        			rsc.getSkewtPaneRsc().getSkewTBackground().paintForPrint(world, gc);
        			gc.setLineWidth(2);
        			gc.setLineStyle(SWT.LINE_SOLID);
        			rsc.printNsharpPressureTempCurve( world,  rsc.TEMP_TYPE, gc,  rsc.getSoundingLys());
        			rsc.printNsharpPressureTempCurve( world,  rsc.DEWPOINT_TYPE, gc,rsc.getSoundingLys());

        			gc.setLineStyle(SWT.LINE_DASH);
        			rsc.printNsharpWetbulbTraceCurve(world,  gc);
        			gc.setLineStyle(SWT.LINE_DASHDOTDOT);
        			rsc.printNsharpParcelTraceCurve(world, gc);
        			gc.setLineStyle(SWT.LINE_SOLID);
        			//fill/cover this skewt area to be used by Hodo
        			gc.fillRectangle(leftMargin+HODO_X_ORIG, topMargin, HODO_WIDTH, HODO_HEIGHT);
        		
        	
        } catch (VizException e) {
        	e.printStackTrace();
        }
        gc.setLineWidth(2);
        // Print Hodo square
        gc.drawRectangle(leftMargin+HODO_X_ORIG, topMargin, HODO_WIDTH, HODO_HEIGHT);
        
        //set HODO view world
        world = new WGraphics(leftMargin+HODO_X_ORIG, topMargin, leftMargin+HODO_X_ORIG+HODO_WIDTH, topMargin+HODO_HEIGHT);
        //set HODO real world coordinate. 
        world.setWorldCoordinates(-50, 90, 90, -50);
        
        gc.setLineWidth(1);
        //print hodo background 
        rsc.getHodoPaneRsc().getHodoBackground().paintForPrint(world, gc);
        try {
            //print hodo 
            gc.setLineStyle(SWT.LINE_SOLID);
            
            rsc.printNsharpHodoWind(world, gc, rsc.getSoundingLys());
        } catch (VizException e) {
            e.printStackTrace();
        }
        nsharpNative = getNsharpNativePtr();
        //reset clipping
        gc.setClipping(leftMargin-15, topMargin+SKEWT_HEIGHT, rightMargin-leftMargin+30,bottomMargin-topMargin+30);

        gc.setLineWidth(2);
        //print thermodynamic data title and its box
        gc.drawString("THERMODYNAMIC PARAMETERS",leftMargin+50, topMargin+SKEWT_HEIGHT+20);
        //gc.drawRectangle(leftMargin-15, topMargin+SKEWT_HEIGHT+30, 220,280);
        gc.drawLine(leftMargin-15, topMargin+SKEWT_HEIGHT+30, leftMargin+205,topMargin+SKEWT_HEIGHT+30);
        //set clipping 
        //gc.setClipping(leftMargin-16, topMargin+SKEWT_HEIGHT+30, 222,280);
        String textStr = printThermodynamicParametersBox1();
        //System.out.println(textStr);
        int curY= printText(textStr,leftMargin-12, topMargin+SKEWT_HEIGHT+35, leftMargin+205,topMargin+SKEWT_HEIGHT+295);
        gc.drawLine(leftMargin-15, curY, leftMargin+205,curY);
        gc.drawLine(leftMargin-15, curY, leftMargin-15,topMargin+SKEWT_HEIGHT+30);
        gc.drawLine(leftMargin+205, curY, leftMargin+205,topMargin+SKEWT_HEIGHT+30);
        String str1="", str2="", str3="";
        textStr = printThermodynamicParametersBox2();
        int gapIndex1 = textStr.indexOf("BOXLINE");
        str1= textStr.substring(0, gapIndex1);
        int gapIndex2 = textStr.indexOf("BOXLINE",gapIndex1+1);
        str2= textStr.substring( gapIndex1+("BOXLINE".length()),gapIndex2);
        str3= textStr.substring( gapIndex2+("BOXLINE".length()));
        int preY= curY;
        curY= printText(str1,leftMargin-12, curY+1, leftMargin+205,topMargin+SKEWT_HEIGHT+295);
        gc.drawLine(leftMargin-15, curY, leftMargin+205,curY);
        gc.drawLine(leftMargin-15, curY, leftMargin-15,preY);
        gc.drawLine(leftMargin+205, curY, leftMargin+205,preY);
        preY= curY;
        curY= printText(str2,leftMargin-12, curY+1, leftMargin+205,topMargin+SKEWT_HEIGHT+295);
        gc.drawLine(leftMargin-15, curY, leftMargin+205,curY);
        gc.drawLine(leftMargin-15, curY, leftMargin-15,preY);
        gc.drawLine(leftMargin+205, curY, leftMargin+205,preY);
        preY= curY;
        curY= printText(str3,leftMargin-12, curY+1, leftMargin+205,topMargin+SKEWT_HEIGHT+295);
        gc.drawLine(leftMargin-15, curY, leftMargin+205,curY);
        gc.drawLine(leftMargin-15, curY, leftMargin-15,preY);
        gc.drawLine(leftMargin+205, curY, leftMargin+205,preY);

        textStr = "Output produced by: NCO-SIB AWIPS2 NSHARP\nNational SkewT-Hodograph Analysis and Research Program\n";
        printText(textStr,leftMargin-12, curY+100, leftMargin+300,curY+120);


        //print kinematic data title and its box
        gc.drawString("KINEMATIC PARAMETERS",leftMargin+280, topMargin+SKEWT_HEIGHT+20);
        gc.drawLine(leftMargin+225, topMargin+SKEWT_HEIGHT+30, leftMargin+445,topMargin+SKEWT_HEIGHT+30);
        textStr = printKinematicParametersBox();
        curY= printText(textStr,leftMargin+228,topMargin+SKEWT_HEIGHT+35, leftMargin+444,topMargin+SKEWT_HEIGHT+295);
        //System.out.println("curreny y = "+ curY);
        gc.drawLine(leftMargin+225, curY, leftMargin+445,curY);
        gc.drawLine(leftMargin+225, curY, leftMargin+225,topMargin+SKEWT_HEIGHT+30);
        gc.drawLine(leftMargin+445, curY, leftMargin+445,topMargin+SKEWT_HEIGHT+30);

        // print STORM STRUCTURE PARAMETERS
        gc.drawString("STORM STRUCTURE PARAMETERS",leftMargin+280, curY+20);
        gc.drawLine(leftMargin+225, curY+30, leftMargin+445,curY+30);
        textStr = printStormStructureParametersBox();
        preY = curY+30;
        curY= printText(textStr,leftMargin+228,curY+35, leftMargin+444,topMargin+SKEWT_HEIGHT+295);
        gc.drawLine(leftMargin+225, curY, leftMargin+445,curY);
        gc.drawLine(leftMargin+225, curY, leftMargin+225,preY);
        gc.drawLine(leftMargin+445, curY, leftMargin+445,preY);

        printer.endPage();
    		}
    	}
	}
	
   public void endJob(){
        printer.endJob();

        /* Cleanup graphics resources used in printing */
        printerFont.dispose();
        printerForegroundColor.dispose();
        printerBackgroundColor.dispose();
        transform.dispose();
        gc.dispose();
    }
   
   public void disposePrinter(){
       printer.dispose();
   }
	
	private int x, y;
	private int index, end;		
	
	private int printText(String textToPrint, int xmin, int ymin, int xmax,int ymax) {
		
		wordBuffer = new StringBuffer();
		x = xmin;
		y = ymin;
		index = 0;
		end = textToPrint.length();
		while (index < end) {
			char c = textToPrint.charAt(index);
			index++;
			if (c != 0) {
				if (c == 0x0a || c == 0x0d) {
					if (c == 0x0d && index < end && textToPrint.charAt(index) == 0x0a) {
						index++; // if this is cr-lf, skip the lf
					}
					printWordBuffer(xmin, ymin, xmax, ymax);
					newline(xmin, ymin, xmax, ymax);
				} else {
					if (c != '\t') {
						wordBuffer.append(c);
					}
					if (Character.isWhitespace(c)) {
						printWordBuffer(xmin, ymin, xmax, ymax);
						if (c == '\t') {
							x += tabWidth;
						}
					}
				}
			}
		}
		return y;
	}

	void printWordBuffer(int xmin, int ymin, int xmax,int ymax) {
		if (wordBuffer.length() > 0) {
			String word = wordBuffer.toString();
			int wordWidth = gc.stringExtent(word).x;
			if (x + wordWidth > xmax) {
				/* word doesn't fit on current line, so wrap */
				System.out.println("word doesn't fit on current line, so wrap ");
				newline(xmin, ymin, xmax, ymax);
			}
			gc.drawString(word, x, y, false);
			
			x += wordWidth;
			wordBuffer = new StringBuffer();
		}
	}

	void newline(int xmin, int ymin, int xmax,int ymax) {
		x = xmin;
		y += lineHeight;
	}
	
    protected WGraphics computeWorld(int x1, int y1, int x2, int y2) {
        WGraphics world = new WGraphics(x1, y1, x1, y1);
        world.setWorldCoordinates(-50, 90, 90, -50);
        return world;
    }
    private NsharpNative getNsharpNativePtr(){
    	NsharpNative nsharpNative=null;
    	NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
		if( editor != null) {
			NsharpResourceHandler rsc = editor.getRscHandler();
			if(rsc != null){
				nsharpNative = rsc.getNsharpNative();
			}
		}
		return  nsharpNative;
    }
    private boolean qc ( float value )
	/*************************************************************/
	/*                                                           */
	/*  Quality control of sndg data.  Searches for missing      */
	/*  data (-999) and returns (1 = OK), (0 = missing)          */
	/*************************************************************/
	{
		if( value < -998.0F ) { return false; }
		if( value > 2.0E+05F ) { return false; }
		return true;
	}
	private String printKinematicParametersBox(){
		/*
		 * Chin's NOTE::::this function is for printing and coded based on legacy nsharp software 		
		 * show_meanwind()
		 *  in xwvid3.c
		 *  
		 *  void mean_wind ( float pbot, float ptop, float *mnu, float *mnv,
		 *	float *wdir, float *wspd )
		 *  Calculates a pressure-weighted mean wind thru the        
		 *  layer (pbot-ptop).  Default layer is LFC-EL.             
		 *                                                           
		 *  pbot             - Bottom level of layer (mb)            
		 *  ptop             - Top level of layer (mb)               
		 *  mnu              - U-Component of mean wind (kt)         
		 *  mnv              - V-Component of mean wind (kt)         
		 */

		
		if(nsharpNative != null){
			FloatByReference mnu= new FloatByReference(0);
			FloatByReference mnv= new FloatByReference(0);
			FloatByReference wdir= new FloatByReference(0);
			FloatByReference wspd= new FloatByReference(0);
			String textStr, finalTextStr="\t";
			finalTextStr =  finalTextStr+ NsharpNativeConstants.MEAN_WIND_STR;

			//Calculate mean wind at 0-6 km 
			nsharpNative.nsharpLib.mean_wind( -1, nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.agl(6000)), mnu, mnv, wdir, wspd);
			if(qc(wdir.getValue())&&qc(wspd.getValue())) {
				textStr = NsharpNativeConstants.MEANWIND_SFC6KM_LINE;
				textStr = String.format(textStr,wdir.getValue(), wspd.getValue(),
						nsharpNative.nsharpLib.kt_to_mps(wspd.getValue()));
			}
			else {
				textStr = NsharpNativeConstants.MEANWIND_SFC6KM_MISSING;
			}
			finalTextStr = finalTextStr + textStr;
			//Calculate mean wind at LFC-EL 
			nsharpNative.nsharpLib.mean_wind( -1, -1, mnu, mnv, wdir, wspd);
			if(qc(wdir.getValue())&&qc(wspd.getValue())) {
				textStr = NsharpNativeConstants.MEANWIND_LFC_EL_LINE;
				textStr = String.format(textStr,wdir.getValue(), wspd.getValue(),
						nsharpNative.nsharpLib.kt_to_mps(wspd.getValue()));
			}
			else {
				textStr = NsharpNativeConstants.MEANWIND_LFC_EL_MISSING;
			}
			finalTextStr = finalTextStr + textStr;
			//Calculate mean wind at 850-200 mb 
			nsharpNative.nsharpLib.mean_wind( 850,200, mnu, mnv, wdir, wspd);
			if(qc(wdir.getValue())&&qc(wspd.getValue())) {
				textStr = NsharpNativeConstants.MEANWIND_850_200MB_LINE;
				textStr = String.format(textStr,wdir.getValue(), wspd.getValue(),
						nsharpNative.nsharpLib.kt_to_mps(wspd.getValue()));
			}
			else {
				textStr = NsharpNativeConstants.MEANWIND_850_200MB_MISSING;
			}
			finalTextStr = finalTextStr + textStr + "\n";

			/*
			 * Chin's NOTE::::the following function is for pritning and coded based on legacy nsharp software 		
			 * show_shear()
			 *  in xwvid3.c
			 *  
			 *  void wind_shear ( float pbot, float ptop, float *shu, float *shv,
			 *	 float *sdir, float *smag )
			 *
			 *  Calculates the shear between the wind at (pbot) and      
			 *  (ptop).  Default lower wind is a 1km mean wind, while    
			 *  the default upper layer is 3km.                          
			 *                                                           
			 *  pbot             - Bottom level of layer (mb)            
			 *  ptop             - Top level of layer (mb)               
			 *  shu              - U-Component of shear (m/s)            
			 *  shv              - V-Component of shear (m/s)            
			 *  sdir             - Direction of shear vector (degrees)   
			 *  smag             - Magnitude of shear vector (m/s)
			 */
			FloatByReference shu= new FloatByReference(0);
			FloatByReference shv= new FloatByReference(0);
			FloatByReference sdir= new FloatByReference(0);
			FloatByReference smag= new FloatByReference(0);
			finalTextStr =  finalTextStr + NsharpNativeConstants.ENVIRONMENTAL_SHEAR_STR;

			finalTextStr = finalTextStr + NsharpNativeConstants.SHEAR_LAYER_DELTA_STR;

			//Calculate wind shear at Low - 3 km
			nsharpNative.nsharpLib.wind_shear( -1, nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(3000)), 
					shu,shv,sdir,smag);
			if(qc(smag.getValue())) {
				textStr = NsharpNativeConstants.SHEAR_LOW_3KM_LINE;
				textStr = String.format(textStr,smag.getValue(), 
						nsharpNative.nsharpLib.kt_to_mps(smag.getValue()),
						nsharpNative.nsharpLib.kt_to_mps(smag.getValue())/.3F);
			}
			else {
				textStr = NsharpNativeConstants.SHEAR_LOW_3KM_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			//Calculate wind shear at Sfc - 2 km
			nsharpNative.nsharpLib.wind_shear( -1, nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(2000)), 
					shu,shv,sdir,smag);
			if(qc(smag.getValue())) {
				textStr = NsharpNativeConstants.SHEAR_SFC_2KM_LINE;
				textStr = String.format(textStr,smag.getValue(), 
						nsharpNative.nsharpLib.kt_to_mps(smag.getValue()),
						nsharpNative.nsharpLib.kt_to_mps(smag.getValue())/.2F);
			}
			else {
				textStr = NsharpNativeConstants.SHEAR_SFC_2KM_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			//Calculate wind shear at Sfc - 6 km
			nsharpNative.nsharpLib.wind_shear( -1, nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(6000)), 
					shu,shv,sdir,smag);
			if(qc(smag.getValue())) {
				textStr = NsharpNativeConstants.SHEAR_SFC_6KM_LINE;
				textStr = String.format(textStr,smag.getValue(), 
						nsharpNative.nsharpLib.kt_to_mps(smag.getValue()),
						nsharpNative.nsharpLib.kt_to_mps(smag.getValue())/.6F);
			}
			else {
				textStr = NsharpNativeConstants.SHEAR_SFC_6KM_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			//Calculate wind shear at Sfc - 12 km
			nsharpNative.nsharpLib.wind_shear( -1, nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(12000)), 
					shu,shv,sdir,smag);
			if(qc(smag.getValue())) {
				textStr = NsharpNativeConstants.SHEAR_SFC_12KM_LINE;
				textStr = String.format(textStr,smag.getValue(), 
						nsharpNative.nsharpLib.kt_to_mps(smag.getValue()),
						nsharpNative.nsharpLib.kt_to_mps(smag.getValue())/1.2F);
			}
			else {
				textStr = NsharpNativeConstants.SHEAR_SFC_12KM_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			//BRN Shear
			/*_lplvalues lpvls = new _lplvalues();
			nsharpNative.nsharpLib.get_lpvaluesData(lpvls);		
			float sfctemp, sfcdwpt, sfcpres;
			sfctemp = lpvls.temp;
			sfcdwpt = lpvls.dwpt;
			sfcpres = lpvls.pres;				
			_parcel pcl= new _parcel();
			nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);*/
			FloatByReference fvalue= new FloatByReference(0);
			nsharpNative.nsharpLib.cave_bulk_rich2( fvalue );
			if(qc(fvalue.getValue())) {
				textStr = NsharpNativeConstants.STORM_TYPE_BRNSHEAR_LINE;
				textStr = String.format(textStr,fvalue.getValue(),NsharpConstants.SQUARE_SYMBOL, NsharpConstants.SQUARE_SYMBOL);
			}
			else
				textStr = NsharpNativeConstants.STORM_TYPE_BRNSHEAR_MISSING;	
			finalTextStr = finalTextStr + textStr;

			return finalTextStr;
		}

		return "";
	}
	private String printThermodynamicParametersBox1(){
		//Chin's NOTE::::this function is coded based on native nsharp show_parcel() in xwvid3.c
		//if we can not Interpolates a temp with 700 mb pressure, then we dont have enough raw data 
		//This function is called to construct text string for printing
		
		if(nsharpNative != null){
			if (nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.itemp(700.0F)) == 0)
				return null;
			String finalTextStr =  "";
			// set default
			short currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE; 
			String hdrStr  = NsharpNativeConstants.parcelToHdrStrMap.get(currentParcel);
			float layerPressure = NsharpNativeConstants.MU_LAYER;
			//get user selected parcel type
			/*TBD 
			if(NsharpDrawPanels.getInstance() != null){
				currentParcel = NsharpDrawPanels.getInstance().getCurrentParcel();
				hdrStr  = NsharpNativeConstants.parcelToHdrStrMap.get(currentParcel);
				layerPressure = NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
				
			}*/
			finalTextStr= finalTextStr+hdrStr;

			//call native define_parcel() with parcel type and user defined pressure (if user defined it)
			nsharpNative.nsharpLib.define_parcel(currentParcel,layerPressure);

			_lplvalues lpvls = new _lplvalues();
			nsharpNative.nsharpLib.get_lpvaluesData(lpvls);

			float sfctemp, sfcdwpt, sfcpres;
			sfctemp = lpvls.temp;
			sfcdwpt = lpvls.dwpt;
			sfcpres = lpvls.pres;

			// get parcel data by calling native nsharp parcel() API. value is returned in pcl 
			_parcel pcl = new _parcel();
			nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);


			String textStr = NsharpNativeConstants.PARCEL_LPL_LINE;
			textStr = String.format(textStr, (int)pcl.lplpres,(int)pcl.lpltemp,(int)pcl.lpldwpt,
					(int)nsharpNative.nsharpLib.ctof(pcl.lpltemp),(int)nsharpNative.nsharpLib.ctof(pcl.lpldwpt));
			//text.append(textStr);
			finalTextStr = finalTextStr+textStr;

			if(qc(pcl.bplus)){
				textStr = NsharpNativeConstants.PARCEL_CAPE_LINE;
				textStr = String.format(textStr,pcl.bplus);				
			}
			else {
				textStr = NsharpNativeConstants.PARCEL_CAPE_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			if(qc(pcl.li5)){
				textStr = NsharpNativeConstants.PARCEL_LI_LINE;
				textStr = String.format(textStr,pcl.li5);				
			}
			else {
				textStr = NsharpNativeConstants.PARCEL_LI_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			if(qc(pcl.bfzl)){
				textStr = NsharpNativeConstants.PARCEL_BFZL_LINE;
				textStr = String.format(textStr,pcl.bfzl);
			}
			else{
				textStr = NsharpNativeConstants.PARCEL_BFZL_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			if(qc(pcl.limax)){
				textStr = NsharpNativeConstants.PARCEL_LIMIN_LINE;
				textStr = String.format(textStr,pcl.limax,pcl.limaxpres);
			}
			else{
				textStr = NsharpNativeConstants.PARCEL_LIMIN_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			if(qc(pcl.bminus)){
				textStr = NsharpNativeConstants.PARCEL_CINH_LINE;
				textStr = String.format(textStr,pcl.bminus);
			}
			else {
				textStr = NsharpNativeConstants.PARCEL_CINH_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			if(qc(pcl.cap)){
				textStr = NsharpNativeConstants.PARCEL_CAP_LINE;
				textStr = String.format(textStr, pcl.cap, pcl.cappres);
			}
			else {
				textStr = NsharpNativeConstants.PARCEL_CAP_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			textStr = NsharpNativeConstants.PARCEL_LEVEL_LINE;
			finalTextStr = finalTextStr + textStr;

			if(qc(pcl.lclpres)&& 
					qc(nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lclpres ))))	)
			{
				textStr = NsharpNativeConstants.PARCEL_LCL_LINE;
				textStr = String.format(textStr,pcl.lclpres, nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lclpres ))));
			}
			else {
				textStr = NsharpNativeConstants.PARCEL_LCL_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			if(qc(pcl.lfcpres) && 
					qc(nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lfcpres )))) &&
					qc(nsharpNative.nsharpLib.itemp(pcl.lfcpres )))
			{
				textStr = NsharpNativeConstants.PARCEL_LFC_LINE;
				textStr = String.format(textStr,pcl.lfcpres, nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lfcpres ))),
						nsharpNative.nsharpLib.itemp(pcl.lfcpres ));
			}
			else {
				textStr = NsharpNativeConstants.PARCEL_LFC_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			if(qc(pcl.elpres) && 
					qc(nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.elpres )))) &&
					qc(nsharpNative.nsharpLib.itemp(pcl.elpres )))			
			{
				textStr = NsharpNativeConstants.PARCEL_EL_LINE;
				textStr = String.format(textStr,pcl.elpres, nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.elpres ))),
						nsharpNative.nsharpLib.itemp(pcl.elpres ));
			}
			else {
				textStr = NsharpNativeConstants.PARCEL_EL_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			if(qc(pcl.mplpres) && 
					qc(nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.mplpres ))))	)
			{
				textStr = NsharpNativeConstants.PARCEL_MPL_LINE;
				textStr = String.format(textStr,pcl.mplpres, nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.mplpres ))));
			}
			else {
				textStr = NsharpNativeConstants.PARCEL_MPL_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			return finalTextStr;

		}

		return "";
	}

	@SuppressWarnings("deprecation")
	public String printThermodynamicParametersBox2(){
		/*
		 * Chin's NOTE::::this function is coded based on legacy native nsharp software show_thermoparms(), 
		 * show_moisture(),show_instability() in xwvid3.c
		 * This function is called to construct text string for printing
		 */
		
		if(nsharpNative != null){
			String finalTextStr="";
			FloatByReference fValue= new FloatByReference(0);
			FloatByReference fValue1= new FloatByReference(0);
			FloatByReference fValue2= new FloatByReference(0);
			FloatByReference fValue3= new FloatByReference(0);
			//thermo data--------------//
			//moisture data--------------//
			String textStr;

			fValue.setValue(0);
			nsharpNative.nsharpLib.precip_water(fValue, -1.0F, -1.0F);
			if(qc(fValue.getValue())) {
				textStr = NsharpNativeConstants.THERMO_PWATER_LINE;
				textStr = String.format(textStr,fValue.getValue());
			}
			else {
				textStr = NsharpNativeConstants.THERMO_PWATER_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			fValue.setValue(0);
			nsharpNative.nsharpLib.mean_relhum(fValue, -1.0F, -1.0F);
			if(qc(fValue.getValue())) {
				textStr = NsharpNativeConstants.THERMO_MEANRH_LINE;
				textStr = String.format(textStr,fValue.getValue(),NsharpConstants.PERCENT_SYMBOL);
			}
			else {
				textStr = NsharpNativeConstants.THERMO_MEANRH_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			fValue.setValue(0);
			nsharpNative.nsharpLib.mean_mixratio(fValue, -1.0F, -1.0F);
			if(qc(fValue.getValue())) {
				textStr = NsharpNativeConstants.THERMO_MEANW_LINE;
				textStr = String.format(textStr,fValue.getValue());
			}
			else {
				textStr = NsharpNativeConstants.THERMO_MEANW_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			fValue.setValue(0);
			fValue1.setValue(0);
			// get surface pressure (fValue1) before getting mean LRH value
			nsharpNative.nsharpLib.get_surface(fValue1, fValue2, fValue3); //fValue 2 and fValue3 are not of concern here
			nsharpNative.nsharpLib.mean_relhum( fValue, -1.0F, fValue1.getValue() - 150 );
			if(qc(fValue.getValue())) {
				textStr = NsharpNativeConstants.THERMO_MEANLRH_LINE;
				textStr = String.format(textStr,fValue.getValue(),NsharpConstants.PERCENT_SYMBOL);
			}
			else {
				textStr = NsharpNativeConstants.THERMO_MEANLRH_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			fValue.setValue(0);
			nsharpNative.nsharpLib.top_moistlyr(fValue);
			if(qc(fValue.getValue())) {
				textStr = NsharpNativeConstants.THERMO_TOP_LINE;
				textStr = String.format(textStr,fValue.getValue(),nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(fValue.getValue()))));
			}
			else {
				textStr = NsharpNativeConstants.THERMO_TOP_MISSING;
			}
			finalTextStr = finalTextStr + textStr;
			//System.out.println(finalTextStr);
			finalTextStr = finalTextStr + "BOXLINE";

			//instability data--------------//
			//finalTextStr=""; // reset str

			fValue.setValue(0);
			fValue1.setValue(0);

			nsharpNative.nsharpLib.delta_t(fValue);
			nsharpNative.nsharpLib.lapse_rate( fValue1, 700.0F, 500.0F );

			if(qc(fValue.getValue()) && qc(fValue1.getValue())) {
				textStr = NsharpNativeConstants.THERMO_700500mb_LINE;
				textStr = String.format(textStr,fValue.getValue(), fValue1.getValue());
			}
			else {
				textStr = NsharpNativeConstants.THERMO_700500mb_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			fValue.setValue(0);
			fValue1.setValue(0);

			nsharpNative.nsharpLib.vert_tot(fValue);
			nsharpNative.nsharpLib.lapse_rate( fValue1, 850.0F, 500.0F );

			if(qc(fValue.getValue()) && qc(fValue1.getValue())) {
				textStr = NsharpNativeConstants.THERMO_850500mb_LINE;
				textStr = String.format(textStr,fValue.getValue(), fValue1.getValue());
			}
			else {
				textStr = NsharpNativeConstants.THERMO_850500mb_MISSING;
			}
			finalTextStr = finalTextStr + textStr;
			finalTextStr = finalTextStr + "BOXLINE";

			//misc parameters data--------------//
			//finalTextStr=""; // reset str
			fValue.setValue(0);
			fValue1.setValue(0);
			fValue2.setValue(0);
			nsharpNative.nsharpLib.t_totals(fValue, fValue1, fValue2);
			if(qc(fValue.getValue())) {
				textStr = NsharpNativeConstants.THERMO_TOTAL_LINE;
				textStr = String.format(textStr,fValue.getValue());
			}
			else {
				textStr = NsharpNativeConstants.THERMO_TOTAL_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			fValue.setValue(0);
			nsharpNative.nsharpLib.k_index(fValue);
			if(qc(fValue.getValue())) {
				textStr = NsharpNativeConstants.THERMO_KINDEX_LINE;
				textStr = String.format(textStr,fValue.getValue());
			}
			else {
				textStr = NsharpNativeConstants.THERMO_KINDEX_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			fValue.setValue(0);
			nsharpNative.nsharpLib.sweat_index(fValue);
			if(qc(fValue.getValue())) {
				textStr = NsharpNativeConstants.THERMO_SWEAT_LINE;
				textStr = String.format(textStr,fValue.getValue());
			}
			else {
				textStr = NsharpNativeConstants.THERMO_SWEAT_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			fValue.setValue(0);
			float maxTempF = nsharpNative.nsharpLib.ctof(nsharpNative.nsharpLib.max_temp( fValue, -1));
			if(qc(maxTempF)) {
				textStr = NsharpNativeConstants.THERMO_MAXT_LINE;
				textStr = String.format(textStr,maxTempF);
			}
			else {
				textStr = NsharpNativeConstants.THERMO_MAXT_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			fValue.setValue(0);
			float theDiff = nsharpNative.nsharpLib.ThetaE_diff( fValue);
			if(qc(theDiff)) {
				textStr = NsharpNativeConstants.THERMO_THETAE_LINE;
				textStr = String.format(textStr,theDiff);
			}
			else {
				textStr = NsharpNativeConstants.THERMO_THETAE_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			fValue.setValue(0);
			float conTempF = nsharpNative.nsharpLib.ctof(nsharpNative.nsharpLib.cnvtv_temp( fValue, -50));
			if(qc(conTempF)) {
				textStr = NsharpNativeConstants.THERMO_CONVT_LINE;
				textStr = String.format(textStr,conTempF);
			}
			else {
				textStr = NsharpNativeConstants.THERMO_CONVT_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			fValue.setValue(0);
			float wbzft = nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.wb_lvl( 0, fValue ))));
			if(qc(wbzft)) {
				textStr = NsharpNativeConstants.THERMO_WBZ_LINE;
				textStr = String.format(textStr,wbzft);
			}
			else {
				textStr = NsharpNativeConstants.THERMO_WBZ_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			fValue.setValue(0);
			float fgzft = nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.temp_lvl( 0, fValue ))));
			if(qc(fgzft)) {
				textStr = NsharpNativeConstants.THERMO_FGZ_LINE;
				textStr = String.format(textStr,fgzft);
			}
			else {
				textStr = NsharpNativeConstants.THERMO_FGZ_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			return finalTextStr;
		}
		return"";
	}
	public String printStormStructureParametersBox(){		
		if(nsharpNative != null){
			FloatByReference smdir= new FloatByReference(0);
			FloatByReference smspd= new FloatByReference(0);
			FloatByReference wdir= new FloatByReference(0); 
			FloatByReference wspd= new FloatByReference(0);
			FloatByReference mnu= new FloatByReference(0);
			FloatByReference mnv= new FloatByReference(0);
			FloatByReference phel= new FloatByReference(0);
			FloatByReference nhel= new FloatByReference(0);
			String textStr, finalTextStr="";
			nsharpNative.nsharpLib.get_storm(smspd, smdir);	
			//calculate helicity for sfc-3 km
			float totHeli = nsharpNative.nsharpLib.helicity( (float)0, (float)3000, smdir.getValue(), smspd.getValue(), phel, nhel);
			if(qc(phel.getValue())&&qc(nhel.getValue())) {
				textStr = "Sfc - 3km SREH =\t\t%.0f m%c/s%c\r\n";
				textStr = String.format(textStr, totHeli,
						NsharpConstants.SQUARE_SYMBOL, NsharpConstants.SQUARE_SYMBOL);
			}
			else {
				textStr = "Sfc - 3km SREH =\t\tM";
			}
			finalTextStr = finalTextStr + textStr;
			//EFF. SREH
			//nsharpNative.nsharpLib.get_storm(wspd,wdir);
			if(qc(smdir.getValue())&&qc(smspd.getValue())) {
				totHeli =
					nsharpNative.nsharpLib.helicity( -1.0F, -1.0F, smdir.getValue(), smspd.getValue(), phel, nhel);
				if(qc(totHeli)){
					textStr = NsharpNativeConstants.STORM_TYPE_EFF_LINE;
					textStr = String.format(textStr,totHeli,NsharpConstants.SQUARE_SYMBOL,NsharpConstants.SQUARE_SYMBOL);
				}
				else {
					textStr = NsharpNativeConstants.STORM_TYPE_EFF_MISSING;
				}
			}
			else {
				textStr = NsharpNativeConstants.STORM_TYPE_EFF_MISSING;
			}
			finalTextStr = finalTextStr + textStr;
			//EHI
			_parcel pcl = new _parcel();;
			_lplvalues lpvls = new _lplvalues();
			nsharpNative.nsharpLib.get_lpvaluesData(lpvls);		
			float sfctemp, sfcdwpt, sfcpres;
			sfctemp = lpvls.temp;
			sfcdwpt = lpvls.dwpt;
			sfcpres = lpvls.pres;				
			nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);
			if(qc(pcl.bplus)) {
				float ehi =
					nsharpNative.nsharpLib.ehi( pcl.bplus, totHeli);
				if(qc(ehi)){
					textStr = NsharpNativeConstants.STORM_TYPE_EHI_LINE;
					textStr = String.format(textStr,ehi);
				}
				else {
					textStr = NsharpNativeConstants.STORM_TYPE_EHI_MISSING;
				}		
			}
			else {
				textStr = NsharpNativeConstants.STORM_TYPE_EHI_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			//BRN
			if(qc(pcl.brn)) {
				textStr = NsharpNativeConstants.STORM_TYPE_BRN_LINE;
				textStr = String.format(textStr,pcl.brn);
			}
			else {
				textStr = NsharpNativeConstants.STORM_TYPE_BRN_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			//Strom wind
			finalTextStr = finalTextStr +NsharpNativeConstants.STORM_WIND_STR;
			finalTextStr = finalTextStr + NsharpNativeConstants.STORM_LAYER_VECTOR_STR;
			//calculate pressure-weighted SR mean wind  at sfc-2 km
			nsharpNative.nsharpLib.sr_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(0)), 
					nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(2000)), smdir.getValue(), smspd.getValue(), 
					mnu, mnv, wdir, wspd);
			if(qc(wdir.getValue())) {
				textStr = NsharpNativeConstants.STORM_SFC2KM_VECT_LINE;
				textStr = String.format(textStr,wdir.getValue(), wspd.getValue(), nsharpNative.nsharpLib.kt_to_mps(wspd.getValue()));
			}
			else {
				textStr = NsharpNativeConstants.STORM_SFC2KM_VECT_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			//calculate pressure-weighted SR mean wind  at 4-6 km
			nsharpNative.nsharpLib.sr_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(4000)), 
					nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(6000)), smdir.getValue(), smspd.getValue(), 
					mnu, mnv, wdir, wspd);
			if(qc(wdir.getValue())) {
				textStr = NsharpNativeConstants.STORM_4_6KM_VECT_LINE;
				textStr = String.format(textStr,wdir.getValue(), wspd.getValue(), nsharpNative.nsharpLib.kt_to_mps(wspd.getValue()));
			}
			else {
				textStr = NsharpNativeConstants.STORM_4_6KM_VECT_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			//calculate pressure-weighted SR mean wind  at 9-11 km
			nsharpNative.nsharpLib.sr_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(9000)), 
					nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(11000)), smdir.getValue(), smspd.getValue(), 
					mnu, mnv, wdir, wspd);
			if(qc(wdir.getValue())) {
				textStr = NsharpNativeConstants.STORM_9_11KM_VECT_LINE;
				textStr = String.format(textStr,wdir.getValue(), wspd.getValue(), nsharpNative.nsharpLib.kt_to_mps(wspd.getValue()));
			}
			else {
				textStr = NsharpNativeConstants.STORM_9_11KM_VECT_MISSING;
			}
			finalTextStr = finalTextStr + textStr;

			return finalTextStr;
		}
		return"";
	}
}
