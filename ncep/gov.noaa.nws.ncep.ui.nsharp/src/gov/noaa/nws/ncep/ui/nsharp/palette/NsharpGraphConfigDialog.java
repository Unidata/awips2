package gov.noaa.nws.ncep.ui.nsharp.palette;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpGraphConfigDialog
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

import gov.noaa.nws.ncep.ui.nsharp.menu.NsharpLoadDialog;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import com.raytheon.uf.viz.core.exception.VizException;

public class NsharpGraphConfigDialog extends Dialog {
	private static NsharpGraphConfigDialog thisDialog=null;
	private int btnWidth = 300;
	private int btnHeight = 20;
	private int labelGap = 20;
	private int btnGapX = 5;
	private int btnGapY = 5;
	private Button tempBtn, dewpBtn, parcelBtn, vTempBtn, wetBulbBtn, mixingRatioBtn, 
		dryAdiabatBtn,moisAdiabatBtn,omegaBtn, meanWindVectorBtn, stormMVector3075Btn, stormMVector1585Btn,
		stormMVectorBunkersRightBtn,stormMVectorBunkersLeftBtn, corfidiVectorBtn, hodoBtn;	
	//default value for button initial setup
	private  boolean temp=true, dewp=true, parcel=true, vTemp=true, wetBulb=true, hodo=true,
		mixratio=false, dryAdiabat=true, moistAdiabat=false, omega=true, meanWind=true, 
		smv3075=false, smv1585=false, smvBunkersR=true, smvBunkersL=true,corfidiV=false;

	
	public boolean isHodo() {
		return hodo;
	}


	public boolean isMeanWind() {
		return meanWind;
	}


	public boolean isSmv3075() {
		return smv3075;
	}


	public boolean isSmv1585() {
		return smv1585;
	}


	public boolean isSmvBunkersR() {
		return smvBunkersR;
	}


	public boolean isSmvBunkersL() {
		return smvBunkersL;
	}


	public boolean isCorfidiV() {
		return corfidiV;
	}


	public boolean isOmega() {
		return omega;
	}


	public boolean isDryAdiabat() {
		return dryAdiabat;
	}

	public boolean isMoistAdiabat() {
		return moistAdiabat;
	}

	public boolean isMixratio() {
		return mixratio;
	}

	public  boolean isTemp() {
		return temp;
	}

	public  boolean isDewp() {
		return dewp;
	}

	public  boolean isParcel() {
		return parcel;
	}

	public  boolean isVTemp() {
		return vTemp;
	}

	public  boolean isWetBulb() {
		return wetBulb;
	}

	public static NsharpGraphConfigDialog getInstance( Shell parShell){

		if ( thisDialog == null ){
			try {
				thisDialog = new NsharpGraphConfigDialog( parShell );
				
			} catch (VizException e) {
				e.printStackTrace();
			}

		}

		return thisDialog;

	}

	public static NsharpGraphConfigDialog getAccess() {
		return thisDialog;
	}

	protected NsharpGraphConfigDialog(Shell parentShell) throws VizException {
		super(parentShell);
		thisDialog = this;
	}
	private void createDialogContents(Composite parent){
		
		Group  btnGp = new Group(parent, SWT.SHADOW_ETCHED_IN | SWT.NO_RADIO_GROUP);

		tempBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		tempBtn.setText(NsharpNativeConstants.TEMP_TRACE);
		tempBtn.setEnabled( true );
		tempBtn.setBounds(btnGp.getBounds().x+ btnGapX, btnGp.getBounds().y + labelGap, btnWidth,btnHeight);
		if(temp == true)
			tempBtn.setSelection(true);
		else
			tempBtn.setSelection(false);
		tempBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) { 
				if(temp == true)
					temp=false;
				else
					temp=true;
			}          		            	 	
		} );  
		dewpBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		dewpBtn.setText(NsharpNativeConstants.DEWP_TRACE);
		dewpBtn.setEnabled( true );
		dewpBtn.setBounds(btnGp.getBounds().x+ btnGapX, tempBtn.getBounds().y + tempBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(dewp == true)
			dewpBtn.setSelection(true);
		else
			dewpBtn.setSelection(false);
		dewpBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(dewp == true)
					dewp=false;
				else
					dewp=true;
			}          		            	 	
		} );  
		parcelBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		parcelBtn.setText(NsharpNativeConstants.PARCEL_TRACE);
		parcelBtn.setEnabled( true );
		parcelBtn.setBounds(btnGp.getBounds().x+ btnGapX, dewpBtn.getBounds().y + dewpBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(parcel == true)
			parcelBtn.setSelection(true);
		else
			parcelBtn.setSelection(false);
		parcelBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				if(parcel == true)
					parcel=false;
				else
					parcel=true;
			}          		            	 	
		} );  
		vTempBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		vTempBtn.setText(NsharpNativeConstants.VTEMP_TRACE);
		vTempBtn.setEnabled( true );
		vTempBtn.setBounds(btnGp.getBounds().x+ btnGapX, parcelBtn.getBounds().y + parcelBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(vTemp == true)
			vTempBtn.setSelection(true);
		else
			vTempBtn.setSelection(false);
		vTempBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(vTemp == true)
					vTemp=false;
				else
					vTemp=true;
			}          		            	 	
		} );  
		
		wetBulbBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		wetBulbBtn.setText(NsharpNativeConstants.WETBULB_TRACE);
		wetBulbBtn.setEnabled( true );
		wetBulbBtn.setBounds(btnGp.getBounds().x+ btnGapX, vTempBtn.getBounds().y + vTempBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(wetBulb == true)
			wetBulbBtn.setSelection(true);
		else
			wetBulbBtn.setSelection(false);
		wetBulbBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(wetBulb == true)
					wetBulb=false;
				else
					wetBulb=true;
			}          		            	 	
		} );  

		mixingRatioBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		mixingRatioBtn.setText(NsharpNativeConstants.MIXING_RATIO);
		mixingRatioBtn.setEnabled( true );
		mixingRatioBtn.setBounds(btnGp.getBounds().x+ btnGapX, wetBulbBtn.getBounds().y + wetBulbBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(mixratio == true)
			mixingRatioBtn.setSelection(true);
		else
			mixingRatioBtn.setSelection(false);
		mixingRatioBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(mixratio == true)
					mixratio=false;
				else
					mixratio=true;
			}          		            	 	
		} );  
		dryAdiabatBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		dryAdiabatBtn.setText(NsharpNativeConstants.DRY_ADIABAT);
		dryAdiabatBtn.setEnabled( true );
		dryAdiabatBtn.setBounds(btnGp.getBounds().x+ btnGapX, mixingRatioBtn.getBounds().y + mixingRatioBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(dryAdiabat == true)
			dryAdiabatBtn.setSelection(true);
		else
			dryAdiabatBtn.setSelection(false);
		dryAdiabatBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(dryAdiabat == true)
					dryAdiabat=false;
				else
					dryAdiabat=true;
			}          		            	 	
		} );  

		moisAdiabatBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		moisAdiabatBtn.setText(NsharpNativeConstants.MOIST_ADIABAT);
		moisAdiabatBtn.setEnabled( true );
		moisAdiabatBtn.setBounds(btnGp.getBounds().x+ btnGapX, dryAdiabatBtn.getBounds().y + dryAdiabatBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(moistAdiabat == true)
			moisAdiabatBtn.setSelection(true);
		else
			moisAdiabatBtn.setSelection(false);
		moisAdiabatBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(moistAdiabat == true)
					moistAdiabat=false;
				else
					moistAdiabat=true;
			}          		            	 	
		} );  
		
		hodoBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		hodoBtn.setText(NsharpNativeConstants.HODOGRAPH);
		hodoBtn.setEnabled( true );
		hodoBtn.setBounds(btnGp.getBounds().x+ btnGapX, moisAdiabatBtn.getBounds().y + moisAdiabatBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(hodo == true)
			hodoBtn.setSelection(true);
		else
			hodoBtn.setSelection(false);
		hodoBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(hodo == true)
					hodo=false;
				else
					hodo=true;
			}          		            	 	
		} ); 
		meanWindVectorBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		meanWindVectorBtn.setText(NsharpNativeConstants.MEAN_WIND_VECTOR);
		meanWindVectorBtn.setEnabled( true );
		meanWindVectorBtn.setBounds(btnGp.getBounds().x+ btnGapX, hodoBtn.getBounds().y + hodoBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(meanWind == true)
			meanWindVectorBtn.setSelection(true);
		else
			meanWindVectorBtn.setSelection(false);
		meanWindVectorBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(meanWind == true)
					meanWind=false;
				else
					meanWind=true;
			}          		            	 	
		} ); 
		stormMVector3075Btn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		stormMVector3075Btn.setText(NsharpNativeConstants.STORM_MOTION_VECTOR_3075);
		stormMVector3075Btn.setEnabled( true );
		stormMVector3075Btn.setBounds(btnGp.getBounds().x+ btnGapX, meanWindVectorBtn.getBounds().y + meanWindVectorBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(smv3075 == true)
			stormMVector3075Btn.setSelection(true);
		else
			stormMVector3075Btn.setSelection(false);
		stormMVector3075Btn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(smv3075 == true)
					smv3075=false;
				else
					smv3075=true;
			}          		            	 	
		} );  
		stormMVector1585Btn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		stormMVector1585Btn.setText(NsharpNativeConstants.STORM_MOTION_VECTOR_1585);
		stormMVector1585Btn.setEnabled( true );
		stormMVector1585Btn.setBounds(btnGp.getBounds().x+ btnGapX, stormMVector3075Btn.getBounds().y + stormMVector3075Btn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(smv1585 == true)
			stormMVector1585Btn.setSelection(true);
		else
			stormMVector1585Btn.setSelection(false);
		stormMVector1585Btn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(smv1585 == true)
					smv1585=false;
				else
					smv1585=true;
			}          		            	 	
		} );  
		stormMVectorBunkersRightBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		stormMVectorBunkersRightBtn.setText(NsharpNativeConstants.STORM_MOTION_VECTOR_BUNKERS_R);
		stormMVectorBunkersRightBtn.setEnabled( true );
		stormMVectorBunkersRightBtn.setBounds(btnGp.getBounds().x+ btnGapX, stormMVector1585Btn.getBounds().y + stormMVector1585Btn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(smvBunkersR == true)
			stormMVectorBunkersRightBtn.setSelection(true);
		else
			stormMVectorBunkersRightBtn.setSelection(false);
		stormMVectorBunkersRightBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(smvBunkersR == true)
					smvBunkersR=false;
				else
					smvBunkersR=true;
			}          		            	 	
		} );  
		stormMVectorBunkersLeftBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		stormMVectorBunkersLeftBtn.setText(NsharpNativeConstants.STORM_MOTION_VECTOR_BUNKERS_L);
		stormMVectorBunkersLeftBtn.setEnabled( true );
		stormMVectorBunkersLeftBtn.setBounds(btnGp.getBounds().x+ btnGapX, stormMVectorBunkersRightBtn.getBounds().y + stormMVectorBunkersRightBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(smvBunkersL == true)
			stormMVectorBunkersLeftBtn.setSelection(true);
		else
			stormMVectorBunkersLeftBtn.setSelection(false);
		stormMVectorBunkersLeftBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(smvBunkersL == true)
					smvBunkersL=false;
				else
					smvBunkersL=true;
			}          		            	 	
		} );  
		
		corfidiVectorBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		corfidiVectorBtn.setText(NsharpNativeConstants.CORFIDI_VECTORS);
		corfidiVectorBtn.setEnabled( true );
		corfidiVectorBtn.setBounds(btnGp.getBounds().x+ btnGapX, stormMVectorBunkersLeftBtn.getBounds().y + stormMVectorBunkersLeftBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(corfidiV == true)
			corfidiVectorBtn.setSelection(true);
		else
			corfidiVectorBtn.setSelection(false);
		corfidiVectorBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(corfidiV == true)
					corfidiV=false;
				else
					corfidiV=true;
			}          		            	 	
		} );  

		if(NsharpLoadDialog.getAccess()!= null && 
				   (NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.MODEL_SND ||
						   NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.PFC_SND )){
			omegaBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
			omegaBtn.setText(NsharpNativeConstants.OMEGA);
			omegaBtn.setEnabled( true );
			omegaBtn.setBounds(btnGp.getBounds().x+ btnGapX, corfidiVectorBtn.getBounds().y + corfidiVectorBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
			if(omega == true)
				omegaBtn.setSelection(true);
			else
				omegaBtn.setSelection(false);
			omegaBtn.addListener( SWT.MouseUp, new Listener() {
				public void handleEvent(Event event) {    
					if(omega == true)
						omega=false;
					else
						omega=true;
				}          		            	 	
			} );  
		}

	}
	@Override
	public void createButtonsForButtonBar(Composite parent) {
		// create OK and Cancel buttons by default
		
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,
				true);
	}


	@Override
	public void okPressed() {
		//System.out.println("OK is pressed");
		NsharpSkewTEditor editor = NsharpSkewTEditor.getActiveNsharpEditor();
        if (editor != null) {
        	NsharpSkewTResource rsc = editor.getNsharpSkewTDescriptor().getSkewtResource();
        	rsc.createRscPressTempCurveShapeAll();
        	editor.refresh();
        }
		close();
	}
	
	/*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
       */
    @Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        shell.setText( "Graphs Configuration" );
        
    }
	@Override
	public Control createDialogArea(Composite parent) {
		Composite top;
		top = (Composite) super.createDialogArea(parent);

		// Create the main layout for the shell.
		GridLayout mainLayout = new GridLayout(1, false);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;
		top.setLayout(mainLayout);

		// Initialize all of the menus, controls, and layouts
		createDialogContents(top);

		return top;
	}   

	
	@Override
    public int open( ) {
        if ( this.getShell() == null ){
			this.create();
		}
   	    this.getShell().setLocation(this.getShell().getParent().getLocation().x+1100,
   	    		this.getShell().getParent().getLocation().y+200);
   	    return super.open();
    	
    }
	@Override
	public boolean close() {
		tempBtn= dewpBtn= parcelBtn= vTempBtn= wetBulbBtn= mixingRatioBtn= 
		dryAdiabatBtn=moisAdiabatBtn=omegaBtn= meanWindVectorBtn= stormMVector3075Btn= stormMVector1585Btn=
		stormMVectorBunkersRightBtn=stormMVectorBunkersLeftBtn= corfidiVectorBtn= hodoBtn=null;
		return (super.close());
    }


}
