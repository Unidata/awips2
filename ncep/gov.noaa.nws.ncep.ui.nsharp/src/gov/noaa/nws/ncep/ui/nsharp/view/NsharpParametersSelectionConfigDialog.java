package gov.noaa.nws.ncep.ui.nsharp.view;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpParametersSelectionConfigDialog
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

import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpLoadDialog;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;

public class NsharpParametersSelectionConfigDialog extends Dialog {
	private static NsharpParametersSelectionConfigDialog thisDialog=null;
	private NsharpConfigStore configStore=null;
	private NsharpGraphProperty graphProperty=null;
	private NsharpConfigManager mgr;
	private int btnWidth = 300;
	private int btnHeight = 20;
	private int labelGap = 20;
	private int btnGapX = 5;
	private int btnGapY = 5;
	private Button tempBtn, dewpBtn, parcelTvBtn,parcelBtn, dcapeBtn,vTempBtn, wetBulbBtn, mixingRatioBtn, 
		dryAdiabatBtn,moisAdiabatBtn,omegaBtn, meanWindVectorBtn, stormMVector3075Btn, stormMVector1585Btn,
		stormMVectorBunkersRightBtn,stormMVectorBunkersLeftBtn, corfidiVectorBtn, hodoBtn, efflayerBtn, cloudBtn, windBarbBtn;	
	private Text tempOffsetText;
	//default value for button initial setup
	private  boolean temp=true, dewp=true, parcel=true,parcelTv=true,dcape=true, vTemp=true, wetBulb=true, hodo=true,
		mixratio=false, dryAdiabat=true, moistAdiabat=false, omega=true, meanWind=true, 
		smv3075=false, smv1585=false, smvBunkersR=true, smvBunkersL=true,corfidiV=false, effLayer=true, cloud=false, windBarb=true;
	//private int windBarbDistance=NsharpNativeConstants.WINDBARB_DISTANCE_DEFAULT;
	private int tempOffset = 0;
	private void updateGraphProperty(){
		if(graphProperty != null){
			graphProperty.setTemp(temp);
			graphProperty.setDewp(dewp);
			graphProperty.setParcel(parcel);
			graphProperty.setParcelTv(parcelTv);
			graphProperty.setDcape(dcape);
			graphProperty.setVTemp(vTemp);
			graphProperty.setWetBulb(wetBulb);
			graphProperty.setHodo(hodo);
			graphProperty.setMixratio(mixratio);
			graphProperty.setDryAdiabat(dryAdiabat);
			graphProperty.setMoistAdiabat(moistAdiabat);
			graphProperty.setOmega(omega);
			graphProperty.setMeanWind(meanWind);
			graphProperty.setSmv3075(smv3075);
			graphProperty.setSmv1585(smv1585);
			graphProperty.setSmvBunkersR(smvBunkersR);
			graphProperty.setSmvBunkersL(smvBunkersL);
			graphProperty.setCloud(cloud);
			graphProperty.setCorfidiV(corfidiV);
			graphProperty.setEffLayer(effLayer);
			graphProperty.setWindBarb(windBarb);
			//graphProperty.setWindBarbDistance(windBarbDistance);
			graphProperty.setTempOffset(tempOffset);
		}
	}
	
	public boolean isDcape() {
		return dcape;
	}

	public boolean isCloud() {
		return cloud;
	}


	public boolean isEffLayer() {
		return effLayer;
	}


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

	public boolean isParcelAscent() {
		return parcelTv;
	}
	
	public  boolean isVTemp() {
		return vTemp;
	}

	public  boolean isWetBulb() {
		return wetBulb;
	}
	

	public boolean isWindBarb() {
		return windBarb;
	}


	//public int getWindBarbDistance() {
	//	return windBarbDistance;
	//}


	public static NsharpParametersSelectionConfigDialog getInstance( Shell parShell){

		if ( thisDialog == null ){
			try {
				thisDialog = new NsharpParametersSelectionConfigDialog( parShell );
				
			} catch (VizException e) {
				e.printStackTrace();
			}

		}

		return thisDialog;

	}

	public static NsharpParametersSelectionConfigDialog getAccess() {
		return thisDialog;
	}
	
	public NsharpParametersSelectionConfigDialog(Shell parentShell) throws VizException {
		super(parentShell);
		thisDialog = this;
		mgr =NsharpConfigManager.getInstance();
		configStore = mgr.retrieveNsharpConfigStoreFromFs();
		//if(configStore== null){
		//	configStore = new NsharpConfigStore();
        //	configStore = setDefaultGraphConfig(configStore);
        //	configStore=NsharpDataDisplayConfigDialog.setDefaultLineConfig(configStore);
		//}		
		graphProperty = configStore.getGraphProperty();
		if(graphProperty != null){
			temp=graphProperty.isTemp();
			dewp=graphProperty.isDewp();
			parcel=graphProperty.isParcel();
			parcelTv=graphProperty.isParcelTv();
			dcape = graphProperty.isDcape();
			vTemp=graphProperty.isVTemp();
			wetBulb=graphProperty.isWetBulb();
			hodo=graphProperty.isHodo();
			mixratio=graphProperty.isMixratio();
			dryAdiabat=graphProperty.isDryAdiabat();
			moistAdiabat=graphProperty.isMoistAdiabat(); 
			omega=graphProperty.isOmega();
			meanWind=graphProperty.isMeanWind();
			smv3075=graphProperty.isSmv3075();
			smv1585=graphProperty.isSmv1585();
			smvBunkersR=graphProperty.isSmvBunkersR(); 
			smvBunkersL=graphProperty.isSmvBunkersL();
			corfidiV=graphProperty.isCorfidiV(); 
			effLayer=graphProperty.isEffLayer();
			cloud=graphProperty.isCloud();
			windBarb=graphProperty.isWindBarb();
			//windBarbDistance = graphProperty.getWindBarbDistance();
			tempOffset = graphProperty.getTempOffset();
		}
		
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
				applyChange();
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
				applyChange();
			}          		            	 	
		} );  
		parcelTvBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		parcelTvBtn.setText(NsharpNativeConstants.PARCEL_VT_TRACE);
		parcelTvBtn.setEnabled( true );
		parcelTvBtn.setBounds(btnGp.getBounds().x+ btnGapX, dewpBtn.getBounds().y + dewpBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(parcelTv == true)
			parcelTvBtn.setSelection(true);
		else
			parcelTvBtn.setSelection(false);
		parcelTvBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				if(parcelTv == true)
					parcelTv=false;
				else
					parcelTv=true;
				applyChange();
			}          		            	 	
		} );  
		parcelBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		parcelBtn.setText(NsharpNativeConstants.PARCEL_T_TRACE);
		parcelBtn.setEnabled( true );
		parcelBtn.setBounds(btnGp.getBounds().x+ btnGapX, parcelTvBtn.getBounds().y + parcelTvBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
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
				applyChange();
			}          		            	 	
		} ); 
		dcapeBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		dcapeBtn.setText(NsharpNativeConstants.DCAPE_TRACE);
		dcapeBtn.setEnabled( true );
		dcapeBtn.setBounds(btnGp.getBounds().x+ btnGapX, parcelBtn.getBounds().y + parcelBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(dcape == true)
			dcapeBtn.setSelection(true);
		else
			dcapeBtn.setSelection(false);
		dcapeBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(dcape == true)
					dcape=false;
				else
					dcape=true;
				applyChange();
			}          		            	 	
		} );  
		
		vTempBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		vTempBtn.setText(NsharpNativeConstants.VTEMP_TRACE);
		vTempBtn.setEnabled( true );
		vTempBtn.setBounds(btnGp.getBounds().x+ btnGapX, dcapeBtn.getBounds().y + dcapeBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
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
				applyChange();
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
				applyChange();
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
				applyChange();
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
				applyChange();
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
				applyChange();
			}          		            	 	
		} );  
		efflayerBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		efflayerBtn.setText(NsharpNativeConstants.EFFECTIVE_LAYER);
		efflayerBtn.setEnabled( true );
		efflayerBtn.setBounds(btnGp.getBounds().x+ btnGapX, moisAdiabatBtn.getBounds().y + moisAdiabatBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(effLayer == true)
			efflayerBtn.setSelection(true);
		else
			efflayerBtn.setSelection(false);
		efflayerBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(effLayer == true)
					effLayer=false;
				else
					effLayer=true;
				applyChange();
			}          		            	 	
		} ); 
		cloudBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		cloudBtn.setText(NsharpNativeConstants.CLOUD);
		cloudBtn.setEnabled( true );
		cloudBtn.setBounds(btnGp.getBounds().x+ btnGapX, efflayerBtn.getBounds().y + efflayerBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(cloud == true)
			cloudBtn.setSelection(true);
		else
			cloudBtn.setSelection(false);
		cloudBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				if(cloud == true)
					cloud=false;
				else
					cloud=true;
				applyChange();
			}          		            	 	
		} ); 
		hodoBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
		hodoBtn.setText(NsharpNativeConstants.HODOGRAPH);
		hodoBtn.setEnabled( true );
		hodoBtn.setBounds(btnGp.getBounds().x+ btnGapX, cloudBtn.getBounds().y + cloudBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
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
				applyChange();
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
				applyChange();
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
				applyChange();
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
				applyChange();
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
				applyChange();
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
				applyChange();
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
				applyChange();
			}          		            	 	
		} );  
		windBarbBtn= new Button(btnGp, SWT.RADIO | SWT.BORDER);
		windBarbBtn.setText(NsharpNativeConstants.WINDBARB);
		windBarbBtn.setEnabled(true);
		windBarbBtn.setBounds(btnGp.getBounds().x+ btnGapX, corfidiVectorBtn.getBounds().y + corfidiVectorBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		if(windBarb == true)
			windBarbBtn.setSelection(true);
		else
			windBarbBtn.setSelection(false);
		windBarbBtn.addListener(SWT.MouseUp, new Listener() {
				public void handleEvent(Event event) {    
					if(windBarb == true)
						windBarb=false;
					else
						windBarb=true;
					applyChange();
				}          		            	 	
			} );  
		/*
		windBarbText = new Text(btnGp, SWT.BORDER | SWT.SINGLE);
		windBarbText.setText(Integer.toString(windBarbDistance));
		windBarbText.setBounds(windBarbBtn.getBounds().x+windBarbBtn.getBounds().width, windBarbBtn.getBounds().y,btnWidth/4,btnHeight);
		windBarbText.setEditable(true);
		windBarbText.setVisible(true); 
		//to make sure user enter digits only
		windBarbText.addListener (SWT.Verify, new Listener () {
			public void handleEvent (Event e) {
				String string = e.text;
				char [] chars = new char [string.length ()];
				string.getChars (0, chars.length, chars, 0);
				//System.out.println("entered "+ string);
				
				for (int i=0; i<chars.length; i++) {
					if (!('0' <= chars [i] && chars [i] <= '9')) {
						e.doit = false;
						return;
					}
				}
				
			}
		});*/
		Label tempOffsetLbl = new Label(btnGp, SWT.BORDER );
		tempOffsetLbl.setText("Temperature Range Offset ");
		tempOffsetLbl.setBounds(btnGp.getBounds().x+ btnGapX, windBarbBtn.getBounds().y + windBarbBtn.getBounds().height+ btnGapY, btnWidth,btnHeight);
		tempOffsetText = new Text(btnGp, SWT.BORDER | SWT.SINGLE);
		tempOffsetText.setText(Integer.toString(tempOffset));
		tempOffsetText.setBounds(tempOffsetLbl.getBounds().x+tempOffsetLbl.getBounds().width, tempOffsetLbl.getBounds().y,btnWidth/4,btnHeight);
		tempOffsetText.setEditable(true);
		tempOffsetText.setVisible(true); 
		//to make sure user enter digits only
		tempOffsetText.addListener (SWT.Verify, new Listener () {
			public void handleEvent (Event e) {
				String string = e.text;
				char [] chars = new char [string.length ()];
				string.getChars (0, chars.length, chars, 0);
				//System.out.println("entered s="+ string);
				//Chin note: when "Delete", "Backspace" entered, this handler will be called, but
				// its chars.length = 0
				if(chars.length == 1){
					if(chars [0] == '-' ){
						//if "-" is not first char
						String textStr = tempOffsetText.getText();
						if((textStr != null) && (textStr.length() >= 1) && (textStr.contains("-"))){
							e.doit = false;
							return;
						}
					}else if (!('0' <= chars [0] && chars [0] <= '9')) {
						e.doit = false;
						return;
					}

				}
			}
		});
		if(NsharpLoadDialog.getAccess()!= null && 
				   (NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.MODEL_SND ||
						   NsharpLoadDialog.getAccess().getActiveLoadSoundingType()== NsharpLoadDialog.PFC_SND )){
			omegaBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
			omegaBtn.setText(NsharpNativeConstants.OMEGA);
			omegaBtn.setEnabled( true );
			omegaBtn.setBounds(btnGp.getBounds().x+ btnGapX, tempOffsetLbl.getBounds().y + tempOffsetLbl.getBounds().height+ btnGapY, btnWidth,btnHeight);
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
					applyChange();
				}          		            	 	
			} );  
		}

	}
	
	@Override
	public void createButtonsForButtonBar(Composite parent) {
		// create OK button but using Apply label for applying user entered data
		//Chin note: when "apply" button is selected or Return key is entered, 
		// okPressed() will be called. So, handle event at one place, ie.e at okPressed(). 
		createButton(parent, IDialogConstants.OK_ID,
				"Apply",
				true);
		
		/*appBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				//System.out.println("App listener is called");
				//String textStr = windBarbText.getText();
				//if((textStr != null) && !(textStr.isEmpty())){
				//	windBarbDistance = Integer.decode(textStr);
				//}
				//applyChange();
				
			}          		            	 	
		} );  */
		Button saveBtn = createButton(parent, IDialogConstants.INTERNAL_ID,
				"Save",false);
		saveBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {  
				//System.out.println("save listener is called, also apply changes");
				okPressed();
				try {
		        	//save to xml
					mgr.saveConfigStoreToFs(configStore);
				} catch (VizException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		        
				
			}          		            	 	
		} );  

		Button canBtn = createButton(parent, IDialogConstants.CLOSE_ID,
				IDialogConstants.CLOSE_LABEL, false);
		canBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {    
				//System.out.println("close listener is called");
				close();
			}          		            	 	
		} );  
	}


	@Override
	public void okPressed() {
		//"Enter" key is pressed, or "Apply" button is pressed.
		//Chin: handle user entered data and apply its changes.
		//System.out.println("CR is pressed");
		//String textStr = windBarbText.getText();
		//if((textStr != null) && !(textStr.isEmpty())){
		//	windBarbDistance = Integer.decode(textStr);
		//}
		String textStr = tempOffsetText.getText();
		if((textStr != null) && !(textStr.isEmpty())){
			if(!textStr.contains("-") || textStr.length() > 1){
				tempOffset = Integer.decode(textStr);
				//System.out.println("temp offset is ="+tempOffset);
			}
		}
		applyChange();
		setReturnCode(OK);
	}
	
	/*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
       */
    @Override   
    protected void configureShell( Shell shell ) {
        super.configureShell( shell );       
        shell.setText( "Nsharp Parameters Selection" );
        
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
		tempBtn= dewpBtn= parcelTvBtn= parcelBtn=dcapeBtn=vTempBtn= wetBulbBtn= mixingRatioBtn= 
		dryAdiabatBtn=moisAdiabatBtn=efflayerBtn=omegaBtn= meanWindVectorBtn= stormMVector3075Btn= stormMVector1585Btn=
		stormMVectorBunkersRightBtn=stormMVectorBunkersLeftBtn= corfidiVectorBtn= hodoBtn=windBarbBtn=null;
		//thisDialog= null;
		return (super.close());
    }

	public void reset(){
		//windBarbDistance=NsharpNativeConstants.WINDBARB_DISTANCE_DEFAULT;
		tempOffset=0;
	}

	private void applyChange(){
		updateGraphProperty();
		NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
        if (editor != null) {
        	NsharpResourceHandler rsc = editor.getRscHandler();
        	rsc.setGraphConfigProperty(graphProperty);
         	editor.refresh();
        }
	}
	
}
