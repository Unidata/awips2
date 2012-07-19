package gov.noaa.nws.ncep.ui.nsharp.skewt.rsc;


import java.util.List;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._lplvalues;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._parcel;
import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpParcelDialog;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor;
import org.eclipse.swt.graphics.Rectangle;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.LineStroke;
import com.raytheon.viz.core.graphing.WindBarbFactory;
import com.sun.jna.ptr.FloatByReference;
import com.vividsolutions.jts.geom.Coordinate;



public class NsharpDrawPanels {
	//private NsharpNative nsharpNative = NsharpNative.getInstance();
	private static final String NO_DATA = "NO VALID DATA AVAILABLE";
	private double charHeight = 40;
	private double curY;
	private double parcelLineYStart, parcelLineYEnd;
	private double firstToken, secondToken, thirdToken, forthToken, fifthToken, sixthToken;
	private FloatByReference fValue= new FloatByReference(0);
	private FloatByReference fValue1= new FloatByReference(0);
	private FloatByReference fValue2= new FloatByReference(0);
	private FloatByReference fValue3= new FloatByReference(0);	
	private FloatByReference fValue4= new FloatByReference(0);
	private FloatByReference fValue5= new FloatByReference(0);
	private FloatByReference fValue6= new FloatByReference(0);
	private FloatByReference fValue7= new FloatByReference(0);	
	private FloatByReference fValue8= new FloatByReference(0);
	private PixelExtent extent;
	private NsharpBackgroundResource bkRsc;
	private NsharpSkewTResource rsc;
	// physical number of panel in editor display
	
	// total software defined panels to be displayed 
	private final int virtualPanelSize = 10;
	private Rectangle[] panelRectArray= new Rectangle[NsharpConstants.dsiplayPanelSize] ;
	private IFont myFont;
	
	//private String fontName = "Monospace";
	//private int fontSize =11;
	private NsharpNative nsharpNative=null;
	private short  currentParcel= NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;;
	private static NsharpDrawPanels instance=null;
	
	
	public void setMyFont(IFont myFont) {
		this.myFont = myFont;
	}

	public static NsharpDrawPanels getInstance(){
		return instance;
	}
	
	public void setCurrentParcel(short currentParcel) {
		this.currentParcel = currentParcel;
	}

	public short getCurrentParcel() {
		return currentParcel;
	}
	public void resetCurrentParcel() {
		currentParcel= NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
	}

	/*class ShapeAndColor {
		IWireframeShape shape;
		RGB color;
	}
	//dynamic resource shapes
	private List<ShapeAndColor>panel1ShapeList  = new ArrayList<ShapeAndColor>();
	
	public void disposeShapes(){
		if(panel1ShapeList.size()>0){
			for(ShapeAndColor shapeColor: panel1ShapeList){
				shapeColor.shape.dispose();
			}
			panel1ShapeList.clear();
		}
		//if(myFont != null) {
		//    myFont.dispose();
		//    myFont = null;
		//}
	}
	public void createShape(IGraphicsTarget target, int virtualPanelNumber, int dsiplayPanelNumber){
		if(virtualPanelNumber > virtualPanelSize || dsiplayPanelNumber > NsharpConstants.dsiplayPanelSize)
			return;
		int physicalPanelNumber = dsiplayPanelNumber -1;
		NsharpSkewTDescriptor desc = bkRsc.getDescriptor();
		if( desc != null) {
			NsharpSkewTResource rsc = desc.getSkewtResource();
			if(rsc != null)
				nsharpNative = rsc.getNsharpNative();
		}
		if(nsharpNative == null)
			return;
		switch(virtualPanelNumber){
		case 1:
			createPanel1Shape(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 2:
			createPanel1Shape(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 3:
			createPanel1Shape(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 4:
			createPanel1Shape(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 5:
			createPanel1Shape(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 6:
			createPanel1Shape(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 7:
			createPanel1Shape(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 8:
			createPanel1Shape(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 9:
			createPanel1Shape(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 10:
			createPanel1Shape(target,  panelRectArray[physicalPanelNumber]);
			break;
		default:
			break;
		}
	}
	*/
	/*private void createPanel1Shape(IGraphicsTarget target, Rectangle rect){
  		 
   		 * Chin's NOTE::::
   		 * Use wire frame shape for drawing string (lable) cause a problem that
   		 * it can not put strings in line properly. Therefore, I dont use
   		 * it for draw panel for now. Until there is a better solution to 
   		 * draw stings in line.
   		 * 
   		 * This pages based on newer version nsharp from SPC. We dont have source code as of 5/24/2011.
   		 * Therefore, coding is purly based on a captured screen shot given by SPC's John Hart. 
   		 * This function is coded based on native nsharp codes which can be found 
   		 * in other pages's show functions.
   		 *
   		 *
   		 *
		ShapeAndColor shNcolor = new ShapeAndColor();
		IWireframeShape shapeR = shNcolor.shape = target.createWireframeShape(false,bkRsc.getDescriptor() );
        shNcolor.color = NsharpConstants.color_red;
        panel1ShapeList.add(shNcolor);
        shNcolor = new ShapeAndColor();
        IWireframeShape shapeW= shNcolor.shape = target.createWireframeShape(false,bkRsc.getDescriptor() );
        shNcolor.color = NsharpConstants.color_white;
        panel1ShapeList.add(shNcolor);
        shNcolor = new ShapeAndColor();
        IWireframeShape shapeY= shNcolor.shape = target.createWireframeShape(false,bkRsc.getDescriptor() );
        shNcolor.color = NsharpConstants.color_yellow;
        panel1ShapeList.add(shNcolor);
   		//if we can not Interpolates a temp with 700 mb pressure, then we dont have enough raw data 	
   		if ((nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.itemp(700.0F)) == 0))
   		{
   			double [] lblXy = {rect.x, rect.y};
   			shapeR.addLabel("               " +NO_DATA, lblXy);
   			shapeR.compile();
   			shapeW.compile();
   			shapeY.compile();
   			return;
   		}
   		
   		String textStr;
   		curY=rect.y;
   		
   		//
   		// Start with Parcel Data
   		// 
   		double widthGap = (rect.width-200)/6;
   		xbegin = rect.x + 80;
   		firstToken=xbegin+120.0;
   		secondToken=firstToken+widthGap;
   		thirdToken=secondToken+widthGap;
   		forthToken=thirdToken+widthGap;
   		fifthToken = forthToken+widthGap;
   		sixthToken = fifthToken+widthGap;
   		curY = curY+charHeight;
   		double [] lblXy = {firstToken,curY};
		shapeW.addLabel("CAPE", lblXy);
		double [] lblXy1 = {secondToken,curY};
		shapeW.addLabel("CINH", lblXy1);
   		double [] lblXy2 = {thirdToken,curY};
		shapeW.addLabel("LCL", lblXy2);
		double [] lblXy3 = {forthToken,curY};
		shapeW.addLabel("LI", lblXy3);
		double [] lblXy4 = {fifthToken,curY};
		shapeW.addLabel("LFC", lblXy4);
		double [] lblXy5 = {sixthToken,curY};
		shapeW.addLabel("EL", lblXy5);
   	    
   	    double [][] lines = {{rect.x, curY},{rect.x+rect.width, curY}};
		shapeW.addLineSegment(lines);
   		float layerPressure = 0; 
   		short  currentParcel;
   			
   		for (short parcelNumber=1; parcelNumber <= NsharpNativeConstants.PARCEL_MAX ; parcelNumber++){
   			curY = curY+charHeight;
   			//call native define_parcel() with parcel type and user defined pressure (if user defined it)
   			textStr = NsharpNativeConstants.parcelToTypeStrMap.get(parcelNumber);
   			
   			//draw parcel name
			double [] lblXy6 = {xbegin,curY};
   			shapeW.addLabel(textStr, lblXy6);
   			{
   				layerPressure = NsharpNativeConstants.parcelToLayerMap.get(parcelNumber);
   				if(parcelNumber == NsharpNativeConstants.PARCELTYPE_USER_DEFINED){
   					//get user selected parcel type, if available
   			   		if(NsharpParcelDialog.getAccess() != null){
   			   			layerPressure = NsharpParcelDialog.getAccess().getUserDefdParcelMb();   			   			
   			   		}
   				}
   				
   				nsharpNative.nsharpLib.define_parcel(parcelNumber,layerPressure);

   				_lplvalues lpvls = new _lplvalues();
   				nsharpNative.nsharpLib.get_lpvaluesData(lpvls);

   				float sfctemp, sfcdwpt, sfcpres;
   				sfctemp = lpvls.temp;
   				sfcdwpt = lpvls.dwpt;
   				sfcpres = lpvls.pres;
   				// get parcel data by calling native nsharp parcel() API. value is returned in pcl 
   				_parcel pcl = new _parcel();
   				nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);
   				//draw CAPE
   				if(pcl.bplus != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA){
   					double [] lblXy7 = {firstToken,curY};
   	   				shapeW.addLabel(String.format( "%.0f", pcl.bplus), lblXy7);
   				}
   				else {
   					double [] lblXy7 = {firstToken,curY};
	   				shapeW.addLabel("M", lblXy7);
				}
   					
   				//draw CINH
   				if(pcl.bminus != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA){
   					double [] lblXy7 = {secondToken,curY};
   	   				shapeW.addLabel(String.format( "%.0f", pcl.bminus), lblXy7);
   				}   					
   				else {
   					double [] lblXy7 = {secondToken,curY};
   	   				shapeW.addLabel("M", lblXy7);
   				}
   				//draw LCL
   				float lcl = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lclpres ));
   				if(lcl != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA){
   					double [] lblXy7 = {thirdToken,curY};
   	   				shapeW.addLabel(String.format("%.0fm", lcl), lblXy7);
   				}   					
   				else {
   					double [] lblXy7 = {thirdToken,curY};
   	   				shapeW.addLabel("M", lblXy7);
   				}
   				//draw LI
   				if(pcl.li5 != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA){
   					double [] lblXy7 = {forthToken,curY};
   	   				shapeW.addLabel(String.format("%.0f", pcl.li5), lblXy7);
   				}   					
   				else {
   					double [] lblXy7 = {forthToken,curY};
   	   				shapeW.addLabel("M", lblXy7);
   				}
   				//draw LFC
   				float lfc = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lfcpres ));
   				if(lfc != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA){
   					double [] lblXy7 = {fifthToken,curY};
   	   				shapeW.addLabel(String.format("%.0fm", lfc), lblXy7);
   				}   					
   				else {
   					double [] lblXy7 = {fifthToken,curY};
   	   				shapeW.addLabel("M", lblXy7);
   				}
    				// draw EL
   				float el = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.elpres ));
   				if(el != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA){
   					double [] lblXy7 = {sixthToken,curY};
   	   				shapeW.addLabel(String.format("%.0fm", el), lblXy7);
   				}   					
   				else {
   					double [] lblXy7 = {sixthToken,curY};
   	   				shapeW.addLabel("M", lblXy7);
   				}
   			}
   		}
   		double [][] lines1 = {{rect.x, curY},{rect.x+rect.width, curY}};
   		shapeW.addLineSegment(lines1);
   		curY = curY+charHeight; //move to new line
   		//reset and define current parcel
   		if(NsharpParcelDialog.getAccess() != null){

   			currentParcel = NsharpParcelDialog.getAccess().getCurrentParcel();
   			if(currentParcel == NsharpNativeConstants.PARCELTYPE_USER_DEFINED)
   				layerPressure = NsharpParcelDialog.getAccess().getUserDefdParcelMb();
   			else
   				layerPressure = NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
   		}
   		else {//default - use mU
   			currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
   			layerPressure = NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
   		}
   		nsharpNative.nsharpLib.define_parcel(currentParcel,layerPressure);
   		//
   		// THERMO DYNAMIC DATA
   		// 
   		firstToken = xbegin + rect.width/3;
   		secondToken = firstToken+ rect.width/4;
   		thirdToken = secondToken + rect.width/4;
   		fValue.setValue(0);
   		nsharpNative.nsharpLib.precip_water(fValue, -1.0F, -1.0F);
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
   			textStr = String.format("PW=%.2f in", fValue.getValue());
   		}
   		else {
   			textStr = "PW= M";
   		}
   		{
   			double [] lblXy7 = {xbegin,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}

   		//dont know what is 3CAPE...
   		{
   			textStr = "3CAPE= M";
   			double [] lblXy7 = {firstToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}
  		
   		fValue.setValue(0);
   		float wbzft = nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.wb_lvl( 0, fValue ))));
   		if(nsharpNative.nsharpLib.qc(wbzft)==1) {
   			textStr = String.format("WBZ=%.0f'",wbzft);
   		}
   		else {
   			textStr = "WBZ= M";
   		}
   		{
   			double [] lblXy7 = {secondToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		} 		
   		{//dont know what is WNDG...
   			textStr = "WNDG= M";
   			double [] lblXy7 = {thirdToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}
		
		curY = curY+charHeight; //move to new line

   		fValue.setValue(0);
   		nsharpNative.nsharpLib.k_index(fValue);
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
   			textStr = String.format("K=%.0f",fValue.getValue());
   		}
   		else {
   			textStr = "K= M";
   		}	
   		{
   			double [] lblXy7 = {xbegin,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}
   		//dont know what is DCAPE...
   		{
   			textStr = "DCAPE= M";
   			double [] lblXy7 = {firstToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}

   		fValue.setValue(0);
   		float fgzft = nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.temp_lvl( 0, fValue ))));
   		if(nsharpNative.nsharpLib.qc(fgzft)==1) {
   			textStr = String.format("FZL=%.0f'",fgzft);
   		}
   		else {
   			textStr = "FZL= M";
   		}
   		{
   			double [] lblXy7 = {secondToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		} 
   		{//dont know what is ESP...
   			textStr = "ESP= M";
   			double [] lblXy7 = {thirdToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}
   		
   		
		curY = curY+charHeight; //move to new line
   		
   		fValue.setValue(0);
   		nsharpNative.nsharpLib.mean_relhum(fValue, -1.0F, -1.0F);
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
   			textStr = String.format("MidRH=%.1f %c",fValue.getValue(),NsharpConstants.PERCENT_SYMBOL);
   		}
   		else {
   			textStr = "MidRH = M";
   		}
   		{
   			double [] lblXy7 = {xbegin,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}
   		{//dont know what is DownT...
   			textStr = "DownT= M";
   			double [] lblXy7 = {firstToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}
   		   		fValue.setValue(0);
   		float conTempF = nsharpNative.nsharpLib.ctof(nsharpNative.nsharpLib.cnvtv_temp( fValue, -50));
   		
   		if(nsharpNative.nsharpLib.qc( conTempF )==1) {
   			textStr = String.format("ConvT=%.0fF",conTempF);
   		}
   		else {
   			textStr = "ConvT = M";
   		}
   		{
   			double [] lblXy7 = {secondToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		} 
   		{//dont know what is MMP...
   			textStr = "MMP= M";
   			double [] lblXy7 = {thirdToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}

   		
   		curY = curY+charHeight; //move to new line

   		fValue.setValue(0);
   		fValue1.setValue(0);
   		// get surface pressure (fValue1) before getting mean LRH value
   		nsharpNative.nsharpLib.get_surface(fValue1, fValue2, fValue3); //fValue 2 and fValue3 are not of concern here
   		nsharpNative.nsharpLib.mean_relhum( fValue, -1.0F, fValue1.getValue() - 150 );
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
   			textStr = NsharpNativeConstants.THERMO_MEANLRH_LINE;
   			textStr = String.format("LowRH=%.1f %c",fValue.getValue(),NsharpConstants.PERCENT_SYMBOL);
   		}
   		else {
   			textStr = "LowRH = M";
   		}
   		{
   			double [] lblXy7 = {xbegin,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}   		
   		fValue.setValue(0);
   		nsharpNative.nsharpLib.mean_mixratio(fValue, -1.0F, -1.0F);
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
   			textStr = String.format("MeanW=%.1f g/Kg",fValue.getValue());
   		}
   		else {
   			textStr = "MeanW = M";
   		}
   		{
   			double [] lblXy7 = {firstToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}		
   		fValue.setValue(0);
   		float maxT= nsharpNative.nsharpLib.ctof(nsharpNative.nsharpLib.max_temp( fValue, -1));
   		if(nsharpNative.nsharpLib.qc(maxT)==1) {
   			textStr = String.format("MaxT=%.0fF",maxT);
   		}
   		else {
   			textStr = "MaxT= M";
   		}
   		{
   			double [] lblXy7 = {secondToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}
   		{//dont know what is NCAPE...
   			textStr = "NCAPE= M";
   			double [] lblXy7 = {thirdToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}

   		
   		double [][] lines2 = {{rect.x, curY},{rect.x+rect.width, curY}};
   		shapeW.addLineSegment(lines2);
   		
   		//draw a vertical line from 2/3 of x axis
   		firstToken = rect.x + rect.width/3*2+ 50;
   		double [][] lines3 = {{firstToken-10, curY},{firstToken-10, rect.y+rect.height}};
   		shapeW.addLineSegment(lines3);
   		
   		curY = curY+charHeight; //move to new line
   		// more thermodynamic data
   		
   		// sfc-3km Lapse rate
   		//
   		// Chin: NOTE: I just "guess" and follow how 850-500 mb Lapse rate is calculated to calculate sfc-3km and 3-6km Lapse rate
   		// 
   		
   		fValue.setValue(0);
   		// get surface pressure (fValue)
   		nsharpNative.nsharpLib.get_surface(fValue, fValue1, fValue2);
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
   			// get 3 km pressure (fValue) 
   			float threekmPre = nsharpNative.nsharpLib.ipres(3000);
   			fValue1.setValue(0);
   			nsharpNative.nsharpLib.lapse_rate( fValue1, fValue.getValue(), threekmPre );
   			if(nsharpNative.nsharpLib.qc(fValue1.getValue())==1) {
   				textStr = String.format("sfc-3km Agl Lapse Rate=M/%.1fC/Km", fValue1.getValue());
   			}
   			else {
   				textStr = "sfc-3km Agl Lapse Rate=  M";
   			}
   		}
   		else {
   			textStr = "sfc-3km Agl Lapse Rate=  M";
   		}
   		{
   			double [] lblXy7 = {xbegin,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		} 
   		//place holder for "Supercell"
   		{
   			textStr = "Supercell = M";
   			double [] lblXy7 = {firstToken,curY};
  			shapeY.addLabel(textStr, lblXy7);
   		} 
   		curY = curY+charHeight; //move to new line
   		
   		// 3km-6km Lapse rate
   		fValue.setValue(0);
   		fValue1.setValue(0);
   		// get 3 and 6km pressure (fValue) 
   		float threekmPre = nsharpNative.nsharpLib.ipres(3000);
   		float sixkmPre = nsharpNative.nsharpLib.ipres(6000);
   		fValue1.setValue(0);
   		nsharpNative.nsharpLib.lapse_rate( fValue1, threekmPre,sixkmPre );
   		if(nsharpNative.nsharpLib.qc(fValue1.getValue())==1) {
   			textStr = String.format("3-6km Agl Lapse Rate  =M/%.1fC/Km", fValue1.getValue());
   		}
   		else {
   			textStr = "3-6km Agl Lapse Rate  =  M";
   		}
   		{
   			double [] lblXy7 = {xbegin,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}
   		//place holder for "STP (CIN)"
   		{
   			textStr = "STP(CIN) = M";
   			double [] lblXy7 = {firstToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		} 

   		curY = curY+charHeight; //move to new line


   		fValue.setValue(0);
   		fValue1.setValue(0);		
   		nsharpNative.nsharpLib.vert_tot(fValue);
   		nsharpNative.nsharpLib.lapse_rate( fValue1, 850.0F, 500.0F );		
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1 && nsharpNative.nsharpLib.qc(fValue1.getValue())==1) {
   			textStr = String.format("850-500mb Lapse Rate  =%.0fC/%.1fC/Km",fValue.getValue(), fValue1.getValue());
   		}
   		else {
   			textStr = "850-500mb Lapse Rate  = M";
   		}
   		{
   			double [] lblXy7 = {xbegin,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}
   		//place holder for "STP(fixed)"
   		{
   			textStr = "STP(fixed) = M";
   			double [] lblXy7 = {firstToken,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}

   		curY = curY+charHeight; //move to new line

   		fValue.setValue(0);
   		fValue1.setValue(0);		
   		nsharpNative.nsharpLib.delta_t(fValue);
   		nsharpNative.nsharpLib.lapse_rate( fValue1, 700.0F, 500.0F );		
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1 && nsharpNative.nsharpLib.qc(fValue1.getValue())==1) {
   			textStr = String.format("700-500mb Lapse Rate  =%.0fC/%.1fC/Km",fValue.getValue(), fValue1.getValue());
   		}
   		else {
   			textStr = "700-500mb Lapse Rate  = M";
   		}
   		{
   			double [] lblXy7 = {xbegin,curY};
  			shapeW.addLabel(textStr, lblXy7);
   		}
   		//place holder for "SHIP"
   		{
   			textStr = "SHIP = M";
   			double [] lblXy7 = {firstToken,curY};
  			shapeR.addLabel(textStr, lblXy7);
   		}
   		
   		shapeR.compile();
		shapeW.compile();
		shapeY.compile();
		
		try {
			extent = new PixelExtent(rect);
	        target.setupClippingPlane(extent);
			target.drawWireframeShape(shapeW, NsharpConstants.color_white, 1);
			target.drawWireframeShape(shapeY, NsharpConstants.color_yellow, 1);
			target.drawWireframeShape(shapeR, NsharpConstants.color_red, 1);
			target.clearClippingPlane();
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
	}*/
	public NsharpDrawPanels(NsharpBackgroundResource backgroundResource, IFont font) {
		bkRsc = backgroundResource;
		NsharpSkewTDescriptor desc = bkRsc.getDescriptor();
		if( desc != null) {
			rsc = desc.getSkewtResource();
		}
		if (bkRsc != null) {
			panelRectArray[0] = bkRsc.getDataPanel1Background().getRectangle();
			panelRectArray[1] = bkRsc.getDataPanel2Background().getRectangle();
			//panelRectArray[2] = bkRsc.getDataPanel3Background().getRectangle();
			//panelRectArray[3] = bkRsc.getDataPanel4Background().getRectangle();
		}
		myFont = font;
		instance = this;
	}
	public void drawPanel(IGraphicsTarget target, int virtualPanelNumber, int dsiplayPanelNumber
    ) throws VizException {
		if(virtualPanelNumber > virtualPanelSize || dsiplayPanelNumber > NsharpConstants.dsiplayPanelSize)
			return;
		int physicalPanelNumber = dsiplayPanelNumber -1;
		NsharpSkewTDescriptor desc = bkRsc.getDescriptor();
		NsharpSkewTResource rsc;
		if( desc != null) {
			rsc = desc.getSkewtResource();
			if(rsc != null)
				nsharpNative = rsc.getNsharpNative();
		}
		if(nsharpNative == null)
			return;
		//if(myFont == null) {
        //    myFont = target.initializeFont(fontName, 10, null);
        //}
		switch(virtualPanelNumber){
		case 1:
			drawPanel1(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 2:
			drawPanel2(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 3:
			drawPanel3(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 4:
			drawPanel4(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 5:
			drawPanel5(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 6:
			drawPanel6(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 7:
			drawPanel7(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 8:
			drawPanel8(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 9:
			drawPanel9(target,  panelRectArray[physicalPanelNumber]);
			break;
		case 10:
			drawPanel10(target,  panelRectArray[physicalPanelNumber]);
			break;
		default:
			break;
		}
	}
	public void setUserPickedParcelLine(Coordinate c) {
		//make sure is in virtualPanel 1 as Parcel Line is defined in it.
		if(rsc != null){
			if(rsc.getParcelLinesInPhysicalPanelNumber()==0)
				return;
		}
		else
			return;
		//System.out.println("setUserPickedParcelLine c.y="+ c.y+ " parcelLineYStart="+ parcelLineYStart+" parcelLineYEnd="+parcelLineYEnd);
		//make sure within parcel line area
		if(c.y >= parcelLineYStart && c.y <=parcelLineYEnd){
			int index =((int)(c.y - parcelLineYStart))/ (int)charHeight;
			if( index  < NsharpNativeConstants.PARCEL_MAX){
				currentParcel = (short) (index + 1);		
				//System.out.println("setUserPickedParcelLine at "+ currentParcel);
				//notify skewtRsc
				rsc.updateParcelFromPanel(currentParcel);
			}
		}
	}
	@SuppressWarnings("deprecation")
	private void drawPanel1(IGraphicsTarget target, Rectangle rect
            ) throws VizException {
    	 
		extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        //myFont = target.initializeFont(fontName, 10, null);
   		 /*
   		 * Chin's NOTE::::
   		 * This pages based on newer version nsharp from SPC. We dont have source code as of 7/8/2010.
   		 * Therefore, coding is purly based on a captured screen shot given by SPC's John Hart. 
   		 * This function is coded based on native nsharp codes which can be found 
   		 * in other pages's show functions.
   		 *
   		 *
   		 */
   		//if we can not Interpolates a temp with 700 mb pressure, then we dont have enough raw data 	
   		if ((nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.itemp(700.0F)) == 0))
   		{
   			target.drawString(myFont, "               " +NO_DATA, rect.x, rect.y, 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);
   			return;
   		}
   		
   		//call get_topBotPres to set p_top and p_bot
   		FloatByReference topPF= new FloatByReference(0);
		FloatByReference botPF= new FloatByReference(0);
		nsharpNative.nsharpLib.get_effectLayertopBotPres(topPF, botPF);
   		
   		String textStr, CAPE3Str="3CAPE= M", NCAPEStr = "NCAPE= M";
   		curY=rect.y;
   		
   		//
   		// Start with Parcel Data
   		// 
   		double widthGap = (rect.width-200)/6;
   		firstToken=rect.x+200.0;
   		secondToken=firstToken+widthGap;
   		thirdToken=secondToken+widthGap;
   		forthToken=thirdToken+widthGap;
   		fifthToken = forthToken+widthGap;
   		sixthToken = fifthToken+widthGap;
   		
   		target.drawString(myFont, "CAPE", firstToken, rect.y , 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);
   		target.drawString(myFont, "CINH", secondToken, rect.y , 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);
   		target.drawString(myFont, "LCL", thirdToken, rect.y , 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);
   		target.drawString(myFont, "LI", forthToken, rect.y , 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);
   		target.drawString(myFont, "LFC", fifthToken, rect.y , 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);
   		target.drawString(myFont, "EL", sixthToken, rect.y , 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null); 
   	    curY = curY+charHeight;
   	    target.drawLine(rect.x, curY, 0.0, rect.x+rect.width, curY, 0.0, NsharpConstants.color_white, 1);
   	    parcelLineYStart = curY;
   		float layerPressure = 0; 
   		
   		
   		//get current parcel type here to use it later
   		//if(NsharpParcelDialog.getAccess() != null){

   			//currentParcel = NsharpParcelDialog.getAccess().getCurrentParcel();
   			//if(currentParcel == NsharpNativeConstants.PARCELTYPE_USER_DEFINED)
   			//	layerPressure = NsharpParcelDialog.getAccess().getUserDefdParcelMb();
   			//else
   				//layerPressure = NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
   		//}
   		
   		//else {//default - use mU
   		//	currentParcel = NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE;
   		//	layerPressure = NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
   		//}
   		//get user selected parcel type
   		_lplvalues lpvls;
   		_parcel pcl;
   		for (short parcelNumber=1; parcelNumber <= NsharpNativeConstants.PARCEL_MAX  ; parcelNumber++){
   			if(parcelNumber == currentParcel ){
   				PixelExtent pixExt = new PixelExtent(rect.x, rect.x+ rect.width-10,curY, curY+charHeight);
   				//target.setupClippingPlane(pixExt);
   				target.drawRect(pixExt, NsharpConstants.color_gold, 1.0f, 1.0f);
   			}
   			//call native define_parcel() with parcel type and user defined pressure (if user defined it)
   			textStr = NsharpNativeConstants.parcelToTypeStrMap.get(parcelNumber);
   			target.drawString(myFont, textStr, rect.x, curY , 0.0,
   					TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   					VerticalAlignment.TOP, null);
   			float layerPressure1 = NsharpNativeConstants.parcelToLayerMap.get(parcelNumber);
   			if(parcelNumber == NsharpNativeConstants.PARCELTYPE_USER_DEFINED){
   				//get user selected parcel type, if available
   				if(NsharpParcelDialog.getAccess() != null){
   					layerPressure1 = NsharpParcelDialog.getAccess().getUserDefdParcelMb();   			   			
   				}
   			}
   			//System.out.println("drawPanel1-1 called define_parcel pType="+parcelNumber+" pre="+ layerPressure1);

   			nsharpNative.nsharpLib.define_parcel(parcelNumber,layerPressure1);

   			lpvls = new _lplvalues();
   			nsharpNative.nsharpLib.get_lpvaluesData(lpvls);

   			float sfctemp, sfcdwpt, sfcpres;
   			sfctemp = lpvls.temp;
   			sfcdwpt = lpvls.dwpt;
   			sfcpres = lpvls.pres;
   			// get parcel data by calling native nsharp parcel() API. value is returned in pcl 
   			pcl = new _parcel();
   			nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);
   			//draw parcel name
   			//draw CAPE
   			if(pcl.bplus != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA)
   				target.drawString(myFont, String.format( "%.0f", pcl.bplus), firstToken, curY , 0.0,
   						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   						VerticalAlignment.TOP, null);
   			else 
   				target.drawString(myFont, "M", firstToken, curY , 0.0,
   						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   						VerticalAlignment.TOP, null);
   			//draw CINH
   			if(pcl.bminus != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA)
   				target.drawString(myFont, String.format("%.0f", pcl.bminus), secondToken, curY , 0.0,
   						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   						VerticalAlignment.TOP, null);
   			else 
   				target.drawString(myFont, "M", secondToken, curY , 0.0,
   						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   						VerticalAlignment.TOP, null);
   			//draw LCL
   			float lcl = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lclpres ));
   			if(lcl != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA)
   				target.drawString(myFont, String.format("%.0fm", lcl), thirdToken, curY , 0.0,
   						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   						VerticalAlignment.TOP, null);

   			else
   				target.drawString(myFont, "M", thirdToken, curY , 0.0,
   						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   						VerticalAlignment.TOP, null);
   			//draw LI
   			if(pcl.li5 != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA)
   				target.drawString(myFont, String.format("%5.0f", pcl.li5), forthToken, curY , 0.0,
   						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   						VerticalAlignment.TOP, null);
   			else 
   				target.drawString(myFont, "M", forthToken, curY , 0.0,
   						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   						VerticalAlignment.TOP, null);
   			//draw LFC
   			float lfc = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lfcpres ));
   			if(lfc != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA)
   				target.drawString(myFont, String.format("%.0fm", lfc), fifthToken, curY , 0.0,
   						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   						VerticalAlignment.TOP, null);
   			else
   				target.drawString(myFont, "M", fifthToken, curY , 0.0,
   						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   						VerticalAlignment.TOP, null);
   			// draw EL
   			float el = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.elpres ));
   			if(el != NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA)
   				target.drawString(myFont, String.format("%.0f'", NsharpConstants.metersToFeet.convert(el)), sixthToken, curY , 0.0,
   						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   						VerticalAlignment.TOP, null);
   			else
   				target.drawString(myFont, "M", sixthToken, curY , 0.0,
   						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   						VerticalAlignment.TOP, null);

   			curY = curY+charHeight;
   			//get 3CAPE value for later to use	
   			if(parcelNumber == NsharpNativeConstants.PARCELTYPE_MEAN_MIXING){
   				if(nsharpNative.nsharpLib.qc(pcl.cape3km)==1) {
   					CAPE3Str = String.format("3CAPE=%.0fJ/kg", pcl.cape3km);  			
   				}
   				else {
   					CAPE3Str = "3CAPE= M";
   				}
   			}//get NCAPE value for later to use	
   			else if(parcelNumber == NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE){
   				float j1 = pcl.bplus;
   		        float j2 = nsharpNative.nsharpLib.ihght(pcl.elpres) - nsharpNative.nsharpLib.ihght(pcl.lfcpres);
   		        if(nsharpNative.nsharpLib.qc(j1/j2)==1) {
   		        	NCAPEStr = String.format("NCAPE=%.2f",j1/j2);
   		   		}
   		   		else {
   		   			NCAPEStr = "NCAPE= M";
   		   		} 
   			}
   		}
   	    target.drawLine(rect.x, curY, 0.0, rect.x+rect.width, curY, 0.0, NsharpConstants.color_white, 1);
   	    parcelLineYEnd= curY;
   	    if(currentParcel == NsharpNativeConstants.PARCELTYPE_USER_DEFINED){
   	    	if(NsharpParcelDialog.getAccess() != null){
   	    		layerPressure = NsharpParcelDialog.getAccess().getUserDefdParcelMb();
   	    	}
   	    	else
   	    		layerPressure = NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
   	    }
   	    else
   	    	layerPressure = NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
   	 //System.out.println("drawPanel1-2 called define_parcel pType="+currentParcel+" pre="+ layerPressure);

   	    //reset and define current parcel
   		nsharpNative.nsharpLib.define_parcel(currentParcel,layerPressure);
   		lpvls = new _lplvalues();
   		nsharpNative.nsharpLib.get_lpvaluesData(lpvls);

   		float sfctemp, sfcdwpt, sfcpres;
   		sfctemp = lpvls.temp;
   		sfcdwpt = lpvls.dwpt;
   		sfcpres = lpvls.pres;
   		// get parcel data by calling native nsharp parcel() API. value is returned in pcl 
   		pcl = new _parcel();
   		nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);
   		//
   		// THERMO DYNAMIC DATA
   		// 
   		firstToken = rect.x + rect.width/48*11;
   		secondToken = rect.x + rect.width/48*27;
   		thirdToken = rect.x + rect.width/48*38;
   		fValue.setValue(0);
   		nsharpNative.nsharpLib.precip_water(fValue, -1.0F, -1.0F);
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
   			textStr = String.format("PW=%.2f in", fValue.getValue());
   			target.drawString(myFont, textStr, rect.x, curY , 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);
   		}
   		else {
   			target.drawString(myFont, "PW=M", rect.x, curY , 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);
   		}
   		//3CAPE...value was retrieved earlier
   		target.drawString(myFont, CAPE3Str, firstToken, curY , 0.0,
	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
	                VerticalAlignment.TOP, null);
   		
   		fValue.setValue(0);
   		float wbzft = nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.wb_lvl( 0, fValue ))));
   		if(nsharpNative.nsharpLib.qc(wbzft)==1) {
   			textStr = String.format("WBZ=%.0f'",wbzft);
   		}
   		else {
   			textStr = "WBZ= M";
   		}
   		target.drawString(myFont, textStr, secondToken, curY , 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);
   		
   		//WNDG
   		float wndg = nsharpNative.nsharpLib.damaging_wind();
   		if(nsharpNative.nsharpLib.qc(wndg)==1) {
   			textStr = String.format("WNDG=%.2f",wndg);
   		}
   		else {
   			textStr = "WNDG= M";
   		}
   		
		target.drawString(myFont, textStr, thirdToken, curY , 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);

   		
		curY = curY+charHeight; //move to new line

   		fValue.setValue(0);
   		nsharpNative.nsharpLib.k_index(fValue);
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
   			textStr = String.format("K=%.0f",fValue.getValue());
   		}
   		else {
   			textStr = "K= M";
   		}		
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);
		//DCAPE
		//fValue1 will be used for DownT to use
   		float dcape= nsharpNative.nsharpLib.dcape(fValue, fValue1);
   		float downT = fValue1.getValue();
   		if(nsharpNative.nsharpLib.qc(dcape)==1) {
   			textStr = String.format("DCAPE=%.0fJ/kg",dcape);
   		}
   		else {
   			textStr = "DCAPE= M";
   		}	   		
		target.drawString(myFont, textStr, firstToken, curY , 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);

		//FZL
   		fValue.setValue(0);
   		float fgzft = nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.temp_lvl( 0, fValue ))));
   		if(nsharpNative.nsharpLib.qc(fgzft)==1) {
   			textStr = String.format("FZL=%.0f'",fgzft);
   		}
   		else {
   			textStr = "FZL= M";
   		}
		target.drawString(myFont, textStr, secondToken, curY , 0.0,
	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
	                VerticalAlignment.TOP, null);
		//ESP
		float esp= nsharpNative.nsharpLib.esp();
		if(nsharpNative.nsharpLib.qc(esp)==1) {
   			textStr = String.format("ESP=%.2f",esp);
   		}
   		else {
   			textStr = "ESP= M";
   		}
		target.drawString(myFont, textStr, thirdToken, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
   		
		curY = curY+charHeight; //move to new line
   		
   		fValue.setValue(0);
   		//MidRH
   		nsharpNative.nsharpLib.get_surface(fValue1, fValue2, fValue3); //fValue 2 and fValue3 are not of concern here
		nsharpNative.nsharpLib.mean_relhum( fValue, fValue1.getValue() - 150, fValue1.getValue() - 350 );
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
   			textStr = String.format("MidRH=%.0f%c",fValue.getValue(),NsharpConstants.PERCENT_SYMBOL);
   		}
   		else {
   			textStr = "MidRH = M";
   		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
	                VerticalAlignment.TOP, null);
   		//DownT
		downT = nsharpNative.nsharpLib.ctof(downT) ; //convert to F
		if(nsharpNative.nsharpLib.qc( downT)==1) {
   			textStr = String.format("DownT=%.0fF",downT);
   		}
   		else {
   			textStr = "DownT= M";
   		}
   		target.drawString(myFont, textStr, firstToken, curY , 0.0,
	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
	                VerticalAlignment.TOP, null);

   		//ConvT
   		fValue.setValue(0);
   		float conTempF = nsharpNative.nsharpLib.ctof(nsharpNative.nsharpLib.cnvtv_temp( fValue, -1));
   		
   		if(nsharpNative.nsharpLib.qc( conTempF )==1) {
   			textStr = String.format("ConvT=%.0fF",conTempF);
   		}
   		else {
   			textStr = "ConvT = M";
   		}
		target.drawString(myFont, textStr, secondToken, curY , 0.0,
	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
	                VerticalAlignment.TOP, null);

		//MMP:  Coniglio MCS Maintenance Parameter 
		float mmp = nsharpNative.nsharpLib.coniglio1();
		if(nsharpNative.nsharpLib.qc( mmp )==1) {
   			textStr = String.format("MMP=%.2f",mmp);
   		}
   		else {
   			textStr = "MMP= M";
   		}
   		target.drawString(myFont, textStr, thirdToken, curY , 0.0,
	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
	                VerticalAlignment.TOP, null);

   		
   		curY = curY+charHeight; //move to new line

   		fValue.setValue(0);
   		fValue1.setValue(0);
   		// get surface pressure (fValue1) before getting mean LRH value
   		nsharpNative.nsharpLib.get_surface(fValue1, fValue2, fValue3); //fValue 2 and fValue3 are not of concern here
   		nsharpNative.nsharpLib.mean_relhum( fValue, -1.0F, fValue1.getValue() - 150 );
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
   			//textStr = NsharpNativeConstants.THERMO_MEANLRH_LINE;
   			textStr = String.format("LowRH=%.0f%c",fValue.getValue(),NsharpConstants.PERCENT_SYMBOL);
   		}
   		else {
   			textStr = "LowRH = M";
   		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
	                VerticalAlignment.TOP, null);

   		
   		fValue.setValue(0);
   		nsharpNative.nsharpLib.mean_mixratio(fValue, -1.0F, -1.0F);
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
   			textStr = String.format("MeanW=%.1fg/kg",fValue.getValue());
   		}
   		else {
   			textStr = "MeanW = M";
   		}
		target.drawString(myFont, textStr, firstToken, curY , 0.0,
	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
	                VerticalAlignment.TOP, null);


   		fValue.setValue(0);
   		float maxT= nsharpNative.nsharpLib.ctof(nsharpNative.nsharpLib.max_temp( fValue, -1));
   		if(nsharpNative.nsharpLib.qc(maxT)==1) {
   			textStr = String.format("MaxT=%.0fF",maxT);
   		}
   		else {
   			textStr = "MaxT= M";
   		}
		target.drawString(myFont, textStr, secondToken, curY , 0.0,
	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
	                VerticalAlignment.TOP, null);

   		//NCAPE
		/*float j1 = pcl.bplus;
        float j2 = nsharpNative.nsharpLib.ihght(pcl.elpres) - nsharpNative.nsharpLib.ihght(pcl.lfcpres);
        if(nsharpNative.nsharpLib.qc(j1/j2)==1) {
   			textStr = String.format("NCAPE=%.2f",j1/j2);
   		}
   		else {
   			textStr = "NCAPE= M";
   		}       */
		target.drawString(myFont, NCAPEStr, thirdToken, curY , 0.0,
	                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
	                VerticalAlignment.TOP, null);

   		curY = curY+charHeight; //move to new line
   		target.drawLine(rect.x, curY, 0.0, rect.x+rect.width, curY, 0.0, NsharpConstants.color_white, 1);
   		
   		//draw a vertical line from 2/3 of x axis
   		firstToken = rect.x + rect.width/3*2+ 50;
   		target.drawLine(firstToken-10, curY, 0.0, firstToken-10, rect.y+rect.height, 0.0, NsharpConstants.color_white, 1);
   		
   		// more thermodynamic data
   		// the following follow show_parcel_new() at xwvid.c of bigNsharp implementation
   		// sfc-3km Lapse rate
   		//
   		float htsfc =0;
   		float tDelta = nsharpNative.nsharpLib.aglT(0, 3000);
   		fValue.setValue(0);
   		// get surface pressure (fValue)
   		nsharpNative.nsharpLib.get_surface(fValue, fValue1, fValue2);
   		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
   			htsfc = nsharpNative.nsharpLib.ihght(fValue.getValue());
   			// get sfc to (sfc+ 3 km) pressure (fValue) 
   			float threekmPre = nsharpNative.nsharpLib.ipres(htsfc+3000);
   			fValue1.setValue(0);
   			nsharpNative.nsharpLib.lapse_rate( fValue1, fValue.getValue(), threekmPre );
   			if(nsharpNative.nsharpLib.qc(fValue1.getValue())==1) {
   				textStr = String.format("sfc-3km AglLapseRate=%.0fC/%.1fC/km", tDelta, fValue1.getValue());
   			}
   			else {
   				textStr = "sfc-3km AglLapseRate=  M";
   			}
   		}
   		else {
   			textStr = "sfc-3km AglLapseRate=  M";
   		}
   		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
   		
   		
   		//"Supercell"
   		NsharpSkewTResource rsc = bkRsc.getDescriptor().getSkewtResource();
   		float smdir = rsc.getSmWindDir();//bkRsc.getSmDir(); #10438
		float smspd = rsc.getSmWindSpd();//bkRsc.getSmSpd();
   		float superCell = nsharpNative.nsharpLib.scp(smdir, smspd);
   		if(nsharpNative.nsharpLib.qc(superCell)==1) {
   			textStr = String.format("Supercell=%.1f",superCell);
   		}
   		else {
   			textStr = "Supercell = M";
   	   	}  
   		target.drawString(myFont, textStr, firstToken, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
   		
   		curY = curY+charHeight; //move to new line
   		
   		// 3km-6km Lapse rate
   		fValue.setValue(0);
   		fValue1.setValue(0);
   		tDelta = nsharpNative.nsharpLib.aglT(3000, 6000);
   		// get 3 and 6km pressure (fValue) 
   		float threekmPre = nsharpNative.nsharpLib.ipres(htsfc+3000);
   		float sixkmPre = nsharpNative.nsharpLib.ipres(htsfc+6000);
   		fValue1.setValue(0);
   		nsharpNative.nsharpLib.lapse_rate( fValue1, threekmPre,sixkmPre );
   		if(nsharpNative.nsharpLib.qc(fValue1.getValue())==1) {
   			textStr = String.format("3-6km AglLapseRate=%.0fC/%.1fC/km", tDelta, fValue1.getValue());

   		}
   		else {
   			textStr = "3-6km AglLapseRate  =  M";
   		}
   		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
   		// "STP (CIN)"
   		float cin = nsharpNative.nsharpLib.sigtorn_cin(smdir, smspd);
   		if(nsharpNative.nsharpLib.qc(cin)==1) {
   			textStr = String.format("STP(CIN)=%.1f",cin);
   		}
   		else {
   			textStr = "STP(CIN) = M";
   	   	}
   		target.drawString(myFont, textStr, firstToken, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);

   		curY = curY+charHeight; //move to new line


   		fValue.setValue(0);
   		fValue1.setValue(0);		
   		//nsharpNative.nsharpLib.vert_tot(fValue);
   		float delta= nsharpNative.nsharpLib.itemp(850) - nsharpNative.nsharpLib.itemp(500);
   		nsharpNative.nsharpLib.lapse_rate( fValue1, 850.0F, 500.0F );		
   		if(nsharpNative.nsharpLib.qc(delta)==1 && nsharpNative.nsharpLib.qc(fValue1.getValue())==1) {
   			textStr = String.format("850-500mb LapseRate=%3.0fC/%3.1fC/km",delta, fValue1.getValue());
   		}
   		else {
   			textStr = "850-500mb LapseRate  = M";
   		}
   		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
   		// "STP(fixed)"
   		float fixedStp = nsharpNative.nsharpLib.sigtorn_fixed(smdir, smspd);
   		if(nsharpNative.nsharpLib.qc(fixedStp)==1) {
   			textStr = String.format("STP(fixed)=%.1f",fixedStp);
   		}
   		else {
   			textStr = "STP(fixed) = M";
   	   	}
   		target.drawString(myFont, textStr, firstToken, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);

   		curY = curY+charHeight; //move to new line

   		fValue.setValue(0);
   		fValue1.setValue(0);		
   		//nsharpNative.nsharpLib.delta_t(fValue);
   		nsharpNative.nsharpLib.lapse_rate( fValue1, 700.0F, 500.0F );	
   		delta= nsharpNative.nsharpLib.itemp(700) - nsharpNative.nsharpLib.itemp(500);
   		if(nsharpNative.nsharpLib.qc(/*fValue.getValue())*/delta)==1 && nsharpNative.nsharpLib.qc(fValue1.getValue())==1) {
   			textStr = String.format("700-500mb LapseRate=%3.0fC/%3.1fC/km",delta/*fValue.getValue()*/, fValue1.getValue());
   		}
   		else {
   			textStr = "700-500mb LapseRate  = M";
   		}
   		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
   		// "SHIP"
   		float ship = nsharpNative.nsharpLib.cave_ship();
   		if(nsharpNative.nsharpLib.qc(ship)==1) {
   			textStr = String.format("SHIP=%4.1f",ship);
   		}
   		else 
   			textStr = "SHIP = M";
   		target.drawString(myFont, textStr, firstToken, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_red, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
        //System.out.println("shop="+ship);
   		//myFont.dispose();
    }
	@SuppressWarnings("deprecation")
	private void drawPanel2(IGraphicsTarget target,  Rectangle rect) 
    throws VizException {
		 /*
		 * Chin's NOTE::::
		 * This pages based on BigNsharp 
		 * show_shear_new() at xwvid3.c
		 *
		 */
		extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        //myFont = target.initializeFont(fontName, 10, null);
        curY= rect.y;
    	String textStr;
    	
    	//if we can not Interpolates a temp with 700 mb pressure, then we dont have enough raw data 	
    	if (nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.itemp(700.0F)) == 0)
    	{
   			target.drawString(myFont, "               " +NO_DATA, rect.x, rect.y, 0.0,
   	                TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
   	                VerticalAlignment.TOP, null);
    		return;
    	}

    	//
    	//  Start with Header SRH(m%c/s%c)   Shear(kt)    MnWind     SRW
    	// 
   		firstToken=rect.x+200.0;
   		secondToken=firstToken+140;
   		thirdToken=secondToken+120;
   		forthToken=thirdToken+120;

    	textStr = String.format("SRH(m%c/s%c)",NsharpConstants.SQUARE_SYMBOL, NsharpConstants.SQUARE_SYMBOL);
    	target.drawString(myFont, textStr, firstToken, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
    	target.drawString(myFont, "Shear(kt)", secondToken, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
    	target.drawString(myFont, "MnWind", thirdToken, curY , 0.0,
                        TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                        VerticalAlignment.TOP, null);
    	target.drawString(myFont, "SRW", forthToken, curY , 0.0,
                                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                                VerticalAlignment.TOP, null);
    	curY = curY+charHeight; //move to new line
    	target.drawLine(rect.x, curY, 0.0, rect.x+rect.width, curY, 0.0, NsharpConstants.color_white, 1);

		FloatByReference smdir=new FloatByReference(0), smspd = new FloatByReference(0);
		nsharpNative.nsharpLib.get_storm(smspd, smdir);
		FloatByReference topPF= new FloatByReference(0);
		FloatByReference botPF= new FloatByReference(0);
		nsharpNative.nsharpLib.get_effectLayertopBotPres(topPF, botPF);
		//System.out.println("top="+topPF.getValue()+" bot="+botPF.getValue());
		for (int i =0; i< NsharpNativeConstants.STORM_MOTION_TYPE_STR1.length; i++){
			float h1, h2;
			if(NsharpNativeConstants.STORM_MOTION_TYPE_STR1[i].equals("Eff Inflow Layer")){

				if(botPF.getValue()>0){
					h1 = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(botPF.getValue()));
					h2 = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(topPF.getValue()));
				}
				else{
					h1=-999;
					h2=-999;
				}
			}
			else {
				h1 = NsharpNativeConstants.STORM_MOTION_HEIGHT1[i][0];
				h2 = NsharpNativeConstants.STORM_MOTION_HEIGHT1[i][1];
			}
			//h1 = NsharpNativeConstants.STORM_MOTION_HEIGHT1[i][0];
			//h2 = NsharpNativeConstants.STORM_MOTION_HEIGHT1[i][1];
			if(h1!=-999&& h2!=-999){
				//SRH
				//calculate helicity 

				float totHeli = nsharpNative.nsharpLib.helicity( h1, 
						h2, smdir.getValue(), smspd.getValue(), fValue, fValue1);
				target.drawString(myFont, NsharpNativeConstants.STORM_MOTION_TYPE_STR1[i], rect.x, curY , 0.0,
						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
						VerticalAlignment.TOP, null);

				//gc.drawText(NsharpNativeConstants.STORM_MOTION_TYPE_STR[i],0, textLineNumber*textHeight + graphLineNumber);
				if(nsharpNative.nsharpLib.qc(fValue.getValue())==1&&nsharpNative.nsharpLib.qc(fValue1.getValue())==1) {
					//textStr = NsharpNativeConstants.STORM_MOTION_TYPE_STR[i]+"          %.0f";
					textStr = String.format("%.0f", totHeli);
				}
				else {
					textStr = "M";
				}
				target.drawString(myFont, textStr, firstToken, curY , 0.0,
						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
						VerticalAlignment.TOP, null);

				//Calculate wind shear 
				//Note: -1 as first parameter indicating bottom layer is surface layer, see wind.c for wind_shear() source code
				nsharpNative.nsharpLib.wind_shear( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h1))/*-1*/, nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h2)), 
						fValue, fValue1,fValue2, fValue3);
				if(nsharpNative.nsharpLib.qc(fValue3.getValue())==1) {
					textStr = String.format("%.0f",fValue3.getValue());

				}
				else {
					textStr = "M";
				}
				target.drawString(myFont, textStr, secondToken, curY , 0.0,
						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
						VerticalAlignment.TOP, null);

				//Calculate mean wind 
				nsharpNative.nsharpLib.mean_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h1)), nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h2)), fValue, fValue1,fValue2, fValue3);
				if(nsharpNative.nsharpLib.qc(fValue2.getValue())==1&&nsharpNative.nsharpLib.qc(fValue3.getValue())==1) {
					textStr = String.format("%.0f/%.0f",fValue2.getValue(), fValue3.getValue());
				}
				else {
					textStr = "M";		
				}
				target.drawString(myFont, textStr, thirdToken, curY , 0.0,
						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
						VerticalAlignment.TOP, null);

				//calculate pressure-weighted SR mean wind
				nsharpNative.nsharpLib.sr_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h1)), 
						nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h2)), smdir.getValue(), smspd.getValue(), 
						fValue, fValue1,fValue2, fValue3);
				if(nsharpNative.nsharpLib.qc(fValue2.getValue())==1) {
					textStr = String.format("%.0f/%.0f",fValue2.getValue(), fValue3.getValue());
				}
				else {
					textStr = "M";		
				}
				target.drawString(myFont, textStr, forthToken, curY , 0.0,
						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
						VerticalAlignment.TOP, null);
				
				if(NsharpNativeConstants.STORM_MOTION_TYPE_STR1[i].equals("Eff Inflow Layer")){
					//draw bax around it
					Rectangle rectangle = new Rectangle(rect.x,(int)curY,rect.width-10,(int)charHeight);
					//System.out.println("rect.x="+ rectangle.x+ " y="+ rectangle.y+" w="+rectangle.width+ " h="+rectangle.height);
					PixelExtent pixExt = new PixelExtent(rectangle);
					target.drawRect(pixExt,NsharpConstants.color_gold, 1.0f, 1.0f);
				}
			}
			curY = curY+charHeight; //move to new line

		}
		float pres=0;
		short oldlplchoice=3;
		float sfctemp, sfcdwpt, sfcpres;
		_lplvalues lpvls;
   		_parcel pcl;
		lpvls = new _lplvalues();
		//nsharpNative.nsharpLib.get_lpvaluesData(lpvls);
		//sfctemp = lpvls.temp;
		//sfcdwpt = lpvls.dwpt;
		//sfcpres = lpvls.pres;
			// get parcel data by calling native nsharp parcel() API. value is returned in pcl 
		pcl = new _parcel();
		for (int i =0; i< NsharpNativeConstants.STORM_MOTION_TYPE_STR2.length; i++){
			float h1, h2;
			
			h1=-999;
			h2=-999;
			nsharpNative.nsharpLib.get_lpvaluesData(lpvls);
			if(NsharpNativeConstants.STORM_MOTION_TYPE_STR2[i].equals("LCL-EL(Cloud Layer)")){
				sfctemp = lpvls.temp;
				sfcdwpt = lpvls.dwpt;
				sfcpres = lpvls.pres;
	   			nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);
				if (pcl.bplus > 0)
				{
					h1 = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lclpres));
					h2 = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.elpres));
				}
			}
			else if (NsharpNativeConstants.STORM_MOTION_TYPE_STR2[i].equals("Lower Half Storm Depth")){
				oldlplchoice = lpvls.flag;
				nsharpNative.nsharpLib.define_parcel(NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE, NsharpNativeConstants.MU_LAYER);
				nsharpNative.nsharpLib.get_lpvaluesData(lpvls);//regain lpvls value after defined parcel
				sfctemp = lpvls.temp;
				sfcdwpt = lpvls.dwpt;
				sfcpres = lpvls.pres;
				nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);
		        float el = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.elpres));
		        if (pcl.bplus >= 100.0)
		        {
		
		           float base = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(botPF.getValue()));
		           float depth = (el - base);
		           h1 = base;
		           h2 = base + (depth * 0.5f);
		           
		           nsharpNative.nsharpLib.get_lpvaluesData(lpvls);
		        }
		        // set parcel back to user selection 
		        if (oldlplchoice == 1)
		          pres = 0;
		        else if (oldlplchoice == 2)
		          pres = 0;
		        else if (oldlplchoice == 3)
		          pres = NsharpNativeConstants.MU_LAYER;
		        else if (oldlplchoice == 4)
		          pres = NsharpNativeConstants.MML_LAYER;
		        else if (oldlplchoice == 5)
		          pres = NsharpNativeConstants.USER_LAYER;
		        else if (oldlplchoice == 6)
		          pres = NsharpNativeConstants.MU_LAYER;
		        //System.out.println("drawPanel2-2 called define_parcel pType="+oldlplchoice+" pre="+ pres);

		        nsharpNative.nsharpLib.define_parcel(oldlplchoice, pres);
			}
			else {
				h1 = NsharpNativeConstants.STORM_MOTION_HEIGHT2[i][0];
				h2 = NsharpNativeConstants.STORM_MOTION_HEIGHT2[i][1];
			}
			if(h1!=-999&& h2!=-999){
				//SRH
				//calculate helicity 
				float totHeli = nsharpNative.nsharpLib.helicity( h1, 
						h2, smdir.getValue(), smspd.getValue(), fValue, fValue1);
				target.drawString(myFont, NsharpNativeConstants.STORM_MOTION_TYPE_STR2[i], rect.x, curY , 0.0,
						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
						VerticalAlignment.TOP, null);

				//gc.drawText(NsharpNativeConstants.STORM_MOTION_TYPE_STR[i],0, textLineNumber*textHeight + graphLineNumber);
				if(nsharpNative.nsharpLib.qc(fValue.getValue())==1&&nsharpNative.nsharpLib.qc(fValue1.getValue())==1) {
					//textStr = NsharpNativeConstants.STORM_MOTION_TYPE_STR[i]+"          %.0f";
					textStr = String.format("%.0f", totHeli);
				}
				else {
					textStr = "M";
				}
				// SR Helicity is not shown for these 4 storm motions, see oroginal BigNsharp show_shear_new()
				//target.drawString(myFont, textStr, firstToken, curY , 0.0,
				//		TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				//		VerticalAlignment.TOP, null);

				//Calculate wind shear 
				//Note: -1 as first parameter indicating bottom layer is surface layer, see wind.c for wind_shear() source code
				nsharpNative.nsharpLib.wind_shear( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h1)), nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h2)), 
						fValue, fValue1,fValue2, fValue3);
				if(nsharpNative.nsharpLib.qc(fValue3.getValue())==1) {
					textStr = String.format("%.0f",fValue3.getValue());

				}
				else {
					textStr = "M";
				}
				target.drawString(myFont, textStr, secondToken, curY , 0.0,
						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
						VerticalAlignment.TOP, null);

				//Calculate mean wind 
				nsharpNative.nsharpLib.mean_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h1)), nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h2)), fValue, fValue1,fValue2, fValue3);
				if(nsharpNative.nsharpLib.qc(fValue2.getValue())==1&&nsharpNative.nsharpLib.qc(fValue3.getValue())==1) {
					textStr = String.format("%.0f/%.0f",fValue2.getValue(), fValue3.getValue());
				}
				else {
					textStr = "M";		
				}
				target.drawString(myFont, textStr, thirdToken, curY , 0.0,
						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
						VerticalAlignment.TOP, null);

				//calculate pressure-weighted SR mean wind
				nsharpNative.nsharpLib.sr_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h1)), 
						nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(h2)), smdir.getValue(), smspd.getValue(), 
						fValue, fValue1,fValue2, fValue3);
				if(nsharpNative.nsharpLib.qc(fValue2.getValue())==1) {
					textStr = String.format("%.0f/%.0f",fValue2.getValue(), fValue3.getValue());
				}
				else {
					textStr = "M";		
				}
				target.drawString(myFont, textStr, forthToken, curY , 0.0,
						TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
						VerticalAlignment.TOP, null);
				if (NsharpNativeConstants.STORM_MOTION_TYPE_STR2[i].equals("Lower Half Storm Depth")){
					//draw bax around it
					Rectangle rectangle = new Rectangle(rect.x,(int)curY,rect.width-10,(int)charHeight);
					//System.out.println("rect.x="+ rectangle.x+ " y="+ rectangle.y+" w="+rectangle.width+ " h="+rectangle.height);
					PixelExtent pixExt = new PixelExtent(rectangle);
					target.drawRect(pixExt,NsharpConstants.color_gold, 1.0f, 1.0f);
				}
			}
			curY = curY+charHeight; //move to new line
			
			
		}
		
		//target.drawLine(rect.x, curY, 0.0, rect.x+rect.width, curY, 0.0, NsharpConstants.color_white, 1);
		
		//BRN Shear		
		// get parcel data by calling native nsharp parcel() API. value is returned in pcl 
		// current parcel is reset earlief already we dont have to call define_parcel() again.
		nsharpNative.nsharpLib.get_lpvaluesData(lpvls);
		sfctemp = lpvls.temp;
		sfcdwpt = lpvls.dwpt;
		sfcpres = lpvls.pres;
		nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);

		nsharpNative.nsharpLib.cave_bulk_rich2(  fValue );
		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) 
			textStr = String.format("BRN Shear = %.0f m%c/s%c",fValue.getValue(),NsharpConstants.SQUARE_SYMBOL, NsharpConstants.SQUARE_SYMBOL);
		else
			textStr = "BRN Shear =   M";	
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY+charHeight; //move to new line
		//4-6km srw
		nsharpNative.nsharpLib.sr_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(4000)), 
				nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(6000)), smdir.getValue(), smspd.getValue(), 
				fValue, fValue1,fValue2, fValue3);
		if(nsharpNative.nsharpLib.qc(fValue2.getValue())==1) {
			textStr = String.format("4-6km SR Wind =  %.0f/%.0f kt",fValue2.getValue(), fValue3.getValue());
		}
		else {
			textStr = "4-6km SR Wind =     M";		
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY+charHeight; //move to new line
		
		//Corfidi Downshear, we use  fValue3, fValue4 only by calling corfidi_MCS_motion
		nsharpNative.nsharpLib.corfidi_MCS_motion(fValue1, fValue2, fValue3, fValue4, fValue5, fValue6, fValue7, fValue8);
		textStr = String.format("Corfidi Downshear = %4.0f/%.0f kt",fValue3.getValue(), fValue4.getValue());
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY+charHeight; //move to new line
		
		//Corfidi Upshear, we use fValue7, fValue8 only by calling corfidi_MCS_motion
		textStr = String.format("Corfidi Upshear = %4.0f/%.0f kt",fValue7.getValue(), fValue8.getValue());
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY+charHeight; //move to new line
		
		//Bunkers Right
		nsharpNative.nsharpLib.bunkers_storm_motion(fValue1, fValue2, fValue3, fValue4);
		textStr = String.format("Bunkers Right = %4.0f/%.0f kt",fValue3.getValue(), fValue4.getValue());
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_red, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY+charHeight; //move to new line
		//Bunkers Left
		nsharpNative.nsharpLib.bunkers_left_motion(fValue1, fValue2, fValue3, fValue4);
		textStr = String.format("Bunkers Left = %4.0f/%.0f kt",fValue3.getValue(), fValue4.getValue());
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		
		textStr = "1km";
		target.drawString(myFont, textStr, rect.x+370, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_red, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		textStr = " & ";
		target.drawString(myFont, textStr, rect.x+420, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		textStr = "6km";
		target.drawString(myFont, textStr, rect.x+450, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		textStr = " AGL Wind Barbs";
		target.drawString(myFont, textStr, rect.x+500, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY+charHeight; //move to new line
		
		// STPC(test) - test STP using cape6km 
		//Chin Note: BigNsharp sigtorn_test always return -9999..a bug
		float stpTest = nsharpNative.nsharpLib.sigtorn_test(smdir.getValue(), smspd.getValue()); 
		//System.out.println("smdir="+smdir.getValue()+"smspd="+ smspd.getValue()+"stpTest="+stpTest+
		//		"p_bot="+botPF.getValue()+"p_top="+topPF.getValue());
		if(nsharpNative.nsharpLib.qc(stpTest)==1) 
			textStr = String.format("STPC(test) = %4.1f", stpTest);
		else
			textStr = "STPC(test) = M";
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		

		//1 km wind barb
		firstToken = rect.x+  rect.width * 3/4;
		double yOri =  curY-150;
		double barbScaleF= 25;
		List<LineStroke> barb = WindBarbFactory.getWindGraphics(
			(double)nsharpNative.nsharpLib.iwspd(nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(1000))), 
			(double)nsharpNative.nsharpLib.iwdir(nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(1000))));
        if (barb != null) {
        	WindBarbFactory.scaleBarb(barb, barbScaleF);
        	
            double cur0X=firstToken, cur0Y=yOri, cur1X=firstToken, cur1Y=yOri,newY=0;
            WindBarbFactory.translateBarb(barb,cur0X ,yOri );
            
            for (LineStroke stroke : barb) {            	
            	Coordinate point = stroke.getPoint();
            	//  Chin NOte; when using WindBarbFactory.getWindGraphics() to create barb, the Y axis is growing
            	//  upwards. However, on this canvas, Y axis is growing downwards. Therefore, the following Y coordinate
            	//  adjustment is necessary.
            	// 
           		newY = yOri - (point.y - yOri );
        		//  Note: stroke.render(gc, relativeX, relativeY) is not working here. Therefore, need to 
        		//  draw wind barb ourself.
            	if(stroke.getType() == "M"){
            		cur0X = point.x;
            		cur0Y = newY;
             	}
            	else if(stroke.getType() == "D"){
            		cur1X = point.x;
            		cur1Y = newY;            		
            		target.drawLine(cur0X,cur0Y, 0.0, cur1X, cur1Y,0.0, NsharpConstants.color_red, 1);
            		// bsteffen added these two lines to fix 50 kts wind barbs
            		cur0X = cur1X;
                    cur0Y = cur1Y;
            	}
            }
        }
		//6 km wind barb
		barb.clear();
		barb = WindBarbFactory.getWindGraphics(
			(double)nsharpNative.nsharpLib.iwspd(nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(6000))), 
			(double)nsharpNative.nsharpLib.iwdir(nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(6000))));
        if (barb != null) {
        	WindBarbFactory.scaleBarb(barb, barbScaleF);
        	
            double cur0X=firstToken, cur0Y=yOri, cur1X=firstToken, cur1Y=yOri,newY=0;
            WindBarbFactory.translateBarb(barb,cur0X ,yOri );
            
            for (LineStroke stroke : barb) {            	
            	Coordinate point = stroke.getPoint();
            	//  Chin NOte; when using WindBarbFactory.getWindGraphics() to create barb, the Y axis is growing
            	//  upwards. However, on this canvas, Y axis is growing downwards. Therefore, the following Y coordinate
            	//  adjustment is necessary.
           		newY = yOri - (point.y - yOri );
           		//
        		//  Note: stroke.render(gc, relativeX, relativeY) is not working here. Therefore, need to 
        		//  draw wind barb ourself.
             	if(stroke.getType() == "M"){
            		cur0X = point.x;
            		cur0Y = newY;
             	}
            	else if(stroke.getType() == "D"){
            		cur1X = point.x;
            		cur1Y = newY;            		
            		target.drawLine(cur0X,cur0Y, 0.0, cur1X, cur1Y,0.0, NsharpConstants.color_cyan, 1);
            		// bsteffen added these two lines to fix 50 kts wind barbs
            		cur0X = cur1X;
                    cur0Y = cur1Y;
            	}
            }
        }
        //myFont.dispose();
	}
	@SuppressWarnings("deprecation")
	private void drawPanel3(IGraphicsTarget target,Rectangle rect) 
    throws VizException {
    	extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        //myFont = target.initializeFont(fontName, fontSize, null);
        String splitedStr[];
        String textStr;
        curY = rect.y;
		//Chin's NOTE::::this function is coded based on native nsharp show_parcel() in xwvid3.c
        // moved from NsharpPaletteWindow.showParcelData()

        
		//if we can not Interpolates a temp with 700 mb pressure, then we dont have enough raw data 
		if (nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.itemp(700.0F)) == 0)
			return;
		String title =  NsharpNativeConstants.PARCEL_DATA_STR;
		
		target.drawString(myFont, title, rect.x + rect.width /3, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		
		
		String hdrStr;
		//short currentParcel;
		float layerPressure = 0;; 
		//get user selected parcel type
		hdrStr  = NsharpNativeConstants.parcelToHdrStrMap.get(currentParcel);
		layerPressure = NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
		if(currentParcel == NsharpNativeConstants.PARCELTYPE_USER_DEFINED ){
			if(NsharpParcelDialog.getAccess() != null){
				layerPressure = NsharpParcelDialog.getAccess().getUserDefdParcelMb();
			}
			
			hdrStr = String.format(hdrStr, layerPressure);
		}
		curY = curY + charHeight;
		target.drawString(myFont, hdrStr, rect.x + rect.width /4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY + charHeight;
		

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
		textStr = NsharpNativeConstants.PARCEL_LPL_LINE_;
		textStr = String.format(textStr, (int)pcl.lplpres,(int)pcl.lpltemp,(int)pcl.lpldwpt,
				(int)nsharpNative.nsharpLib.ctof(pcl.lpltemp),(int)nsharpNative.nsharpLib.ctof(pcl.lpldwpt));
		// Chin: note: target.drawString does NOT handle formatted string. For example, "ABC\tabc" will be printed
		// as "ABCabc" So, we have to add '_' to separate each value when formatting String. 
		// Then, use String.split() to have each value in a sub-string.
				
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		
		curY = curY + 2*charHeight;

		if(nsharpNative.nsharpLib.qc(pcl.bplus)==1){
			textStr = NsharpNativeConstants.PARCEL_CAPE_LINE;
			textStr = String.format(textStr,pcl.bplus);				
		}
		else {
			textStr = NsharpNativeConstants.PARCEL_CAPE_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);

		if(nsharpNative.nsharpLib.qc(pcl.li5)==1){
			textStr = NsharpNativeConstants.PARCEL_LI_LINE;
			textStr = String.format(textStr,pcl.li5);				
		}
		else {
			textStr = NsharpNativeConstants.PARCEL_LI_MISSING;
		}
		target.drawString(myFont, textStr, rect.x+rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY + charHeight;
		if(nsharpNative.nsharpLib.qc(pcl.bfzl)==1){
			textStr = NsharpNativeConstants.PARCEL_BFZL_LINE;
			textStr = String.format(textStr,pcl.bfzl);
		}
		else{
			textStr = NsharpNativeConstants.PARCEL_BFZL_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		if(nsharpNative.nsharpLib.qc(pcl.limax)==1){
			textStr = NsharpNativeConstants.PARCEL_LIMIN_LINE;
			textStr = String.format(textStr,pcl.limax,pcl.limaxpres);
		}
		else{
			textStr = NsharpNativeConstants.PARCEL_LIMIN_MISSING;
		}
		target.drawString(myFont, textStr, rect.x+rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY + charHeight;
		if(nsharpNative.nsharpLib.qc(pcl.bminus)==1){
			textStr = NsharpNativeConstants.PARCEL_CINH_LINE;
			textStr = String.format(textStr,pcl.bminus);
		}
		else {
			textStr = NsharpNativeConstants.PARCEL_CINH_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);

		if(nsharpNative.nsharpLib.qc(pcl.cap)==1){
			textStr = NsharpNativeConstants.PARCEL_CAP_LINE;
			textStr = String.format(textStr, pcl.cap, pcl.cappres);
		}
		else {
			textStr = NsharpNativeConstants.PARCEL_CAP_MISSING;
		}
		target.drawString(myFont, textStr, rect.x+rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY + 2* charHeight;
		

		textStr = NsharpNativeConstants.PARCEL_LEVEL_LINE_;
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		target.drawLine(rect.x, curY, 0.0, rect.x+rect.width, curY, 0.0, NsharpConstants.color_white, 1);
		
		if(nsharpNative.nsharpLib.qc(pcl.lclpres)==1&& 
				nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lclpres ))))==1	)
		{
			textStr = NsharpNativeConstants.PARCEL_LCL_LINE_;
			textStr = String.format(textStr,pcl.lclpres, nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lclpres ))));
		}
		else {
			textStr = NsharpNativeConstants.PARCEL_LCL_MISSING_;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;

		if(nsharpNative.nsharpLib.qc(pcl.lfcpres)==1 && 
				nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lfcpres ))))==1 &&
				nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.itemp(pcl.lfcpres ))==1)
		{
			textStr = NsharpNativeConstants.PARCEL_LFC_LINE_;
			textStr = String.format(textStr,pcl.lfcpres, nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lfcpres ))),
					nsharpNative.nsharpLib.itemp(pcl.lfcpres ));
		}
		else {
			textStr = NsharpNativeConstants.PARCEL_LFC_MISSING_;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;

		if(nsharpNative.nsharpLib.qc(pcl.elpres) ==1&& 
				nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.elpres )))) ==1&&
				nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.itemp(pcl.elpres ))==1)			
		{
			textStr = NsharpNativeConstants.PARCEL_EL_LINE_;
			textStr = String.format(textStr,pcl.elpres, nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.elpres ))),
					nsharpNative.nsharpLib.itemp(pcl.elpres ));
		}
		else {
			textStr = NsharpNativeConstants.PARCEL_EL_MISSING_;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;

		if(nsharpNative.nsharpLib.qc(pcl.mplpres)==1 && 
				nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.mplpres ))))==1	)
		{
			textStr = NsharpNativeConstants.PARCEL_MPL_LINE_;
			textStr = String.format(textStr,pcl.mplpres, nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.mplpres ))));
		}
		else {
			textStr = NsharpNativeConstants.PARCEL_MPL_MISSING_;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		
		/*
		 * Compute CCL (TL)
		 */	
		/*
		FloatByReference pccl = new FloatByReference(0);		
		FloatByReference tccl = new FloatByReference(0);
		FloatByReference zccl = new FloatByReference(0);
		nsharpNative.nsharpLib.cave_ccl (-9999.F, pccl, tccl, zccl);
		System.out.println (" \n\n surface-based CCL ");
		System.out.println (" pccl (mb): " + pccl.getValue() );
		System.out.println (" tccl (C): " + tccl.getValue() );
		System.out.println (" zccl (m): " + zccl.getValue() ); 
		*/
		//myFont.dispose();
    }
	@SuppressWarnings("deprecation")
	private void drawPanel4(IGraphicsTarget target,Rectangle rect) 
	throws VizException {
		extent = new PixelExtent(rect);
		target.setupClippingPlane(extent);
		//myFont = target.initializeFont(fontName, fontSize, null);
		String textStr;
		curY = rect.y;

		/*
		 * Chin's NOTE::::this function is coded based on legacy native nsharp software show_thermoparms(), 
		 * show_moisture(),show_instability() in xwvid3.c
		 * 
		 * Moved from NsharpPaletteWindow.showThermoparms()
		 */

		target.drawString(myFont, NsharpNativeConstants.THERMO_DATA_STR, rect.x + rect.width /3, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		target.drawString(myFont, NsharpNativeConstants.THERMO_MOISTURE_STR, rect.x + rect.width /4, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;


		fValue.setValue(0);
		nsharpNative.nsharpLib.precip_water(fValue, -1.0F, -1.0F);
		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
			textStr = NsharpNativeConstants.THERMO_PWATER_LINE;
			textStr = String.format(textStr,fValue.getValue());
		}
		else {
			textStr = NsharpNativeConstants.THERMO_PWATER_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		
		fValue.setValue(0);
		nsharpNative.nsharpLib.mean_relhum(fValue, -1.0F, -1.0F);
		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
			textStr = NsharpNativeConstants.THERMO_MEANRH_LINE;
			textStr = String.format(textStr,fValue.getValue(),NsharpConstants.PERCENT_SYMBOL);
		}
		else {
			textStr = NsharpNativeConstants.THERMO_MEANRH_MISSING;
		}
		target.drawString(myFont, textStr, rect.x + rect.width/2, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;


		fValue.setValue(0);
		nsharpNative.nsharpLib.mean_mixratio(fValue, -1.0F, -1.0F);
		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
			textStr = NsharpNativeConstants.THERMO_MEANW_LINE;
			textStr = String.format(textStr,fValue.getValue());
		}
		else {
			textStr = NsharpNativeConstants.THERMO_MEANW_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		fValue.setValue(0);
		fValue1.setValue(0);
		// get surface pressure (fValue1) before getting mean LRH value
		nsharpNative.nsharpLib.get_surface(fValue1, fValue2, fValue3); //fValue 2 and fValue3 are not of concern here
		nsharpNative.nsharpLib.mean_relhum( fValue, -1.0F, fValue1.getValue() - 150 );
		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
			textStr = NsharpNativeConstants.THERMO_MEANLRH_LINE;
			textStr = String.format(textStr,fValue.getValue(),NsharpConstants.PERCENT_SYMBOL);
		}
		else {
			textStr = NsharpNativeConstants.THERMO_MEANLRH_MISSING;
		}
		target.drawString(myFont, textStr, rect.x + rect.width/2, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;


		fValue.setValue(0);
		nsharpNative.nsharpLib.top_moistlyr(fValue);
		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
			textStr = NsharpNativeConstants.THERMO_TOP_LINE;
			textStr = String.format(textStr,fValue.getValue(),nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(fValue.getValue()))));
		}
		else {
			textStr = NsharpNativeConstants.THERMO_TOP_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;


		//instability data--------------//
		//yellow and bold for parcel header 
		target.drawString(myFont, NsharpNativeConstants.THERMO_INSTABILITY_STR, rect.x + rect.width /4, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);

		curY = curY +  charHeight;


		fValue.setValue(0);
		fValue1.setValue(0);

		nsharpNative.nsharpLib.delta_t(fValue);
		nsharpNative.nsharpLib.lapse_rate( fValue1, 700.0F, 500.0F );

		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1 && nsharpNative.nsharpLib.qc(fValue1.getValue())==1) {
			textStr = NsharpNativeConstants.THERMO_700500mb_LINE;
			textStr = String.format(textStr,fValue.getValue(), fValue1.getValue());
		}
		else {
			textStr = NsharpNativeConstants.THERMO_700500mb_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;


		fValue.setValue(0);
		fValue1.setValue(0);

		nsharpNative.nsharpLib.vert_tot(fValue);
		nsharpNative.nsharpLib.lapse_rate( fValue1, 850.0F, 500.0F );

		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1 && nsharpNative.nsharpLib.qc(fValue1.getValue())==1) {
			textStr = NsharpNativeConstants.THERMO_850500mb_LINE;
			textStr = String.format(textStr,fValue.getValue(), fValue1.getValue());
		}
		else {
			textStr = NsharpNativeConstants.THERMO_850500mb_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;


		//misc parameters data--------------//
		target.drawString(myFont, NsharpNativeConstants.THERMO_MISC_PARMS_STR, rect.x + rect.width /4, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;

		fValue.setValue(0);
		fValue1.setValue(0);
		fValue2.setValue(0);
		nsharpNative.nsharpLib.t_totals(fValue, fValue1, fValue2);
		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
			textStr = NsharpNativeConstants.THERMO_TOTAL_LINE;
			textStr = String.format(textStr,fValue.getValue());
		}
		else {
			textStr = NsharpNativeConstants.THERMO_TOTAL_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);

		fValue.setValue(0);
		nsharpNative.nsharpLib.k_index(fValue);
		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
			textStr = NsharpNativeConstants.THERMO_KINDEX_LINE;
			textStr = String.format(textStr,fValue.getValue());
		}
		else {
			textStr = NsharpNativeConstants.THERMO_KINDEX_MISSING;
		}
		target.drawString(myFont, textStr, rect.x + rect.width/2, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;


		fValue.setValue(0);
		nsharpNative.nsharpLib.sweat_index(fValue);
		if(nsharpNative.nsharpLib.qc(fValue.getValue())==1) {
			textStr = NsharpNativeConstants.THERMO_SWEAT_LINE;
			textStr = String.format(textStr,fValue.getValue());
		}
		else {
			textStr = NsharpNativeConstants.THERMO_SWEAT_MISSING;
		}

		target.drawString(myFont, textStr, rect.x, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);

		fValue.setValue(0);
		float maxTempF = nsharpNative.nsharpLib.ctof(nsharpNative.nsharpLib.max_temp( fValue, -1));
		if(nsharpNative.nsharpLib.qc(maxTempF)==1) {
			textStr = NsharpNativeConstants.THERMO_MAXT_LINE;
			textStr = String.format(textStr,maxTempF);
		}
		else {
			textStr = NsharpNativeConstants.THERMO_MAXT_MISSING;
		}
		target.drawString(myFont, textStr, rect.x + rect.width/2, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;


		fValue.setValue(0);
		float theDiff = nsharpNative.nsharpLib.ThetaE_diff( fValue);
		if(nsharpNative.nsharpLib.qc(theDiff)==1) {
			textStr = NsharpNativeConstants.THERMO_THETAE_LINE;
			textStr = String.format(textStr,theDiff);
		}
		else {
			textStr = NsharpNativeConstants.THERMO_THETAE_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		fValue.setValue(0);
		float conTempF = nsharpNative.nsharpLib.ctof(nsharpNative.nsharpLib.cnvtv_temp( fValue, -1));
		if(nsharpNative.nsharpLib.qc(conTempF)==1) {
			textStr = NsharpNativeConstants.THERMO_CONVT_LINE;
			textStr = String.format(textStr,conTempF);
		}
		else {
			textStr = NsharpNativeConstants.THERMO_CONVT_MISSING;
		}
		target.drawString(myFont, textStr, rect.x + rect.width/2, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;

		fValue.setValue(0);
		float wbzft = nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.wb_lvl( 0, fValue ))));
		if(nsharpNative.nsharpLib.qc(wbzft)==1) {
			textStr = NsharpNativeConstants.THERMO_WBZ_LINE;
			textStr = String.format(textStr,wbzft);
		}
		else {
			textStr = NsharpNativeConstants.THERMO_WBZ_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		fValue.setValue(0);
		float fgzft = nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.temp_lvl( 0, fValue ))));
		if(nsharpNative.nsharpLib.qc(fgzft)==1) {
			textStr = NsharpNativeConstants.THERMO_FGZ_LINE;
			textStr = String.format(textStr,fgzft);
		}
		else {
			textStr = NsharpNativeConstants.THERMO_FGZ_MISSING;
		}
		target.drawString(myFont, textStr, rect.x + rect.width/2, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		//myFont.dispose();
	}
	@SuppressWarnings("deprecation")
	private void drawPanel5(IGraphicsTarget target,Rectangle rect) 
    throws VizException {
    	extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        //myFont = target.initializeFont(fontName, fontSize, null);
        String splitedStr[];
        String textStr;
        curY = rect.y;
 
		/*
		 * Chin's NOTE::::this function is coded based on legacy nsharp software 		
		 * show_gradient()
		 *  in xwvid3.c
		 */
		FloatByReference Surfpressure= new FloatByReference(0);
		FloatByReference surfTemp= new FloatByReference(0);
		FloatByReference surfDewpt= new FloatByReference(0);
		target.drawString(myFont, NsharpNativeConstants.OPC_LOW_LEVEL_STR, rect.x + rect.width /3, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		target.drawString(myFont, NsharpNativeConstants.OPC_SURFACE975_STR, rect.x + rect.width /4, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		
		
		textStr = NsharpNativeConstants.OPC_LEVEL_LINE_;
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		target.drawLine(rect.x, curY, 0.0, rect.x+rect.width, curY, 0.0, NsharpConstants.color_white, 1);

		
		float ht = nsharpNative.nsharpLib.ihght(975);
		if(ht == NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA)
			textStr = NsharpNativeConstants.OPC_975_LINE_MISSING_;		    
		else{
			textStr = NsharpNativeConstants.OPC_975_LINE_;
			textStr = String.format(textStr, nsharpNative.nsharpLib.ihght(975), nsharpNative.nsharpLib.itemp(975));
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		ht=0;
		// get surface pressure (fValue1), Surface_temp (fValue2)
		nsharpNative.nsharpLib.get_surface(Surfpressure, surfTemp, surfDewpt);
		if(nsharpNative.nsharpLib.qc(Surfpressure.getValue())==1) 
			ht =  nsharpNative.nsharpLib.ihght( Surfpressure.getValue() );
		if(nsharpNative.nsharpLib.qc(Surfpressure.getValue())==1 && nsharpNative.nsharpLib.qc(surfTemp.getValue())==1) {
			textStr = NsharpNativeConstants.OPC_SURFACE_LINE_;
			textStr = String.format(textStr,Surfpressure.getValue(), ht,surfTemp.getValue());
		}
		else {
			textStr = NsharpNativeConstants.OPC_SURFACE_MISSING_;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;

		/* ----- Sfc-975 Grad ----- */
		/* make sure both 975mb layer and surface layer temperatures are available */
		if(nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.itemp(975))==1 && nsharpNative.nsharpLib.qc(surfTemp.getValue())==1){
			textStr = NsharpNativeConstants.OPC_975_SURFACE_LINE;
			textStr = String.format(textStr,nsharpNative.nsharpLib.itemp(975)-surfTemp.getValue());
		}
		else {
			textStr = NsharpNativeConstants.OPC_975_SURFACE_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		/*
		 * Chin's NOTE::::this function is coded based on legacy nsharp software 		
		 * show_inversion()
		 *  in xwvid3.c
		 *
		*   inv_mb           - Pressure of inversion level (mb)			
		 *  inv_dC           - Change in temperature (C)			
		 *  
		 */ 
		FloatByReference inv_mb= new FloatByReference(0);
		FloatByReference inv_dC= new FloatByReference(0);
		;
		//yellow and bold for parcel header 
		target.drawString(myFont, NsharpNativeConstants.OPC_LOWEST_INV_STR, rect.x + rect.width /4, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;	
		nsharpNative.nsharpLib.low_inv(inv_mb, inv_dC);
		if(nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.ihght(inv_mb.getValue()))==1) {
			textStr = NsharpNativeConstants.OPC_BASEHEIGHT_LINE;
			textStr = String.format(textStr,nsharpNative.nsharpLib.ihght(inv_mb.getValue()));
		}
		else {
			textStr = NsharpNativeConstants.OPC_BASEHEIGHT_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY +  charHeight;

		if(nsharpNative.nsharpLib.qc(inv_mb.getValue())==1) {
			textStr = NsharpNativeConstants.OPC_BASEPRESSURE_LINE;
			textStr = String.format(textStr,inv_mb.getValue());
		}
		else {
			textStr = NsharpNativeConstants.OPC_BASEPRESSURE_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY +  charHeight;

		if(nsharpNative.nsharpLib.qc(inv_dC.getValue())==1) {
			textStr = NsharpNativeConstants.OPC_CHANGE_IN_TEMP_LINE;
			textStr = String.format(textStr,inv_dC.getValue());
		}
		else {
			textStr = NsharpNativeConstants.OPC_CHANGE_IN_TEMP_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);

        //myFont.dispose();
	}
	@SuppressWarnings("deprecation")
	private void drawPanel6(IGraphicsTarget target,Rectangle rect) 
    throws VizException {
    	extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        //myFont = target.initializeFont(fontName, fontSize, null);
        String splitedStr[];
        String textStr;
        curY = rect.y;
     
		/*
		 * Chin's NOTE::::this function is coded based on legacy nsharp software 		
		 * show_mixheight()
		 *  in xwvid3.c
		 *  Calculates the mixing height using legacy mix_height()
		 *  
		 *  void mix_height ( float *mh_mb, float *mh_drct, float *mh_sped,
		 *  float *mh_dC, float *mh_lr, float *mh_drct_max,
		 *  float *mh_sped_max, short flag )
		 *  
		 *  Where:                            		
		 *  flag = 0		Surface-based lapse rate                 	
		 *  flag = 1		Layer-based lapse rate                   	
		 *                                                            		
		 *  mh_mb            - Pressure at mixing height (mb)        		
		 *  mh_drct          - Wind direction at mixing height (deg) 		
		 *  mh_sped          - Wind speed at mixing height (kt)      		
		 *  mh_dC            - Layer change in temperature (C)       		
		 *  mh_lr            - Layer lapse rate (C/km)               		
		 *  mh_drct_max      - Layer maximum wind direction (deg)    		
		 *  mh_sped_max      - Layer maximum wind speed (kt)         		
		 */
		FloatByReference mh_mb= new FloatByReference(0);
		FloatByReference mh_drct= new FloatByReference(0);
		FloatByReference mh_sped= new FloatByReference(0);
		FloatByReference mh_dC= new FloatByReference(0);
		FloatByReference mh_lr= new FloatByReference(0);
		FloatByReference mh_drct_max= new FloatByReference(0);
		FloatByReference mh_sped_max= new FloatByReference(0);
		short flag;
		
		//yellow and bold for parcel header 
		target.drawString(myFont, NsharpNativeConstants.OPC_MIXING_HGT_STR, rect.x + rect.width /10, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;		
		textStr = NsharpNativeConstants.OPC_DRY_AD_LINE;
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		textStr = NsharpNativeConstants.OPC_THRESH_LAPSE_LINE;
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		// Cyan color for Layer Based string
		target.drawString(myFont, NsharpNativeConstants.OPC_LAYER_BASED_STR, rect.x + rect.width /3, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		// calculate Layer-based lapse rate data 
		flag = 1;
		nsharpNative.nsharpLib.mix_height(mh_mb,mh_drct,mh_sped,mh_dC,mh_lr,mh_drct_max,mh_sped_max,flag);
		

		if(nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.ihght(mh_mb.getValue()))==1) {
			textStr = NsharpNativeConstants.OPC_MIXINGHEIGHT_LINE;
			textStr = String.format(textStr,nsharpNative.nsharpLib.ihght(mh_mb.getValue()));
		}
		else {
			textStr = NsharpNativeConstants.OPC_MIXINGHEIGHT_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		if(nsharpNative.nsharpLib.qc(mh_mb.getValue())==1) {
			textStr = NsharpNativeConstants.OPC_MIXINGPRESSURE_LINE;
			textStr = String.format(textStr,mh_mb.getValue());
		}
		else {
			textStr = NsharpNativeConstants.OPC_MIXINGPRESSURE_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		if(nsharpNative.nsharpLib.qc(mh_drct.getValue())==1 && nsharpNative.nsharpLib.qc(mh_sped.getValue())==1) {
			textStr = NsharpNativeConstants.OPC_TOPMIXLAYER_LINE;
			//System.out.println("speed  = " + mh_sped.getValue());
			textStr = String.format(textStr,(int)mh_drct.getValue(),NsharpConstants.DEGREE_SYMBOL,(int)(mh_sped.getValue()));
		}
		else {
			textStr = NsharpNativeConstants.OPC_TOPMIXLAYER_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		if(nsharpNative.nsharpLib.qc(mh_drct_max.getValue()) ==1 && nsharpNative.nsharpLib.qc(mh_sped_max.getValue())==1) {
			textStr = NsharpNativeConstants.OPC_MIXLAYERMAX_LINE;
			textStr = String.format(textStr,(int)mh_drct_max.getValue(),NsharpConstants.DEGREE_SYMBOL,(int)mh_sped_max.getValue());
		}
		else {
			textStr = NsharpNativeConstants.OPC_MIXLAYERMAX_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		if(nsharpNative.nsharpLib.qc(mh_dC.getValue()) ==1 && nsharpNative.nsharpLib.qc(mh_lr.getValue())==1) {
			textStr = NsharpNativeConstants.OPC_LAYER_LAPSE_LINE;
			textStr = String.format(textStr,mh_dC.getValue(),mh_lr.getValue());
		}
		else {
			textStr = NsharpNativeConstants.OPC_LAYER_LAPSE_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
				
		// Purple color for Layer Based string
		target.drawString(myFont, NsharpNativeConstants.OPC_SURFACE_BASED_STR, rect.x + rect.width /3, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_violet, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;	
		// calculate Surface-based lapse rate data 
		flag = 0;
		mh_mb.setValue(0);
		mh_drct.setValue(0);
		mh_sped.setValue(0);
		mh_dC.setValue(0);
		mh_lr.setValue(0);
		mh_drct_max.setValue(0);
		mh_sped_max.setValue(0);;
		nsharpNative.nsharpLib.mix_height(mh_mb,mh_drct,mh_sped,mh_dC,mh_lr,mh_drct_max,mh_sped_max,flag);

		//white color for text
		if(nsharpNative.nsharpLib.qc(nsharpNative.nsharpLib.ihght(mh_mb.getValue()))==1) {
			textStr = NsharpNativeConstants.OPC_MIXINGHEIGHT_LINE;
			textStr = String.format(textStr,nsharpNative.nsharpLib.ihght(mh_mb.getValue()));
		}
		else {
			textStr = NsharpNativeConstants.OPC_MIXINGHEIGHT_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		if(nsharpNative.nsharpLib.qc(mh_mb.getValue())==1) {
			textStr = NsharpNativeConstants.OPC_MIXINGPRESSURE_LINE;
			textStr = String.format(textStr,mh_mb.getValue());
		}
		else {
			textStr = NsharpNativeConstants.OPC_MIXINGPRESSURE_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		if(nsharpNative.nsharpLib.qc(mh_drct.getValue()) == 1 && nsharpNative.nsharpLib.qc(mh_sped.getValue())==1) {
			textStr = NsharpNativeConstants.OPC_TOPMIXLAYER_LINE;
			textStr = String.format(textStr,(int)mh_drct.getValue(),NsharpConstants.DEGREE_SYMBOL,(int)mh_sped.getValue());
		}
		else {
			textStr = NsharpNativeConstants.OPC_TOPMIXLAYER_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		if(nsharpNative.nsharpLib.qc(mh_drct_max.getValue()) ==1 && nsharpNative.nsharpLib.qc(mh_sped_max.getValue())==1) {
			textStr = NsharpNativeConstants.OPC_MIXLAYERMAX_LINE;
			textStr = String.format(textStr,(int)mh_drct_max.getValue(),NsharpConstants.DEGREE_SYMBOL,(int)mh_sped_max.getValue());
		}
		else {
			textStr = NsharpNativeConstants.OPC_MIXLAYERMAX_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		if(nsharpNative.nsharpLib.qc(mh_dC.getValue())==1 && nsharpNative.nsharpLib.qc(mh_lr.getValue())==1) {
			textStr = NsharpNativeConstants.OPC_LAYER_LAPSE_LINE;
			textStr = String.format(textStr,mh_dC.getValue(),mh_lr.getValue());
		}
		else {
			textStr = NsharpNativeConstants.OPC_LAYER_LAPSE_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		
		/* 
		 *  Momentum transfer (TL)
		 */  
		/*
		FloatByReference drct_mean = new FloatByReference(-9999.F);
		FloatByReference sped_mean = new FloatByReference(-9999.F);
		FloatByReference pres = new FloatByReference(-9999.F);
		FloatByReference drct = new FloatByReference(-9999.F);
		FloatByReference sped = new FloatByReference(-9999.F);
		FloatByReference del_t = new FloatByReference(-9999.F);
		FloatByReference lr = new FloatByReference(-9999.F);
		FloatByReference drct_max = new FloatByReference(-9999.F);
		FloatByReference sped_max = new FloatByReference(-9999.F);

		flag = 0;
		nsharpNative.nsharpLib.nc_mix_height(pres,drct,sped,del_t,lr,drct_mean, sped_mean, drct_max,sped_max,flag);
		System.out.println (" \n\n Surface-based momentum transfer ");
		System.out.println (" pressure at mix height: " + pres.getValue() );
		System.out.println (" wind dir at mix height: " + drct.getValue() );
		System.out.println (" wind speed at mix height: " + sped.getValue() );
		System.out.println (" layer change in temp (C): " + del_t.getValue() );
		System.out.println (" Layer lapse rate: " + lr.getValue() );
		System.out.println (" mean wind direction: " + drct_mean.getValue() );
		System.out.println (" mean wind speed: " + sped_mean.getValue() );
		System.out.println (" max wind direction: " + drct_max.getValue() );
		System.out.println (" max wind speed: " + sped_max.getValue() ); 
		
		flag = 1;
		nsharpNative.nsharpLib.nc_mix_height(pres,drct,sped,del_t,lr,drct_mean, sped_mean, drct_max,sped_max,flag);
		System.out.println (" \n\n Layer-based momentum transfer ");
		System.out.println (" pressure at mix height: " + pres.getValue() );
		System.out.println (" wind dir at mix height: " + drct.getValue() );
		System.out.println (" wind speed at mix height: " + sped.getValue() );
		System.out.println (" layer change in temp (C): " + del_t.getValue() );
		System.out.println (" Layer lapse rate: " + lr.getValue() );
		System.out.println (" mean wind direction: " + drct_mean.getValue() );
		System.out.println (" mean wind speed: " + sped_mean.getValue() );
		System.out.println (" max wind direction: " + drct_max.getValue() );
		System.out.println (" max wind speed: " + sped_max.getValue() ); 
		*/
		
        //myFont.dispose();
	}
	@SuppressWarnings("deprecation")
	private void drawPanel7(IGraphicsTarget target,Rectangle rect) 
    throws VizException {
    	extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        //myFont = target.initializeFont(fontName, fontSize, null);
        String splitedStr[];
        String textStr;
        curY = rect.y;
		/*
		 * Chin's NOTE::::this function is coded based on legacy nsharp software 		
		 * show_srdata() in xwvid3.c.
		 *  Hard coded numerical numbers are directly copied from it.
		 *  
		 *  float helicity ( float lower, float upper, float sdir, float sspd,
		 *	float *phel, float *nhel )
		 *  Calculates the storm-relative helicity (m2/s2) of a     
		 *  layer from LOWER(m, agl) to UPPER(m, agl).  Uses the    
		 *  storm motion vector (sdir, sspd).                          
		 *                                                           
		 *  lower            - Bottom level of layer (m, AGL)[-1=LPL]
		 *  upper            - Top level of layer (m, AGL)   [-1=LFC]
		 *  sdir             - Storm motion direction (degrees)      
		 *  sspd             - Storm motion speed (kt)               
		 *  phel             - Positive helicity in layer (m2/s2)    
		 *  nhel             - Negative helicity in layer (m2/s2)    
		 *  RETURN VALUE     - Total helicity (m2/s2)  
		 *  
		 *  void sr_wind ( float pbot, float ptop, float sdir, float sspd,
		 *    float *mnu, float *mnv, float *wdir, float *wspd )
		 *  Calculates a pressure-weighted SR mean wind thru the     
		 *  layer (pbot-ptop).  Default layer is LFC-EL.             
		 *  pbot             - Bottom level of layer (mb)            
		 *  ptop             - Top level of layer (mb)               
		 *  sdir            - Storm motion dirction (deg)           
		 *  sspd            - Storm motion speed (kt)               
		 *  mnu              - U-Component of mean wind (kt)         
		 *  mnv              - V-Component of mean wind (kt)        /
		 */
		
		//FloatByReference sspd= new FloatByReference(0);
		//FloatByReference sdir= new FloatByReference(0);
		FloatByReference phel= new FloatByReference(0);
		FloatByReference nhel= new FloatByReference(0);
		FloatByReference mnu= new FloatByReference(0);
		FloatByReference mnv= new FloatByReference(0);
		FloatByReference smdir= new FloatByReference(0);
		FloatByReference smspd= new FloatByReference(0);
		FloatByReference wdir= new FloatByReference(0);
		FloatByReference wspd= new FloatByReference(0);
		float totHeli;
		
		target.drawString(myFont, NsharpNativeConstants.STORM_RELATIVE_STR, rect.x + rect.width /3, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		
		nsharpNative.nsharpLib.get_storm(smspd, smdir);
		
		if(nsharpNative.nsharpLib.qc(smspd.getValue()) == 1 && nsharpNative.nsharpLib.qc(smdir.getValue())==1) {
			textStr = NsharpNativeConstants.STORM_MOTION_LINE;
			textStr = String.format(textStr,smdir.getValue(),NsharpConstants.DEGREE_SYMBOL,
					smspd.getValue(), nsharpNative.nsharpLib.kt_to_mps(smspd.getValue()));
		}
		else {
			textStr = NsharpNativeConstants.STORM_MOTION_MISSING;
		}
		target.drawString(myFont, textStr, rect.x + rect.width /4, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		
		//yellow and bold for parcel header 
		target.drawString(myFont, NsharpNativeConstants.STORM_HELICITY_STR, rect.x + rect.width /4, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		textStr = NsharpNativeConstants.STORM_LAYER_POS_STR;
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
   	    target.drawLine(rect.x, curY, 0.0, rect.x+rect.width, curY, 0.0, NsharpConstants.color_white, 1);

		//calculate helicity for sfc-2 km
		totHeli = nsharpNative.nsharpLib.helicity( (float)0, (float)2000, smdir.getValue(), smspd.getValue(), phel, nhel);
		if(nsharpNative.nsharpLib.qc(phel.getValue()) ==1 && nsharpNative.nsharpLib.qc(nhel.getValue())==1) {
			textStr = NsharpNativeConstants.STORM_SFC2KM_LINE;
			textStr = String.format(textStr,phel.getValue(), nhel.getValue(), totHeli,
					NsharpConstants.SQUARE_SYMBOL, NsharpConstants.SQUARE_SYMBOL);
			
		}
		else {
			textStr = NsharpNativeConstants.STORM_SFC2KM_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		//calculate helicity for sfc-3 km
		totHeli = nsharpNative.nsharpLib.helicity( (float)0, (float)3000, smdir.getValue(), smspd.getValue(), phel, nhel);
		if(nsharpNative.nsharpLib.qc(phel.getValue()) == 1 && nsharpNative.nsharpLib.qc(nhel.getValue())==1) {
			textStr = NsharpNativeConstants.STORM_SFC3KM_LINE;
			textStr = String.format(textStr,phel.getValue(), nhel.getValue(), totHeli,
					NsharpConstants.SQUARE_SYMBOL, NsharpConstants.SQUARE_SYMBOL);
		}
		else {
			textStr = NsharpNativeConstants.STORM_SFC3KM_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		//calculate helicity for LPL - LFC
		totHeli = nsharpNative.nsharpLib.helicity( (float)-1, (float)-1, smdir.getValue(), smspd.getValue(), phel, nhel);
		if(nsharpNative.nsharpLib.qc(phel.getValue())==1 && nsharpNative.nsharpLib.qc(nhel.getValue())==1) {
			textStr = NsharpNativeConstants.STORM_LPL_LFC_LINE;
			textStr = String.format(textStr,phel.getValue(), nhel.getValue(), totHeli,
					NsharpConstants.SQUARE_SYMBOL, NsharpConstants.SQUARE_SYMBOL);
		}
		else {
			textStr = NsharpNativeConstants.STORM_LPL_LFC_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		//yellow and bold for  header 
		target.drawString(myFont, NsharpNativeConstants.STORM_WIND_STR, rect.x + rect.width /4, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		textStr = NsharpNativeConstants.STORM_LAYER_VECTOR_STR;
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		target.drawLine(rect.x, curY, 0.0, rect.x+rect.width, curY, 0.0, NsharpConstants.color_white, 1);

		//calculate pressure-weighted SR mean wind  at sfc-2 km
		nsharpNative.nsharpLib.sr_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(0)), 
				nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(2000)), smdir.getValue(), smspd.getValue(), 
				mnu, mnv, wdir, wspd);
		if(nsharpNative.nsharpLib.qc(wdir.getValue())==1) {
			textStr = NsharpNativeConstants.STORM_SFC2KM_VECT_LINE;
			textStr = String.format(textStr,wdir.getValue(), wspd.getValue(), nsharpNative.nsharpLib.kt_to_mps(wspd.getValue()));
		}
		else {
			textStr = NsharpNativeConstants.STORM_SFC2KM_VECT_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		//calculate pressure-weighted SR mean wind  at 4-6 km
		//System.out.println("msl(4000))="+ nsharpNative.nsharpLib.msl(4000) + "i_pres(msl(4000))" + nsharpNative.nsharpLib.i_pres(nsharpNative.nsharpLib.msl(4000)));
		//System.out.println("msl(6000))="+ nsharpNative.nsharpLib.msl(6000) + "i_pres(msl(6000))" + nsharpNative.nsharpLib.i_pres(nsharpNative.nsharpLib.msl(6000)));
		//System.out.println("dir "+ smdir.getValue()+ " spd " + smspd.getValue());
		nsharpNative.nsharpLib.sr_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(4000)), 
				nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(6000)), smdir.getValue(), smspd.getValue(), 
				mnu, mnv, wdir, wspd);
		if(nsharpNative.nsharpLib.qc(wdir.getValue()) == 1) {
			textStr = NsharpNativeConstants.STORM_4_6KM_VECT_LINE;
			//System.out.println("wdir "+ wdir.getValue() + " widSp " + wspd.getValue());
			textStr = String.format(textStr,wdir.getValue(), wspd.getValue(), nsharpNative.nsharpLib.kt_to_mps(wspd.getValue()));
		}
		else {
			textStr = NsharpNativeConstants.STORM_4_6KM_VECT_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;

		//calculate pressure-weighted SR mean wind  at 9-11 km
		nsharpNative.nsharpLib.sr_wind( nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(9000)), 
				nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(11000)), smdir.getValue(), smspd.getValue(), 
				mnu, mnv, wdir, wspd);
		if(nsharpNative.nsharpLib.qc(wdir.getValue())==1) {
			textStr = NsharpNativeConstants.STORM_9_11KM_VECT_LINE;
			textStr = String.format(textStr,wdir.getValue(), wspd.getValue(), nsharpNative.nsharpLib.kt_to_mps(wspd.getValue()));
		}
		else {
			textStr = NsharpNativeConstants.STORM_9_11KM_VECT_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
	
        //myFont.dispose();
	}
	@SuppressWarnings("deprecation")
	private void drawPanel8(IGraphicsTarget target,Rectangle rect) 
    throws VizException {
    	extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        //myFont = target.initializeFont(fontName, fontSize, null);
        String splitedStr[];
        String textStr;
        curY = rect.y;
		/*
		 * Chin's NOTE::::this function is coded based on legacy nsharp software 		
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
		FloatByReference mnu= new FloatByReference(0);
		FloatByReference mnv= new FloatByReference(0);
		FloatByReference wdir= new FloatByReference(0);
		FloatByReference wspd= new FloatByReference(0);
		target.drawString(myFont, NsharpNativeConstants.MEAN_WIND_STR, rect.x + rect.width*0.4, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		
		//Calculate mean wind at 0-6 km 
		nsharpNative.nsharpLib.mean_wind( -1, nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.agl(6000)), mnu, mnv, wdir, wspd);
		//System.out.println("wsp ="+ wspd.getValue()+ " wdir "+ wdir.getValue() + " agl(6000)="+nsharpNative.nsharpLib.agl(6000)+ " preAt6000="+nsharpNative.nsharpLib.i_pres(nsharpNative.nsharpLib.agl(6000)));
		if(nsharpNative.nsharpLib.qc(wdir.getValue()) == 1 && nsharpNative.nsharpLib.qc(wspd.getValue())== 1) {
			textStr = NsharpNativeConstants.MEANWIND_SFC6KM_LINE;
			textStr = String.format(textStr,(wdir.getValue()), wspd.getValue(),
					nsharpNative.nsharpLib.kt_to_mps(wspd.getValue()));
		}
		else {
			textStr = NsharpNativeConstants.MEANWIND_SFC6KM_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		//Calculate mean wind at LFC-EL 
		nsharpNative.nsharpLib.mean_wind( -1, -1, mnu, mnv, wdir, wspd);
		if(nsharpNative.nsharpLib.qc(wdir.getValue())==1 && nsharpNative.nsharpLib.qc(wspd.getValue())==1) {
			textStr = NsharpNativeConstants.MEANWIND_LFC_EL_LINE;
			textStr = String.format(textStr,wdir.getValue(), wspd.getValue(),
					nsharpNative.nsharpLib.kt_to_mps(wspd.getValue()));
		}
		else {
			textStr = NsharpNativeConstants.MEANWIND_LFC_EL_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		//Calculate mean wind at 850-200 mb 
		nsharpNative.nsharpLib.mean_wind( 850,200, mnu, mnv, wdir, wspd);
		if(nsharpNative.nsharpLib.qc(wdir.getValue()) ==1 && nsharpNative.nsharpLib.qc(wspd.getValue())==1) {
			textStr = NsharpNativeConstants.MEANWIND_850_200MB_LINE;
			textStr = String.format(textStr,wdir.getValue(), wspd.getValue(),
					nsharpNative.nsharpLib.kt_to_mps(wspd.getValue()));
		}
		else {
			textStr = NsharpNativeConstants.MEANWIND_850_200MB_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		/*
		 * Chin's NOTE::::this function is coded based on legacy nsharp software 		
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
		target.drawString(myFont, NsharpNativeConstants.ENVIRONMENTAL_SHEAR_STR, rect.x + rect.width/3, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;

		
		textStr = NsharpNativeConstants.SHEAR_LAYER_DELTA_STR;
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/3, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		target.drawLine(rect.x, curY, 0.0, rect.x+rect.width, curY, 0.0, NsharpConstants.color_white, 1);
   		
		//Calculate wind shear at Low - 3 km
		nsharpNative.nsharpLib.wind_shear( -1, nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(3000)), 
				shu,shv,sdir,smag);
		if(nsharpNative.nsharpLib.qc(smag.getValue())==1) {
			textStr = NsharpNativeConstants.SHEAR_LOW_3KM_LINE;
			textStr = String.format(textStr,smag.getValue(), 
					nsharpNative.nsharpLib.kt_to_mps(smag.getValue()),
					nsharpNative.nsharpLib.kt_to_mps(smag.getValue())/.3F);
			//System.out.println("from cave "+smag.getValue() + " kt, " + nsharpNative.nsharpLib.kt_to_mps(smag.getValue())+ " m/s, Tot="+ nsharpNative.nsharpLib.kt_to_mps(smag.getValue())/.3F);
		}
		else {
			textStr = NsharpNativeConstants.SHEAR_LOW_3KM_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/3, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		//Calculate wind shear at Sfc - 2 km
		nsharpNative.nsharpLib.wind_shear( -1, nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(2000)), 
				shu,shv,sdir,smag);
		if(nsharpNative.nsharpLib.qc(smag.getValue())==1) {
			textStr = NsharpNativeConstants.SHEAR_SFC_2KM_LINE;
			textStr = String.format(textStr,smag.getValue(), 
					nsharpNative.nsharpLib.kt_to_mps(smag.getValue()),
					nsharpNative.nsharpLib.kt_to_mps(smag.getValue())/.2F);
		}
		else {
			textStr = NsharpNativeConstants.SHEAR_SFC_2KM_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/3, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;

		//Calculate wind shear at Sfc - 6 km
		nsharpNative.nsharpLib.wind_shear( -1, nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(6000)), 
				shu,shv,sdir,smag);
		if(nsharpNative.nsharpLib.qc(smag.getValue())==1) {
			textStr = NsharpNativeConstants.SHEAR_SFC_6KM_LINE;
			textStr = String.format(textStr,smag.getValue(), 
					nsharpNative.nsharpLib.kt_to_mps(smag.getValue()),
					nsharpNative.nsharpLib.kt_to_mps(smag.getValue())/.6F);
		}
		else {
			textStr = NsharpNativeConstants.SHEAR_SFC_6KM_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/3, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;

		//Calculate wind shear at Sfc - 12 km
		nsharpNative.nsharpLib.wind_shear( -1, nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(12000)), 
				shu,shv,sdir,smag);
		if(nsharpNative.nsharpLib.qc(smag.getValue())==1) {
			textStr = NsharpNativeConstants.SHEAR_SFC_12KM_LINE;
			textStr = String.format(textStr,smag.getValue(), 
					nsharpNative.nsharpLib.kt_to_mps(smag.getValue()),
					nsharpNative.nsharpLib.kt_to_mps(smag.getValue())/1.2F);
		}
		else {
			textStr = NsharpNativeConstants.SHEAR_SFC_12KM_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x + i*rect.width/3, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		//myFont.dispose();
	}
	@SuppressWarnings("deprecation")
	private void drawPanel9(IGraphicsTarget target,Rectangle rect) 
    throws VizException {
    	extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        //myFont = target.initializeFont(fontName, fontSize, null);
        String splitedStr[];
        String textStr;
        curY = rect.y;
		/*
		 * Chin's NOTE::::this function is coded based on legacy nsharp software 		
		 * show_initiation(): Displays thunderstorm initiation parameters,
		 * show_heavypcpn(), show_preciptype() and show_stormtype()
		 *  in xwvid3.c
		 *  
		 */
		FloatByReference fvalue2= new FloatByReference(0);
		FloatByReference fvalue3= new FloatByReference(0);
		FloatByReference wdir= new FloatByReference(0);
		FloatByReference wspd= new FloatByReference(0);
		FloatByReference fvalue= new FloatByReference(0);
		FloatByReference fvalue1= new FloatByReference(0);
		// get parcel data by calling native nsharp parcel() API. value is returned in pcl 
		// current parcel is already decided when page 1 is displyed. Note that page 1 is always
		// displayed before this page (page 4). Therefore, we dont have to call define_parcel() again.
		// set default
		//short currentParcel; 
		float layerPressure; 
		if(currentParcel == NsharpNativeConstants.PARCELTYPE_USER_DEFINED && NsharpParcelDialog.getAccess() != null){
			layerPressure = NsharpParcelDialog.getAccess().getUserDefdParcelMb();
		}
		else
			layerPressure = NsharpNativeConstants.parcelToLayerMap.get(currentParcel);
		nsharpNative.nsharpLib.define_parcel(currentParcel,layerPressure);
		
		
	    _parcel pcl = new _parcel();;
		_lplvalues lpvls = new _lplvalues();
		nsharpNative.nsharpLib.get_lpvaluesData(lpvls);		
		float sfctemp, sfcdwpt, sfcpres;
		sfctemp = lpvls.temp;
		sfcdwpt = lpvls.dwpt;
		sfcpres = lpvls.pres;				
		nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);		
		//_parcel.ByValue pcl_byvalue = new _parcel.ByValue();
		//nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl_byvalue);
		

		//CONVECTIVE_INITIATION
		target.drawString(myFont, NsharpNativeConstants.CONVECTIVE_INITIATION_STR, rect.x + rect.width/3, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		//CINH
		if(nsharpNative.nsharpLib.qc(pcl.bminus)==1) {
			textStr = NsharpNativeConstants.CONVECTIVE_CINH_LINE;
			textStr = String.format(textStr,pcl.bminus);
		}
		else {
			textStr = NsharpNativeConstants.CONVECTIVE_CINH_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}

		//cap
		if(nsharpNative.nsharpLib.qc(pcl.cap)==1 && nsharpNative.nsharpLib.qc(pcl.cappres)==1) {
			textStr = NsharpNativeConstants.CONVECTIVE_CAP_LINE;
			textStr = String.format(textStr,pcl.cap, pcl.cappres);
		}
		else {
			textStr = NsharpNativeConstants.CONVECTIVE_CAP_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  rect.width/2+ i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;

		//K-index
		nsharpNative.nsharpLib.k_index(fvalue);
		if(nsharpNative.nsharpLib.qc(fvalue.getValue())==1) {
			textStr = NsharpNativeConstants.CONVECTIVE_KINDEX_LINE;
			textStr = String.format(textStr,fvalue.getValue());
		}
		else {
			textStr = NsharpNativeConstants.CONVECTIVE_KINDEX_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}

		//Mean RH
		nsharpNative.nsharpLib.mean_relhum(fvalue, -1, -1);
		if(nsharpNative.nsharpLib.qc(fvalue.getValue())==1) {
			textStr = NsharpNativeConstants.CONVECTIVE_MEANRH_LINE;
			textStr = String.format(textStr,fvalue.getValue(), NsharpConstants.PERCENT_SYMBOL);
		}
		else {
			textStr = NsharpNativeConstants.CONVECTIVE_MEANRH_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  rect.width/2+ i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  2*charHeight;
		
		//Top of M layer
		nsharpNative.nsharpLib.top_moistlyr(fvalue);
		//System.out.println("top_moistlyr=" + fvalue.getValue() );
		
		if(nsharpNative.nsharpLib.qc(fvalue.getValue())==1) {
			float ht =
				nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(fvalue.getValue())));
			if(nsharpNative.nsharpLib.qc(ht)==1){
				textStr = NsharpNativeConstants.CONVECTIVE_TOP_LINE;
				textStr = String.format(textStr,fvalue.getValue(),ht);
			}
			else {
				textStr = NsharpNativeConstants.CONVECTIVE_TOP_MISSING;
			}
		}
		else {
			textStr = NsharpNativeConstants.CONVECTIVE_TOP_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		//LFC height
		if(nsharpNative.nsharpLib.qc(pcl.lfcpres)==1) {
			float ht =
				nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.lfcpres)));
			if(nsharpNative.nsharpLib.qc(ht)==1){
				textStr = NsharpNativeConstants.CONVECTIVE_LFC_LINE;
				textStr = String.format(textStr,pcl.lfcpres,ht);
			}
			else {
				textStr = NsharpNativeConstants.CONVECTIVE_LFC_MISSING;
			}
		}
		else {
			textStr = NsharpNativeConstants.CONVECTIVE_LFC_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		
		//STORM TYPE
		target.drawString(myFont, NsharpNativeConstants.STORM_TYPE_STR, rect.x + rect.width/3, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		
		//CAPE
		if(nsharpNative.nsharpLib.qc(pcl.bplus)==1) {
			textStr = NsharpNativeConstants.STORM_TYPE_CAPE_LINE;
			textStr = String.format(textStr,pcl.bplus);
		}
		else {
			textStr = NsharpNativeConstants.STORM_TYPE_CAPE_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}

		//EFF. SREH
		float hel=0;
		nsharpNative.nsharpLib.get_storm(wspd,wdir);
		if(nsharpNative.nsharpLib.qc(wdir.getValue()) ==1 && nsharpNative.nsharpLib.qc(wspd.getValue())==1) {
			hel =
				nsharpNative.nsharpLib.helicity( -1.0F, -1.0F, wdir.getValue(), wspd.getValue(), fvalue, fvalue1);
			if(nsharpNative.nsharpLib.qc(hel)==1){
				textStr = NsharpNativeConstants.STORM_TYPE_EFF_LINE;
				textStr = String.format(textStr,hel,NsharpConstants.SQUARE_SYMBOL,NsharpConstants.SQUARE_SYMBOL);
			}
			else {
				textStr = NsharpNativeConstants.STORM_TYPE_EFF_MISSING;
			}
		}
		else {
			textStr = NsharpNativeConstants.STORM_TYPE_EFF_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  rect.width/2+ i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;

		//EHI
		if(nsharpNative.nsharpLib.qc(pcl.bplus)==1) {
			float ehi =
				nsharpNative.nsharpLib.ehi( pcl.bplus, hel);
			if(nsharpNative.nsharpLib.qc(ehi)==1){
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
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}

		//3km Shear
		nsharpNative.nsharpLib.wind_shear( -1, -1, fvalue, fvalue1,fvalue2,fvalue3);
		if(nsharpNative.nsharpLib.qc(fvalue3.getValue())==1) {
			textStr = NsharpNativeConstants.STORM_TYPE_3KM_LINE;
			textStr = String.format(textStr,nsharpNative.nsharpLib.kt_to_mps(fvalue3.getValue()));			
		}
		else {
			textStr = NsharpNativeConstants.STORM_TYPE_3KM_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  rect.width/2+ i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;

		//BRN
		if(nsharpNative.nsharpLib.qc(pcl.brn)==1) {
			textStr = NsharpNativeConstants.STORM_TYPE_BRN_LINE;
			textStr = String.format(textStr,pcl.brn);
		}
		else {
			textStr = NsharpNativeConstants.STORM_TYPE_BRN_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}

		//BRN Shear
		//nsharpNative.nsharpLib.cave_bulk_rich(pcl.lplpres, pcl.bplus, fvalue );
		//System.out.println("bulk_rich fvalue = "+ fvalue.getValue());
		nsharpNative.nsharpLib.cave_bulk_rich2(fvalue );
		if(nsharpNative.nsharpLib.qc(fvalue.getValue())==1) {
			textStr = NsharpNativeConstants.STORM_TYPE_BRNSHEAR_LINE;
			textStr = String.format(textStr,fvalue.getValue(),NsharpConstants.SQUARE_SYMBOL, NsharpConstants.SQUARE_SYMBOL);
		}
		else
			textStr = NsharpNativeConstants.STORM_TYPE_BRNSHEAR_MISSING;	
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  rect.width/2+ i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		//PRECIPITATION_TYPE
		target.drawString(myFont, NsharpNativeConstants.PRECIPITATION_TYPE_STR, rect.x + rect.width/3, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		
		//Melting Level
		float web = nsharpNative.nsharpLib.wb_lvl( 0, fvalue );
		if(nsharpNative.nsharpLib.qc(web)==1) {
			
			float aglft = nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(web)));
			if(nsharpNative.nsharpLib.qc(aglft)==1){
				textStr = NsharpNativeConstants.PRECIPITATION_MELTING_LINE;
				textStr = String.format(textStr,aglft, web);
			}
			else 
				textStr = NsharpNativeConstants.PRECIPITATION_MELTING_MISSING;
		}
		else {
			textStr = NsharpNativeConstants.PRECIPITATION_MELTING_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		//HEAVY_RAINFAL
		target.drawString(myFont, NsharpNativeConstants.HEAVY_RAINFALL_STR, rect.x + rect.width/3, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		
		//Rogash_QPF, Chin: note: BigNsharp has different implementation of Rogash_QPF()
		// We are using bigNsharp now.
		nsharpNative.nsharpLib.Rogash_QPF(fvalue);
		if(nsharpNative.nsharpLib.qc(fvalue.getValue())==1) {
			textStr = NsharpNativeConstants.HEAVY_ROGASH_LINE;
			textStr = String.format(textStr,fvalue.getValue());
		}
		else {
			textStr = NsharpNativeConstants.HEAVY_ROGASH_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		
		/*
		 * Compute MDPI_WINDEX (TL)
		 */
		/*		
		FloatByReference windex = new FloatByReference(0);
		float mdpi = nsharpNative.nsharpLib.cave_mdpi_windex(windex);
		System.out.println (" \n\n MDPI: Microburst Day Potential Index");
		System.out.println (" MDPI: " + mdpi );
		System.out.println (" Max wind gust: " + windex.getValue() );	
		*/
		
		/* 
		 * Compute DMPI (TL)
		 */
		/*
		int dmpi = nsharpNative.nsharpLib.cave_dmpi();
		System.out.println (" \n\n DMPI: Dry Microburst Potential Index");
		System.out.println (" DMPI: " + dmpi ); 		
		*/
		
		/*
		 * Compute Soaring Index (TL)
		 */
		/*
		FloatByReference ctax = new FloatByReference(0);
		FloatByReference zconv = new FloatByReference(0);
		FloatByReference tconv= new FloatByReference(0);
		FloatByReference zthrm = new FloatByReference(0);
		FloatByReference tthrm = new FloatByReference(0);
		FloatByReference soar = new FloatByReference(0);
		FloatByReference ttrig = new FloatByReference(0);

		nsharpNative.nsharpLib.cave_soar ( -9999.F, ctax, zconv,tconv, zthrm, tthrm, soar, ttrig);   	
		System.out.println (" \n\n Soaring Index ");
		System.out.println (" potential temp of forecast Tmax (C): " + ctax.getValue() );
		System.out.println (" height of minimum effective convection (m): " + zconv.getValue() );
		System.out.println (" temp of minimum effective convection (C): " + tconv.getValue() );
		System.out.println (" height at thrm altitude (m): " + zthrm.getValue() );
		System.out.println (" temperature at thrm altitude (C): " + tthrm.getValue() );
		System.out.println (" soaring index (ft/min): " + soar.getValue() );
		System.out.println (" triggering temp (C): " + ttrig.getValue() );	
		*/
       // myFont.dispose();
	}
	@SuppressWarnings("deprecation")
	private void drawPanel10(IGraphicsTarget target,Rectangle rect) 
    throws VizException {
    	extent = new PixelExtent(rect);
        target.setupClippingPlane(extent);
        //myFont = target.initializeFont(fontName, fontSize, null);
        String splitedStr[];
        String textStr;
        curY = rect.y;
		/*
		 * Chin's NOTE::::this function is coded based on legacy nsharp software 		
		 * show_hailpot(), show_torpot()
		 *  in xwvid3.c
		 *  
		 */
		FloatByReference fvalue2= new FloatByReference(0);
		FloatByReference fvalue3= new FloatByReference(0);
		FloatByReference wdir= new FloatByReference(0);
		FloatByReference wspd= new FloatByReference(0);
		FloatByReference fvalue= new FloatByReference(0);
		FloatByReference fvalue1= new FloatByReference(0);
		
		target.drawString(myFont, NsharpNativeConstants.SEVERE_POTENTIAL_STR, rect.x + rect.width/3, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_cyan, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		
		target.drawString(myFont, NsharpNativeConstants.SEVERE_HAIL_POTENTIAL_STR, rect.x + rect.width/4, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		
	    _parcel pcl = new _parcel();;
		_lplvalues lpvls = new _lplvalues();
		nsharpNative.nsharpLib.get_lpvaluesData(lpvls);		
		float sfctemp, sfcdwpt, sfcpres;
		sfctemp = lpvls.temp;
		sfcdwpt = lpvls.dwpt;
		sfcpres = lpvls.pres;				
		nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);
		//CAPE
		if(nsharpNative.nsharpLib.qc(pcl.bplus)==1) {
			textStr = NsharpNativeConstants.SEVERE_CAPE_LINE;
			textStr = String.format(textStr,pcl.bplus);
		}
		else {
			textStr = NsharpNativeConstants.SEVERE_CAPE_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}

		// WBZ level
		float wbzft = nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.wb_lvl( 0, fvalue ))));
		if(nsharpNative.nsharpLib.qc(wbzft)==1) {
			textStr = NsharpNativeConstants.SEVERE_WBZ_LINE;
			textStr = String.format(textStr,wbzft);
		}
		else {
			textStr = NsharpNativeConstants.SEVERE_WBZ_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  rect.width/2+ i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
		//Mid Level RH
		nsharpNative.nsharpLib.mean_relhum( fvalue, 700, 500);
		if(nsharpNative.nsharpLib.qc(fvalue.getValue())==1) {
			textStr = NsharpNativeConstants.SEVERE_MIDRH_LINE;
			textStr = String.format(textStr,fvalue.getValue(),NsharpConstants.PERCENT_SYMBOL);
		}
		else {
			textStr = NsharpNativeConstants.SEVERE_MIDSRW_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		
		//FZG level
		float fgzft = nsharpNative.nsharpLib.mtof(nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.temp_lvl( 0, fvalue ))));
		if(nsharpNative.nsharpLib.qc(fgzft)==1) {
			textStr = NsharpNativeConstants.SEVERE_FGZ_LINE;
			textStr = String.format(textStr,fgzft);
		}
		else {
			textStr = NsharpNativeConstants.SEVERE_FGZ_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  rect.width/2+ i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;

		//EL Storm
		nsharpNative.nsharpLib.get_storm(wspd,wdir);
		nsharpNative.nsharpLib.sr_wind( pcl.elpres+25, pcl.elpres-25,wdir.getValue(), wspd.getValue(), fvalue,fvalue1,fvalue2,fvalue3);
		if(nsharpNative.nsharpLib.qc(fvalue3.getValue())==1) {
			textStr = NsharpNativeConstants.SEVERE_ELSTORM_LINE;
			textStr = String.format(textStr,fvalue3.getValue());
		}
		else {
			textStr = NsharpNativeConstants.SEVERE_ELSTORM_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		// CHI1
		//_parcel.ByValue pcl_byvalue = new _parcel.ByValue();
		// pr = nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl_byvalue);
		//Assigne values that needed for bulk_rich()
		//pcl_byvalue.lplpres = sfcpres;
		//pcl_byvalue.bplus = pcl.bplus;
		nsharpNative.nsharpLib.cave_bulk_rich2( fvalue );
		float rtn = (pcl.bplus * fvalue.getValue()) / nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(nsharpNative.nsharpLib.wb_lvl( 0, fvalue1 )));
		if(nsharpNative.nsharpLib.qc(rtn)==1) {
			textStr = NsharpNativeConstants.SEVERE_CHI1_LINE;
			textStr = String.format(textStr,rtn);
		}
		else {
			textStr = NsharpNativeConstants.SEVERE_CHI1_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		//CHI2
		nsharpNative.nsharpLib.Mean_WBtemp( fvalue2, -1, -1);
		if(nsharpNative.nsharpLib.qc(rtn/fvalue2.getValue())==1) {
			textStr = NsharpNativeConstants.SEVERE_CHI2_LINE;
			textStr = String.format(textStr,rtn/fvalue2.getValue());
		}
		else {
			textStr = NsharpNativeConstants.SEVERE_CHI2_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  rect.width/2+ i*rect.width/4, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;

		//Avg BL 
		nsharpNative.nsharpLib.Mean_WBtemp( fvalue, -1, -1);
		if(nsharpNative.nsharpLib.qc(fvalue.getValue())==1) {
			textStr = NsharpNativeConstants.SEVERE_AVGBL_LINE;
			textStr = String.format(textStr,fvalue.getValue(),NsharpConstants.DEGREE_SYMBOL);
		}
		else {
			textStr = NsharpNativeConstants.SEVERE_AVGBL_MISSING;
		}
		target.drawString(myFont, textStr, rect.x, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		curY = curY +  charHeight;

		//TORNADO_POTENTIAL
		target.drawString(myFont, NsharpNativeConstants.SEVERE_TORNADO_POTENTIAL_STR, rect.x + rect.width/4, curY , 0.0,
				TextStyle.NORMAL, NsharpConstants.color_yellow, HorizontalAlignment.LEFT,
				VerticalAlignment.TOP, null);
		curY = curY +  charHeight;
		
		//Low SRW Sfc
		float blyr = nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(0));
		float tlyr = pcl.lfcpres;
		if(tlyr > 0){
			nsharpNative.nsharpLib.sr_wind( blyr, tlyr, wdir.getValue(), wspd.getValue(), fvalue,fvalue1,fvalue2,fvalue3);
			if(nsharpNative.nsharpLib.qc(fvalue3.getValue())==1) {
				textStr = NsharpNativeConstants.SEVERE_LOWSRWSFC_LINE;
				textStr = String.format(textStr,fvalue3.getValue());
			}
			else {
				textStr = NsharpNativeConstants.SEVERE_LOWSRWSFC_MISSING;
			}
		}
		else {
			textStr = NsharpNativeConstants.SEVERE_LOWSRWSFC_MISSING;
		}
		splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;	
		
		blyr = pcl.lfcpres;
        tlyr = nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.ihght(pcl.lfcpres) + 4000);
        if((tlyr > 0)&&(blyr > 0)) {
			nsharpNative.nsharpLib.sr_wind( blyr, tlyr, wdir.getValue(), wspd.getValue(), fvalue,fvalue1,fvalue2,fvalue3);
			if(nsharpNative.nsharpLib.qc(fvalue3.getValue())==1) {
				textStr = NsharpNativeConstants.SEVERE_MIDSRW_LINE;
				textStr = String.format(textStr,fvalue3.getValue());
			}
			else {
				textStr = NsharpNativeConstants.SEVERE_MIDSRW_MISSING;
			}
				
        }
        else {
			textStr = NsharpNativeConstants.SEVERE_MIDSRW_MISSING;
		}
        splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		curY = curY +  charHeight;
		
        blyr = nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.ihght(pcl.elpres) - 4000);
        tlyr = pcl.elpres;
        if((tlyr > 0)&&(blyr > 0)) {
			nsharpNative.nsharpLib.sr_wind( blyr, tlyr, wdir.getValue(), wspd.getValue(), fvalue,fvalue1,fvalue2,fvalue3);
			if(nsharpNative.nsharpLib.qc(fvalue3.getValue())==1) {
				textStr = NsharpNativeConstants.SEVERE_LOWSRWEL_LINE;
				textStr = String.format(textStr,fvalue3.getValue());
			}
			else {
				textStr = NsharpNativeConstants.SEVERE_LOWSRWEL_MISSING;
			}
					
        }
        else {
			textStr = NsharpNativeConstants.SEVERE_LOWSRWEL_MISSING;
		}
        splitedStr = textStr.split("_", -1);
		for (int i =0; i < splitedStr.length; i++){
			target.drawString(myFont, splitedStr[i], rect.x +  i*rect.width/2, curY , 0.0,
                TextStyle.NORMAL, NsharpConstants.color_white, HorizontalAlignment.LEFT,
                VerticalAlignment.TOP, null);
		}
		
		/*
		 * Compute vertical motion stemming from thermal buoyancy. (TL)
		 */		
		/*
		float [] vvel = null;
		vvel = new float[150];
		float wmax = nsharpNative.nsharpLib.cave_wmax (vvel);
		System.out.println (" \n\n Thermodynamic buoyancy ");
		for (int i = 0; i < vvel.length; i++ ) {
			if ( vvel[i] > 0. ) { 
				System.out.print ( " W [" + i +"]" + " = " + vvel[i] + "\n");
			}
		}	 
		System.out.println (" Max vertical motions: " + wmax );
		*/
        //myFont.dispose();
	}

}
