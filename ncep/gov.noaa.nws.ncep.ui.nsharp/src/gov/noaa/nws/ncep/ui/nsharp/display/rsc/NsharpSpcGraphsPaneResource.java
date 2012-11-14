package gov.noaa.nws.ncep.ui.nsharp.display.rsc;
/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 07/10/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpAbstractPaneDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._lplvalues;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative.NsharpLibrary._parcel;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpPaletteWindow;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.sun.jna.ptr.FloatByReference;

public class NsharpSpcGraphsPaneResource extends NsharpAbstractPaneResource{
	private double spcLeftXOrig;
	private double spcRightXOrig;
	private double spcYOrig;
	private double spcYEnd;
	private double spcWidth;
	private double spcFrameWidth;
	private double spcHeight;
	private NsharpConstants.SPCGraph leftGraph = NsharpConstants.SPCGraph.EBS;
	private NsharpConstants.SPCGraph rightGraph = NsharpConstants.SPCGraph.STP;
	private static int left =0;
	private static int right =1;
	private double xpos, xstart, xend;
	private double ypos, ystart;
	private double hRatio;
	/* sb supercell mean ebs values by percentage of storm depth */
    /* values in m/s */
    /*	s10 = 9.0;
		s20 = 13.8;	
		s30 = 17.2;
		s40 = 20.0;
		s50 = 22.2;
		s60 = 24.4;
		s70 = 27.0;
		s80 = 29.6;
		s90 = 31.3;
		s100 = 31.1;
     */
    /* values in kt */
	private float supercell[] = {18.0f,27.6f,34.4f,40.0f,44.4f,48.8f,54.0f,59.2f, 62.6f,62.2f};
	
	/* mrgl supercell mean ebs values by percentage of storm depth */
    /* values in m/s */
    /*	m10 = 6.2;
	        m20 = 10.2;
	        m30 = 12.8;
	        m40 = 14.3;
	        m50 = 16.1;
	        m60 = 18.0;
	        m70 = 19.9;
	        m80 = 21.8;
	        m90 = 23.8;
	        m100 = 24.2;
     */
    /* values in kt */
	private float mrglSupercell[] = {12.4f,20.4f,25.6f, 28.6f, 32.2f, 36.0f, 39.8f,43.6f,47.6f, 48.4f};
	
	/* nonsupercell mean ebs values by percentage of storm depth */
	/* values in m/s */
    /*      n10 = 4.1;
	        n20 = 6.1;
	        n30 = 7.2;
	        n40 = 7.9;
	        n50 = 8.5;
	        n60 = 10.0;
	        n70 = 11.6;
	        n80 = 13.5;
	        n90 = 15.2;
	        n100 = 16.0;
     */
    /* values in kt */
	private float nonSupercell[] = {8.2f, 12.2f,14.4f,15.8f, 17.0f, 20.0f, 23.2f,27.0f, 30.4f, 32.0f};
	/*private IWireframeShape ebsBkgLblShape;
	private IWireframeShape ebsBkgLineShape;
	private IWireframeShape ebsSupercellShape;
	private IWireframeShape ebsMrglSupShape;
	private IWireframeShape ebsNonSupSgape;*/
	public NsharpSpcGraphsPaneResource(AbstractResourceData resourceData,
			LoadProperties loadProperties, NsharpAbstractPaneDescriptor desc) {
		super(resourceData, loadProperties, desc);
		leftGraph = NsharpPaletteWindow.getLeftGraph();
		rightGraph = NsharpPaletteWindow.getRightGraph();
	}
	private void underDevelopment(int side) throws VizException{
		double xpos;
		if(side == left )
			xpos = spcLeftXOrig+ spcFrameWidth/2;
		else
			xpos = spcRightXOrig+ spcFrameWidth/2;
		DrawableString str = new DrawableString("under development", NsharpConstants.color_green);  
   		str.font = font12;
   		str.horizontalAlignment = HorizontalAlignment.LEFT;
		str.verticallAlignment = VerticalAlignment.TOP;
		ypos = spcYOrig + spcHeight/2;
		str.setCoordinates(xpos, ypos);
		target.drawStrings(str);
	}
	private void setXyStartingPosition(int side){
		ystart = spcYOrig;
		if(side == left ){
			xstart = spcLeftXOrig+   0.5*charWidth;
			xend = spcLeftXOrig + spcFrameWidth;
	}
		else{
			xstart = spcRightXOrig + 0.5*charWidth;
			xend = spcRightXOrig + spcFrameWidth;
		}
	}
	/*
	 * This function is based on show_ebs_stats() in xwvid3.c of BigNsharp
	 */
	/*
	private void createEbsShapes(){
		if(ebsBkgLblShape == null)
			ebsBkgLblShape = target.createWireframeShape(false,descriptor );
		if(ebsBkgLineShape == null)
			ebsBkgLineShape = target.createWireframeShape(false,descriptor );
		if(ebsSupercellShape == null)
			ebsSupercellShape = target.createWireframeShape(false,descriptor );
		if(ebsMrglSupShape == null)
			ebsMrglSupShape = target.createWireframeShape(false,descriptor );
		if(ebsNonSupSgape == null)
			ebsNonSupSgape = target.createWireframeShape(false,descriptor );
		ypos = ystart+ 3 * charHeight;
		//----- Plot Y-Coordinate hash marks, 0 - 70 kt ----- 
		double [][] dummyline = {{0,0}, {0,0}};
		ebsBkgLblShape.addLineSegment(dummyline);
		int maxval = 70;
		double ygap = (spcYEnd - 2 * charHeight- ypos)/7.0;
		for (int i = maxval; i >=0; i = i - 10) {
			xpos = xstart;
			 double [] lblXy = {xpos, ypos};
			ebsBkgLblShape.addLabel(Integer.toString(i), lblXy);
			xpos = xpos + 2 *charWidth;
			double [][] line = {{xpos, ypos},{xend, ypos}};
			ebsBkgLineShape.addLineSegment(line);
			ypos = ypos + ygap;
		}
	}*/
	private void plotEBS(int side) throws VizException{
		List<DrawableLine> lineList = new ArrayList<DrawableLine>();
		List<DrawableString> strList = new ArrayList<DrawableString>();	
		this.font12.setSmoothing(false);
		this.font12.setScaleFont(false);
		setXyStartingPosition(side);
		DrawableString titleStr = new DrawableString("Effective Bulk Wind Difference (kt, y axis)", NsharpConstants.color_white);  
   		titleStr.font = font12;
   		titleStr.horizontalAlignment = HorizontalAlignment.LEFT;
		titleStr.verticallAlignment = VerticalAlignment.TOP;
		xpos = xstart + 0.1 *spcFrameWidth;
		ypos = ystart;
		titleStr.setCoordinates(xpos, ypos);
		strList.add(titleStr);
		//target.drawStrings(titleStr);
		DrawableString subTStr1 = new DrawableString("supercell    mrgl supercell (dashed)    ", NsharpConstants.color_lawngreen);
		ypos = ypos + 1.5* charHeight;
		subTStr1.font = font10;
		subTStr1.horizontalAlignment = HorizontalAlignment.LEFT;
		subTStr1.verticallAlignment = VerticalAlignment.TOP;
		subTStr1.setCoordinates(xpos, ypos);
		strList.add(subTStr1);
		DrawableString subTStr2 = new DrawableString("non-supercell", NsharpConstants.color_darkorange);  
		subTStr2.font = font10;
   		subTStr2.horizontalAlignment = HorizontalAlignment.LEFT;
		subTStr2.verticallAlignment = VerticalAlignment.TOP;
		xpos = xpos + (target.getStringsBounds(subTStr1).getWidth())*hRatio;
		subTStr2.setCoordinates(xpos, ypos);
		strList.add(subTStr2);
		//target.drawStrings(subTStr1, subTStr2);
	
		ypos = ypos + 1.5* charHeight;
		//----- Plot Y-Coordinate hash marks, 0 - 70 kt ----- 
		int maxval = 70;
		//knotDist = one Knot distance in Y-axis
		// 70 Knots in total at Y axis
		double knotDist = (spcYEnd - 2 * charHeight- ypos)/70.0;
		for (int i = maxval; i >=0; i = i - 10) {
			DrawableString lb = new DrawableString(Integer.toString(i), NsharpConstants.color_white);
			lb.font = font10;
			lb.horizontalAlignment = HorizontalAlignment.LEFT;
			lb.verticallAlignment = VerticalAlignment.MIDDLE;
			xpos = xstart;
			lb.setCoordinates(xpos, ypos);
			strList.add(lb);
			DrawableLine line = new DrawableLine();
			line.lineStyle = LineStyle.DASHED;
			line.basics.color = NsharpConstants.temperatureColor;
			line.width = 1;
			xpos = xpos + 2 *charWidth;
			line.setCoordinates(xpos, ypos);
			line.addPoint(xend, ypos);
			lineList.add(line);
			ypos = ypos + 10* knotDist;
		}
		//target.drawLine(lineList.toArray(new DrawableLine[lineList.size()]));
		//x axis 10 -100	
		double xgap = (spcFrameWidth - 5 * charWidth) /10 ;
		ypos = spcYEnd- 0.5*charHeight;
		xpos = xstart;
		double cellYPosStart = spcYEnd - 2 * charHeight;
		// supercell line
		DrawableLine supercellline = new DrawableLine();
		supercellline.lineStyle = LineStyle.SOLID;
		supercellline.basics.color = NsharpConstants.color_lawngreen;
		supercellline.width = 2;
		// meglsupercell line
		DrawableLine mrglSupercellline = new DrawableLine();
		mrglSupercellline.lineStyle = LineStyle.DASHED;
		mrglSupercellline.basics.color = NsharpConstants.color_lawngreen;
		mrglSupercellline.width = 2;// nonsupercell line
		DrawableLine nonSupercellline = new DrawableLine();
		nonSupercellline.lineStyle = LineStyle.SOLID;
		nonSupercellline.basics.color = NsharpConstants.color_darkorange;
		nonSupercellline.width = 2;
		for (int i = 10; i <= 100; i=i+10) {
			xpos = xpos+ xgap;
			//lb for x-axis number
			DrawableString lb = new DrawableString(Integer.toString(i), NsharpConstants.color_white);
			lb.font = font10;
			lb.horizontalAlignment = HorizontalAlignment.CENTER;
			lb.verticallAlignment = VerticalAlignment.BOTTOM;	
			lb.setCoordinates(xpos, ypos);
			strList.add(lb);
			int cellIndex = i/10-1;
			nonSupercellline.addPoint(xpos, cellYPosStart - nonSupercell[cellIndex] * knotDist);
			supercellline.addPoint(xpos, cellYPosStart - supercell[cellIndex] * knotDist);
			mrglSupercellline.addPoint(xpos, cellYPosStart - mrglSupercell[cellIndex] * knotDist);
		}
		lineList.add(nonSupercellline);
		lineList.add(supercellline);
		lineList.add(mrglSupercellline);
		//target.drawLine(supercellline,mrglSupercellline,nonSupercellline);
		//call get_topBotPres to set p_top and p_bot
   		FloatByReference topPF= new FloatByReference(0);
		FloatByReference botPF= new FloatByReference(0);
		nsharpNative.nsharpLib.get_effectLayertopBotPres(topPF, botPF);
		float base = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(botPF.getValue()));
		nsharpNative.nsharpLib.define_parcel(NsharpNativeConstants.PARCELTYPE_MOST_UNSTABLE,400f);
		_lplvalues lpvls;
		_parcel pcl;
		lpvls = new _lplvalues();
		nsharpNative.nsharpLib.get_lpvaluesData(lpvls);
		float sfctemp, sfcdwpt, sfcpres;
		sfctemp = lpvls.temp;
		sfcdwpt = lpvls.dwpt;
		sfcpres = lpvls.pres;
		// get parcel data by calling native nsharp parcel() API. value is returned in pcl 
		pcl = new _parcel();
		nsharpNative.nsharpLib.parcel( -1.0F, -1.0F, sfcpres, sfctemp, sfcdwpt, pcl);
		float el = nsharpNative.nsharpLib.agl(nsharpNative.nsharpLib.ihght(pcl.elpres));
		if(botPF.getValue() < 0 || pcl.bplus < 100.0 || el == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA){
			xpos = xstart + 0.2 *spcFrameWidth;
			DrawableString lb = new DrawableString("No Effective Inflow Layer", NsharpConstants.color_yellow);
			lb.font = font12;
			lb.horizontalAlignment = HorizontalAlignment.LEFT;
			lb.verticallAlignment = VerticalAlignment.TOP;	
			lb.setCoordinates(xpos, cellYPosStart - 7 * knotDist);
			strList.add(lb);
		}
		else{
			float depth = (el - base);
			xpos = xstart;
			FloatByReference ix1= new FloatByReference(0);
			FloatByReference ix2= new FloatByReference(0);
			FloatByReference ix3= new FloatByReference(0);
			FloatByReference ix4= new FloatByReference(0);
			DrawableLine ebsline = new DrawableLine();
			ebsline.lineStyle = LineStyle.SOLID;
			ebsline.basics.color = NsharpConstants.color_yellow;
			ebsline.width = 3;
			for (int i = 10; i <= 100; i=i+10) {
				xpos = xpos+ xgap;
				nsharpNative.nsharpLib.wind_shear(botPF.getValue(), nsharpNative.nsharpLib.ipres(nsharpNative.nsharpLib.msl(base + (depth * 0.1f * (i/10)))), ix1, ix2, ix3, ix4);
				float ebs = ix4.getValue();
				if(ebs > 70) 
					ebs = 70;
				ebsline.addPoint(xpos, cellYPosStart - ebs * knotDist);
			}
			lineList.add(ebsline);
		}
		target.drawStrings(strList.toArray(new DrawableString[strList.size()])); // x-axis mark number
		target.drawLine(lineList.toArray(new DrawableLine[lineList.size()]));
	}
	@Override
	protected void paintInternal(IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		super.paintInternal(target, paintProps);
		//defineCharHeight(font10);
		if(rscHandler== null)
			return;
		this.font10.setSmoothing(false);
		this.font10.setScaleFont(false);
		hRatio = paintProps.getView().getExtent().getWidth() / paintProps.getCanvasBounds().width;
		DrawableLine line = new DrawableLine();
		line.lineStyle = LineStyle.SOLID;
		line.basics.color = NsharpConstants.color_white;
		line.width = 1;
		line.setCoordinates(spcRightXOrig, spcYOrig);
		line.addPoint(spcRightXOrig, spcYOrig+spcHeight);
		target.drawLine(line);
		PixelExtent extent = new PixelExtent(new Rectangle((int)spcLeftXOrig, (int)spcYOrig,(int)spcFrameWidth,(int)spcHeight ));
		target.setupClippingPlane(extent);
		switch(leftGraph){
		case EBS:
			plotEBS(left);
			break;
		case STP:
			underDevelopment(0);
			break;
		case SHIP:
			underDevelopment(0);
			break;
		case FIRE:
			underDevelopment(0);
			break;
		case WINTER:
			underDevelopment(0);
			break;
		case HAIL:
			underDevelopment(0);
			break;
		case SARS:
			underDevelopment(0);
			break;
		}
		target.clearClippingPlane();
		extent = new PixelExtent(new Rectangle((int)spcRightXOrig, (int)spcYOrig,(int)spcFrameWidth,(int)spcHeight ));
		target.setupClippingPlane(extent);
		switch(rightGraph){
		case EBS:
			plotEBS(right);
			break;
		case STP:
			underDevelopment(1);
			break;
		case SHIP:
			underDevelopment(1);
			break;
		case FIRE:
			underDevelopment(1);
			break;
		case WINTER:
			underDevelopment(1);
			break;
		case HAIL:
			underDevelopment(1);
			break;
		case SARS:
			underDevelopment(1);
			break;
		}
		target.clearClippingPlane();
	}

	@Override
	protected void initInternal(IGraphicsTarget target) throws VizException {
		super.initInternal(target);
		
	}
	/*private void disposeEbsShape(){
		if(ebsBkgLblShape != null)
			ebsBkgLblShape.dispose();
		if(ebsBkgLineShape != null)
			ebsBkgLineShape.dispose();
		if(ebsSupercellShape != null)
			ebsSupercellShape.dispose();
		if(ebsMrglSupShape != null)
			ebsMrglSupShape.dispose();
		if(ebsNonSupSgape != null)
			ebsNonSupSgape.dispose();
	}*/
	@Override
	protected void disposeInternal() {
		super.disposeInternal();
	}
	@Override
	public void handleResize() {
		
		super.handleResize();
		IExtent ext = getDescriptor().getRenderableDisplay().getExtent();
		ext.reset();
		defineCharHeight(font10);
		spcLeftXOrig = (ext.getMinX());
		spcYOrig =  ext.getMinY();
		spcWidth =  (ext.getWidth());
		spcFrameWidth = spcWidth/2;
		spcRightXOrig = spcLeftXOrig + spcFrameWidth;
		spcHeight = ext.getHeight();
		spcYEnd = ext.getMaxY();
	}

	public NsharpConstants.SPCGraph getLeftGraph() {
		return leftGraph;
	}

	public void setGraphs(NsharpConstants.SPCGraph leftGraph,NsharpConstants.SPCGraph rightGraph) {
		this.leftGraph = leftGraph;
		this.rightGraph = rightGraph;
		rscHandler.refreshPane();
	}

	public NsharpConstants.SPCGraph getRightGraph() {
		return rightGraph;
	}

	
}
