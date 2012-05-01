/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpIcingBackground
 * 
 * This java class performs the NSHARP NsharpIcingBackground functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 02/01/2012	TBD			Chin Chen	Initial coding
 * 										
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd;

import java.util.HashMap;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpLineProperty;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.WGraphics;

public class NsharpIcingBackground extends NsharpAbstractBackground implements
		IRenderable {
	private NsharpSkewTDescriptor desc;
	private PixelExtent pe;
	private IWireframeShape linesNumbersShape;
	private IWireframeShape RHLabelShape;
	private IWireframeShape tempLabelShape;
	private IWireframeShape EPILabelShape;//Equivalent Potential Instability
	public NsharpIcingBackground(NsharpSkewTDescriptor desc) {
        super();

        this.rectangle = new Rectangle(NsharpConstants.ICING_REC_X_ORIG, NsharpConstants.ICING_REC_Y_ORIG,
        		NsharpConstants.ICING_REC_WIDTH, NsharpConstants.ICING_REC_HEIGHT);
        pe = new PixelExtent(this.rectangle);
        world = new WGraphics(this.rectangle);

        
        //System.out.println("NsharpIcingBackground created");
        this.desc = desc;
    }
	public double toLogScale (double j) {
		  return(Math.log(j)/Math.log(1000)*1000);
	}
	public PixelExtent getPe() {
		return pe;
	}
	@Override
	protected WGraphics computeWorld() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public synchronized void initInternal(IGraphicsTarget target) {
		// TODO Auto-generated method stub
		super.initInternal(target);
		String s = "";
        linesNumbersShape = target.createWireframeShape(false,desc );
        tempLabelShape = target.createWireframeShape(false,desc );
        RHLabelShape = target.createWireframeShape(false,desc );
        EPILabelShape = target.createWireframeShape(false,desc );
        linesNumbersShape.allocate(100);
        tempLabelShape.allocate(20);
        RHLabelShape.allocate(20);
        EPILabelShape.allocate(4);
        //set world based on pressure/RH
        world.setWorldCoordinates(NsharpConstants.ICING_RELATIVE_HUMIDITY_LEFT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_TOP),
        		NsharpConstants.ICING_RELATIVE_HUMIDITY_RIGHT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM));
        //pressure lines and labels
        for (double i = NsharpConstants.ICING_PRESSURE_LEVEL_TOP; i <= NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM; i=i+ NsharpConstants.ICING_PRESSURE_LEVEL_INC) {
        	//Pressure lines
        	double [][] lines = {{NsharpConstants.ICING_REC_X_ORIG, world.mapY(toLogScale(i))},{NsharpConstants.ICING_VIEW_X_END, world.mapY(toLogScale(i))}};
        	linesNumbersShape.addLineSegment(lines);
        	s = NsharpConstants.pressFormat.format(i);
        	//pressure labels
        	double [] lblXy = {NsharpConstants.ICING_REC_X_ORIG-30,world.mapY(toLogScale(i))+5};
        	linesNumbersShape.addLabel(s, lblXy);
        }
        //RHLabel
        double [] lblRhXy = {NsharpConstants.ICING_REC_X_ORIG+ 50, NsharpConstants.ICING_REC_Y_ORIG-50};
    	RHLabelShape.addLabel("*****ICING Display*****", lblRhXy);
    	double [] lblRhXy1 = {NsharpConstants.ICING_REC_X_ORIG+ 500, NsharpConstants.ICING_REC_Y_ORIG-50};
    	RHLabelShape.addLabel("RELATIVE HUMIDITY", lblRhXy1);
    	double [][] lineRH = {{0,0},{0,0}};
    	RHLabelShape.addLineSegment(lineRH);//add dummy line
        for (double i = NsharpConstants.ICING_RELATIVE_HUMIDITY_LEFT; i <= NsharpConstants.ICING_RELATIVE_HUMIDITY_RIGHT; i= i+NsharpConstants.ICING_RELATIVE_HUMIDITY_INC) {
        	// temperature/humidity vertical lines
        	double [][] lines = {{world.mapX(i), NsharpConstants.ICING_REC_Y_ORIG},{world.mapX(i), NsharpConstants.ICING_VIEW_Y_END}};
        	linesNumbersShape.addLineSegment(lines);
        	//RH label
        	s = NsharpConstants.pressFormat.format(i);
        	double [] lblXy = {world.mapX(i), NsharpConstants.ICING_REC_Y_ORIG-20};
        	RHLabelShape.addLabel(s, lblXy);
        }
        //temperature label
        double [] lblTXy = {NsharpConstants.ICING_REC_X_ORIG+ 500, NsharpConstants.ICING_VIEW_Y_END+50};
        tempLabelShape.addLabel("TEMPERATURE (C)", lblTXy);
    	double [][] lineT = {{0,0},{0,0}};
    	tempLabelShape.addLineSegment(lineT);//add dummy line
    	//set world based on pressure/twmp
    	world.setWorldCoordinates(NsharpConstants.ICING_TEMPERATURE_LEFT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_TOP),
        		NsharpConstants.ICING_TEMPERATURE_RIGHT, toLogScale(NsharpConstants.ICING_PRESSURE_LEVEL_BOTTOM));
    	for (double i = NsharpConstants.ICING_TEMPERATURE_LEFT; i <= NsharpConstants.ICING_TEMPERATURE_RIGHT; i= i+NsharpConstants.ICING_TEMPERATURE_INC) {
        	// temperature label
        	s = NsharpConstants.pressFormat.format(i);
        	double [] lblXy = {world.mapX(i), NsharpConstants.ICING_VIEW_Y_END+20};
        	tempLabelShape.addLabel(s, lblXy);
        }
    	//EPI label
    	double [] lblEPIXy = {NsharpConstants.ICING_REC_X_ORIG+ 500, NsharpConstants.ICING_VIEW_Y_END+75};
    	EPILabelShape.addLabel("EQUIVALENT POTENTIAL INSTABILITY x 1E-3 K/m", lblEPIXy);
    	double [][] lineE = {{0,0},{0,0}};
    	EPILabelShape.addLineSegment(lineE);//add dummy line
        linesNumbersShape.compile();
        RHLabelShape.compile();
        tempLabelShape.compile();
        EPILabelShape.compile();
	}

	@Override
	protected void paintInternal(IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		// TODO Auto-generated method stub
		//super.paintInternal(target, paintProps);
		target.setupClippingPlane(pe);
		target.drawRect(pe, NsharpConstants.backgroundColor, 1.0f, 1.0f);
		target.clearClippingPlane();
		target.drawWireframeShape(linesNumbersShape, NsharpConstants.pressureColor, 1, LineStyle.SOLID,smallFont);
		HashMap<String, NsharpLineProperty> lpMap = this.desc.getSkewtResource().getLinePropertyMap();
		if(lpMap!=null){
			NsharpLineProperty lp =lpMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_ICING_RH]);
			target.drawWireframeShape(RHLabelShape, lp.getLineColor(), 1, LineStyle.SOLID,smallFont);
			lp =lpMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_ICING_TEMP]);
			target.drawWireframeShape(tempLabelShape,lp.getLineColor(), 1, LineStyle.SOLID,smallFont);
			lp =lpMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_ICING_EPI]);
			target.drawWireframeShape(EPILabelShape, lp.getLineColor(), 1, LineStyle.SOLID,smallFont);
		}
		else {
			target.drawWireframeShape(RHLabelShape, NsharpConstants.color_green, 1, LineStyle.SOLID,smallFont);
			target.drawWireframeShape(tempLabelShape, NsharpConstants.color_red, 1, LineStyle.SOLID,smallFont);
			target.drawWireframeShape(EPILabelShape, NsharpConstants.color_violet_red, 1, LineStyle.SOLID,smallFont);
		}
		
	}
	@Override
	public void disposeInternal() {
		// TODO Auto-generated method stub
		super.disposeInternal();
		if(linesNumbersShape!= null){
			linesNumbersShape.dispose();
		}
		if(RHLabelShape!= null){
			RHLabelShape.dispose();
		}
		if(tempLabelShape!= null){
			tempLabelShape.dispose();
		}
		if(EPILabelShape!= null){
			EPILabelShape.dispose();
		}
	}
	

}
