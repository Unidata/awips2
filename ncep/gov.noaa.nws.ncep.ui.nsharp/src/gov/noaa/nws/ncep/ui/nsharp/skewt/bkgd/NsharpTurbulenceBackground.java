/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.bkgd.NsharpTurblenceBackground
 * 
 * This java class performs the NSHARP NsharpTurblenceBackground functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 02/01/2012	TBD			Chin Chen	Initial coding
 * 03/21/2012                           "Refactor" to new name
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

public class NsharpTurbulenceBackground extends NsharpAbstractBackground implements
		IRenderable {
	private NsharpSkewTDescriptor desc;
	private PixelExtent pe;
	private IWireframeShape linesNumbersShape;
	private IWireframeShape lNLabelShape; //Richardson Number
	private IWireframeShape windShearLabelShape;
	public NsharpTurbulenceBackground(NsharpSkewTDescriptor desc) {
        super();

        this.rectangle = new Rectangle(NsharpConstants.TURB_REC_X_ORIG, NsharpConstants.TURB_REC_Y_ORIG,
        		NsharpConstants.TURB_REC_WIDTH, NsharpConstants.TURB_REC_HEIGHT);
        pe = new PixelExtent(this.rectangle);
        world = new WGraphics(this.rectangle);
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
        windShearLabelShape = target.createWireframeShape(false,desc );
        lNLabelShape = target.createWireframeShape(false,desc );
        linesNumbersShape.allocate(100);
        windShearLabelShape.allocate(20);
        lNLabelShape.allocate(20);
        //set world based on pressure/In
        world.setWorldCoordinates(NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_LEFT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP),
        		NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_RIGHT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM));
        //pressure lines and labels
        for (double i = NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP; i <= NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM; i=i+ NsharpConstants.TURBULENCE_PRESSURE_LEVEL_INC) {
        	//Pressure lines
        	double [][] lines = {{NsharpConstants.TURB_REC_X_ORIG, world.mapY(toLogScale(i))},{NsharpConstants.TURB_VIEW_X_END, world.mapY(toLogScale(i))}};
        	linesNumbersShape.addLineSegment(lines);
        	s = NsharpConstants.pressFormat.format(i);
        	//pressure labels
        	double [] lblXy = {NsharpConstants.TURB_REC_X_ORIG-30,world.mapY(toLogScale(i))+5};
        	linesNumbersShape.addLabel(s, lblXy);
        }
        //LN label
        double [] lblRhXy = {NsharpConstants.TURB_REC_X_ORIG+ 50, NsharpConstants.TURB_REC_Y_ORIG-50};
    	lNLabelShape.addLabel("*****TURBULENCE Display*****", lblRhXy);
    	double [] lblRhXy1 = {NsharpConstants.TURB_REC_X_ORIG+ 500, NsharpConstants.TURB_REC_Y_ORIG-50};
    	lNLabelShape.addLabel("LN(RICHARDSON NUMBER)", lblRhXy1);
    	double [][] lineRH = {{0,0},{0,0}};
    	lNLabelShape.addLineSegment(lineRH);//add dummy line

        for (double i = NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_LEFT; i >= NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_RIGHT; i= i+NsharpConstants.TURBULENCE_LN_RICHARDSON_NUMBER_INC) {
        	// temperature/LN vertical lines
        	double [][] lines = {{world.mapX(i), NsharpConstants.TURB_REC_Y_ORIG},{world.mapX(i), NsharpConstants.TURB_VIEW_Y_END}};
        	linesNumbersShape.addLineSegment(lines);
        	//RH label
        	s = NsharpConstants.pressFormat.format(i);
        	double [] lblXy = {world.mapX(i), NsharpConstants.TURB_REC_Y_ORIG-20};
        	lNLabelShape.addLabel(s, lblXy);
        }
        //wind shear label
        double [] lblTXy = {NsharpConstants.TURB_REC_X_ORIG+ 500, NsharpConstants.TURB_VIEW_Y_END+50};
        windShearLabelShape.addLabel("WIND SHEAR TKE PRODUCTION x 1E3 joules/sec", lblTXy);
    	double [][] lineT = {{0,0},{0,0}};
    	windShearLabelShape.addLineSegment(lineT);//add dummy line
    	//set world based on pressure/windShear
    	world.setWorldCoordinates(NsharpConstants.TURBULENCE_WIND_SHEAR_TKE_LEFT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_TOP),
        		NsharpConstants.TURBULENCE_WIND_SHEAR_TKE_RIGHT, toLogScale(NsharpConstants.TURBULENCE_PRESSURE_LEVEL_BOTTOM));
    	for (double i = NsharpConstants.TURBULENCE_WIND_SHEAR_TKE_LEFT; i <= NsharpConstants.TURBULENCE_WIND_SHEAR_TKE_RIGHT; i= i+NsharpConstants.TURBULENCE_WIND_SHEAR_TKE_INC) {
        	// temperature label
        	s = NsharpConstants.pressFormat.format(i);
        	double [] lblXy = {world.mapX(i), NsharpConstants.TURB_VIEW_Y_END+20};
        	windShearLabelShape.addLabel(s, lblXy);
        }
    	linesNumbersShape.compile();
        lNLabelShape.compile();
        windShearLabelShape.compile();
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
			NsharpLineProperty lp =lpMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_TURBULENCE_LN]);
			target.drawWireframeShape(lNLabelShape,lp.getLineColor(), 1, LineStyle.SOLID,smallFont);
			lp =lpMap.get(NsharpConstants.lineNameArray[NsharpConstants.LINE_TURBULENCE_WS]);
			target.drawWireframeShape(windShearLabelShape, lp.getLineColor(), 1, LineStyle.SOLID,smallFont);
		}
		else{
			target.drawWireframeShape(lNLabelShape, NsharpConstants.color_violet_red, 1, LineStyle.SOLID,smallFont);
			target.drawWireframeShape(windShearLabelShape, NsharpConstants.color_pink, 1, LineStyle.SOLID,smallFont);
		}
		
		
		
		
	}
	@Override
	public void disposeInternal() {
		// TODO Auto-generated method stub
		super.disposeInternal();
		if(linesNumbersShape!= null){
			linesNumbersShape.dispose();
		}
		if(lNLabelShape!= null){
			lNLabelShape.dispose();
		}
		if(windShearLabelShape!= null){
			windShearLabelShape.dispose();
		}
	}
	

}
