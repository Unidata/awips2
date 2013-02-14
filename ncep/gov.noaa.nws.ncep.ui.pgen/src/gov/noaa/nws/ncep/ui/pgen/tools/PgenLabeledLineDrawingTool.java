/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenLabeledLineDrawingTool
 * 
 * 5 September 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.CloudAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.TurbAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.CcfpAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.LabeledLine;

import java.util.ArrayList;
import java.util.Iterator;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a modal map tool for PGEN labeled line drawing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/10		304			B. Yin   	Initial Creation.
 * 09/10		305/306		B. Yin		Added Cloud and Turbulence
 * 12/11		?			B. Yin		Added open/close line functions
 * 08/12		?			B. Yin		One Cloud/Turb per layer
 * 
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenLabeledLineDrawingTool extends AbstractPgenDrawingTool implements ILabeledLine{
	
	LabeledLine labeledLine;
	CcfpAttrDlg ccdlg = null;
	/*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool( ) {

    	super.activateTool();
    	if (super.isDelObj()) return;
    	
    	if ( attrDlg instanceof CloudAttrDlg ){
    		((CloudAttrDlg)attrDlg).setCloudDrawingTool(this);
    	}
    	else if ( attrDlg instanceof TurbAttrDlg ){
    		((TurbAttrDlg)attrDlg).setTurbDrawingTool(this);
    	}
    	else if( attrDlg instanceof CcfpAttrDlg ){
    		ccdlg = (CcfpAttrDlg)attrDlg;
    		((CcfpAttrDlg)attrDlg).setCcfpDrawingTool(this);
    	}
    	this.resetMouseHandler();
    	
        return;
        
    }

    /**
     * Constructor
     */
    public PgenLabeledLineDrawingTool(){
    	super();
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {
    	
    	super.deactivateTool();
    	        	
    	labeledLine = null;
    	if ( mouseHandler instanceof PgenLabeledLineDrawingHandler ){
    		PgenLabeledLineDrawingHandler cdh = (PgenLabeledLineDrawingHandler) mouseHandler;
    		if (cdh != null) cdh.clearPoints();
    	}
        
    }

    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {
        	
        	this.mouseHandler = new PgenLabeledLineDrawingHandler();
        	
        }
        
        return this.mouseHandler;
    }
   
    
    /**
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
          
    private class PgenLabeledLineDrawingHandler extends InputHandlerDefaultImpl {
    	/**
    	 * flag for CCFP.
    	 */
    	boolean	ccfpTxtFlag = false; 
    	
    	/**
    	 * Points of the new element.
    	 */
        protected ArrayList<Coordinate> points = new ArrayList<Coordinate>();
        
       	/**
    	 * Current element.
    	 */     
        protected AbstractDrawableComponent elem;
        
       	/**
    	 * An instance of DrawableElementFactory, which is used to 
    	 * create new elements.
    	 */
    	protected DrawableElementFactory def = new DrawableElementFactory();

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         *      int, int)
         */
        @Override	
        public boolean handleMouseDown(int anX, int aY, int button) {
        	if ( !isResourceEditable() ) return false;
            
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null || shiftDown ) return false;
        	
        	if ( button == 1 ) {
        		
        		if ( "CCFP_SIGMET".equalsIgnoreCase(pgenType) 
        				|| attrDlg.isAddLineMode() ){
                points.add( loc );                
        		}
                
                return true;
                
            }
        	else if ( button == 3 ) {

        		if ( points.size() == 0 ) {

        			if ( attrDlg.isAddLineMode() ){
        				//add mode
        				attrDlg.resetLabeledLineBtns();
        			}
        			else {
        				//clean up
        				closeAttrDlg(attrDlg, pgenType); 
        				attrDlg = null; 
        				PgenUtil.setSelectingMode();
        			}
        			
        			labeledLine = null;

        		}
        		else if ( points.size() < 2 ){

        			drawingLayer.removeGhostLine();
        			points.clear();

        			mapEditor.refresh();

        		}
        		else {

        			// add the labeled line to PGEN resource
        			if ( attrDlg.isAddLineMode()) {
        	/*			if( labeledLine == null ) {
        					labeledLine = getLabeledLineInCurrentLayer( pgenType );
        				}
        				
        				if( labeledLine != null ) {

        				//add line to labeled line collection
        				LabeledLine newll = labeledLine.copy();
        				elem = def.createLabeledLine( pgenCategory, pgenType, (IAttribute)attrDlg,
        						points, newll, drawingLayer.getActiveLayer());
        				drawingLayer.replaceElement(labeledLine, newll);
        				labeledLine = newll;
        			}
        			else {
        		*/			//new labeled line
        				elem = def.createLabeledLine( pgenCategory, pgenType, (IAttribute)attrDlg,
        						points, null, drawingLayer.getActiveLayer());
        					
        				drawingLayer.addElement( elem );
        				labeledLine = (LabeledLine)elem;
        	//			}
        			}

        			if("CCFP_SIGMET".equals(pgenType) ) {
        				elem = def.createLabeledLine( pgenCategory, pgenType, (IAttribute)attrDlg,
        						points, null, drawingLayer.getActiveLayer());

        				drawingLayer.addElement( elem );
        				labeledLine = (LabeledLine)elem;


        				if ( ccdlg.isAreaType()){//avoid 2 Sigmet elements issue
        			
        			    ccfpTxtFlag = true; 
        			    setAddingLabelHandler( );//return true;//avoid right click cause no showing issue
        			}
        			}
        			
        			drawingLayer.removeGhostLine();
        			if( ! ccfpTxtFlag ) 	points.clear();

        			mapEditor.refresh();

        		}

            	return true;
            	
            }
        	else if ( button == 2 ){
        		return true;
        	}
            else{
               	return false;
            }
        }
        
        private void closeAttrDlg(AttrDlg attrDlgObject, String pgenTypeString) {
        	if(attrDlgObject == null)
        		return; 
        	attrDlgObject.close(); 
        }
        
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseMove(int,
         *      int)
         */
        @Override
        public boolean handleMouseMove(int x, int y) {
        	if ( !isResourceEditable() ) return false;

        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(x, y);
        	if ( loc == null ) return false;
        	
        	String lineType = "LINE_DASHED_4";
        	if ( pgenType.equalsIgnoreCase("Cloud")){
        		lineType = "SCALLOPED";
        	}
        	else if ( pgenType.equalsIgnoreCase("Turbulence") ) {
        		lineType =  "LINE_DASHED_4";
        	}else if("CCFP_SIGMET".equalsIgnoreCase(pgenType)) { 
        		lineType = ccdlg.getCcfpLineType(); 
        	}
        	
        	// create the ghost element and put it in the drawing layer
           	AbstractDrawableComponent ghost = def.create(DrawableType.LINE, (IAttribute)attrDlg,
        			"Lines", lineType, points, drawingLayer.getActiveLayer());
           	
            if ( points != null && points.size() >= 1) {
            	
                ArrayList<Coordinate> ghostPts = new ArrayList<Coordinate>(points);
                ghostPts.add(loc);
                Line ln = (Line)ghost;
            	ln.setLinePoints( new ArrayList<Coordinate>( ghostPts ) );
            	
            	if("CCFP_SIGMET".equalsIgnoreCase(pgenType)){ 
            		ln.setClosed(ccdlg.isAreaType()); 
            		ln.setFilled(ccdlg.isAreaType());
            	}            	
            	drawingLayer.setGhostLine(ghost);
            	mapEditor.refresh();
            	
            }
            
        	return true;
        	
        }
        
        @Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        	if (  !isResourceEditable() || shiftDown ) return false;
        	else return true;
		}

		private void clearPoints(){
        	points.clear();
        }

		/**
		 * Returns the Labeled Line in the current layer with input type(Turb/Cloud).
		 * @param type
		 * @return
		 */
		private LabeledLine getLabeledLineInCurrentLayer( String type){
			Iterator<AbstractDrawableComponent> it = drawingLayer.getActiveLayer().getComponentIterator();
			while ( it.hasNext() ){
				AbstractDrawableComponent adc = it.next();
				if ( adc instanceof LabeledLine && adc.getPgenType().equalsIgnoreCase(type)){
					return (LabeledLine) adc;
				}
			}
			return null;
		}

    }
    
    public void setLabeledLine(LabeledLine labeledLine) {
		this.labeledLine = labeledLine;
	}

	public LabeledLine getLabeledLine(){
    	return labeledLine;
    }

	public void resetMouseHandler(){
		setHandler(new PgenLabeledLineDrawingHandler() );
	}  
	
	public void setAddingLabelHandler( ){
		setHandler(new PgenAddLabelHandler(mapEditor, drawingLayer, this, attrDlg));
	}
	
	@Override
	public void setDeleteHandler( boolean delLine, boolean flipFlag, boolean openClose ){
		setHandler(new PgenLabeledLineDelHandler(mapEditor, drawingLayer, this, attrDlg, delLine, flipFlag, openClose));
	}
	
}
