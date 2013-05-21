/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenSinglePointDrawingTool
 * 
 * 2 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;

//import gov.noaa.nws.ncep.ui.display.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrSettings;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.LabeledSymbolAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.SymbolAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.VolcanoAttrDlg;

/**
 * Implements a modal map tool for PGEN single point drawing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/09					B. Yin   	Initial Creation.
 * 04/09            72      S. Gilbert  Modified to use PgenSession and PgenCommands
 * 04/09            88      J. Wu  		Added Text drawing
 * 04/24            99      G. Hull     Use NmapUiUtils
 * 04/09		   103		B. Yin		Extends from AbstractPgenTool
 * 05/09           #42      S. Gilbert  Added pgenType and pgenCategory
 * 05/09			79		B. Yin		Extends from AbstractPgenDrawingTool
 * 06/09		116			B. Yin		Use AbstractDrawableComponent
 * 
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenSinglePointDrawingTool extends AbstractPgenDrawingTool {
	
	static private DECollection dec;
	
	private AbstractDrawableComponent prevElem;
	boolean usePrevColor;

    public PgenSinglePointDrawingTool(){
    	
    	super();
    	dec = null;
    	
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool( ) {
    	
    	super.activateTool();
        
    	if ( attrDlg != null && !isDelObj()){
    		
    		String param;
    		if ( event.getTrigger() instanceof AbstractDrawableComponent ) 
    			prevElem = (AbstractDrawableComponent)event.getTrigger();

    		if ( (param = event.getParameter("usePrevColor")) != null ) {

    			if ( param.equalsIgnoreCase("true"))
    				usePrevColor = true;

    			if ( usePrevColor){
    				((SymbolAttrDlg)attrDlg).setColor(prevElem.getPrimaryDE().getColors()[0]);
    			}
    		}

    	}
       
    }

    /**
     * Returns the current mouse handler.
     * @return
     */
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {
        	
        	this.mouseHandler = new PgenSinglePointDrawingHandler();
        	
        }
        
        return this.mouseHandler;
    }
    
    /**
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
        
    public class PgenSinglePointDrawingHandler extends InputHandlerDefaultImpl {
    	
    	/**
    	 * An instance of DrawableElementFactory, which is used to 
    	 * create new elements.
    	 */
    	private DrawableElementFactory def = new DrawableElementFactory();
    	
    	/**
    	 * Current element.
    	 */
    	private DrawableElement elem = null;
    	
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
            	            	
            	// create an element.
        			elem = (DrawableElement)def.create(getDrawableType(), (IAttribute)attrDlg,
            			        pgenCategory, pgenType, loc,
            			        drawingLayer.getActiveLayer());
          		
           		    ((SymbolAttrDlg)attrDlg).setLatitude( loc.y );
           	        ((SymbolAttrDlg)attrDlg).setLongitude( loc.x );
           	        ((SymbolAttrDlg)attrDlg).enableUndoBtn(true);
            	
            	// add the product to the PGEN resource and repaint.
            	if ( elem != null ) {
            		if ( prevElem != null && prevElem.getName().equalsIgnoreCase(Outlook.OUTLOOK_LABELED_LINE)){
            			((DECollection)prevElem).add(elem);
            		}
            		else if (((SymbolAttrDlg)attrDlg).labelEnabled()){
                		dec = new DECollection("labeledSymbol");
                		dec.setPgenCategory(pgenCategory);
                		dec.setPgenType(pgenType);
                		dec.addElement(elem);
                		drawingLayer.addElement(dec);
                	}
                	else {
                		drawingLayer.addElement( elem );
                	}
            		
            		attrDlg.setDrawableElement(elem);
            		AttrSettings.getInstance().setSettings(elem);
    	            mapEditor.refresh();
        	    }
            	
            	if ( prevElem != null){
        			usePrevColor = false;
        			if ( prevElem.getName().equalsIgnoreCase(Outlook.OUTLOOK_LABELED_LINE)){
        				PgenUtil.loadOutlookDrawingTool();
        			}
        			prevElem = null;
        		}
        			
                return true;
                
            }
            else if ( button == 3 ) {
            	
            	if ( elem != null && ((SymbolAttrDlg)attrDlg).labelEnabled()){
            		drawingLayer.removeGhostLine();
            		mapEditor.refresh();
            		
            		String defaultTxt = "";
            		if ( attrDlg instanceof VolcanoAttrDlg ){
            			defaultTxt = ((VolcanoAttrDlg)attrDlg).getVolText();
            			dec.setCollectionName("Volcano");
            		}
            		
            		//in case the label is enabled after symbol is placed.
            		if (dec == null && ((SymbolAttrDlg)attrDlg).labelEnabled()){ 
                 		dec = new DECollection("labeledSymbol");
                 		dec.setPgenCategory(pgenCategory);
                 		dec.setPgenType(pgenType);
                 		dec.addElement(elem);
                 		drawingLayer.replaceElement(elem, dec);
            		 }
            		 
            		PgenUtil.setDrawingTextMode( true, ((LabeledSymbolAttrDlg)attrDlg).useSymbolColor(), defaultTxt, dec );
            		dec = null;
            		elem = null;
            		
            	}
            	else {
            		if ( prevElem != null){
            			usePrevColor = false;
            			if ( prevElem.getParent().getPgenCategory().equalsIgnoreCase("OUTLOOK")){
            				PgenUtil.loadOutlookDrawingTool();
            			}
            			prevElem = null;
            		}
            		else {
            			elem = null;
            			PgenUtil.setSelectingMode();
            		}
            	}
            	
            	return true;
            	
            }
            else{
            	
               	return false;
               	
            }
        	
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

        	if ( attrDlg != null ) {
        	    
            	AbstractDrawableComponent ghost = null;
            	
        		    ((SymbolAttrDlg)attrDlg).setLatitude(loc.y );
        	        ((SymbolAttrDlg)attrDlg).setLongitude(loc.x );       
        	
          	        ghost = def.create( getDrawableType(), (IAttribute)attrDlg,
       			                    pgenCategory, pgenType, loc, 
       			                    drawingLayer.getActiveLayer() );
            	drawingLayer.setGhostLine( ghost );      
          	    mapEditor.refresh();
           
        	}
       	
        	return false;
        	
        }
        
        @Override
        public boolean handleMouseDownMove(int aX, int aY, int button) {
        	
        	if (  !isResourceEditable() || shiftDown ) return false;
        	else return true;        

        }
    }
    
    /**
     * Determine the proper DrawableType from the value of the pgenCategory attribute.
     * @return
     */
    private DrawableType getDrawableType() {
    	DrawableType which = DrawableType.SYMBOL;
    	if ( pgenCategory.equals("Combo") ) which = DrawableType.COMBO_SYMBOL;
    	return which;
    }
    
    /**
     * Get the symbol color which may be used for its label
     * @return
     */
  /*  public static Color getSymbolColor(){
    	return symbolColor;
    }
    */
    /**
     * Get the collection that contains the symbol. When add the label,
     * add it to the same collection
     * @return
     */
    public static DECollection getCollection(){
    	return dec;
    } 
}
