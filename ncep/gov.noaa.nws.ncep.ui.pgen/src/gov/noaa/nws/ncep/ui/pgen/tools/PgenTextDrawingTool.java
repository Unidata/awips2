/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenTextDrawingTool
 * 
 * 22 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.awt.geom.Line2D;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;

//import gov.noaa.nws.ncep.ui.display.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.ComboSymbol;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.IText;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrSettings;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.TextAttrDlg;

/**
 * Implements a modal map tool for PGEN text drawing.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09			79		B. Yin   	Moved from PgenSinglePointDrawingTool
 * 06/09			116		B. Yin		Handle the labeled symbol case
 * 07/10			?		B. Yin		Added '[' or ']' for front labels 
 * 02/11			?		B. Yin		Fixed Outlook type problem.
 * 04/11			?		B. Yin		Re-factor IAttribute
 * 08/12         #802       Q. Zhou     Fixed Front text of 2 lines. Modified handleMouseMove.
 * 12/12		 #591		J. Wu		TTR343 - added default label value for some fronts.
 * 10/13		 TTR768		J. Wu		Set default attributes for outlook labels (Text).
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenTextDrawingTool extends AbstractPgenDrawingTool {
	
	private boolean addLabelToSymbol;
	private AbstractDrawableComponent prevElem;
	private boolean usePrevColor;
    
    public PgenTextDrawingTool(){
    	
    	super();
    	
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
    		
        	String txt = "";
    		String param;
        	if ( (param = event.getParameter("addLabel")) != null ) {
        		
        		if ( param.equalsIgnoreCase("true"))
        			addLabelToSymbol = true;
        		if ( event.getTrigger() instanceof AbstractDrawableComponent ) {
        			prevElem = (AbstractDrawableComponent)event.getTrigger();
        			if ( prevElem.getParent() instanceof Outlook &&  
        					 ((Outlook)prevElem.getParent()).getOutlookType().equalsIgnoreCase("MESO_DSC")){
        				((TextAttrDlg) attrDlg).setBoxText(true, DisplayType.BOX);
        			}	
        			else if ( prevElem.getName().equalsIgnoreCase(Outlook.OUTLOOK_LABELED_LINE)
        					|| prevElem.getPgenCategory().equalsIgnoreCase("Front")){
        				((TextAttrDlg) attrDlg).setBoxText(false, DisplayType.NORMAL);
        				((TextAttrDlg) attrDlg).setFontSize(18);
        			}
        			
        			/*
        			 * Set default text attributes for outlook labels - depending on the outlook type!
        			 */
        			AbstractDrawableComponent dfltText = null;
        			if ( prevElem.getParent() instanceof Outlook ) {
        				String outlookType = ( (Outlook)(prevElem.getParent() ) ).getOutlookType();
        				String key = new String( outlookType );
        				if ( ( param = event.getParameter("defaultTxt") ) != null && !param.equalsIgnoreCase("Other") ) {
        			        key = key + param;
        				}
        				
        				dfltText = AttrSettings.getInstance().getOutlookLabelSettings().get( key );
        				if ( dfltText == null ) {
        					for ( String skey : AttrSettings.getInstance().getOutlookLabelSettings().keySet() ) {
        						dfltText = AttrSettings.getInstance().getOutlookLabelSettings().get( skey );
        						if ( dfltText != null )  break;
        					}
        				}
        				
        				if ( dfltText != null ) {
        					((TextAttrDlg) attrDlg).setAttr( dfltText  );
        				}        				
        			}       			
        		}
        		
        		if ( (param = event.getParameter("usePrevColor")) != null ) {
        			
            		if ( param.equalsIgnoreCase("true"))
            			usePrevColor = true;
            		
        			if ( usePrevColor){
        				((TextAttrDlg)attrDlg).setColor(prevElem.getPrimaryDE().getColors()[0]);
        			}
        		}
        		
        		if ( (param = event.getParameter("defaultTxt")) != null && !param.equalsIgnoreCase("Other") ) {
        			txt = param;
        			if ( !txt.isEmpty() ){
        				String[] txtArray = {"",""};
        				if ( txt.contains("\n" )){
        					txtArray[0] = txt.substring(0, txt.indexOf('\n'));
        					txtArray[1] = txt.substring(txt.indexOf('\n')+1, txt.length());
        				}
        				else {
        					txtArray[0] = txt;
        				}
        				((TextAttrDlg) attrDlg).setText( txtArray );
        			}
        		}

        		/*
        		 * for fronts, don't remember last value. 
        		 * 
        		 *  Jun (12/18/2012, TTR 343/TTN 591) - However, for Squall Line,Tropical Wave, 
        		 *  Outflow boundary, Dry Line, and Shear Line, need to use the default label 
        		 *  so the client won't need to type it - 
        		 */
        		if (  prevElem.getName().equalsIgnoreCase("labeledFront")){
    				String flbl = getDefaultFrontLabel( prevElem );   				
    				((TextAttrDlg) attrDlg).setText(new String[] { flbl });
        		}
        		else if ( prevElem.getName().equalsIgnoreCase("Volcano")){
        			((TextAttrDlg) attrDlg).setFontSize(18);
        			((TextAttrDlg) attrDlg).setBoxText(true, DisplayType.BOX);
        		}
        	}
    		
    	}
       
        return;
        
    }

    /**
     * Returns the current mouse handler.
     * @return
     */
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {
        	
        	this.mouseHandler = new PgenTextDrawingHandler();
        	
        }
        
        return this.mouseHandler;
    }
    
    /**
     * Implements input handler for mouse events.
     * @author bingfan
     *
     */
        
    public class PgenTextDrawingHandler extends InputHandlerDefaultImpl {
    	
    	/**
    	 * An instance of DrawableElementFactory, which is used to 
    	 * create new elements.
    	 */
    	private DrawableElementFactory def = new DrawableElementFactory();
    	
    	/**
    	 * Current element.
    	 */
    	private AbstractDrawableComponent elem = null;
    	
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

        			if ( ((IText)attrDlg).getString().length > 0 ) {  
        			    elem= def.create( DrawableType.TEXT, (IAttribute)attrDlg,
		                    pgenCategory, pgenType, loc, drawingLayer.getActiveLayer());
            		}			

            	// add the product to the PGEN resource and repaint.
            	if ( elem != null ) {

            		DECollection dec = PgenSinglePointDrawingTool.getCollection();
            		if (addLabelToSymbol && prevElem != null && (prevElem.getName().equalsIgnoreCase("labeledSymbol")||
            				prevElem.getName().equalsIgnoreCase("Volcano"))){
            			((DECollection)prevElem).add(elem);
            		}
            		else if ( prevElem != null && prevElem.getName().equalsIgnoreCase(Outlook.OUTLOOK_LABELED_LINE)){
            			((DECollection)prevElem).add(elem);
            		}
            		else if ( prevElem instanceof DECollection && prevElem.getPgenCategory().equalsIgnoreCase("Front") ){
            			((DECollection)prevElem).add(elem);
            		}	
            		else {
            			drawingLayer.addElement( elem );
            		}
            		AttrSettings.getInstance().setSettings((DrawableElement)elem);
    	            mapEditor.refresh();
    	            
    	            attrDlg.getShell().setActive();
        	    }
            	
        		if (addLabelToSymbol){
        			addLabelToSymbol = false;
        			usePrevColor = false;
        			if ( prevElem.getName().equalsIgnoreCase("labeledSymbol") || prevElem.getName().equalsIgnoreCase("Volcano")){
        				if ( prevElem.getPrimaryDE() instanceof Symbol ) {
        					PgenUtil.setDrawingSymbolMode( prevElem.getPrimaryDE().getPgenCategory(), prevElem.getPgenType(), false, null );
        				}        			
        				else if ( prevElem.getPrimaryDE() instanceof  ComboSymbol ){
        					PgenUtil.setDrawingSymbolMode( "Combo", prevElem.getPgenType(), false, null );
        				}
        			}            		
        			else if ( prevElem instanceof DECollection && prevElem.getPgenCategory().equalsIgnoreCase("Front") ){
        				PgenUtil.setDrawingFrontMode((Line)prevElem.getPrimaryDE());
        			}
        			else if ( prevElem.getName().equalsIgnoreCase(Outlook.OUTLOOK_LABELED_LINE)){
        				PgenUtil.loadOutlookDrawingTool();
        			}
        			
        			prevElem = null;
        		}
        		
                return true;
                
            }
            else if ( button == 3 ) {
            	
            	drawingLayer.removeGhostLine();
    	        mapEditor.refresh();
 
        		if (addLabelToSymbol){
        			addLabelToSymbol = false;
        			usePrevColor = false;
        			if ( prevElem.getName().equalsIgnoreCase("labeledSymbol") ){
        				if ( prevElem.getPrimaryDE() instanceof Symbol ) {
        					PgenUtil.setDrawingSymbolMode( prevElem.getPrimaryDE().getPgenCategory(), prevElem.getPgenType(), false, null );
        				}        			
        				else if ( prevElem.getPrimaryDE() instanceof  ComboSymbol ){
        					PgenUtil.setDrawingSymbolMode( "Combo", prevElem.getPgenType(), false, null );
        				}
        			}            		
        			else if (  prevElem instanceof DECollection && prevElem.getPgenCategory().equalsIgnoreCase("Front") ){
        				PgenUtil.setDrawingFrontMode((Line)prevElem.getPrimaryDE());
        			}
        			else if ( prevElem.getName().equalsIgnoreCase(Outlook.OUTLOOK_LABELED_LINE)){
        				PgenUtil.loadOutlookDrawingTool();
        			}
        			prevElem = null;
        		}
        		else {
        			PgenUtil.setSelectingMode();
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
            	
            		if ( ((IText)attrDlg).getString().length > 0 ) {  
            			//add "[" or "]" to front labels. The rule: If the label is only number and in one line, will be surrounded by [,]. 
            			if (addLabelToSymbol && prevElem.getPgenCategory() != null 
            					&& prevElem.getPgenCategory().equalsIgnoreCase("Front")){
            				
            				String[] text = ((IText)attrDlg).getString();
            				if ( text.length == 1 ) {  
            					StringBuffer lbl = new StringBuffer(((TextAttrDlg)attrDlg).getString()[0]);
            				
            					if ( lbl.length() > 0 ){
            						if ( lbl.charAt(0) == '[')  lbl.deleteCharAt(0);
            						if ( lbl.charAt(lbl.length()-1) == ']') lbl.deleteCharAt(lbl.length()-1);

            						try {
            							Integer.parseInt(lbl.toString());
            							//check if the text is right or left of the front
            							if ( rightOfLine(mapEditor, loc, (Line)prevElem.getPrimaryDE()) >= 0 ){

            								((TextAttrDlg)attrDlg).setText(new String[]{lbl+"]"});
            							}
            							else {
            								((TextAttrDlg)attrDlg).setText(new String[]{"[" + lbl});
            							}
            						} catch (NumberFormatException e) {
            							/*do nothing*/}
            					}
            				}
            				
            				ghost = def.create( DrawableType.TEXT, (IAttribute)attrDlg,
            						pgenCategory, pgenType, loc, drawingLayer.getActiveLayer());

            			}
                		else {
                			ghost = def.create( DrawableType.TEXT, (IText)attrDlg,
    			                    pgenCategory, pgenType, loc, drawingLayer.getActiveLayer());
                		}
            		}        			
      	        
            	drawingLayer.setGhostLine( ghost );      
          	    mapEditor.refresh();
           
        	}
       	
        	return false;
        	
        }

		@Override
		public boolean handleMouseDownMove(int x, int y, int mouseButton) {
			if ( !isResourceEditable()|| shiftDown ) return false;
			else return true;
		}
 
    }
    /**
     *	Check if a point is at right or left of the line.
     *  return: 1  - right side of the line
     *		    -1 - left side of the line
     *  	    0  - on the line
     */
//    public static int rightOfLine( NCMapEditor mEditor, Coordinate pt, Line ln ){
    public static int rightOfLine( AbstractEditor mEditor, Coordinate pt, Line ln ){

    	double screenPt[] = mEditor.translateInverseClick(pt);

    	Coordinate lnPts[] = ln.getLinePoints();

    	double minDist = 0;
    	double startPt[] = new double[2];
    	double endPt[] = new double[2];

    	for ( int ii = 0; ii < lnPts.length - 1; ii++ ){
    		double pt0[] = mEditor.translateInverseClick(lnPts[ii]);
    		double pt1[] = mEditor.translateInverseClick(lnPts[ii+1]);

    		double min = Line2D.ptSegDist(pt0[0], pt0[1], pt1[0], pt1[1], screenPt[0], screenPt[1]);

    		if (ii==0 || min < minDist ){
    			minDist = min;
    			startPt[0] = pt0[0];
    			startPt[1] = pt0[1];
    			endPt[0] = pt1[0];
    			endPt[1] = pt1[1];
    		}
    	}

    	if ( minDist == 0 ) return 0;

    	else return Line2D.relativeCCW(screenPt[0], screenPt[1], startPt[0], startPt[1], endPt[0], endPt[1]);

    }
    
    /*
     * Set default label for a few specific fronts.
     */
    private String getDefaultFrontLabel( AbstractDrawableComponent elem ) {

    	//Use default label for specific fronts.
		String frontLabel = "";
		String ptype = elem.getPgenType();
		if ( ptype.equalsIgnoreCase("TROF") ) {
			frontLabel = new String( "OUTFLOW BOUNDARY" );
		}
		else if ( ptype.equals( "TROPICAL_TROF") ) {
			frontLabel =  new String( "TROPICAL WAVE" );               			
		}
		else if ( ptype.equals( "DRY_LINE") ) {
			frontLabel = new String( "DRYLINE" );               			
		}
		else if ( ptype.equals( "INSTABILITY") ) {
			frontLabel = new String( "SQUALL LINE" );              			
		}
		else if ( ptype.equals( "SHEAR_LINE") ) {
			frontLabel = new String( "SHEARLINE" );                			
		}
		
		return frontLabel;

    }

}

