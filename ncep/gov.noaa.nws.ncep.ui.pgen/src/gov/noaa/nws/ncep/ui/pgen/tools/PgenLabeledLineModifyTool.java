/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenLabeledLineModifyTool
 * 
 * 08 September 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.Cloud;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.Label;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.LabeledLine;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.Turbulence;
import gov.noaa.nws.ncep.ui.pgen.sigmet.Ccfp;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.CloudAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.TurbAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.CcfpAttrDlg;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Iterator;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;


/**
 * Implements a modal map tool to modify PGEN labeled lines.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/10			305		B. Yin   	Initial Creation.
 * 12/11			?		B. Yin		Added open/close line functions
 * 03/12        #697        Q. Zhou     Fixed line arrow head size
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenLabeledLineModifyTool extends PgenSelectingTool implements ILabeledLine{
	Ccfp ccfp = null;
	//labeled line working on
	LabeledLine labeledLine;
	
	//element selected in the labeled line group
	DrawableElement elSelected;
	
	//location of the first mouse down
	Coordinate click;

	/*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool( ) {

    	super.activateTool();
    	
    	elSelected = null;
    	click = null;
    	
    	if ( event.getTrigger() instanceof Cloud ) {
    		labeledLine = (LabeledLine)event.getTrigger();
    		((CloudAttrDlg)attrDlg).setCloudDrawingTool(this);
    		((CloudAttrDlg)attrDlg).resetLabeledLineBtns();
    		
    		if ( !(labeledLine == null )){
    			((CloudAttrDlg)attrDlg).setAttrForDlg(null);
          		attrDlg.enableButtons();
    		}
    	}
    	else if ( event.getTrigger() instanceof Turbulence ) {
    		labeledLine = (LabeledLine)event.getTrigger();
    		((TurbAttrDlg)attrDlg).setTurbDrawingTool(this);
    		((TurbAttrDlg)attrDlg).resetLabeledLineBtns();
    		
    		if ( !(labeledLine == null )){
    			((TurbAttrDlg)attrDlg).setAttrForDlg(null);
          		attrDlg.enableButtons();
    		}
    	}
    	else if (event.getTrigger() instanceof Ccfp){
    		labeledLine=(LabeledLine)event.getTrigger(); 
    		attrDlg.enableButtons();
    		((CcfpAttrDlg)attrDlg).setMouseHandlerName("LabeledLine Modify");    		
    		ccfp=(Ccfp)labeledLine;
    		((CcfpAttrDlg)attrDlg).setAttrForDlg(ccfp.getSigmet());
    		((CcfpAttrDlg)attrDlg).setCcfpDrawingTool(this);
    		ccfp.setAttributes(attrDlg);
    	}
    	this.setHandler( new PgenLabeledLineModifyHandler() );
    	
    }

    /**
     * Returns the current mouse handler.
     * @return
     */   
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {
        	
        	this.mouseHandler = new PgenLabeledLineModifyHandler();
        	
        }
        
        return this.mouseHandler;
        
    }
    
    /**
     * A mouse handler for the watch box modify tool
     * @author bingfan
     *
     */
	private class PgenLabeledLineModifyHandler extends PgenSelectingTool.PgenSelectHandler {
        private ArrayList<Coordinate> points = new ArrayList<Coordinate>();
        private boolean simulate;
       	/**
    	 * An instance of DrawableElementFactory, which is used to 
    	 * create new elements.
    	 */
    	protected DrawableElementFactory def = new DrawableElementFactory();
		protected boolean	ptSelected2	= false;

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
        	if ( loc == null || shiftDown || simulate ) return false;
        	
        	click = loc;
         	ptSelected2 = false;
         	
        	if ( button == 1 ){
        		if ( attrDlg.isAddLineMode() ){
                    points.add( click );                
        		}
        		else {
        			elSelected = drawingLayer.getNearestElement(click, labeledLine);
        			firstDown = loc;
        		}
        		
        		return false;
        	}
        	//clean up
        	else if ( button == 3 ) {
            	
        		if ( attrDlg.isAddLineMode()){
            		if ( points.size() == 0 ) {
            				attrDlg.resetLabeledLineBtns();
            		}
            		else if ( points.size() < 2 ){
            			drawingLayer.removeGhostLine();
            			points.clear();
            			mapEditor.refresh();

            		}
            		else {
            			//add line to labeled line collection
            	//		LabeledLine newll = def.createLabeledLine( labeledLine.getPgenCategory(), labeledLine.getPgenType(), 
            	//				(IAttribute)attrDlg, points, labeledLine.copy(), drawingLayer.getActiveLayer());
            			
            			LabeledLine newll = def.createLabeledLine( pgenCategory, PgenLabeledLineModifyTool.this.pgenType, (IAttribute)attrDlg,
    							points, null, drawingLayer.getActiveLayer());
    					
            	//		drawingLayer.replaceElement(labeledLine, newll);
            			drawingLayer.addElement( newll );
            			labeledLine = newll;

            			drawingLayer.removeGhostLine();
            			points.clear();
            			
            			//re-set selected
            	//		resetSelected();
            			
            			drawingLayer.setSelected(newll);

            			mapEditor.refresh();

            		}
        		}
        		else {
        			// Close the attribute dialog and do the cleanup.
        			if ( attrDlg != null ) {
        				attrDlg.close();
        			}

        			attrDlg = null;

        			drawingLayer.removeGhostLine();
        			ptSelected2 = false;	ptSelected = false;
        			drawingLayer.removeSelected();
        			mapEditor.refresh();
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
        	if ( loc == null || simulate ) return false;
        	
        	if ( attrDlg.isAddLineMode()){
        		// create the ghost line and put it in the group
        		String lineType = "LINE_DASHED_4";
            	if ( labeledLine.getPgenType().equalsIgnoreCase("Cloud")){
            		lineType = "SCALLOPED";
            	}
            	else if ( labeledLine.getPgenType().equalsIgnoreCase("Turbulence") ) {
            		lineType =  "LINE_DASHED_4";
            	}
        		AbstractDrawableComponent ghost = def.create(DrawableType.LINE, (IAttribute)attrDlg,
        				"Lines", lineType, points, drawingLayer.getActiveLayer());

        		if ( points != null && points.size() >= 1) {

        			ArrayList<Coordinate> ghostPts = new ArrayList<Coordinate>(points);
        			ghostPts.add(loc);
        			Line ln = (Line)ghost;
        			ln.setLinePoints( new ArrayList<Coordinate>( ghostPts ) );

        			drawingLayer.setGhostLine(ghost);
        			mapEditor.refresh();

        		}
        	}
        	return false;
        }
        
	    /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         *      int, int)
         */
        @Override
        public boolean handleMouseDownMove(int x, int y, int button) {
        	if (  !isResourceEditable() || shiftDown || simulate ) return false;
        	
        	if ( button == 1 ){
        		//  Check if mouse is in geographic extent
        		Coordinate loc = mapEditor.translateClick(x, y);
        		if ( loc == null || elSelected == null ) return true;
        		
            	//make sure the click is close enough to the element
    			if ( drawingLayer.getDistance(elSelected, loc) > 30 && !ptSelected2 ){

    				if ( firstDown != null && drawingLayer.getDistance(elSelected, firstDown) < 30  ){
    					firstDown = null;
    				}
    				else {
    					return false;
    				}
    			}
        		
        		ptSelected2 = true;
        		if ( elSelected != null ){
        			if ( elSelected.getParent() instanceof Label ){
        				//label selected
        				if ( elSelected instanceof Line ){ 	
        					//arrow line
        					Line ln = (Line)elSelected;
        					int idx = getNearestPtIndex(ln, click);

        					//edit arrow line
        					if ( idx == 0 ){
        						//select text
        						elSelected = ((Label)elSelected.getParent()).getSpe();
        					}
        					else {
        						//select arrow head, draw ghost
        						ghostEl = (Line)ln.copy();
        						ghostEl.setColors(new Color[]{ghostColor, new java.awt.Color( 255,255,255)});
        						ghostEl.setPgenCategory( ln.getPgenCategory());
        						ghostEl.setPgenType( ln.getPgenType());

        						ghostEl.removePoint( idx );
        						ghostEl.addPoint(idx, loc);

        						drawingLayer.setGhostLine(ghostEl);
        						mapEditor.refresh();

        					}
        				}

        				if ( elSelected instanceof SinglePointElement ){
        					//text label selected
        					((SinglePointElement)elSelected).setLocation(loc);
        					drawingLayer.resetElement(elSelected);   // reset display of this element

        					//arrows
        					Label lbl = (Label)elSelected.getParent();

        					for ( Line ln : lbl.getArrows() ){
        						ln.removePoint(0);
        						ln.addPoint(0, loc);
        						drawingLayer.resetElement(ln);
        					}
        					
        					if( ccfp!=null && PgenAddLabelHandler.isPtsInArea(ccfp.getAreaLine(), loc)){
        						for(Line ln : lbl.getArrows()) 
        							lbl.remove(ln);
        					}
        					        					
        					if( ccfp!=null && ! PgenAddLabelHandler.isPtsInArea(ccfp.getAreaLine(), loc) && lbl.getArrows().size()==0) 
        						addArrow(loc,lbl);
        					
        					mapEditor.refresh();
        					if ( oldLoc == null )
        						oldLoc = new Coordinate(((SinglePointElement)elSelected).getLocation());
        				}
        			}
        			else if ( elSelected instanceof Line ){
        				Line ln = (Line)elSelected;

        				ghostEl = (Line)ln.copy();
        				ghostEl.setColors(new Color[]{ghostColor, new java.awt.Color( 255,255,255)});
        				ghostEl.setPgenCategory( ln.getPgenCategory());
        				ghostEl.setPgenType( ln.getPgenType());

        				ptIndex = getNearestPtIndex( ghostEl, click);

        				ghostEl.removePoint( ptIndex );
        				ghostEl.addPoint(ptIndex, loc);

        				drawingLayer.setGhostLine(ghostEl);
        				mapEditor.refresh();

        			}
        		}
        		else {
        		//	elSelected = drawingLayer.getNearestElement(click, labeledLine);
        		}
        		simulate = true;
        		PgenUtil.simulateMouseDown(x, y, button, mapEditor);
        		simulate = false;
        	}
        	
        	return true;

        }
        
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         *      int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int button) {
    		firstDown = null;
        	if (  !isResourceEditable() || shiftDown || simulate ) return false;
        	// Finish the editing
    		if (button == 1 && drawingLayer != null ){
    			
   		    	LabeledLine mergedLine = null;

    	       	if ( elSelected != null){
    	       		 LabeledLine newll = labeledLine.copy();
    	       		 
    	       		 if ( elSelected instanceof SinglePointElement ){
    	       			 //if the label is dropped on another label, merge them to one label.
    	       			 if ( elSelected.getParent() instanceof Label ){

    	       				mergedLine = PgenUtil.mergeLabels( newll, newll.getLabelAt(((SinglePointElement)elSelected).getLocation()), ((SinglePointElement)elSelected).getLocation(), mapEditor,drawingLayer);
    	       			 }
    	       		 }

    	       		 // re-set label
            		if ( elSelected.getParent() instanceof Label ){

            			if ( elSelected instanceof Line  && ghostEl != null   && ptSelected2){
            				ptSelected2 = false;
            				//arrow line
                			DrawableElement el = drawingLayer.getNearestElement(click, newll);
                			
                			//el should be a line
                			if ( el instanceof Line ){
                				((Line)el).setPoints(ghostEl.getPoints());
                				drawingLayer.removeGhostLine();
                			}
            			}
            			else if ( elSelected instanceof SinglePointElement ){
            				//text
                			//should be label
                			if ( elSelected.getParent() instanceof Label ){
                				//put original location back in order for undo to work
                				Label lbl = (Label)elSelected.getParent();
                				lbl.getSpe().setLocation(oldLoc);
                				for ( Line ln : lbl.getArrows() ){
                					ln.removePoint(0);
                					ln.addPoint(0, oldLoc);
                				}
                			}
                			
                			oldLoc = null;
            			}
            			
            		}
            		else if ( elSelected instanceof Line  && ghostEl != null && ptSelected2){
            			ptSelected2 = false;
            			//modify line            			
            			DrawableElement el = drawingLayer.getNearestElement(click, newll);
            			//el should be a line
            			if ( el instanceof Line ){
            				((Line)el).setPoints(ghostEl.getPoints());
            				drawingLayer.removeGhostLine();
            				ghostEl = null;
            			}
            			
            		}
            		
            		if ( mergedLine != null ){
            			
            			ArrayList<AbstractDrawableComponent> old = new ArrayList<AbstractDrawableComponent>();
            			old.add( mergedLine);
            			old.add( labeledLine);
            			
            			ArrayList<AbstractDrawableComponent> newLines = new ArrayList<AbstractDrawableComponent>();
            			newLines.add(newll);

            			drawingLayer.replaceElements(old, newLines);
            		}
            		else {
            		drawingLayer.replaceElement(labeledLine, newll);
            		}
            		
            		labeledLine = newll;

            		if((labeledLine instanceof Ccfp) && labeledLine.getLabels()!=null && labeledLine.getLabels().size()>0){
            			
            			Label lbl = labeledLine.getLabels().get(0);		
            			Coordinate loc = lbl.getSpe().getLocation();
            			
            			if( ! PgenAddLabelHandler.isPtsInArea((Line)labeledLine.getPrimaryDE(), loc) && lbl.getArrows().size()==0) 
            				addArrow(loc,lbl);
            			
            			if(PgenAddLabelHandler.isPtsInArea((Line)labeledLine.getPrimaryDE(), loc) && lbl.getArrows().size()>0){ 
            				for(Line ln : lbl.getArrows()) 
            					lbl.remove(ln);
            			}
            		}
            		
            		//re-select
            		resetSelected();

            		elSelected = null;

    	       	}

    		}
    		
    		//make sure the arrow line won't go through the text box.
    		if(labeledLine instanceof Ccfp)	((Ccfp)labeledLine).moveText2Last();
    		     	
    		mapEditor.refresh();
            return true;
            
        }
        
    	private void addArrow(Coordinate loc, Label lbl){
    		ArrayList<Coordinate> locs = new ArrayList<Coordinate>();
    		locs.add(loc);    		
    		locs.add(ccfp.getAreaLine().getCentroid());    		
    		
    		Line aln = new Line(null, new Color[]{Color.WHITE}, 2.0F, 1.0,false, false, locs, 0, 
    				gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern.FILL_PATTERN_2, "Lines", "POINTED_ARROW");
    		lbl.addArrow(aln);    	
    	}

	}

	@Override
	public void setAddingLabelHandler( ){

		setHandler(new PgenAddLabelHandler(mapEditor, drawingLayer,
							this, attrDlg ));
	}
	
	@Override
	public void resetMouseHandler(){
		
		//resetSelected();
		click = null;
		elSelected = null;
		setHandler(new PgenLabeledLineModifyHandler() );
	}

	@Override
	public LabeledLine getLabeledLine() {
		return labeledLine;
	}

	@Override
	public void setLabeledLine(LabeledLine ln) {
		labeledLine = ln;
	}

	private void resetSelected(){
		if ( labeledLine != null ){
			drawingLayer.removeSelected();
			Iterator<DrawableElement> it = labeledLine.createDEIterator();
			while( it.hasNext() ){
				drawingLayer.addSelected(it.next());
			}
		}
	}
	
	@Override
	public void setDeleteHandler( boolean delLine, boolean flip, boolean openClose ){
		setHandler(new PgenLabeledLineDelHandler(mapEditor, drawingLayer, this, attrDlg, delLine, flip, openClose));
	}
	

}
