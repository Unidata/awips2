/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenAddingLabelHandler
 * 
 * 8 September 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.AttrSettings;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.CloudAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.TurbAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IAvnText.AviationTextType;
import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;
import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextJustification;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.AvnText;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.MidCloudText;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.Label;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.LabeledLine;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
//import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

import java.awt.Color;
import java.awt.Polygon;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * Mouse handler to add labels when drawing labeled lines.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/10		#304		B. Yin   	Initial Creation.
 * 11/10		#?			B. Yin		Use MidLevelCloudText as cloud labels
 * 03/12        #697        Q. Zhou     Fixed line arrow head size for ccf, cloud & turb
 * </pre>
 * 
 * @author	B. Yin
 */
public class PgenAddLabelHandler extends InputHandlerDefaultImpl {
	
//	private NCMapEditor mapEditor;
	private AbstractEditor mapEditor;
	private PgenResource drawingLayer;
	
	//"labeled line drawing tool" or "labeled line modify tool"
	private ILabeledLine prevTool;
	
	//labeled line attribute dialog
	private AttrDlg dlg;
	
	//the element working on
	private Line lineSelected;
	
	private Label ghostLabel;
	
	private List<Coordinate> pts;
	
	/**
	 * public constructor
	 * @param mapEditor
	 * @param drawingLayer
	 * @param prevTool
	 * @param dlg
	 */
//	public PgenAddLabelHandler(NCMapEditor mapEditor, PgenResource drawingLayer,
	public PgenAddLabelHandler(AbstractEditor mapEditor, PgenResource drawingLayer,
			ILabeledLine prevTool, AttrDlg dlg ){
		this.mapEditor= mapEditor;
		this.drawingLayer = drawingLayer;
		this.prevTool = prevTool;
		this.dlg = dlg;
		this.pts = new ArrayList<Coordinate>();
	}
	
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
     *      int, int)
     */
    @Override	
    public boolean handleMouseDown(int anX, int aY, int button) {
    	
    	if ( !drawingLayer.isEditable())  return false;

    	//  Check if mouse is in geographic extent
    	Coordinate loc = mapEditor.translateClick(anX, aY);
    	if ( loc == null || shiftDown) return false;

    	if ( button == 1 ) {
    		
    		if ( lineSelected != null ){
    			
				pts.add( loc );

    			if ( !prevTool.getLabeledLine().getName().contains("CCFP_SIGMET") 
    					&& inPoly(loc, lineSelected) ){
    				addLabel( loc, (LabeledLine) lineSelected.getParent()); 
    				pts.clear();
    			}
    		}
    		else {
    			LabeledLine ll = prevTool.getLabeledLine();
    			AbstractDrawableComponent nearestComp = drawingLayer.getNearestComponent(loc);

    			if ( ( nearestComp != null ) && nearestComp.getPgenType().equalsIgnoreCase(ll.getPgenType())){
    			//find nearest line
    				Iterator<AbstractDrawableComponent> it = ((LabeledLine)nearestComp).getComponentIterator();
				double minDist = Double.MAX_VALUE;

    			while( it.hasNext() ){
    				AbstractDrawableComponent adc = it.next();
    				if ( adc instanceof Line ){
    					Line ln = (Line) adc;
    					double dist = Double.MAX_VALUE;

    					Object pts[] =  adc.getPoints().toArray();

    					for ( int ii = 0; ii <  pts.length; ii++ ) {

    						if ( ii == pts.length - 1){
    							if ( ln.isClosedLine() ){
    								dist = drawingLayer.distanceFromLineSegment(loc, (Coordinate)pts[ii], (Coordinate)pts[0]);
    							}
    							else {
    								break;
    							}
    						}
    						else {	
    							dist = drawingLayer.distanceFromLineSegment(loc, (Coordinate)pts[ii], (Coordinate)pts[ii+1]);
    						}

    						if ( dist < minDist ) {

    							minDist = dist;
    							lineSelected = ln;

    						}
    					}
    				}
    			}
    			}
    			
    		}
    		
    		return true;

    	}
    	else if ( button == 3 ) {

    		if ( ghostLabel != null ){
    			
    			if(prevTool.getLabeledLine().getName().contains("CCFP_SIGMET")){  
    				addCcfpLabel(loc, prevTool.getLabeledLine());
    				cleanUp(); 
    				return true;
    			}
    			else{	 
    		
    				if ( !pts.isEmpty()){
    		//			addLabel( pts.get(pts.size()-1), prevTool.getLabeledLine() );
    					addLabel( pts.get(pts.size()-1), (LabeledLine)lineSelected.getParent() );

    				}
    				else {
    	    			lineSelected = null;
    	    			ghostLabel = null;
    	    			drawingLayer.removeGhostLine();
    	    			mapEditor.refresh();
    				}
    			}
    			pts.clear();
    		}
    		else {
    			//clean up and exit
    			lineSelected = null;
    			drawingLayer.removeGhostLine();
    			mapEditor.refresh();

    			prevTool.resetMouseHandler();
    			dlg.resetLabeledLineBtns();
    		}
    		
    		return true;

    	}
    	else{
    		return false;
    	}
    	
    }

    private void addLabel( Coordinate loc, LabeledLine ll ){
    	// make a copy and add label to the copy,
    	// then replace the original line with the new one so that
    	// undo/redo will work
    	LabeledLine newll = ll.copy();
    	Label lbl = createLabel(loc, newll, lineSelected);
    	newll.addLabel( lbl );

    	//merge two labels if they are close to each other.
    	LabeledLine mergedLine = null;
    	mergedLine = PgenUtil.mergeLabels(newll, lbl, lbl.getSpe().getLocation(), mapEditor, drawingLayer);

    	//save mid-level cloud text settings
    	if ( ll.getName().equalsIgnoreCase("Cloud")){
    		AttrSettings.getInstance().setSettings((DrawableElement)lbl.getSpe());
    	}

    	if ( mergedLine != null ){
			
			ArrayList<AbstractDrawableComponent> old = new ArrayList<AbstractDrawableComponent>();
			old.add( mergedLine);
			old.add( ll );
			
			ArrayList<AbstractDrawableComponent> newLines = new ArrayList<AbstractDrawableComponent>();
			newLines.add(newll);

			drawingLayer.replaceElements(old, newLines);
		}
		else {
    	drawingLayer.replaceElement(ll, newll);
		}

    	prevTool.setLabeledLine( newll );

    	lineSelected = null;

    	if ( drawingLayer.getSelectedDE()!= null ) {
    		resetSelected(newll);
    	}

    	ghostLabel = null;
    	drawingLayer.removeGhostLine();
    	mapEditor.refresh();

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseMove(int,
     *      int)
     */
    @Override
    public boolean handleMouseMove(int x, int y) {
    	if ( !drawingLayer.isEditable())  return false;

    	LabeledLine l = prevTool.getLabeledLine(); 
    	if(l!=null && l instanceof gov.noaa.nws.ncep.ui.pgen.sigmet.Ccfp)
    		return handleCcfpMouseMove(x,y);
    	    	
    	if ( lineSelected != null ){
    		//  Check if mouse is in geographic extent
    		Coordinate loc = mapEditor.translateClick(x, y);
    		if ( loc == null ) return false;

    		//LabeledLine ll = prevTool.getLabeledLine();
    		LabeledLine ll = (LabeledLine) lineSelected.getParent();
    		if ( ll == null ) return false;

    		//create ghost for label
    		ghostLabel = createLabel(loc, ll, lineSelected);
    		drawingLayer.setGhostLine( ghostLabel );      
    		mapEditor.refresh();
    	}
   	
    	return true;
    	
    }

    @Override
	public boolean handleMouseDownMove(int x, int y, int mouseButton) {
		if ( !drawingLayer.isEditable()|| shiftDown ) return false;
		else return true;	}

	/**
     * Create label for line ln at location loc.
     * @param loc
     * @return
     */
    private Label createLabel(Coordinate loc, LabeledLine ll, Line ln){
    	
		if ( ll == null ) return null;
		
		//initialize
		Label lbl = null;
		SinglePointElement spe = null;
		
    	// create label
		if ( ll.getName().equalsIgnoreCase("Cloud")){
			DrawableElementFactory def = new DrawableElementFactory();
			MidCloudText mtxt = (MidCloudText)def.create(DrawableType.MID_CLOUD_TEXT, (IAttribute)((CloudAttrDlg)dlg).getLabelDlg(),
			        		"Text", "MID_LEVEL_CLOUD", loc, drawingLayer.getActiveLayer() );
			
			//mtxt.setTwoColumns(false);
			
			spe = mtxt;
			
			lbl = new Label( mtxt );
			lbl.setParent( ll );
		/*	Text txt = new Text() ;
			spe = txt;
			
			txt.setLocation( loc );
			txt.setParent(lbl);
			txt.setPgenCategory("Text");
			txt.setPgenType("GENERAL_TEXT");
			txt.setMask(true);
			txt.setOutline(false);
			txt.setFontSize(14.0f);
			txt.setFontName("Courier");
			txt.setMask(true);
			txt.setOutline(true);
			txt.setStyle(FontStyle.REGULAR);
			txt.setJustification(TextJustification.CENTER);
			txt.setRotationRelativity(TextRotation.SCREEN_RELATIVE);
			
			txt.setColors(dlg.getColors());
			txt.setText(((CloudAttrDlg)dlg).getString());
				
			lbl= new Label( txt );
			lbl.setParent( ll );
		*/
	/*		AvnText avntxt = new AvnText() ;
			spe = avntxt;
			
			avntxt.setLocation( loc );
			avntxt.setParent(lbl);
			avntxt.setPgenCategory("Text");
			avntxt.setPgenType("AVIATION_TEXT");
			avntxt.setAvnTextType(AviationTextType.CLOUD_LEVEL);
			avntxt.setSymbolPatternName("Not Applicable");
			avntxt.setMask(true);
			avntxt.setOutline(false);
			avntxt.setFontSize(14.0f);
			avntxt.setFontName("Courier");
			avntxt.setStyle(FontStyle.REGULAR);
			avntxt.setJustification(TextJustification.CENTER);
			
			avntxt.setColors(dlg.getColors());
			avntxt.setTopValue(((CloudAttrDlg)dlg).getTopValue());
			avntxt.setBottomValue(((CloudAttrDlg)dlg).getBottomValue());
				
			lbl= new Label( avntxt );
			lbl.setParent( ll );
	*/
			
		}
		else if ( ll.getName().equalsIgnoreCase("Turbulence")){
			AvnText avntxt = new AvnText() ;
			spe = avntxt;
			
			avntxt.setLocation( loc );
			avntxt.setParent(lbl);
			avntxt.setPgenCategory("Text");
			avntxt.setPgenType("AVIATION_TEXT");
			avntxt.setAvnTextType(AviationTextType.HIGH_LEVEL_TURBULENCE);
			//avntxt.setSymbolPatternName("TURBULENCE_4");
			avntxt.setSymbolPatternName(((TurbAttrDlg)dlg).getSymbolPatternName());
			avntxt.setMask(true);
			avntxt.setDisplayType( DisplayType.NORMAL );
			avntxt.setFontSize(14.0f);
			avntxt.setFontName("Courier");
			avntxt.setStyle(FontStyle.REGULAR);
			avntxt.setJustification(TextJustification.CENTER);
			avntxt.setText(new String[]{""});
			
			avntxt.setColors(dlg.getColors());
			avntxt.setTopValue(((TurbAttrDlg)dlg).getTopValue());
			avntxt.setBottomValue(((TurbAttrDlg)dlg).getBottomValue());
				
			lbl= new Label( avntxt );
			lbl.setParent( ll );
		}
		else if ( ll.getName().contains("CCFP_SIGMET")){
			gov.noaa.nws.ncep.ui.pgen.elements.Text t=new gov.noaa.nws.ncep.ui.pgen.elements.Text();
			spe=t;
			lbl=new Label(t);
			lbl.setParent( ll );
			setCcfpText(loc,ll,ln,t,lbl); 
			return lbl;
		}		
		//create arrow line
		if ( spe != null ){
			//check if the text is inside the polygon
			int[] xpoints = new int[ln.getPoints().size()];
			int[] ypoints = new int[ln.getPoints().size()];
			for ( int ii = 0; ii < ln.getPoints().size(); ii++){
				double pt[] = mapEditor.translateInverseClick(ln.getPoints().get(ii));
				xpoints[ii] = (int) pt[0];
				ypoints[ii] = (int) pt[1];
			}

			Polygon poly = new Polygon(xpoints, ypoints, ln.getPoints().size());
    		double txtLoc[] = mapEditor.translateInverseClick(spe.getLocation());

    		if ( !poly.contains(txtLoc[0], txtLoc[1])){

    			//create the arrow line
    			ArrayList<Coordinate> locs = new ArrayList<Coordinate>();
    			locs.add(spe.getLocation());
    			
    		
    			
    			if ( !pts.isEmpty() ){
    				int idx = pts.size() - 1;
    				if ( locs.get(0) == pts.get(idx)){
    					idx--;
    				}
    				for ( int ii = idx; ii >=0 ; ii-- ){
    					locs.add(pts.get(ii));
    				}
    			}
    			
    			Coordinate scnCenter = this.getScreenCentroid(ln);
    			locs.add( mapEditor.translateClick(scnCenter.x, scnCenter.y));

    			Color[] arrowColors = new Color[2];
    			if (  ll.getName().equalsIgnoreCase("Cloud") ){
    				arrowColors = ((CloudAttrDlg)dlg).getLabelDlg().getColors();
    			}
    			else {
    				arrowColors = dlg.getColors();
    			}
    			
    			Line arrowLn = new Line(null, arrowColors, 1.0F, 1.0,
    					false, false, locs, 0, FillPattern.SOLID, "Lines", "POINTED_ARROW");

    			lbl.addArrow( arrowLn );
			}
		}
		
		return lbl;
		
    }

    /**
     * reset the input as selected element of the drawing layer 
     * @param labeledLine
     */
	private void resetSelected( LabeledLine labeledLine){
		if ( labeledLine != null ){
			drawingLayer.removeSelected();
			Iterator<DrawableElement> it = labeledLine.createDEIterator();
			while( it.hasNext() ){
				drawingLayer.addSelected(it.next());
			}
		}
	}	
	
	private void setCcfpText(Coordinate loc, LabeledLine ll, Line ln, gov.noaa.nws.ncep.ui.pgen.elements.Text t,Label lbl){
		t.setLocation( loc );
		t.setParent(lbl);
		t.setPgenCategory("Text");
		t.setPgenType("AVIATION_TEXT");

		t.setMask(true);
		t.setDisplayType(DisplayType.BOX);//false);
		t.setFontSize(14.0f);
		t.setFontName("Courier");
		t.setStyle(FontStyle.REGULAR);
		t.setJustification(TextJustification.CENTER);

		t.setText(gov.noaa.nws.ncep.ui.pgen.sigmet.CcfpInfo.getCcfpTxt2(((gov.noaa.nws.ncep.ui.pgen.sigmet.Ccfp)ll).getSigmet()));//new String[]{"CCFP_SIGMET"});
		t.setRotationRelativity(TextRotation.SCREEN_RELATIVE);		
		t.setColors(dlg.getColors());
		
		ArrayList<Coordinate> locs = new ArrayList<Coordinate>();
		locs.add(loc);//lbl.getSpe().getLocation());//spe.getLocation());
		
		if ( !pts.isEmpty() ){
			for ( int ii = pts.size() - 1; ii >=0 ; ii-- ){
				locs.add(pts.get(ii));
			}
		}

		if( ! isPtsInArea(((gov.noaa.nws.ncep.ui.pgen.sigmet.Ccfp)ll).getAreaLine(), loc)){		
			
			locs.add(((gov.noaa.nws.ncep.ui.pgen.sigmet.Ccfp)ll).getAreaLine().getCentroid());//ln.getCentroid());	
			
			Line arrowLn = new Line(null, new Color[]{Color.WHITE}, 2.0F, 1.0,
					false, false, locs, 0, FillPattern.FILL_PATTERN_2, "Lines", "POINTED_ARROW");	
			lbl.addArrow(arrowLn);
		}
		//lbl.setSpe(t);
	}
	
	private boolean handleCcfpMouseMove(int x,int y){
    	
    	Coordinate loc = mapEditor.translateClick(x, y);
    	if ( loc == null ) return false;

    	LabeledLine ll = prevTool.getLabeledLine();
    	if ( ll == null ) return false;
    		
    	gov.noaa.nws.ncep.ui.pgen.sigmet.Ccfp cc = (gov.noaa.nws.ncep.ui.pgen.sigmet.Ccfp)ll;
    		
    	Line areaLine = cc.getAreaLine();
    	if(areaLine == null) return false;
    		
    	//create ghost for label
    	ghostLabel = createLabel(loc, ll, lineSelected);
    		
    	drawingLayer.setGhostLine( ghostLabel );      
    	mapEditor.refresh();    	
   	
		return true;
	}
	
    private void addCcfpLabel( Coordinate loc, LabeledLine ll ){
		
		// make a copy and add label to the copy,
		// then replace the original line with the new one so that
		// undo/redo will work
		LabeledLine newll = ll.copy();
		Label lbl = createLabel(loc, newll, ((gov.noaa.nws.ncep.ui.pgen.sigmet.Ccfp)ll).getAreaLine());
		newll.addLabel( lbl );
			
		drawingLayer.replaceElement(ll, newll);
		prevTool.setLabeledLine( newll );
		
		if ( drawingLayer.getSelectedDE()!= null ) {
			resetSelected(newll);
		}
		
		ghostLabel = null;
		drawingLayer.removeGhostLine();
		mapEditor.refresh();
    }
    
    
    private boolean inPoly( Coordinate loc,  Line ln ){
		int[] xpoints = new int[ln.getPoints().size()];
		int[] ypoints = new int[ln.getPoints().size()];
		for ( int ii = 0; ii < ln.getPoints().size(); ii++){
			double pt[] = mapEditor.translateInverseClick(ln.getPoints().get(ii));
			xpoints[ii] = (int) pt[0];
			ypoints[ii] = (int) pt[1];
		}

		Polygon poly = new Polygon(xpoints, ypoints, ln.getPoints().size());

		double scnLoc[] = mapEditor.translateInverseClick( loc );

		return poly.contains(scnLoc[0], scnLoc[1]);
    }
    
    private void cleanUp(){
		lineSelected = null;
		drawingLayer.removeGhostLine();
		mapEditor.refresh();

		prevTool.resetMouseHandler();
    }
    
	public static boolean isPtsInArea(Line l, Coordinate pts){			
		
		Coordinate[] c = new Coordinate[l.getLinePoints().length+1];
		c = Arrays.copyOf(l.getLinePoints(), c.length);
		c[c.length-1] = c[0];
		
		if(c.length < 4) return false;//NOT a polygon; so canNOT contain a point.		
		
		GeometryFactory f = new GeometryFactory();	
			
		return f.createPolygon(f.createLinearRing(c), null).contains(new GeometryFactory().createPoint(pts));
	}
	
	private Coordinate getScreenCentroid( Line ln ){
		 GeometryFactory factory = new GeometryFactory();
		 Coordinate[] ptArray = new Coordinate[ln.getPoints().size() +1 ];
		 
		 for ( int ii = 0; ii < ln.getPoints().size(); ii++ ){
			 double xy[] = mapEditor.translateInverseClick( ln.getPoints().get(ii) );
			 ptArray[ ii ]= new Coordinate(xy[0], xy[1]);
		 }
		 
		 ptArray[ptArray.length -1] = ptArray[0]; // add the first point to the end
		 LineString g = factory.createLineString( ptArray );
		 Point p = g.getCentroid();
		 return p.getCoordinate();
	}

}
