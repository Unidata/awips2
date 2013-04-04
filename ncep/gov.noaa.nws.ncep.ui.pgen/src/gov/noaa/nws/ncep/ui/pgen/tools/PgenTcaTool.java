/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenTcaTool
 * 
 * 2 June 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.tools;

import java.awt.Color;
import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

//import gov.noaa.nws.ncep.ui.display.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.TcaAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;
import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextJustification;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.tca.BPGeography;
import gov.noaa.nws.ncep.ui.pgen.tca.Basin;
import gov.noaa.nws.ncep.ui.pgen.tca.Breakpoint;
import gov.noaa.nws.ncep.ui.pgen.tca.BreakpointFilter;
import gov.noaa.nws.ncep.ui.pgen.tca.BreakpointManager;
import gov.noaa.nws.ncep.ui.pgen.tca.BreakpointPair;
import gov.noaa.nws.ncep.ui.pgen.tca.StormAdvisoryNumber;
import gov.noaa.nws.ncep.ui.pgen.tca.TCAElement;
import gov.noaa.nws.ncep.ui.pgen.tca.TCVMessage;
import gov.noaa.nws.ncep.ui.pgen.tca.TropicalCycloneAdvisory;

/**
 * Implements a modal map tool for PGEN TCA .
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/09					S. Gilbert   	Initial Creation.
 * 
 * </pre>
 * 
 * @author	S. Gilbert
 */

public class PgenTcaTool extends AbstractPgenDrawingTool {
	
	/*
	 * possible drawing modes
	 */
	private enum DrawingMode { IDLE, HIGHLIGHT, MODIFY, NEW_SINGLE, NEW_PAIR, SELECT_2ND };
	private DrawingMode mode = DrawingMode.IDLE;      // current drawing mode.
	
	private final double TOL = 0.5;
	
    private static final String tcaFileFormat = "tca_%s%02d%04d_%03d.vgf.xml";
    private static final String intTcaFileFormat = "tca_%s%02d%04d_%03d%s.vgf.xml";
	
	private BreakpointManager bmgr = null;
	
	//private final Symbol CIRCLE = new Symbol(null, new Color[]{Color.CYAN}, 2.0f, 1.0, false, null, "Marker", "OCTAGON");

	/*
	 * Attribute Dialog with all storm/advisory info
	 */
	private TcaAttrDlg tcaDlg;
	
	private TCAElement elem = null;
	private TropicalCycloneAdvisory selectedAdvisory = null;

    public PgenTcaTool(){
    	
    	super();
    	
    }
    
    /*
     * Invoked by the CommandService when starting this tool
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    protected void activateTool( ) {
    	
    	super.activateTool();
    	
    	bmgr = BreakpointManager.getInstance();
    	
    	/*
    	 * if the ExecutionEvent's trigger has been set, it should be the TCAElement
    	 * to start with.  Load it's attributes to the TCA attr Dialog.
    	 * If not. we will start with a new TCAelement
    	 */
    	elem = null;
    	if ( event.getTrigger() instanceof TCAElement ) elem = (TCAElement)event.getTrigger(); 
    		
    	if ( attrDlg instanceof TcaAttrDlg ) {
    		tcaDlg = (TcaAttrDlg)attrDlg;
    		tcaDlg.setTcaTool(this);
    		if ( elem != null ) attrDlg.setAttrForDlg( elem );
    	}
    	
    	//  Initial drawing mode
    	mode = DrawingMode.IDLE;

    	return;
    }
    
    @Override
    public void deactivateTool() {

    	super.deactivateTool();
    	if ( drawingLayer != null ) drawingLayer.setDefaultPtsSelectedColor();
    	
    }

    /**
     * Returns the current mouse handler.
     * @return
     */
    public IInputHandler getMouseHandler() {	
    
        if ( this.mouseHandler == null ) {
        	
        	/*
        	 * There is no initial handler for this tool.  Drawing handlers are
        	 * invoked by the attribute window.
        	 */
        	this.mouseHandler = new PgenTcaHandler();
        	
        }
        
        return this.mouseHandler;
    }
    
    /**
     * Contains the mouse handlers need to create and modify breakpoint
     * segments/advisories for a tropical cyclone
     * @author sgilbert
     *
     */
    public class PgenTcaHandler extends InputHandlerDefaultImpl {
    	
    	/**
    	 * An instance of DrawableElementFactory, which is used to 
    	 * create new elements.
    	 */
    	private DrawableElementFactory def = new DrawableElementFactory();
    	
    	private Symbol DOT = new Symbol(null, new Color[]{Color.WHITE}, 1.0f, 7.5, false, null, "Marker", "DOT");
    	private Symbol firstPt = null;
    	private Breakpoint bkpt1 = null;
    	private Breakpoint bkpt2 = null;
    	private boolean keepFirst;
    	
    	private DECollection ghost = new DECollection();
    	
    	private final String BKPT_TYPE_OFFICIAL = "Official";
    	
        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         *      int, int)
         */
        @Override	
        public boolean handleMouseDown(int anX, int aY, int button) {
        	if ( !isResourceEditable() ) return false;
        	
        	String coast;
        	boolean isIsland;
        	
        	/*
        	 * convert mouse click to geographic location
        	 */
           	Coordinate loc = mapEditor.translateClick(anX, aY);
        	if ( loc == null || shiftDown ) return false;

        	// get current geography type selection from the attr dialog
        	String geog = tcaDlg.getGeogType();

        	/*
        	 * Get current breakpoint type selection from the attr dialog,
        	 * and set the breakpoint filter to look for "Official" breakpoints only,
        	 * if selected.
        	 */
        	BreakpointFilter filter = new BreakpointFilter();
        	if ( tcaDlg.getBreakpointType().equals(BKPT_TYPE_OFFICIAL) ) filter.setOfficialOnly();

        	// left mouse button clicked
        	if ( button == 1 ) {
        		
        		switch (mode) {
        		
        		case IDLE:
        			/*
        			 * When click in IDLE mode, find the nearest breakpoint segment,
        			 * display it as selected, notify the attr dialog, and set
        			 * mode to HIGHLIGHT
        			 */
        			if ( elem != null && !elem.getAdvisories().isEmpty() ) {
        				TropicalCycloneAdvisory adv = elem.findClosestAdvisory(loc);
        				if ( adv != null ) {
        					selectAdvisory(adv);
        					tcaDlg.selectAdvisory(elem.getAdvisories().indexOf(adv));
        					mode = DrawingMode.HIGHLIGHT;
        				}
        			}
        			break;
        			
        		case HIGHLIGHT:
        			/*
        			 * In HIGHLIGHT mode, there is already a breakpoint segment selected.
        			 * If it is NOT a breakpoint pair (such as island or waterway), ignore click.
        			 * If a breakpoint Pair is selected, and the click was close to one of the 
        			 * endpoints, store opposite endpoint in bkpt1 for later and set 
        			 * Drawing mode to MODIFY.  this will allow us to select a new breakpoint for
        			 * segment.
        			 */
        			if ( selectedAdvisory.getSegment() instanceof BreakpointPair ) {
        				GeometryFactory gf = new GeometryFactory();
        				Point pt1 = gf.createPoint( selectedAdvisory.getSegment().getBreakpoints().get(0).getLocation() );
        				Point pt2 = gf.createPoint( selectedAdvisory.getSegment().getBreakpoints().get(1).getLocation() );
        				Point mouse = gf.createPoint(loc);
        				double dist1 = mouse.distance(pt1);
        				double dist2 = mouse.distance(pt2);
        				//System.out.println(dist1+" OR "+dist2);
        				if ( dist1 < dist2 ) {
        					if ( dist1 < TOL ) {
        						bkpt1 = selectedAdvisory.getSegment().getBreakpoints().get(1);
        						keepFirst = false;
                				mode = DrawingMode.MODIFY;
        					}
        				}
        				else {	
        					if ( dist2 < TOL ) {
        						bkpt1 = selectedAdvisory.getSegment().getBreakpoints().get(0);
        						keepFirst = true;
            					mode = DrawingMode.MODIFY;
        					}
        				}
        			}
        			break;
        		
        		case NEW_SINGLE:
        			/*
        			 * in NEW_SINGLE mode, a click creates a new Island or Waterway 
        			 * watch/warning advisory for the closest breakpoint found.
        			 */
            		BPGeography segment = null;

        			if ( geog.equals("Islands") ) {
        				// return closest island breakpoint
        				segment = bmgr.getNearestIsland(loc);
        			}
        			else if ( geog.equals("Water") ) {
        				// return closest waterway breakpoint
        				segment = bmgr.getNearestWaterway(loc);
        			}
        	
        			if ( segment != null ) {
        				/*
        				 * Create new advisory and add it to list in dialog
        				 */
        				TropicalCycloneAdvisory adv = new TropicalCycloneAdvisory(tcaDlg.getSeverity(),
        						 tcaDlg.getAdvisoryType(), geog, segment );
        				tcaDlg.addAdvisory(adv);
        				updateTcaElement();     // update TCAElement in the PGEN resource
        			}
        			break;

        		case NEW_PAIR:
        			/*
        			 * in NEW_PAIR mode, a click saves the nearest breakpoint
        			 * as the first breakpoint in the pair.
        			 * After selection, change mode to SELECT_2ND
        			 */
        			bkpt1 = bmgr.getNearestBreakpoint(loc, filter );
        			if ( bkpt1 == null ) break;
        			firstPt = DOT;
        			firstPt.setLocation(bkpt1.getLocation());
        			mode = DrawingMode.SELECT_2ND;
        			break;
        			
        		case SELECT_2ND:
        			/*
        			 * in SELECT_2ND mode, the first breakpoint has already been selected.
        			 * Set an option in the breakpoint filter to search for breakpoints only
        			 * in the same coast as the first. 
        			 * Get the nearest breakpoint as the second, create a new advisory and
        			 * add it to the advisory list in the dialog.
        			 * Return to IDLE mode.
        			 */
        			coast = bmgr.findCoastName(bkpt1);
        			isIsland = bmgr.isCoastIsland(coast);
        			filter.filterCoastName(coast);
        			bkpt2 = bmgr.getNearestBreakpoint(loc, filter);
        			if ( bkpt2 == null ) break;
        			if ( bkpt1.equals(bkpt2) && ! isIsland ) break;
        			BreakpointPair bkptPair = bmgr.getBreakpointPair(bkpt1,bkpt2);
        			if ( bkptPair != null ) {
        				TropicalCycloneAdvisory adv = new TropicalCycloneAdvisory(tcaDlg.getSeverity(),
        						 tcaDlg.getAdvisoryType(), geog, bkptPair );
        				tcaDlg.addAdvisory(adv);
        				updateTcaElement();   // update TCAElement in the PGEN resource
        			}
        			firstPt = null;
        			mode = DrawingMode.IDLE;
        			break;
        			
        		case MODIFY:
        			/*
        			 * In MODIFY mode, find the nearest breakpoint on same coast as bkpt1.
        			 * pair the new breakpoint w/ bkpt1 and create a new advisory.  Replace the
        			 * selected advisory with this new one.
        			 * Return to HIGHLIGHT mode.
        			 */
        			coast = bmgr.findCoastName(bkpt1);
        			isIsland = bmgr.isCoastIsland(coast);
        			filter.filterCoastName(coast);
        			bkpt2 = bmgr.getNearestBreakpoint(loc, filter);
        			if ( bkpt2 == null ) break;
        			if ( bkpt1.equals(bkpt2) && !isIsland ) break;
        			BreakpointPair bkptmodPair = null;
        			if ( keepFirst )
        				bkptmodPair = bmgr.getBreakpointPair(bkpt1,bkpt2);
        			else
        				bkptmodPair = bmgr.getBreakpointPair(bkpt2,bkpt1);
        			if ( bkptmodPair != null ) {
        				TropicalCycloneAdvisory adv = new TropicalCycloneAdvisory(tcaDlg.getSeverity(),
        						 tcaDlg.getAdvisoryType(), geog, bkptmodPair );
        				int idx = elem.getAdvisories().indexOf(selectedAdvisory);
        				tcaDlg.replaceAdvisory(idx, adv);
        				updateTcaElement();    // update TCAElement in the PGEN resource
        				selectAdvisory(idx);
        			}
        			firstPt = null;
        			mode = DrawingMode.HIGHLIGHT;
        			break;
        			
        		}
        		
        		return false;
        	}

        	// right mouse button pressed
        	else if ( button == 3 ) {

        		switch (mode) {
        		
        		case IDLE:
        			/*
        			 * Load PGEN Select tool
        			 */
        			drawingLayer.removeSelected();
            		PgenUtil.setSelectingMode();
        			break;
        			
        		case HIGHLIGHT:
        			/*
        			 * deselect the current advisory and return to IDLE mode
        			 */
        			tcaDlg.deselectAdvisory();
        			deselectAdvisory();
        			mode = DrawingMode.IDLE;
        			break;
        			
        		case MODIFY:
        			mode = DrawingMode.HIGHLIGHT;
        			break;
        			
        		case NEW_SINGLE:
        			mode = DrawingMode.IDLE;
        			break;
        			
        		case NEW_PAIR:
        			mode = DrawingMode.IDLE;
        			break;
        			
        		case SELECT_2ND:
        			/*
        			 * reset first selected breakpoint.  go back to NEW_PAIR
        			 * mode to select another first breakpoint
        			 */
        			bkpt1 = null;
        			firstPt = null;
        			mode = DrawingMode.NEW_PAIR;
        			break;
        		}

        		return true;

        	}
        	else
        		return false;

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

        	String coast;
        	boolean isIsland;
        	//System.out.println(" IN MODE : "+mode);
        	//  Check if mouse is in geographic extent
        	Coordinate loc = mapEditor.translateClick(x, y);
        	if ( loc == null ) return false;

        	if ( attrDlg != null ) {
        		
        		ghost.clear();      //reset ghosting elements
        		
        		// get current geography type selection from the attr dialog
        		String geog = tcaDlg.getGeogType();
        		
            	/*
            	 * Get current breakpoint type selection from the attr dialog,
            	 * and set the breakpoint filter to look for "Official" breakpoints only,
            	 * if selected.
            	 */
               	BreakpointFilter filter = new BreakpointFilter();
            	if ( tcaDlg.getBreakpointType().equals(BKPT_TYPE_OFFICIAL) ) filter.setOfficialOnly();

        		switch (mode) {
        		
        		case NEW_SINGLE:
        			/*
        			 * Find nearest island or waterway breakpoint and ghost it's name
        			 */
        			BPGeography bkptinfo = null;
        			if ( geog.equals("Islands") )
        				bkptinfo = bmgr.getNearestIsland(loc);
        			else if ( geog.equals("Water") )
        				 bkptinfo = bmgr.getNearestWaterway(loc);
        			if ( bkptinfo != null ) {
        				String name = bkptinfo.getBreakpoints().get(0).getName();
        				ghost.add( new Text(null,"Courier", 16.f, TextJustification.LEFT_JUSTIFY, 
        						loc, 0.0, TextRotation.SCREEN_RELATIVE, new String[] {name},
        						FontStyle.BOLD, Color.YELLOW, 0, 4, true, DisplayType.NORMAL, "TEXT", "General Text" ) );
        			}
        			break;
        			
        		case NEW_PAIR:
        			/*
        			 * Find nearest coast breakpoint and ghost it's name
        			 */
        			Breakpoint bkpt = bmgr.getNearestBreakpoint(loc,filter);
        			if ( bkpt != null ) {
        				String name = bkpt.getName();
        				ghost.add( new Text(null,"Courier", 16.f, TextJustification.LEFT_JUSTIFY, 
        						loc, 0.0, TextRotation.SCREEN_RELATIVE, new String[] {name},
        						FontStyle.BOLD, Color.YELLOW, 0, 1, true, DisplayType.NORMAL, "TEXT", "General Text" ) );
        			}
        			
        			break;

        		case SELECT_2ND:
        			/*
        			 * Find nearest coast breakpoint (on the same coast as the first
        			 * breakpoint) and ghost it's name.  Also, create a temporary TCAElement
        			 * from the Attr Dialog info.  Create a new advisory with the new 
        			 * segment info, and add it to the temporay element.  Add the temp
        			 * element to the ghost collection.
        			 */
        			coast = bmgr.findCoastName(bkpt1);
        			isIsland = bmgr.isCoastIsland(coast);
        			filter.filterCoastName(coast);
        			Breakpoint bkpttmp = bmgr.getNearestBreakpoint(loc, filter );
        			if ( bkpt1.equals(bkpttmp) && ! isIsland ) break;
            		if ( bkpttmp != null ) {
        				String name = bkpttmp.getName();
        				ghost.add( new Text(null,"Courier", 16.f, TextJustification.LEFT_JUSTIFY, 
        						loc, 0.0, TextRotation.SCREEN_RELATIVE, new String[] {name},
        						FontStyle.BOLD, Color.YELLOW, 0, 1, true, DisplayType.NORMAL, "TEXT", "General Text" ) );
        				TCAElement tcaTemp = (TCAElement)def.create(DrawableType.TCA, tcaDlg, pgenCategory, pgenType, loc, drawingLayer.getActiveLayer());
        				BreakpointPair bp = bmgr.getBreakpointPair(bkpt1, bkpttmp);
        				TropicalCycloneAdvisory adv = new TropicalCycloneAdvisory(tcaDlg.getSeverity(),
       						 tcaDlg.getAdvisoryType(), geog, bp );
        				tcaTemp.addAdvisory(adv);
        				ghost.add(tcaTemp);
        			}
        			
        			break;
        			
        		case MODIFY:
        			/*
        			 * Find nearest coast breakpoint (on the same coast as the first
        			 * breakpoint) and ghost it's name.  Also, create a temporary TCAElement
        			 * from the Attr Dialog info.  Create a new advisory with the new 
        			 * segment info, and replace the current selected advisory in the temporary 
        			 * element.  Add the temp element to the ghost collection.
        			 */
        			coast = bmgr.findCoastName(bkpt1);
        			isIsland = bmgr.isCoastIsland(coast);
        			filter.filterCoastName(coast);
        			Breakpoint bkptmod = bmgr.getNearestBreakpoint(loc, filter );
        			if ( bkpt1.equals(bkptmod) && ! isIsland ) break;
        			if ( bkptmod != null ) {
        				String name = bkptmod.getName();
        				ghost.add( new Text(null,"Courier", 16.f, TextJustification.LEFT_JUSTIFY, 
        						loc, 0.0, TextRotation.SCREEN_RELATIVE, new String[] {name},
        						FontStyle.BOLD, Color.YELLOW, 0, 1, true, DisplayType.NORMAL, "TEXT", "General Text" ) );
        				TCAElement tcaTemp = (TCAElement)def.create(DrawableType.TCA, tcaDlg, pgenCategory, pgenType, loc, drawingLayer.getActiveLayer());
        				BreakpointPair bp = null;
        				if ( keepFirst ) 
        					bp = bmgr.getBreakpointPair(bkpt1, bkptmod);
        				else
        					bp = bmgr.getBreakpointPair(bkptmod, bkpt1);
        				TropicalCycloneAdvisory adv = new TropicalCycloneAdvisory(tcaDlg.getSeverity(),
       						 tcaDlg.getAdvisoryType(), geog, bp );
        				int idx = elem.getAdvisories().indexOf(selectedAdvisory);
        				tcaTemp.replaceAdvisory(idx, adv);
        				ghost.add(tcaTemp);
        			}
        			break;
        			
        		}
        		
        		// Set ghost elements and refresh editor
        		if ( firstPt != null ) ghost.add(firstPt);
        		drawingLayer.setGhostLine( ghost );      
        		mapEditor.refresh();

        	}

        	return false;

        }
    }

    /*
     * Sets the drawing mode to NEW_SINGLE
     */
    public void setSingleMode() {
    	mode = DrawingMode.NEW_SINGLE;
    }
    
    /*
     * Sets the drawing mode to NEW_PAIR
     */
    public void setPairMode() {
    	mode = DrawingMode.NEW_PAIR;
    }
    
    /**
     * updates the TCAElement in the PGEN Resource.
     */
    public void updateTcaElement() {
    	
		if ( elem == null ) {
			/*
			 * create a new element with attributes from the Attr Dialog, and add
			 * it to the PGEN Resource
			 */
	    	DrawableElementFactory def = new DrawableElementFactory();
	    	Coordinate dummy = new Coordinate(0.0,0.0);

			elem = (TCAElement)def.create(DrawableType.TCA, tcaDlg, pgenCategory, pgenType, dummy, drawingLayer.getActiveLayer());
			drawingLayer.addElement( elem );
		}
		else {
			/*
			 * Make acopy of the existing element; update its attributes from
			 * those in the Attr Dialog;  replace the existing element with the new one
			 * in the pgen resource.   (This allows Undo/Redo)
			 */
			TCAElement newElem = (TCAElement)elem.copy();
			newElem.update(tcaDlg);
			drawingLayer.replaceElement(elem, newElem);
			elem = newElem;
		}
		drawingLayer.setSelected(elem);
		//drawingLayer.registerSelectedSymbol(elem, CIRCLE);
		mapEditor.refresh();
		
    }

    /*
     * notified when an advisory has been deleted.  Deselect any
     * advisories, and update TCA Element in the PGEN resource
     */
    public void advisoryDeleted() {
    	deselectAdvisory();
    	updateTcaElement();
    }
    
    /*
     * Notified when an advisory has been selected.  sets drawing mode to HIGLIGHT
     */
    public void selectAdvisory(int index) {
    	selectAdvisory(elem.getAdvisories().get(index));
		mode = DrawingMode.HIGHLIGHT;
    }
    
    /*
     * Saves given advisory as selected.  Displays the breakpoint handlebars for this
     * advisory in a different color
     */
    public void selectAdvisory(TropicalCycloneAdvisory adv) {
    	selectedAdvisory = adv;
    	drawingLayer.removePtsSelected();
    	drawingLayer.setPtsSelectedColor(Color.WHITE);
    	List<Coordinate> plist = elem.getPoints();
    	for ( Breakpoint bp : adv.getSegment().getBreakpoints() ) {
    		for ( Coordinate c : plist ) {
    			if ( bp.getLocation().equals2D(c) ) drawingLayer.addPtSelected(plist.indexOf(c));
    		}
    	}
    }
    
    /*
     * removes selected advisory;  get rid of highlight handlebarsand
     * reset drawing mode to IDLE
     */
    public void deselectAdvisory() {
    	selectedAdvisory = null;
    	drawingLayer.removePtsSelected();
    	drawingLayer.setDefaultPtsSelectedColor();
		mode = DrawingMode.IDLE;
    }
    

	/**
	 * SAves the current advisory to a separate VGF XML file.  The filename is
	 * based on the storm information.
	 * @return true, if advisory was saved successfully.
	 */
	public boolean saveAdvisory() {
		
		updateTcaElement();
		
		String filename = PgenUtil.getPgenActivityTextProdPath() + File.separator + generateFilename();
		boolean canWrite = PgenUtil.checkFileStatus(filename);
		if ( ! canWrite ) return false;
		
		Layer defaultLayer = new Layer();
		defaultLayer.addElement(elem);
		
		Product defaultProduct = new Product();
		defaultProduct.addLayer(defaultLayer);
		
		ArrayList<Product> prds = new ArrayList<Product>();
		prds.add( defaultProduct );
		Products filePrds = ProductConverter.convert( prds );

		FileTools.write( filename, filePrds );
		return true;
	}

	/*
	 * Checks to see if a file exists.  If so, pop up a dialog asking for permission
	 * to overwrite the file.
	 */
/*	private boolean checkFileStatus(String filename) {

		boolean canWrite = false;
		File f = new File(filename);
		
		if ( f.exists() ) {
			// display confirmation dialog
			String msg = "VG XML file " + filename + " already exists. Overwrite?";
			MessageDialog confirmDlg = new MessageDialog( 
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), 
					"Confirm", null, msg,
					MessageDialog.QUESTION, new String[]{"OK", "Cancel"}, 0);
			confirmDlg.open();

			if ( confirmDlg.getReturnCode() == MessageDialog.OK ) {
				canWrite = true;
			}
		} else {
			canWrite = true;
		}

        return canWrite;
	}
*/
	/*
	 * Generates the name of the file used to store the TCA Element.
	 * The filename is based on storm information.
	 */
	private String generateFilename() {
		
	    String filename = null;
	    String advnum = elem.getAdvisoryNumber();
	    String basin = Basin.getBasinAbbrev(elem.getBasin());
	    
	    if ( StormAdvisoryNumber.isIntermediate(advnum) ) {
	    	filename = String.format(intTcaFileFormat, basin, elem.getStormNumber(),
	    			                 elem.getAdvisoryTime().get(Calendar.YEAR),
	    			                 StormAdvisoryNumber.getRegularAdvisory(advnum),
	    			                 advnum.substring(advnum.length()-1) );
	    }
	    else {
	    	filename = String.format(tcaFileFormat, basin, elem.getStormNumber(),
	                 elem.getAdvisoryTime().get(Calendar.YEAR),
	                 StormAdvisoryNumber.getRegularAdvisory(advnum) );
	    }
	    return filename;
	   
	}

	/**
	 * Creates a TCV message for the watch/warnings in the current TCA Element advisory
	 * @return
	 */
	public String createTCV() {
		
		TCVMessage tcv;
		
		//  Check to see if a TCA Element exists for the previous advisory
		TCAElement prev = getPreviousAdvisory();
		String office = PgenUtil.getCurrentOffice();
		
		// Create the message
		if ( prev == null ) 
			tcv = new TCVMessage(office, elem);
		else
			tcv = new TCVMessage(office, prev, elem);
			
		// return the message TEXT.
		return tcv.createText();
	
	}

	/*
	 * Searches for a TCA Element for a previous Tropical cyclone advisory.
	 * If found, it is returned.
	 */
	private TCAElement getPreviousAdvisory() {

		TCAElement previous = null;
		String filename = findPreviousVgxFile();
		if ( filename == null ) return null;
		
		Products prods = FileTools.read(filename);
		List<Product> prds = ProductConverter.convert(prods);
		for ( Product p : prds ) {
			for ( Layer l : p.getLayers() ) {
				for ( AbstractDrawableComponent de: l.getDrawables() ) {
					if ( de instanceof TCAElement ) {
						previous = (TCAElement)de;
						break;
					}
				}
			}
		}
		
		return previous;
	}

	/*
	 * Looks for a VGF XML file for the previous advisory.  If found, the filename is returned.
	 * Checks for intermediate advisories as well as regular advisories.
	 */
	private String findPreviousVgxFile() {
		
		String fullname;
		String filename;
		String basin = Basin.getBasinAbbrev(elem.getBasin());
		int stormNum = elem.getStormNumber();
		int year = elem.getAdvisoryTime().get(Calendar.YEAR);
		String advno = elem.getAdvisoryNumber();
		int regnum = StormAdvisoryNumber.getRegularAdvisory(advno);
			
		if ( ! StormAdvisoryNumber.isIntermediate(advno) ) {
			
	    	filename = String.format(intTcaFileFormat, basin, stormNum, year, regnum-1, "b" );
	    	fullname = PgenUtil.getPgenActivityTextProdPath() + File.separator + filename;
	    	if ( fileExists(fullname) ) return fullname;
	   
	    	filename = String.format(intTcaFileFormat, basin, stormNum, year, regnum-1, "a" );
	    	fullname = PgenUtil.getPgenActivityTextProdPath() + File.separator + filename;
	    	if ( fileExists(fullname) ) return fullname;
	   
	    	filename = String.format(tcaFileFormat, basin, stormNum, year, regnum-1 );
	    	fullname = PgenUtil.getPgenActivityTextProdPath() + File.separator + filename;
	    	if ( fileExists(fullname) ) return fullname;
	   
	   }
		else if ( advno.endsWith("a") ) {
	   
			filename = String.format(tcaFileFormat, basin, stormNum, year, regnum );
	    	fullname = PgenUtil.getPgenActivityTextProdPath() + File.separator + filename;
	    	if ( fileExists(fullname) ) return fullname;
	   
		}
		else if ( advno.endsWith("b") ) {
			
	    	filename = String.format(intTcaFileFormat, basin, stormNum, year, regnum, "a" );
	    	fullname = PgenUtil.getPgenActivityTextProdPath() + File.separator + filename;
	    	if ( fileExists(fullname) ) return fullname;
	   
	    	filename = String.format(tcaFileFormat, basin, stormNum, year, regnum );
	    	fullname = PgenUtil.getPgenActivityTextProdPath() + File.separator + filename;
	    	if ( fileExists(fullname) ) return fullname;
	   
		}
		
		return null;
	}

	/*
	 * determines whether a file already exists with the same name
	 */
	private boolean fileExists(String tempname) {
		
		File tempfile = new File(tempname);
		return tempfile.exists();
	}

}

