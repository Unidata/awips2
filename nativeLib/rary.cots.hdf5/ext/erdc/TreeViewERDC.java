/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF Java Products. The full HDF Java copyright       *
 * notice, including terms governing use, modification, and redistribution,  *
 * is contained in the file, COPYING.  COPYING can be found at the root of   *
 * the source code distribution tree. You can also access it online  at      *
 * http://www.hdfgroup.org/products/licenses.html.  If you do not have       *
 * access to the file, you may request a copy from help@hdfgroup.org.        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package ext.erdc;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDesktopPane;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.JViewport;
import javax.swing.Popup;
import javax.swing.PopupFactory;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.CompoundDS;
import ncsa.hdf.object.Dataset;
import ncsa.hdf.object.FileFormat;
import ncsa.hdf.object.Group;
import ncsa.hdf.object.HObject;
import ncsa.hdf.object.ScalarDS;
import ncsa.hdf.view.DataView;
import ncsa.hdf.view.HDFView;
import ncsa.hdf.view.ImageView;
import ncsa.hdf.view.MetaDataView;
import ncsa.hdf.view.Tools;
import ncsa.hdf.view.TreeView;
import ncsa.hdf.view.ViewManager;
import ncsa.hdf.view.ViewProperties;

import org.jgraph.JGraph;
import org.jgraph.graph.AttributeMap;
import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultGraphModel;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.GraphLayoutCache;

public class TreeViewERDC extends JPanel 
	implements TreeView, ActionListener, MouseListener, MouseMotionListener
{
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
	private final static int CELL_W = 80;
	private final static int CELL_H = (int)(CELL_W/1.618);
	private final static int SPACE_X = 20; // cell distance
	private final static int SPACE_Y = 150; // level distance
	private final static String CLASS_ELLIPS = "ext.erdc.JGraphEllipseView";
	private final static String CLASS_DIAMOND = "ext.erdc.JGraphDiamondView";
	private final static String MIME_XLS = "excel";
	private final static String MIME_PDF = "pdf";
	private final static String MIME_HDF = "hdf";
	private final static String MIME_AUDIO = "audio";
	private final static String MIME_VIDEO = "video";
	private final static String MIME_JPG = "jpeg";
	private final static String MIME_TXT = "text/plain";
	private final static String MIME_HTML = "html";
    private final static String MIME_XML = "xml";	
	private final static String MIME_SW = "S/W";
    private final static String BASE_MAP = "BASE_MAP";
    private final static String ID_TABLE = "ID_TABLE";
    private final static String FOOTPRINT = "FOOTPRINT";
    private final static int    N_TOOLBAR_ELMS = 4;
    private final static HashMap<String, ImageIcon> iconMap = new HashMap<String, ImageIcon>(20);
    
    private static enum OBJ_TYPE {LEVEL1, LEVEL2, LEVEL3,
        AUDIO, VIDEO, JPG, PDF, XLS, HTML, SW, XML, TEXT, GROUP, DATASET};
    
	private final static int DISPLAY_TABLE = 0;
	private final static int DISPLAY_IMAGE = 1;
	
	private static enum LINK_TYPE 
	{ 
	    HARD, SOFT, EXT, UD;
	
	    public Color getLineColor() {
	        switch (this) {
	        case HARD: return Color.RED;
	        case SOFT: return Color.BLUE;
	        case EXT: return Color.BLUE;
	        case UD: return Color.MAGENTA;
	        default: return Color.DARK_GRAY;
	        }
	    }
	    
        public boolean isLineDahsed() {
            switch (this) {
            case HARD: return false;
            case SOFT: return false;
            case EXT: return true;
            case UD: return true;
            default: return true;
            }
        }
        
        public int getLineWidth() {
            switch (this) {
            case HARD: return 2;
            case SOFT: return 1;
            case EXT: return 1;
            case UD: return 1;
            default: return 1;
            }
        }        
	};
	
	private static enum ICON_TYPE 
	{
	    APPS, AUDIO, FEED, GENERIC, GRP1, GRP2, GRP3, HELP, MULTIMEDIA, 
	    PDF, URL, VIDEO, XLS, JPG;

	    public String getName() {
	        switch (this) {
	        case APPS: return "apps.gif";
	        case AUDIO: return "audio.gif";
	        case FEED: return "feed.gif";
	        case GENERIC: return "generic.gif";
	        case GRP1: return "grp1.gif";
	        case GRP2: return "grp2.gif";
	        case GRP3: return "grp3.gif";
	        case HELP: return "help.gif";
	        case MULTIMEDIA: return "multimedia.gif";
	        case PDF: return "pdf.gif";
	        case URL: return "url.gif";
	        case VIDEO: return "video.gif";
	        case XLS: return "xls.gif";
	        case JPG: return "image.gif";
	        default: return "apps.gif";
	        }
	    }
	    
        public ImageIcon getIcon() {
            return iconMap.get(getName());
        }	    
	}
	
	private final FileFormat h5format;
	private final JGraph jgraph;
	private final DefaultGraphModel jmodel;
	private final GraphLayoutCache jview;
    private final List<FileFormat> fileList;
    private final Hashtable<String, DefaultGraphCell> createdAttrs;
    private final Hashtable<String, DefaultGraphCell> createdObjs;
    private final JPopupMenu popupMenuHDF, popupMenuExt;
    private final PopupFactory popupFactory;
    private final JTextArea infoArea;
    private final JButton[] tbButtons;
    
    /** GUI component: window to show the Users' Guide */
    private final JFrame helpWindow;

    /** GUI component: editorPane to show the Users' Guide */
    private final JEditorPane helpEditorPane;


    /** popup to show information of data objects */
    private Popup infoPopup = null;

    /** the width of the window */
    private int gWidth = 500;

    /** The current selected node. */
    private DefaultMutableTreeNode selectedNode;

    /** the current selected object */
    private Object selectedObject;

    /** the base image */
    private HObject baseImage, bldgFootprint, idTable;

	private FileFormat currentFile = null;
	
    /** the owner of this treeview */
    private ViewManager viewer;
    
    /** hashmap that hold building information */
    private HashMap idMap; 
    
	public TreeViewERDC(ViewManager theView) 
	{
	    viewer = theView;
	    h5format = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);
	    fileList = new Vector<FileFormat>();
	    createdAttrs = new Hashtable<String, DefaultGraphCell>();
        createdObjs = new Hashtable<String, DefaultGraphCell>();
        popupMenuHDF = createPopupMenuHDF();
        popupMenuExt = createPopupMenuExt();
        baseImage = null;
        idMap = null;
        idTable = null;
        popupFactory = PopupFactory.getSharedInstance();
        
        // setup the Users guide window
        helpWindow = new JFrame("ERDC Concept Map");
        helpEditorPane = new JEditorPane();
        
        loadIcons(); 
        
        infoArea = new JTextArea();
        //infoArea.setPreferredSize(new Dimension(220, 60));
        infoArea.setEditable(false);
        infoArea.setForeground(Color.BLUE.brighter());
        //infoArea.setWrapStyleWord(true);
        //infoArea.setLineWrap(true);
        infoArea.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createRaisedBevelBorder(), 
                BorderFactory.createCompoundBorder(
                BorderFactory.createLoweredBevelBorder(), 
                BorderFactory.createEmptyBorder(5,5,5,5))));
        infoArea.setBackground(Color.LIGHT_GRAY);
        
        tbButtons = new JButton[N_TOOLBAR_ELMS];

	    jmodel = new DefaultGraphModel();
	    jview = new GraphLayoutCache(jmodel, new GPCellViewFactory());
	    jgraph = new JGraph(jmodel, jview);
	    
		jgraph.setEditable(true);
		jgraph.setEdgeLabelsMovable(false);
		//jgraph.setMoveable(false);

		jgraph.addMouseListener(this);
		jgraph.addMouseMotionListener(this);
		
        setLayout(new BorderLayout());
        add(createToolBar(), BorderLayout.NORTH);
		add(jgraph, BorderLayout.CENTER);
		
        createHelpPane();
        ((JFrame)viewer).setTitle(
            "AGC HDFView Prototype - Data Management with HDF5 for Military Operations");

	}
	
	private final static void loadIcons()
	{
        URL u = null;
        ClassLoader classLoader = ClassLoader.getSystemClassLoader();
        
        ICON_TYPE[] iconTypes = ICON_TYPE.values();
        for (int i=0; i<iconTypes.length; i++) {
            String name = iconTypes[i].getName();
            u = classLoader.getResource("ext/erdc/"+name);
            if (u != null)
                iconMap.put(name, new ImageIcon(u));
        }
	}
	
	private JToolBar createToolBar() 
	{
        JToolBar tbar = new JToolBar();
        tbar.setFloatable(true);
        
        // home button
        JButton button = new JButton(ICON_TYPE.HELP.getIcon()) ;
        tbar.add( button );
        button.setToolTipText( "Help" );
        button.addActionListener( this );
        button.setActionCommand( "Show help" );
        button.setMargin( new Insets( 0, 0, 0, 0 ) );
        
        for (int i=0; i<9; i++)
            tbar.addSeparator();
        
	    Icon[] tbIcons = new Icon[N_TOOLBAR_ELMS];
        tbIcons[0] = ICON_TYPE.GRP1.getIcon();
        tbIcons[1] = ICON_TYPE.GRP2.getIcon();
        tbIcons[2] = ICON_TYPE.GRP3.getIcon();
        tbIcons[3] = ICON_TYPE.MULTIMEDIA.getIcon();
        
        String[] tips = new String[N_TOOLBAR_ELMS];
        tips[0] = "Add leve 1 object";
        tips[1] = "Add leve 2 object";
        tips[2] = "Add leve 3 object";
        tips[3] = "Add media";
	    
	    for (int i=0; i<N_TOOLBAR_ELMS; i++) {
	        tbButtons[i] = new JButton(tbIcons[i]);
	        tbButtons[i].setBackground(Color.WHITE);
	        tbButtons[i].setBorder(BorderFactory.createLineBorder(Color.WHITE));
	        tbButtons[i].setActionCommand("Create object "+(i+1));
	        tbButtons[i].addActionListener(this);
	        tbButtons[i].setToolTipText(tips[i]);
	        tbButtons[i].setMargin( new Insets( 0, 0, 0, 0 ) );

	        tbar.add(tbButtons[i]);
	        if (i==2) {
	            tbar.addSeparator();
	            tbar.addSeparator();
	        }
	    }
	    
	    return tbar;
	}

    /** creates a popup menu for a right mouse click on a data object */
    private JPopupMenu createPopupMenuHDF()
    {
        JPopupMenu menu = new JPopupMenu();
        JMenuItem item;

        item = new JMenuItem( "Open As Table");
        item.setMnemonic(KeyEvent.VK_T);
        item.addActionListener(this);
        item.setActionCommand("Open data as table");
        menu.add(item);

        item = new JMenuItem( "Open As Image");
        item.setMnemonic(KeyEvent.VK_I);
        item.addActionListener(this);
        item.setActionCommand("Open data as image");
        menu.add(item);
        
        item = new JMenuItem( "Show Metadata");
        item.setMnemonic(KeyEvent.VK_M);
        item.addActionListener(this);
        item.setActionCommand("Show metadata");
        menu.add(item);
      
        
        return menu;
    }
    
    private JPopupMenu createPopupMenuExt()
    {
        JPopupMenu menu = new JPopupMenu();
        JMenuItem item;

        item = new JMenuItem( "Open Object");
        item.setMnemonic(KeyEvent.VK_L);
        item.addActionListener(this);
        item.setActionCommand("Open ext link");
        menu.add(item);

        return menu;
    }    
    /**
     * Creates a new cell.
     * @param graph  the graph
     * @param parent the parent cell
     * @param name the name of the cell
     * @param x The x coordinate of the cell
     * @param y The y coordinate of the cell
     * @param w The width of the cell
     * @param h The height of the cell
     * @param bg The background color of the cell
     * @param raised True for raised style; otherwise, false.
     * @param icon The image icon of the cell
     * @param isDashedEdge True if the edge is a dashed line; otherwise, false.
     * @param viewClass The name of the view class
     * @return the new cell;
     */
	private DefaultGraphCell createCell(JGraph graph, DefaultGraphCell parent, 
			Object obj, final String name, double x, double y, double w, 
			double h, Color bg, boolean raised, ImageIcon icon, 
			LINK_TYPE linkType, String viewClass) 
	{
		// Create vertex with the given name
	    DefaultGraphCell cell = null;
	    
        if (y<0) y = 0;
        if (x<0) x = 0;
	    
	    if (name == null || name.length()<=0) {
	        cell = new DefaultGraphCell(obj);
	    } else {
	        cell = new DefaultGraphCell(obj) { public String toString() { return name; } };	        
	    }

		AttributeMap map = cell.getAttributes();
		
		// set cell view
		if (viewClass != null)
			GPCellViewFactory.setViewClass(map, viewClass);
		
		if (bg != null) {
			GraphConstants.setGradientColor(cell.getAttributes(), bg);
			GraphConstants.setOpaque(cell.getAttributes(), true);
		}
		
		// set icon
		if (icon !=null) {
			GraphConstants.setIcon(map, icon);
			GraphConstants.setBounds(map, new Rectangle2D.Double(x, y, icon.getIconWidth(), h+20));
		} else {
			GraphConstants.setBounds(map, new Rectangle2D.Double(x, y, w, h));
			if (raised)
				GraphConstants.setBorder(map, BorderFactory.createRaisedBevelBorder());
			else
				GraphConstants.setBorderColor(map, Color.black);
		}
		
		Font f = GraphConstants.getFont(map);
		f = new Font(f.getName(), Font.BOLD, f.getSize());
		GraphConstants.setFont(map, f);

		// Add a Port
		cell.addPort();

		GraphLayoutCache gCache = graph.getGraphLayoutCache();
		gCache.insert(cell);
		if (parent != null) {
			DefaultEdge edge = createEdge(parent, cell, linkType);
			gCache.insert(edge);
		}

		return cell;
	}
	
	private DefaultEdge createEdge(DefaultGraphCell src, DefaultGraphCell dst, 
	        LINK_TYPE linkType)
	{
	    if (src == null || dst == null)
	        return null;
	    
		DefaultEdge edge = new DefaultEdge()
		{
		    @Override
            public String toString() {
		        return null; // do not show anything on the link
		    }
		};
		
		Object port = src.getFirstChild();
		if (port != null)
			edge.setSource(port);
		
		port = dst.getFirstChild();
		if (port != null)
			edge.setTarget(port);
		
		int arrow = GraphConstants.ARROW_CLASSIC;
		AttributeMap map = edge.getAttributes();
		GraphConstants.setLineEnd(map, arrow);
		GraphConstants.setEndFill(map, true);
		GraphConstants.setLineWidth(map, linkType.getLineWidth());
        GraphConstants.setBendable(map, false);
        GraphConstants.setDisconnectable(map, false);
        GraphConstants.setLineColor(map, linkType.getLineColor());
        
        if (linkType.isLineDahsed())
            GraphConstants.setDashPattern(map, new float[] {4, 4});
        
        // set link popup information
        String relation = null;
        
        switch (linkType) {
        case HARD:
            relation = dst + " is part of " +src + "\nH5G_LINK_HARD";
            String dstName = dst.toString();
            String srcName = src.toString();
            if ("IPB".equals(dstName) && "MDMP".equals(srcName))
                relation = dst + " is a path to " +src + "\nH5G_LINK_HARD";
            else if ("IPB".equals(srcName) && "OCOKA".equals(dstName))
                relation = dst + " is tagged to " +src + "\nH5G_LINK_HARD";
            break;
        case SOFT:
            relation = dst + " is referenced to " +src+ "\nH5G_LINK_SOFT";
            break;
        case EXT:
            relation = dst + " is a connection to " +src+ "\nH5G_LINK_EXT";
            break;
        case UD:
            relation = dst + " is a component of " +src+ "\nH5G_LINK_UD";
            break;
        }
		edge.setUserObject(relation);

        
		return edge;
	}
	
	/**
	 * Create a cell for an attribute.
	 * 
	 * @param obj The HObject
	 * @param graph
	 * @param parent
     * @param x The x coordinate of the cell
     * @param y The y coordinate of the cell
     * @param w The width of the cell
     * @param h The height of the cell
	 * @return the new cell
	 */
	private DefaultGraphCell createExtCell(HObject obj, JGraph graph, 
			DefaultGraphCell parent) 
	{
		int nAttrs=0, nCells=0;
		List attrs=null;
		ImageIcon icon=null;
		String uri, name="";
		OBJ_TYPE objType;
		DefaultGraphCell cell=null;
		double x, y;
		int w = CELL_W, h=CELL_H;
		
		if (obj==null)
			return null;
		
		Rectangle2D bound = GraphConstants.getBounds(parent.getAttributes());
        double x0 = bound.getX();
        double y0 = bound.getY();
        x=x0;
        y=y0;
		
		try { 
			attrs = obj.getMetadata();
		} catch (Exception ex) { attrs = null; }
		
		if (attrs == null || (nAttrs=attrs.size())<=0)
			return null;
		
		nCells=0;
		for (int i=0; i<nAttrs; i++) {
			Attribute attr = (Attribute)attrs.get(i);
			String attrName = attr.getName();
			
			if (attrName.startsWith("URI")) {
				int idx = -1;
				try { idx = Integer.parseInt(attrName.substring(3).trim()); }
				catch (Exception ex) {idx = -1; }
				
				objType = getObjType(obj, idx);
                name = getMime(objType);
                
				if (name != null) {
					icon = getIcon(objType);
					LINK_TYPE linkType = LINK_TYPE.SOFT;
					if (objType == OBJ_TYPE.DATASET) {
						String attrValue = ((String[])attr.getValue())[0];
						int tmpIdx = attrValue.lastIndexOf('/');
						name = attrValue.substring(tmpIdx+1);
						name = getNickName(name);
						
						x = x0+(nCells)*(SPACE_X+CELL_W);
						y = y0+SPACE_Y;
						
						if (x0 < gWidth/2)
							x -= (SPACE_X+CELL_W/2);
						else if (x0>gWidth/2)
							x += (SPACE_X+CELL_W/3);
						
						if (createdAttrs.containsKey(name)){
							cell = createdAttrs.get(name);
							jview.insert(createEdge(parent, cell, linkType));
						} else {
							cell = createCell(graph, parent, attr, name, x, y, 
								w+15, h, null, false, null, linkType, CLASS_ELLIPS);
							createdAttrs.put(name, cell);
							nCells++;
						}
					} // if (objType == OBJ_TYPE.HDF)
					else {
					    x = x0 + CELL_W+1.5*SPACE_X;
					    if (x0+SPACE_X <= gWidth/2)
					        x = x0 - CELL_W;
					    y = y0 + SPACE_Y/3-1.5*SPACE_Y/nAttrs*(i-1);
					    
					    linkType = LINK_TYPE.EXT;
					    if (MIME_XML.equalsIgnoreCase(name)) {
					        name = "feed";
					        linkType = LINK_TYPE.UD;
					        x += 30;
					        y -= 20;
					    }
					    else if (MIME_JPG.equalsIgnoreCase(name)) {
                            name = "cmap";
                            x += 2*(CELL_W+SPACE_X);
                            y = CELL_H;
                            linkType = LINK_TYPE.UD;
                            
                            DefaultGraphCell pcell = createdAttrs.get(MIME_SW);
                            if (pcell!= null) {
                                AttributeMap tmp_map = cell.getAttributes();
                                Rectangle2D bounds = GraphConstants.getBounds(tmp_map);
                                x = bounds.getX() - CELL_W +20;
                                y = bounds.getY() + SPACE_Y/5;
                                
                                parent = pcell;
                            }
                        }
					    
						cell = createCell(graph, parent, attr, name, x, y, 
								w, h, null, false, icon, linkType, null);
						
						createdAttrs.put(name, cell);
					}

                    AttributeMap map = cell.getAttributes();
                    GraphConstants.setExtraLabels(map, new Object[] {objType});
                    Font f = GraphConstants.getFont(map);
                    Font newf = null;
                    if (f!= null && (linkType == LINK_TYPE.EXT || linkType == LINK_TYPE.UD)) {
                        newf = new java.awt.Font(f.getName(), Font.ITALIC, f.getSize()); 
                        GraphConstants.setFont(map, newf);
                    }
				} // if (name != null) {
			} // if (attrName.startsWith("URI"))
		} // for (int i=0; i<nAttrs; i++)
		
		return cell;
	}
	
	private String getNickName(String name) {
	    String retName = name;
	    
	    if (name == null)
	        return null;

	    if (name.startsWith("bldg_footprint"))
	        retName = "Bldg_LIDAR";
	    else if (name.startsWith("Omni_ground-50ft"))
	        retName = "Omni-LOS";
	    else if (name.startsWith("ikonos_3band-1m"))
	        retName = "Img-IKONOS";
	    else if (name.startsWith("residential"))
	        retName = "Resd-UTP";
	    
	    return retName;
	}
	
	/** Find the object type pointed by the idx-th attribute. 
	 * Attributes are in pair of <MIMEx=mime_type, URIx=object_url>
	 *  
	 * @param obj object that contains the attribute 
	 * @param idx the index of the attribute
	 * @return the object type pointed by the URL.
	 */
    private OBJ_TYPE getObjType(HObject obj, int idx) 
    {
        OBJ_TYPE objType=null;
        List attrs=null;
        int nAttrs=0;
        
        if (obj==null)
            return null;
        
        try { 
            attrs = obj.getMetadata();
        } catch (Exception ex) { attrs = null; }
        
        if (attrs == null || (nAttrs=attrs.size())<=0)
            return null;
        
        for (int i=0; i<nAttrs; i++) {
            Attribute attr = (Attribute)attrs.get(i);
            String attrName = attr.getName();
            
            if (attrName.startsWith("MIME")) {
                int attrIdx = -1;
                try {
                    // MIMEx index matches with URIx index (idx)
                    attrIdx = Integer.parseInt(attrName.substring(4).trim());
                } catch (Exception ex) { attrIdx = -1; }
                
                if ((idx <0 && attrIdx<0) || (idx==attrIdx)) {
                    String attrValue = ((String[])attr.getValue())[0];
                    if (attrValue.endsWith(MIME_XLS)) {
                        objType = OBJ_TYPE.XLS;
                    }
                    else if (attrValue.endsWith(MIME_PDF)) {
                        objType = OBJ_TYPE.PDF;
                    }
                    else if (attrValue.endsWith(MIME_HDF)) {
                        objType = OBJ_TYPE.DATASET;
                    }
                    else if (attrValue.startsWith(MIME_AUDIO)) {
                        objType = OBJ_TYPE.AUDIO;
                    }
                    else if (attrValue.startsWith(MIME_VIDEO)) {
                        objType = OBJ_TYPE.VIDEO;
                    }
                    else if (attrValue.startsWith(MIME_TXT)) {
                        objType = OBJ_TYPE.TEXT;
                    }
                    else if (attrValue.endsWith(MIME_HTML)) {
                        objType = OBJ_TYPE.HTML;
                    }
                    else if (attrValue.endsWith(MIME_XML)) {
                        objType = OBJ_TYPE.XML;
                    }
                    else if (attrValue.endsWith(MIME_JPG)) {
                        objType = OBJ_TYPE.JPG;
                    }                    
                    else { 
                        objType = OBJ_TYPE.SW;
                    }
                    break;
                }
            } // if ((idx <0 && attrIdx<0) || (idx==attrIdx))
        } // if (attrName.startsWith("MIME"))
        
        return objType;
    } // private String getObjType(HObject obj, int idx) 
	
	private ImageIcon getIcon(OBJ_TYPE type) 
	{
		ImageIcon icon = null;

        switch (type) {
        case XLS: return ICON_TYPE.XLS.getIcon();
        case PDF: return ICON_TYPE.PDF.getIcon();
        case DATASET: return (ImageIcon)ViewProperties.getDatasetIcon();
        case AUDIO: return ICON_TYPE.AUDIO.getIcon();
        case VIDEO: return ICON_TYPE.VIDEO.getIcon();
        case JPG: return ICON_TYPE.JPG.getIcon();
        case TEXT: return (ImageIcon) ViewProperties.getTextIcon();
        case HTML: return ICON_TYPE.URL.getIcon();
        case XML: return ICON_TYPE.FEED.getIcon();        
        case SW: return ICON_TYPE.APPS.getIcon();
        case LEVEL1: return ICON_TYPE.GRP1.getIcon();
        case LEVEL2: return ICON_TYPE.GRP2.getIcon();
        case LEVEL3: return ICON_TYPE.GRP3.getIcon();
        }
        
        return icon;
	}
	
    private boolean isExternal(OBJ_TYPE type) 
    {
        boolean b = false;

        switch (type) {
        case XLS:
        case PDF:
        case DATASET:
        case AUDIO:
        case VIDEO:
        case TEXT:
        case HTML:
        case XML:
        case SW: 
        case LEVEL3:return true;
        case LEVEL1:
        case LEVEL2: return false;
        }
        
        return b;
    }	
	
    private String getMime(OBJ_TYPE type) 
    {
        String str = null;
        
        switch (type) {
        case XLS: return MIME_XLS;
        case PDF: return MIME_PDF;
        case DATASET: return MIME_HDF;
        case AUDIO: return MIME_AUDIO;
        case VIDEO: return MIME_VIDEO;
        case JPG: return MIME_JPG;
        case TEXT: return MIME_TXT;
        case HTML: return MIME_HTML;
        case XML: return MIME_XML;
        case SW: return MIME_SW;
        case LEVEL1: return "L1";
        case LEVEL2: return "L2";
        case LEVEL3: return "L3";
        }
        
        return str;
    }	
	
	/**
	 * find the largest number of groups at the same level. It is used to
	 * estimate the width of graph.
	 * 
	 * @param node the node to be searched.
	 * @return the largest number of groups.
	 */
	private int findMaxNumberOfGroups(DefaultMutableTreeNode node) 
	{
	    int max = 0;

	    if (node == null)
	        return 0;
	    
	    Enumeration allNodes;
	    allNodes = node.breadthFirstEnumeration();
	    DefaultMutableTreeNode theNode;
	    
	    // find the longest path
	    int level = 0;
	    while (allNodes.hasMoreElements()) {
	        theNode = (DefaultMutableTreeNode)allNodes.nextElement();
	        level = Math.max(theNode.getLevel(), level);
	    }

	    if (level <= 0)
	        return 0;
	    
	    int counts[] = new int[level+1];
        for (int i=0; i<counts.length; i++)
            counts[i] = 0;

        allNodes = node.depthFirstEnumeration();
        while (allNodes.hasMoreElements()) {
            theNode = (DefaultMutableTreeNode)allNodes.nextElement();
            if (theNode.isLeaf())
                continue; // only counts for groups
            level = theNode.getLevel();
            counts[level]++;
        }
	    
	    for (int i=0; i<counts.length; i++)
	        max = Math.max(max, counts[i]);
	    
	    return max;
	}
	
	private void createObjCell(DefaultMutableTreeNode node, DefaultGraphCell pCell,
	        double x, double y)
	{
        TreeNode theNode;
	    DefaultGraphCell cell;
	    double _x=x, _y=y;
	    
	    if (node == null)
	        return; // nothing to display
	    
	    HObject obj = (HObject)node.getUserObject();
	    String key = String.valueOf((obj.getOID())[0]);
	    
	    // search if cell exists
	    cell = createdObjs.get(key);
	    
	    // cell does not exists, create a new one
	    if (cell == null) {
	        String cellClass = null;
	        Color color = null;
	        double w=CELL_W, h=CELL_H;
	        switch(node.getLevel()) {
	            case 1: 
	                color = Color.GREEN;
	                _x -= (CELL_W+SPACE_X)/2;
	                break;
	            case 2:
	                color = Color.ORANGE;
	                cellClass = CLASS_DIAMOND;
	                break;
	            case 3:
                    color = Color.GRAY;
                    cellClass = CLASS_ELLIPS;
                    w = h = (CELL_W+CELL_H)/2;
                    break;
                default:
                    break;
	        }

	        cell = createCell(jgraph, null, obj, obj.getName(), _x, y, 
	                w, h, color, true, null, LINK_TYPE.HARD, cellClass);
            createdObjs.put(key, cell);
	        
            // create cell for attribute
            createExtCell(obj, jgraph, cell);
	    } // if (cell == null)
       
	    // create edge
	    DefaultEdge edge = createEdge(pCell, cell, LINK_TYPE.HARD);
	    if (edge != null)
	        jview.insert(edge);
        
	    // count for total number of groups
        y += SPACE_Y;
        int n = node.getChildCount();
        int nCells = 0;
        for (int i=0; i<n; i++) {
            theNode = node.getChildAt(i);
            if (!theNode.isLeaf())
                nCells++;
        }
        
        int cellIdx=0;
        double x0 = (nCells/2.0+1.0)*(CELL_W+SPACE_X);
        x0 = Math.max(x0, x);
        for (int i=0; i<n; i++) {
            theNode = node.getChildAt(i);
            if (theNode.isLeaf())
                continue;

            // Recessively create cells for subgroups
            x = x0 + (cellIdx-nCells/2.0)*(CELL_W+SPACE_X);
            createObjCell((DefaultMutableTreeNode)theNode, cell, x, y);
            
            cellIdx++;
        }
	}
	
    private void createGraph(FileFormat file) 
    {
        double x=0, y=0;
        DefaultMutableTreeNode fileRoot, theNode;
        
        if (file == null)
            return;
        
        fileRoot = (DefaultMutableTreeNode)file.getRootNode();
        if (fileRoot == null) 
            return;
        
        int n = findMaxNumberOfGroups(fileRoot);
        gWidth = Math.max(500, n*(CELL_W+SPACE_X));
        
        n = fileRoot.getChildCount();
        int nCells = 0;
        for (int i=0; i<n; i++) {
            theNode = (DefaultMutableTreeNode)fileRoot.getChildAt(i);
            if (!theNode.isLeaf())
                nCells++;
        }
        
        if (nCells <=0 )
            return; // nothing to display
   
        int cellIdx=0;
        int space_x = gWidth/(nCells+1);
        for (int i=0; i<n; i++) {
            theNode = (DefaultMutableTreeNode)fileRoot.getChildAt(i);
            if (theNode.isLeaf())
                continue;

            cellIdx++;
            x = cellIdx*space_x-SPACE_X/2;
            createObjCell(theNode, null, x, y);
        }
    }
	   
    private static void launchBrowser(String url) throws Exception {
    	String os = System.getProperty("os.name");
    	Runtime runtime=Runtime.getRuntime();
    	
    	try{
    		// Block for Windows Platform
    		if (os.startsWith("Windows")){
    		    String cmd = "rundll32 url.dll,FileProtocolHandler "+ url;
    		    
    		    if (new File(url).exists())
    		        cmd = "cmd /c start \"\" \""+url+"\"";
    			Process p = runtime.exec(cmd);
    		}
    		//Block for Mac OS
    		else if(os.startsWith("Mac OS")){
    			Class fileMgr = Class.forName("com.apple.eio.FileManager");
    			Method openURL = fileMgr.getDeclaredMethod("openURL", new Class[] {String.class});
    			openURL.invoke(null, new Object[] {url});
    		}
    		//Block for UNIX Platform
    		else {
    			String[] browsers = {"firefox", "opera", "konqueror", "epiphany", "mozilla", "netscape" };
    			String browser = null;
    			for (int count = 0; count < browsers.length && browser == null; count++)
    				if (runtime.exec(new String[] {"which", browsers[count]}).waitFor() == 0)
    					browser = browsers[count];
    			if (browser == null)
    				throw new Exception("Could not find web browser");
    			else
    				runtime.exec(new String[] {browser, url});
    		}
    	}catch(Throwable x){
    		System.err.println("Exception occurd while invoking Browser!");
    		x.printStackTrace();
    	}
    }    
	
    private HObject openExtObject(String uri, boolean isExtLink)
    {
    	HObject hobj = null;
    	
    	System.setProperty("user.dir", currentFile.getParent());
    	File f = new File(uri);
    	if (f.exists()) { // it is an absolute file path
    		uri = f.getAbsolutePath();
    	} else  { // it is a relative file path
    		f = new File(f.getAbsolutePath());
    		if (f.exists()) 
    			uri = f.getAbsolutePath();
    	}
    	
    	if (isExtLink) {
    	     try { launchBrowser(uri); } catch (Exception ex) {}
    	}
    	
    	// it is an external hdf5 dataset
    	int idx = uri.indexOf(FileFormat.FILE_OBJ_SEP);
    	if (idx <= 0) return null;
    	f = new File(uri.substring(0,idx));
    	f = new File(f.getAbsolutePath());
    	
    	if (!f.exists())
    		return null;
    	
    	if (uri.endsWith("bldg_footprint") && bldgFootprint != null)
    	    return bldgFootprint;
    	else if (uri.endsWith("ikonos_3band-1m") && baseImage != null)
    	    return baseImage;

    	try {
    		hobj = FileFormat.getHObject(f.getAbsolutePath(), uri.substring(idx+FileFormat.FILE_OBJ_SEP.length()));
    	} catch (Exception ex) {ex.printStackTrace();}
  	
    	return hobj;
    }
    
    private HObject getSelectedHObject() 
    {
		Object obj = jgraph.getSelectionCell();
		if (!(obj instanceof DefaultGraphCell))
			return null;
		
		DefaultGraphCell theCell = (DefaultGraphCell)obj;
        obj = theCell.getUserObject();
        if(	!(obj instanceof Attribute))
        	return null;

		Attribute attr = (Attribute) obj;
		obj = attr.getValue();
		if (!obj.getClass().isArray())
			return null;
	
		obj = Array.get(obj, 0);
		if (!(obj instanceof String))
			return null;

		HObject hobj = openExtObject((String)obj, false);
		
		if (hobj != null)
			selectedObject = hobj;
		
		return hobj; 
    }
    
    private void getBaseMap(FileFormat file) throws Exception
    {
        String mapName = null;
        DefaultMutableTreeNode node = null;
        
        if (file == null)
            return;
        
        node = (DefaultMutableTreeNode)file.getRootNode();
        if (node == null) 
            return;
        
        node = (DefaultMutableTreeNode)node.getChildAt(0);
        if (node == null) 
            return;
        
        HObject obj = (HObject)node.getUserObject();
        if (obj == null)
            return;
        
        List attrs = obj.getMetadata();
        int n = attrs.size();
        Attribute attr = null;
        for (int i=0; i<n; i++) {
            attr = (Attribute) attrs.get(i);
            if (attr.getName().equalsIgnoreCase(BASE_MAP)) {
                mapName = (String) Array.get(attr.getValue(),0);
                break;
            }
        }
        
        // no base map
        if (mapName == null)
            return;

        HObject hobj = openExtObject(mapName, false);
        
        if (hobj != null && (hobj instanceof ScalarDS)) {
            selectedObject = baseImage = hobj;
            ((ScalarDS)hobj).setIsImage(true);
            try { 
                showDataContent(baseImage, DISPLAY_IMAGE);
            } catch (Exception ex) {}
        }
    }
    
    private void getIDTable(FileFormat file) throws Exception
    {
        String tblName = null;
        DefaultMutableTreeNode node = null;
        
        if (file == null)
            return;
        
        node = (DefaultMutableTreeNode)file.getRootNode();
        if (node == null) 
            return;
        
        node = (DefaultMutableTreeNode)node.getChildAt(0);
        if (node == null) 
            return;
        
        HObject obj = (HObject)node.getUserObject();
        if (obj == null)
            return;
        
        List attrs = obj.getMetadata();
        int n = attrs.size();
        Attribute attr = null;
        for (int i=0; i<n; i++) {
            attr = (Attribute) attrs.get(i);
            if (attr.getName().equalsIgnoreCase(ID_TABLE)) {
                tblName = (String) Array.get(attr.getValue(),0);
                break;
            }
        }
        
        // no base map
        if (tblName == null)
            return;

        idTable = openExtObject(tblName, false);
    }
    
    private void getFootPrint(FileFormat file) throws Exception
    {
        String tblName = null;
        DefaultMutableTreeNode node = null;
        
        if (file == null)
            return;
        
        node = (DefaultMutableTreeNode)file.getRootNode();
        if (node == null) 
            return;
        
        node = (DefaultMutableTreeNode)node.getChildAt(0);
        if (node == null) 
            return;
        
        HObject obj = (HObject)node.getUserObject();
        if (obj == null)
            return;
        
        List attrs = obj.getMetadata();
        int n = attrs.size();
        Attribute attr = null;
        for (int i=0; i<n; i++) {
            attr = (Attribute) attrs.get(i);
            if (attr.getName().equalsIgnoreCase(FOOTPRINT)) {
                tblName = (String) Array.get(attr.getValue(),0);
                break;
            }
        }
        
        // no base map
        if (tblName == null)
            return;

        HObject hobj = openExtObject(tblName, false);
        
        if (hobj != null && (hobj instanceof ScalarDS)) {
            selectedObject = bldgFootprint = hobj;
        }
    }    
    
    /** create HashMap for building IDs */
    private void createIDMap() throws Exception
    {
        Vector data = null;
        
        if (idTable == null || !(idTable instanceof CompoundDS))
            return;
    
        CompoundDS dset = (CompoundDS)idTable;
        data = (Vector)dset.getData();
        int rows = dset.getHeight();
        int cols = data.size();
        if (rows <=0 || cols <12)
            return;
        
        if (idMap == null)
            idMap = new HashMap(rows+5);
       
        int[] ids = (int[])data.get(11);
        float[] areas = (float[])data.get(0);
        float[] avghts = (float[])data.get(1);
        float[] minhts = (float[])data.get(2);
        float[] maxhts = (float[])data.get(3) ;
        float[] bases = (float[])data.get(4);
        float[] orients = (float[])data.get(7);
        float[] lens = (float[])data.get(8);
        float[] wids = (float[])data.get(9);
    
        for (int i=0; i<rows; i++) {
            BuildingInfo info = new BuildingInfo(ids[i], areas[i], avghts[i], 
                    minhts[i], maxhts[i], bases[i], orients[i], lens[i], wids[i]);
            idMap.put(Integer.valueOf(ids[i]), info);
        }
    }

    
    public DataView showDataContent(HObject dataObject, int displayType)
    throws Exception
    {
        String dataViewName = null;
        boolean isImage = (displayType==DISPLAY_IMAGE);
        
        if ((dataObject == null) ||!(dataObject instanceof Dataset)) {
            return null; // can only display dataset
        }

        Dataset d = (Dataset)dataObject;
        if (isImage) {
            dataViewName = (String)HDFView.getListOfImageView().get(0);
            if (d != null || !(d instanceof ScalarDS))
                ((ScalarDS)d).setIsImage(true);            
        }
        else {
            dataViewName = (String)HDFView.getListOfTableView().get(0);
        }

        if (d.getRank() <= 0) {
            d.init();
        }

        DataView dataView = viewer.getDataView(d);
        if (dataView != null) {
            boolean isImageView = (dataView instanceof ImageView);
            if (isImage == isImageView) {
                ((JInternalFrame)dataView).toFront();
                return null;
            }
            else {
                ((JInternalFrame)dataView).dispose();
            }
        }

        // enables use of JHDF5 in JNLP (Web Start) applications, the system class loader with reflection first.
        Class theClass = null;
        try { theClass = Class.forName(dataViewName); }
        catch (Exception ex) { theClass = ViewProperties.loadExtClass().loadClass(dataViewName); }

        Object theView = null;
        Object[] initargs = {viewer};
        ((JFrame)viewer).setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        try {
            theView = Tools.newInstance(theClass, initargs);
            viewer.addDataView((DataView)theView);
        }finally {
            ((JFrame)viewer).setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
        
        return (DataView)theView;
    }
    
    /**
     * Opens a file and retrieves the file structure of the file.
     * It also can be used to create a new file by setting the accessID to
     * FileFormat.CREATE.
     *
     * <p>
     * Subclasses must implement this function to take appropriate steps to
     * open a file.</p>
     *
     * @param filename the name of the file to open.
     * @param accessID identifier for the file access. Valid value of accessID is:
     * <ul>
     * <li>FileFormat.READ --- allow read-only access to file.</li>
     * <li>FileFormat.WRITE --- allow read and write access to file.</li>
     * <li>FileFormat.CREATE --- create a new file.</li>
     * </ul>
     *
     * @return the FileFormat of this file if successful; otherwise returns null.
     */
    public FileFormat openFile(String filename, int accessID)
        throws Exception
    {
        FileFormat h5file = null;
        File tmpFile = new File(filename);
        if (!tmpFile.exists()) {
            throw new UnsupportedOperationException("File does not exist.");
        }

        if (!h5format.isThisType(filename))
            throw new UnsupportedOperationException("It is not an HDF5 file.");
        	
        // read only
        h5file = h5format.open(filename, FileFormat.READ);        	

        if (h5file == null) {
            throw new java.io.IOException("Unsupported fileformat - "+filename);
        }

        ((JFrame)viewer).setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        try {
        	h5file.open();
        } finally {
            ((JFrame)viewer).setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
        
        // opens one file a time
        if (currentFile != null) {
        	closeFile(currentFile);
        }

        // create the graph for the file structure
        currentFile = h5file;
        fileList.add(h5file);
        createGraph(h5file);
        
        try { getIDTable(h5file); } catch (Exception ex) {}
        try { getBaseMap(h5file); } catch (Exception ex) {}
        try { createIDMap(); } catch (Exception ex) {}
        try { getFootPrint(h5file); } catch (Exception ex) {}
      
        return h5file;
    }
    
    /**
     * close a file
     * @param file the file to close
     */
    public void closeFile(FileFormat file) throws Exception
    {
        if (file == null) {
            return;
        }
        
        // find the desktoppane
        JDesktopPane contentPane = null;
        Component comps[] = ((JFrame)viewer).getContentPane().getComponents();
        for (int i=0; i<comps.length; i++) {
            if (comps[i] instanceof JSplitPane) {
                JSplitPane split = (JSplitPane)comps[i];
                Component comp = split.getTopComponent();
                if (!(comp instanceof JSplitPane))
                    break;

                JSplitPane sp1 = (JSplitPane) comp;
                comp = sp1.getRightComponent();

                if (!(comp instanceof JScrollPane))
                    break;

                JScrollPane sp = (JScrollPane)comp;
                Component comps2[] = sp.getComponents();
                for (int j=0; j<comps2.length; j++) {
                    if (comps2[j] instanceof JViewport) {
                        JViewport vp = (JViewport)comps2[j];
                        Component view = vp.getView();
                        if (view instanceof JDesktopPane) {
                            contentPane = (JDesktopPane) view;
                            break;
                        }
                    } // if (comps2[j] instanceof JViewport) {
                } //for (int j=0; j<comps2.length; j++) {
            } // if (comps[i] instanceof JSplitPane) {
        } // for (int i=0; i<comps.length; i++) {
        
        // close all the data windows of this file
        if (contentPane != null) {
            JInternalFrame[] frames = contentPane.getAllFrames();
            FileFormat rawdataFile = null;
            if (frames != null)
            {
                for (int i=0; i<frames.length; i++)
                {
                    HObject obj = (((DataView)frames[i]).getDataObject());
                    if (obj != null) {
                        rawdataFile = obj.getFileFormat();
                        frames[i].dispose();
                        frames[i] = null;
                    }
                }
            } // if (frames != null)
            
            // close the raw data file
            if (rawdataFile != null) {
                try { rawdataFile.close(); } catch (Exception ex) {;}
            }
        } // if (contentPane != null)

        try { file.close(); } catch (Exception ex) {;}
        fileList.clear();
        createdAttrs.clear();
        createdObjs.clear();
        currentFile = null;
        baseImage = null;
        idTable = null;
        idMap.clear();
        
        // remove all children of GUI components
        List roots = jmodel.getRoots();
        int n = roots.size();
        
        for (int i=0; i<n; i++) {
            int m = jmodel.getChildCount(roots.get(i));
            for (int j=0; j<m; j++){
                jmodel.remove(new Object[] {jmodel.getChild(roots.get(i), j)});
            }
        }
        // remove all root nodes
        jmodel.remove(jmodel.getRoots().toArray());
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.view.TreeView#saveFile(ncsa.hdf.object.FileFormat)
     */
    public void saveFile(FileFormat file)
    {;}

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.view.TreeView#getSelectedFile()
     */
    public FileFormat getSelectedFile()
    { return currentFile;}

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.view.TreeView#getSelectedObjects()
     */
    public List getSelectedObjects()
    { return null;}

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.view.TreeView#getCurrentObject()
     */
    public HObject getCurrentObject()
    { 
        if (selectedObject instanceof HObject)
            return  (HObject) selectedObject;
       
        return null;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.view.TreeView#showDataContent(ncsa.hdf.object.HObject)
     */
    public DataView showDataContent(HObject dataObject) throws Exception
    {
        return showDataContent(dataObject, DISPLAY_IMAGE);
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.view.TreeView#showMetaData(ncsa.hdf.object.HObject)
     */
    public MetaDataView showMetaData(HObject dataObject) throws Exception
    {
        if (dataObject == null) {
            return null;
        }

        List metaDataViewList = HDFView.getListOfMetaDataView();
        if ((metaDataViewList == null) || (metaDataViewList.size() <=0)) {
            return null;
        }

        int n = metaDataViewList.size();
        Class viewClass = null;
        String className = (String)metaDataViewList.get(0);

        // enables use of JHDF5 in JNLP (Web Start) applications, the system class loader with reflection first.
        Class theClass = null;
        try { theClass = Class.forName(className); }
        catch (Exception ex) { theClass = ViewProperties.loadExtClass().loadClass(className); }

        Object[] initargs = {viewer};
        MetaDataView dataView = (MetaDataView)Tools.newInstance(theClass, initargs);
        
        if (idTable != null && dataObject.equals(bldgFootprint)) {
            selectedObject = idTable;
            showDataContent(idTable, DISPLAY_TABLE);
        }

        return dataView;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.view.TreeView#addObject(ncsa.hdf.object.HObject, ncsa.hdf.object.Group)
     */
    public void addObject(HObject newObject, Group parentGroup)
    {;}

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.view.TreeView#getTree()
     */
    public JTree getTree()
    { return null;}

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.view.TreeView#getCurrentFiles()
     */
    public List getCurrentFiles()
    { return fileList;}


    /*
     * (non-Javadoc)
     * @see ncsa.hdf.view.TreeView#findTreeNode(ncsa.hdf.object.HObject)
     */
    public TreeNode findTreeNode(HObject obj)
    { return null;}
	
	@Override
	public void mouseClicked(MouseEvent e) {
		// TODO Auto-generated method stub
		
        int eMod = e.getModifiers();
		Object obj = jgraph.getSelectionCell();
		
		if (infoPopup != null)
		    infoPopup.hide();

        // right mouse click on node to display node info
		if ( (obj instanceof DefaultEdge) ||
		        (eMod & InputEvent.BUTTON3_MASK) !=0) 
		{
		    showInfo(obj, e.getXOnScreen()+5, e.getYOnScreen()+5);
		}
	
		// do nothing on edge or other types of objects.
        if ((obj instanceof DefaultEdge) || !(obj instanceof DefaultGraphCell))
            return;
        
		DefaultGraphCell theCell = (DefaultGraphCell)obj;
		selectedObject = obj = theCell.getUserObject();

        if (!theCell.equals(selectedNode))  {
        	viewer.mouseEventFired(e);
        	selectedNode = theCell;
        	
        	// show image that is associated with the group
        	_demoShowImage(selectedObject);
        }
        
        // right-mouse click to open data content
        // this only apply to leaf nodes
        if(	!(obj instanceof Attribute))
        	return;

		Attribute attr = (Attribute) obj;
		obj = attr.getValue();
		if (!obj.getClass().isArray())
			return;
	
		obj = Array.get(obj, 0);
		if (!(obj instanceof String))
			return;

		String uri = (String)obj;
		try { _demoShowMetaData(uri); } catch (Exception ex) {}
		
		// right mouse click
        if (e.isPopupTrigger() || (eMod == InputEvent.BUTTON3_MASK) ||
            (System.getProperty("os.name").startsWith("Mac") &&
            (eMod == (InputEvent.BUTTON1_MASK|InputEvent.CTRL_MASK))))
        {
            if (infoPopup != null)
                infoPopup.hide();
            
            // open external HObject
        	if (uri.indexOf(FileFormat.FILE_OBJ_SEP)>0) 
        	{
        		popupMenuHDF.show((JComponent)e.getSource(), e.getX(), e.getY());
        	} else {
        	    // other external object
        	    selectedObject = uri;
                popupMenuExt.show((JComponent)e.getSource(), e.getX(), e.getY());
        	}
        	
            return;
        }
	}


	@Override
	public void mouseEntered(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}
	

	@Override
	public void mouseExited(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void mouseReleased(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		// TODO Auto-generated method stub
        String cmd = e.getActionCommand();

        if (cmd.equals("Open data as table")) {
            try { showDataContent(getSelectedHObject(), DISPLAY_TABLE);}
            catch (Exception ex) {}
        }
        else if (cmd.equals("Open data as image")) {
            try { showDataContent(getSelectedHObject(), DISPLAY_IMAGE); }
            catch (Exception ex) {}
        } 
        else if (cmd.equals("Show metadata")) {
            try { showMetaData(getSelectedHObject()); }
            catch (Exception ex) {}
        }         
        else if (cmd.equals("Open ext link")) {
            if (selectedObject instanceof String)
                openExtObject((String)selectedObject, true);
        }         
        else if (cmd.startsWith("Create object")) {
            Object[] selectionValues = createdObjs.values().toArray();
            Object retVal = JOptionPane.showInputDialog(this, "Add object to:", 
                    "Create New Object", JOptionPane.PLAIN_MESSAGE, null, 
                    selectionValues, null);
            if (retVal == null || !(retVal instanceof DefaultGraphCell))
                return;
            
            DefaultGraphCell pcell = (DefaultGraphCell) retVal;
            OBJ_TYPE type = null;
            if (cmd.endsWith("1"))
                type = OBJ_TYPE.LEVEL1;
            else if (cmd.endsWith("2"))
                type = OBJ_TYPE.LEVEL2;
            else if (cmd.endsWith("3"))
                type = OBJ_TYPE.LEVEL3;            
            else if (cmd.endsWith("4"))
                type = OBJ_TYPE.AUDIO;
            else if (cmd.endsWith("5"))
                type = OBJ_TYPE.VIDEO;
            else if (cmd.endsWith("6"))
                type = OBJ_TYPE.PDF;
            else if (cmd.endsWith("7"))
                type = OBJ_TYPE.XLS;
            
            addNode(pcell, type);
        }
        else if (cmd.endsWith("Show help")) {
            helpWindow.setVisible(true);
        }
        else if (cmd.endsWith("Close help")) {
            helpWindow.setVisible(false);
        }
 	}
	
	private void addNode(DefaultGraphCell parent, OBJ_TYPE type) 
	{
	    if (parent == null || type == null)
	        return;

        AttributeMap map = parent.getAttributes();
        Rectangle2D rect = GraphConstants.getBounds(map);
        double x = rect.getX()+ SPACE_X;
        double y = rect.getY() + SPACE_Y/2;
        double w = CELL_W;
        double h = CELL_H;
        boolean isDashedEdge = isExternal(type);
        boolean isRaised = true;
        String viewClass = null;
        String name = "";
        ImageIcon icon = getIcon(type);
        
        Color bg = null;
        LINK_TYPE linkType=null;
        
        switch (type) {
        case LEVEL1:
            icon = null;
            viewClass = CLASS_DIAMOND;
            bg = Color.ORANGE;
            linkType = LINK_TYPE.HARD;
            break;
        case LEVEL2:
            icon = null;
            w = h = (CELL_W+CELL_H)/2;
            viewClass = CLASS_ELLIPS;
            bg = Color.GRAY;
            linkType = LINK_TYPE.HARD;
            break;
        case LEVEL3:
            icon = null;
            viewClass = CLASS_ELLIPS;
            isRaised = false;
            linkType = LINK_TYPE.HARD;
            break;
        default:
            icon = ICON_TYPE.GENERIC.getIcon();
            name = "freeware";
            linkType = LINK_TYPE.UD;
            break;
        }
        
	    createCell(jgraph, parent, getMime(type), name, x, y, w, h, bg, isRaised, 
	            icon, linkType, viewClass);
	}
	
	/**
	 *  display information in a popup box. 
	 * @param obj object of which information to be shown
	 * @param x horizontal position of the information box
	 * @param y vertical position of the information box
	 */
	private void showInfo(Object obj, int x, int y) 
	{
	    if (obj == null) return;
	    
	    if (x < 0) x = 0;
	    if (y < 0) y = 0;
	    
	    String info = obj.toString();
	    
	    if (obj instanceof DefaultEdge) {
	        Object edgeObj = ((DefaultEdge) obj).getUserObject();
	        if (edgeObj!=null)
	            info = edgeObj.toString();
	    } else if (obj instanceof DefaultGraphCell) {
	        DefaultGraphCell cell = (DefaultGraphCell)obj;
	        if (cell.toString().startsWith("MDMP"))
	            info = "Military Decision Making Process\n\n" +
	            		"    Receipt of mission\n" +
	            		"    Mission analysis\n"+
	            		"    Course of action development\n"+
	            		"    Course of action analysis\n"+
	            		"    Course of action comparison\n"+
	            		"    Course of action approval\n"+
	            		"    Orders production";
	        
	        else if (cell.toString().startsWith("IPB"))
	            info = "Intelligence Preparation of Battlefield\n\n"+
	                   "    Define the battlefield environment\n"+
	                   "    Describe the battlefield’s effects\n"+
	                   "    Evaluate the threat\n"+
	                   "    Determine threat COAs";
	        
            else if (cell.toString().startsWith("RECON"))
                info = "    Route - movement and communications\n"+
                       "    Area - terrain or enemy activity with a specified region\n"+
                       "    Zone - total breadth area of a battlefield\n"+ 
                       "    Force_Oriented - enemy organization or target";
            else if (cell.toString().startsWith("OCOKA"))
                info = "Observation and Fields of Fire\nCover and Concealment\nObstacles (man made and natural)\nKey or Decisive Terrain\nAvenues of Approach";
	    }
	    
	    infoArea.setText(info);
        infoPopup = popupFactory.getPopup((Component)viewer, infoArea, x, y);
        infoPopup.show();
	}


    @Override
    public void mouseDragged(MouseEvent e) {
        // TODO Auto-generated method stub
        
    }


    @Override
    public void mouseMoved(MouseEvent e) {
        // TODO Auto-generated method stub
        
    }
    
    /** get building information for a given point */
    public String getBldgInfo(HObject obj, int index) 
    {
        String info = null;
        Object bldgInfo = null;
        
        if ( (obj.equals(baseImage) || obj.equals(bldgFootprint)) && (bldgFootprint!=null)) {
            int[] ids = null;
            try {
                ids = (int[]) ((Dataset)bldgFootprint).getData();
                bldgInfo = idMap.get(Integer.valueOf(ids[index]));
            } catch (Throwable err) { bldgInfo = null; }
        }
        
        if (bldgInfo != null)
            info = bldgInfo.toString();
        
        return info;
    }

    /**
     * Create GUI components to show the users guide.
     */
    private void createHelpPane()
    {
        ClassLoader classLoader = ClassLoader.getSystemClassLoader();
        URL helpURL = classLoader.getResource("ext/erdc/help.html");
         
        if (helpURL == null) {
            viewer.showStatus("Cannot open erdc/help.html file.");
        }

        // set up the help window
        helpWindow.setLocation(220, 120);
        ((JPanel)helpWindow.getContentPane()).setPreferredSize(new Dimension(1000, 1000));
        
        try {
            helpWindow.setIconImage(ICON_TYPE.GENERIC.getIcon().getImage());
        } catch (Exception ex) {}

        JToolBar tbar = new JToolBar();
        tbar.setFloatable(false);

        JButton button = new JButton( "Close" );
        tbar.addSeparator();
        tbar.addSeparator();
        tbar.add( button );
        button.setMargin( new Insets( 0, 0, 0, 0 ) );
        button.addActionListener( this );
        button.setActionCommand( "Close help" );

        helpEditorPane.setEditable(false);
        try {
            helpEditorPane.setPage(helpURL);
        } catch (IOException e) {
            viewer.showStatus(e.toString());
        }

        JScrollPane editorScrollPane = new JScrollPane(helpEditorPane);
        JPanel contentPane = (JPanel)helpWindow.getContentPane();
        contentPane.setLayout(new BorderLayout());
        JPanel northP = new JPanel();
        northP.setLayout(new GridLayout(2,1));
        northP.add(tbar);
        contentPane.add (northP, BorderLayout.NORTH);
        contentPane.add (editorScrollPane, BorderLayout.CENTER);
        
        contentPane.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
        
        helpWindow.pack();
    }
    
    /** for demo purpose only */
    private void _demoShowMetaData(String uri) {
        String info = null;
        if (uri==null)
            return;

        if (uri.endsWith("bldg_footprint"))
            info = "Bldg_LIDAR\n"+
            "Source = Footprints extracted from LIDAR\n"+
            "Projection = WGS_1984_UTM_Zone_18N\n"+
            "Resolution = 1m\n"+
            "Value = Feature ID\n";
        else if (uri.endsWith("Omni_ground-50ft"))
            info = "Omni-LOS\n"+
            "Source = Derived from LIDAR DEM\n"+
            "Projection = WGS_1984_UTM_Zone_18N\n"+
            "Resolution = 1m\n"+
            "Range = 50m\n"+
            "Value = Calculated Visibility \n"+
            "9999 -  Nodata\n"+
            "00000001 - 45 deg\n"+
            "00000010 - 90 deg\n"+
            "00000100 - 135 deg\n"+
            "00001000 - 180 deg\n"+
            "00010000 - 225 deg\n"+
            "00100000 - 270 deg\n"+
            "01000000 - 315 deg\n"+
            "10000000 - 360 deg\n";
        else if (uri.endsWith("ikonos_3band-1m"))
            info = "Img-IKONOS\n"+
            "Source = Commercial Imagery\n"+
            "Projection = WGS_1984_UTM_Zone_18N\n"+
            "Resolution = 1m\n"+
            "Value = 3 Band (Red, Green, Blue)\n";
        else if (uri.endsWith("residential"))
            info = "Resd-UTP\n"+
            "Source = USATEC Urban Tactical Planner\n"+
            "Projection = WGS_1984_UTM_Zone_18N\n"+
            "Resolution = 1:12500\n"+
            "Value = Zone ID\n";

        if (info == null)
            return;

        try {
            JTextArea a = null;
            
            // find the text area that shows attribute information
            Component[] comps = (((HDFView)viewer).getContentPane()).getComponents();
            JSplitPane sp = null;
            for (int i=0; i<comps.length; i++) {
                if (comps[i] instanceof JSplitPane) {
                    sp = (JSplitPane) comps[i];
                    Component comp = sp.getBottomComponent();
                    if (comp instanceof JTabbedPane) {
                        JTabbedPane tp = (JTabbedPane) comp;
                        comp = tp.getComponentAt(1);
                        if (comp instanceof JScrollPane) {
                            comp = ((JScrollPane)comp).getViewport().getView();
                            if (comp instanceof JTextArea)
                                a = (JTextArea) comp;
                        }
                    }
                }
            }

            if (a != null) {
                a.setText(info);
                a.setCaretPosition(0);         
            }
        } catch (Throwable err) {err.printStackTrace();}
    }
    
    // for demo purpose only
    private void _demoShowImage(Object obj) 
    {
        // do this only for HDF5 Groups
        if (!(obj instanceof Group))
            return;
        
        Group g = (Group)obj;
        List objs = g.getMemberList();
        HObject o = null;
        ScalarDS img = null;
        int n = objs.size();
        for (int i=0; i<n; i++) {
            o = (HObject)objs.get(i);
            if ( (o instanceof ScalarDS) && (o.getName().equalsIgnoreCase("image")) ) {
                img = (ScalarDS)o;
                break;
            }
        }
        
        if (img==null)
            return;
        
        selectedObject = img;
        ((ScalarDS)img).setIsImage(true);
        try { 
            showDataContent(img, DISPLAY_IMAGE);
        } catch (Exception ex) {}

    }

}
