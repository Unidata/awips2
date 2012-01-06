/*****************************************************************************
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of the HDF Java Products distribution.                  *
 * The full copyright notice, including terms governing use, modification,   *
 * and redistribution, is contained in the files COPYING and Copyright.html. *
 * COPYING can be found at the root of the source code distribution tree.    *
 * Or, see http://hdfgroup.org/products/hdf-java/doc/Copyright.html.         *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 ****************************************************************************/

package ncsa.hdf.view;

import ncsa.hdf.object.*;

import javax.swing.*;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.*;
import java.io.*;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.*;

import javax.swing.event.*;
import java.net.URL;
import java.lang.reflect.*;
import java.awt.Event;
import java.awt.Insets;
import java.awt.Dimension;
import java.awt.Component;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.Image;
import java.awt.Font;

/**
 * HDFView is the main class of this HDF visual tool.
 * It is used to layout the graphical components of the hdfview. The major GUI
 * components of the HDFView include Menubar, Toolbar, TreeView, ContentView,
 * and MessageArea.
 * <p>
 * The HDFView is designed in such a way that it does not have direct access to
 * the HDF library. All the HDF library access is done through HDF objects.
 * Therefore, the HDFView package depends on the object package but not the
 * library package. The source code of the view package (ncsa.hdf.view) should
 * be complied with the library package (ncsa.hdf.hdflib and ncsa.hdf.hdf5lib).
 *
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */

public class HDFView extends JFrame implements ViewManager, ActionListener, 
ChangeListener, DropTargetListener
{
  public static final long serialVersionUID = HObject.serialVersionUID;
    /** a list of tree view implementation. */
    private static List treeViews;

    /** a list of image view implementation. */
    private static List imageViews;

    /** a list of tree table implementation. */
    private static List tableViews;

    /** a list of Text view implementation. */
    private static List textViews;

    /** a list of metadata view implementation. */
    private static List metaDataViews;

    /** a list of palette view implementation. */
    private static List paletteViews;

    /** a list of help view implementation. */
    private static List helpViews;

    private static final String aboutHDFView =
        "HDF Viewer, "+ "Version "+ViewProperties.VERSION+"\n"+
        "For "+System.getProperty("os.name")+"\n\n"+
        "Copyright "+'\u00a9'+" 2006-2010 The HDF Group.\n"+
        "All rights reserved.";

    private static final String JAVA_COMPILER = "jdk 1.6.0";

    /** the directory where the HDFView is installed */
    private String rootDir;

    /** the current working directory */
    private String currentDir;

    /** the current working file */
    private String currentFile;

    /** the view properties */
    private ViewProperties props;

    /** the list of most recent files */
    //private Vector recentFiles;

    /** GUI component: the TreeView */
    private TreeView treeView;

    /** The offset when a new dataview is added into the main window. */
    private int frameOffset;

    /** GUI component: the panel which is used to display the data content */
    private final JDesktopPane contentPane;

    /** GUI component: the text area for showing status message */
    private final JTextArea statusArea;

    /** GUI component: the text area for quick attribute view */
    private final JTextArea attributeArea;

    /* create tab pane to display attributes and status information */
    private final JTabbedPane infoTabbedPane;
    
    /** the main menu bar */
    private JMenuBar menuBar;

    /** GUI component: a list of current data windwos */
    private final JMenu windowMenu;

    /** GUI component: file menu on the menubar */
    private final JMenu fileMenu;

    /** the string buffer holding the status message */
    private final StringBuffer message;

    /** the string buffer holding the meadata information */
    private final StringBuffer metadata;

    private final Toolkit toolkit;

    /** The list of GUI components related to editing */
    private final List editGUIs;

    /** The list of GUI components related to HDF5 */
    private final List h5GUIs;

    /** The list of GUI components related to HDF4 */
    private final List h4GUIs;

    /** to add and display url */
    private JComboBox urlBar;

    private UserOptionsDialog userOptionDialog;    
    
    private Constructor ctrSrbFileDialog = null;
    
    private JDialog srbFileDialog = null;
    
    /**
     * Constructs the HDFView with a given root directory, where the
     * HDFView is installed, and opens the given file in the viewer.
     * <p>
     * @param root the directory where the HDFView is installed.
     * @param flist a list of files to open.
     */
    public HDFView(String root, List flist, int width, int height, int x, int y)
    {
        super("HDFView");

        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        
        // set the module class jar files to the class path

        rootDir = root;
        currentFile = null;
        frameOffset = 0;
        userOptionDialog = null;
        ctrSrbFileDialog = null;
        toolkit = Toolkit.getDefaultToolkit();
        ViewProperties.loadIcons(rootDir);
        ViewProperties.loadExtClass();

        editGUIs = new Vector();
        h4GUIs = new Vector();
        h5GUIs = new Vector();

        // load the view properties
        props = new ViewProperties(rootDir);
       try { props.load();} catch (Exception ex){;}
    
        //recentFiles = ViewProperties.getMRF();
        currentDir = ViewProperties.getWorkDir();
        if (currentDir == null) {
            currentDir = System.getProperty("user.dir");
        }

        treeViews = ViewProperties.getTreeViewList();
        metaDataViews = ViewProperties.getMetaDataViewList();
        textViews = ViewProperties.getTextViewList();
        tableViews = ViewProperties.getTableViewList();
        imageViews = ViewProperties.getImageViewList();
        paletteViews = ViewProperties.getPaletteViewList();
        helpViews = ViewProperties.getHelpViewList();

        // initialize GUI components
        statusArea = new JTextArea();
        statusArea.setEditable(false);
        statusArea.setBackground(new java.awt.Color(240, 240, 240));
        statusArea.setLineWrap(true);
        message = new StringBuffer();
        metadata = new StringBuffer();
        showStatus("HDFView root - "+rootDir);
        showStatus("User property file - "+ViewProperties.getPropertyFile());

        attributeArea = new JTextArea();
        attributeArea.setEditable(false);
        attributeArea.setBackground(new java.awt.Color(240, 240, 240));
        attributeArea.setLineWrap(true);

        // create tab pane to display attributes and status information
        infoTabbedPane = new JTabbedPane(JTabbedPane.BOTTOM);
        infoTabbedPane.addChangeListener(this);

        contentPane = new JDesktopPane();
        windowMenu = new JMenu( "Window" );
        fileMenu = new JMenu( "File" );

        int n = treeViews.size();
        Class theClass = null;
        for (int i=0; i<n; i++)  {
          // use the first available treeview
            String className = (String)treeViews.get(i);
            // enables use of JHDF5 in JNLP (Web Start) applications, the system class loader with reflection first.
            try { theClass = Class.forName(className); }
            catch (Exception ex)
            {
                try { theClass = ViewProperties.loadExtClass().loadClass(className); }
                catch (Exception ex2) {theClass = null;}
            }
           
            if (theClass != null)
              break;
        }
        
        if (theClass != null) {
            try {
                Class[] paramClass = {Class.forName("ncsa.hdf.view.ViewManager")};
                Constructor constructor = theClass.getConstructor(paramClass);
                Object[] paramObj = {this};
                treeView = (TreeView)constructor.newInstance(paramObj);
            } catch (Exception ex) { treeView = null; }
        }
        
        // could not load user's treeview, use default treeview.
        if (treeView == null)
            treeView = new DefaultTreeView(this);

        createMainWindow(width, height, x, y);
        
        try
        {
            java.awt.Font font = null;
            String ftype = ViewProperties.getFontType();
            int fsize = ViewProperties.getFontSize();
            try { font = new java.awt.Font(ftype, java.awt.Font.PLAIN, fsize); }
            catch (Exception ex) { font = null; }
            if (font != null) {
                updateFontSize(font);
             }
        } catch (Exception ex){;}

        // need to call pack() before open any file so that 
        // all GUI components will be in place.
        pack();
        
        /* add support for drag and drop file */
        new DropTarget(this, this) ;

        int nfiles = flist.size();

        File theFile = null;
        for (int i=0; i<nfiles; i++) {
            theFile = (File)flist.get(i);
            if (theFile.isFile()) {
                currentDir = theFile.getParentFile().getAbsolutePath();
                currentFile = theFile.getAbsolutePath();

                try {
                    treeView.openFile(currentFile, FileFormat.WRITE);
                    try {
                        urlBar.removeItem(currentFile);
                        urlBar.insertItemAt(currentFile, 0);
                        urlBar.setSelectedIndex(0);
                        } catch (Exception ex2 ) {}
                } catch (Exception ex) {
                    showStatus(ex.toString());
                }
            }
            else {
                currentDir = theFile.getAbsolutePath();;
            }
        }

        if ( FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF4)== null) {
            setEnabled(h4GUIs, false);
        }

        if ( FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5) == null) {
            setEnabled(h5GUIs, false);
        }
        
    }
    
    /**
     * Set default UI fonts.
     */
    private void updateFontSize(Font font) {
        if (font == null) {
            return;
        }

        UIDefaults defaults = UIManager.getLookAndFeelDefaults();
        
        for ( Iterator i = defaults.keySet().iterator(); i.hasNext(); )
        {
            Object key = i.next();
            if (defaults.getFont( key ) != null ) {
                UIManager.put( key, new javax.swing.plaf.FontUIResource(font) );
            }
        }
        SwingUtilities.updateComponentTreeUI( this );
    }

    /**
     * Creates and lays out GUI compoents.
     * <pre>
     * ||=========||=============================||
     * ||         ||                             ||
     * ||         ||                             ||
     * || TreeView||       ContentPane           ||
     * ||         ||                             ||
     * ||=========||=============================||
     * ||            Message Area                ||
     * ||========================================||
     * </pre>
     */
    private void createMainWindow(int width, int height, int x, int y)
    {
        // create splitpane to separate treeview and the contentpane
        JScrollPane treeScroller = new JScrollPane((Component)treeView);
        JScrollPane contentScroller = new JScrollPane(contentPane);
        JSplitPane topSplitPane = new JSplitPane(
            JSplitPane.HORIZONTAL_SPLIT,
            treeScroller,
            contentScroller);
        topSplitPane.setDividerLocation(200);

        infoTabbedPane.addTab("Log Info", new JScrollPane(statusArea));
        infoTabbedPane.addTab("Metadata ", new JScrollPane(attributeArea));
        infoTabbedPane.setSelectedIndex(1);

        // create splitpane to separate message area and treeview-contentpane
        topSplitPane.setBorder(null); // refer to Java bug #4131528
        JSplitPane splitPane = new JSplitPane(
            JSplitPane.VERTICAL_SPLIT,
            topSplitPane,
            infoTabbedPane);

        // set the window size
        //float inset = 0.17f; // for UG only.
        float inset = 0.04f;
        Dimension d = toolkit.getScreenSize();

        if (height > 300) {
            d.height = height;
        } else {
            d.height = (int)((1-2*inset)*d.height);
        }
        
        if (width > 300) {
            d.width = width;
        } else {
            d.width = (int)(0.9*(double)d.height);
        }
        
        // TEST
        if (treeView.getClass().getName().startsWith("ext.erdc")) {
            topSplitPane.setDividerLocation(500);
            d.width = (int)(0.9*toolkit.getScreenSize().width);
            d.height = (int)(d.width*0.618);
        }

        splitPane.setDividerLocation(d.height-180);
        this.setLocation(x, y);

        try {
            this.setIconImage(((ImageIcon)ViewProperties.getHdfIcon()).getImage());
        } catch (Exception ex ) {}

        this.setJMenuBar(menuBar = createMenuBar());
        JToolBar toolBar = createToolBar();

        /** create URL address bar */
        urlBar = new JComboBox(ViewProperties.getMRF());
        urlBar.setMaximumRowCount(ViewProperties.MAX_RECENT_FILES);
        urlBar.setEditable(true);
        urlBar.addActionListener(this);
        urlBar.setActionCommand("Open file: from file bar");
        urlBar.setSelectedIndex(-1);

        JPanel urlPane = new JPanel();
        urlPane.setLayout(new BorderLayout());
        JButton b = new JButton("File/URL");
        urlPane.add(b, BorderLayout.WEST);
        b.addActionListener(this);
        b.setActionCommand("Popup URL list");
        urlPane.add(urlBar, BorderLayout.CENTER);
        JPanel toolPane = new JPanel();
        toolPane.setLayout(new GridLayout(2,1,0,0));
        toolPane.add(toolBar);
        toolPane.add(urlPane);

        JPanel mainPane = (JPanel)getContentPane();
        mainPane.setLayout(new BorderLayout());
        mainPane.add(toolPane, BorderLayout.NORTH);
        mainPane.add(splitPane, BorderLayout.CENTER);
        mainPane.setPreferredSize(d);
    }

    private JMenuBar createMenuBar()
    {
        JMenuBar mbar = new JMenuBar();
        JMenu menu = null;
        JMenuItem item;

        // add file menu
        fileMenu.setMnemonic('f');
        mbar.add(fileMenu);

        item = new JMenuItem( "Open");
        item.setMnemonic(KeyEvent.VK_O);
        item.addActionListener(this);
        item.setActionCommand("Open file");
        item.setAccelerator( KeyStroke.getKeyStroke(KeyEvent.VK_O, Event.CTRL_MASK, true));
        fileMenu.add(item);

        item = new JMenuItem( "Open Read-Only");
        item.setMnemonic(KeyEvent.VK_R);
        item.addActionListener(this);
        item.setActionCommand("Open file read-only");
        if (!ViewProperties.isReadOnly())
            fileMenu.add(item);

//        boolean isSrbSupported = true;
//        try { 
//            Class.forName("ncsa.hdf.srb.H5SRB");
//            Class.forName("ncsa.hdf.srb.SRBFileDialog"); 
//        } catch (Throwable ex) {isSrbSupported = false;}
//       
//        if (isSrbSupported) {
//            item = new JMenuItem( "Open from iRODS");
//            item.setMnemonic(KeyEvent.VK_S);
//            item.addActionListener(this);
//            item.setActionCommand("Open from irods");
//            fileMenu.add(item);
//        }

        fileMenu.addSeparator();

        JMenu newFileMenu = new JMenu("New");
        item = new JMenuItem( "HDF4");
        item.setActionCommand("New HDF4 file");
        item.setMnemonic(KeyEvent.VK_4);
        item.addActionListener(this);
        h4GUIs.add(item);
        newFileMenu.add(item);
        item = new JMenuItem( "HDF5");
        item.setActionCommand("New HDF5 file");
        item.setMnemonic(KeyEvent.VK_5);
        item.addActionListener(this);
        h5GUIs.add(item);
        newFileMenu.add(item);
        fileMenu.add(newFileMenu);

        fileMenu.addSeparator();

        item = new JMenuItem( "Close");
        item.setMnemonic(KeyEvent.VK_C);
        item.addActionListener(this);
        item.setActionCommand("Close file");
        fileMenu.add(item);

        item = new JMenuItem( "Close All");
        item.setMnemonic(KeyEvent.VK_A);
        item.addActionListener(this);
        item.setActionCommand("Close all file");
        fileMenu.add(item);

        fileMenu.addSeparator();

        item = new JMenuItem( "Save");
        item.setMnemonic(KeyEvent.VK_S);
        item.addActionListener(this);
        item.setActionCommand("Save current file");
        fileMenu.add(item);

        item = new JMenuItem( "Save As");
        item.setMnemonic(KeyEvent.VK_A);
        item.addActionListener(this);
        item.setActionCommand("Save current file as");
        fileMenu.add(item);

        fileMenu.addSeparator();

        item = new JMenuItem( "Exit");
        item.setMnemonic(KeyEvent.VK_E);
        item.addActionListener(this);
        item.setActionCommand("Exit");
        item.setAccelerator(
            KeyStroke.getKeyStroke(KeyEvent.VK_Q, Event.CTRL_MASK, true));
        fileMenu.add(item);

        fileMenu.addSeparator();

        // add window menu
        windowMenu.setMnemonic('w');
        mbar.add(windowMenu);

        item = new JMenuItem( "Cascade");
        item.setMnemonic(KeyEvent.VK_C);
        item.setActionCommand("Cascade all windows");
        item.addActionListener(this);
        windowMenu.add(item);

        item = new JMenuItem( "Tile");
        item.setMnemonic(KeyEvent.VK_T);
        item.setActionCommand("Tile all windows");
        item.addActionListener(this);
        windowMenu.add(item);

        windowMenu.addSeparator();

        item = new JMenuItem( "Close Window");
        item.setMnemonic(KeyEvent.VK_W);
        item.setActionCommand("Close a window");
        item.addActionListener(this);
        windowMenu.add(item);

        item = new JMenuItem( "Close All");
        item.setMnemonic(KeyEvent.VK_A);
        item.setActionCommand("Close all windows");
        item.addActionListener(this);
        windowMenu.add(item);

        windowMenu.addSeparator();

        // add tool menu
        menu = new JMenu( "Tools" );
        menu.setMnemonic('T');
        mbar.add(menu);

        JMenu imageSubmenu = new JMenu("Convert Image To");
        item = new JMenuItem( "HDF4");
        item.setActionCommand("Convert image file: Image to HDF4");
        item.addActionListener(this);
        h4GUIs.add(item);
        imageSubmenu.add(item);
        item = new JMenuItem( "HDF5");
        item.setActionCommand("Convert image file: Image to HDF5");
        item.addActionListener(this);
        h5GUIs.add(item);
        imageSubmenu.add(item);
        menu.add(imageSubmenu);

        menu.addSeparator();

        item = new JMenuItem( "User Options");
        item.setMnemonic(KeyEvent.VK_O);
        item.setActionCommand("User options");
        item.addActionListener(this);
        menu.add(item);

        menu.addSeparator();

        item = new JMenuItem( "Register File Format");
        item.setMnemonic(KeyEvent.VK_R);
        item.setActionCommand("Register file format");
        item.addActionListener(this);
        menu.add(item);

        item = new JMenuItem( "Unregister File Format");
        item.setMnemonic(KeyEvent.VK_U);
        item.setActionCommand("Unregister file format");
        item.addActionListener(this);
        menu.add(item);

        // add help menu
        menu = new JMenu("Help");
        menu.setMnemonic('H');
        mbar.add(menu);

        item = new JMenuItem( "User's Guide");
        item.setMnemonic(KeyEvent.VK_U);
        item.setActionCommand("Users guide");
        item.addActionListener(this);
        menu.add(item);

        menu.addSeparator();

        if ((helpViews != null) && (helpViews.size() > 0))
        {
            int n = helpViews.size();
            for (int i=0; i<n; i++)
            {
                HelpView theView = (HelpView)helpViews.get(i);
                item = new JMenuItem( theView.getLabel());
                item.setActionCommand(theView.getActionCommand());
                item.addActionListener(this);
                menu.add(item);
            }
            menu.addSeparator();
        }

        item = new JMenuItem( "HDF4 Library Version");
        item.setMnemonic(KeyEvent.VK_4);
        item.setActionCommand("HDF4 library");
        item.addActionListener(this);
        h4GUIs.add(item);
        menu.add(item);

        item = new JMenuItem( "HDF5 Library Version");
        item.setMnemonic(KeyEvent.VK_5);
        item.setActionCommand("HDF5 library");
        item.addActionListener(this);
        h5GUIs.add(item);
        menu.add(item);

        item = new JMenuItem( "Java Version");
        item.setMnemonic(KeyEvent.VK_5);
        item.setActionCommand("Java version");
        item.addActionListener(this);
        menu.add(item);

        menu.addSeparator();

        item = new JMenuItem( "Supported File Formats");
        item.setMnemonic(KeyEvent.VK_L);
        item.setActionCommand("File format list");
        item.addActionListener(this);
        menu.add(item);

        menu.addSeparator();

        item = new JMenuItem( "About...");
        item.setMnemonic(KeyEvent.VK_A);
        item.setActionCommand("About");
        item.addActionListener(this);
        menu.add(item);

        return mbar;
    }

    private JToolBar createToolBar()
    {
        JToolBar tbar = new JToolBar();
        tbar.setFloatable(false);

        // open file button
        JButton button = new JButton(ViewProperties.getFileopenIcon() );
        tbar.add( button );
        button.setToolTipText( "Open" );
        button.addActionListener( this );
        button.setActionCommand( "Open file" );

        // close file button
        button = new JButton(ViewProperties.getFilecloseIcon() );
        tbar.add( button );
        button.setToolTipText( "Close" );
        button.addActionListener( this );
        button.setActionCommand( "Close file" );

        tbar.addSeparator(new Dimension(20, 20));

        // help button
        button = new JButton( ViewProperties.getHelpIcon() );
        tbar.add( button );
        button.setToolTipText( "Help" );
        button.addActionListener( this );
        button.setActionCommand( "Users guide" );

        // HDF4 Library Version button
        button = new JButton( ViewProperties.getH4Icon() );
        tbar.add( button );
        button.setToolTipText( "HDF4 Library Version" );
        button.addActionListener( this );
        button.setActionCommand( "HDF4 library" );
        if (FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF4) == null) {
            button.setEnabled(false);
        }

        // HDF5 Library Version button
        button = new JButton( ViewProperties.getH5Icon() );
        tbar.add( button );
        button.setToolTipText( "HDF5 Library Version" );
        button.addActionListener( this );
        button.setActionCommand( "HDF5 library" );
        if (FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5) == null) {
            button.setEnabled(false);
        }

        return tbar;
    }

    /** Bring the window to the front.
     *  <p>
     *  @param name the name of the window to show.
     */
    private void showWindow(String name)
    {
        int n  = contentPane.getComponentCount();
        if (n<=0) {
            return;
        }

        Component comp = null;
        JInternalFrame jif = null;
        for (int i=0; i<n; i++)
        {
            comp = contentPane.getComponent(i);
            if (!(comp instanceof JInternalFrame))
                continue;

            jif = (JInternalFrame)contentPane.getComponent(i);
            
            if (jif.getName().equals(name))
            {
                 jif.toFront();
                return;
            }
        }
    }

    /** Cascade all windows. */
    private void cascadeWindow()
    {
        int y=2, x=2;
        JInternalFrame jif = null;
        Component[] clist = contentPane.getComponents();

        if ((clist == null) || (clist.length <=0) ) {
            return;
        }

        Dimension d = contentPane.getSize();
        int w = Math.max(50, d.width-100);
        int h = Math.max(50, d.height-100);

        for (int i=0; i<clist.length; i++)
        {
            jif = (JInternalFrame)clist[i];
            jif.setBounds(x, y, w, h);
            contentPane.moveToFront(jif);
            x += 20;
            y += 20;
        }
    }

    /** Tile all windows. */
    private void tileWindow()
    {
        int y=0, x=0, idx=0;
        JInternalFrame jif = null;
        Component[] clist = contentPane.getComponents();

        if ((clist == null) || (clist.length <=0) ) {
            return;
        }

        int n = clist.length;
        int cols = (int)Math.sqrt(n);
        int rows = (int)Math.ceil((double)n/(double)cols);

        Dimension d = contentPane.getSize();
        int w = d.width/cols;
        int h = d.height/rows;

        for (int i=0; i<rows; i++)
        {
            x = 0;
            for (int j=0; j<cols; j++)
            {
                idx = i*cols+j;
                if (idx >= n) {
                    return;
                }

                jif = (JInternalFrame)clist[idx];
                jif.setBounds(x, y, w, h);
                x += w;
            }
            y += h;
        }
    }

    /** Closes all windows. */
    private void closeAllWindow()
    {
        JInternalFrame jif = null;
        Component[] clist = contentPane.getComponents();

        if ((clist == null) || (clist.length <=0) ) {
            return;
        }

        for (int i=0; i<clist.length; i++)
        {
            jif = (JInternalFrame)clist[i];
            jif.dispose();
            jif = null;
        }
    }

    /** disable/enable GUI components */
    private static void setEnabled(List list, boolean b)
    {
        Component item = null;
        Iterator it = list.iterator();
        while (it.hasNext())
        {
            item = (Component)it.next();
            item.setEnabled(b);
        }
    }

    // To do: Implementing java.io.ActionListener
    public void actionPerformed(ActionEvent e)
    {
        String cmd = e.getActionCommand();

        if (cmd.equals("Exit")) {
            dispose();  // terminate the application
        }
        else if (cmd.startsWith("Open file"))
        {
            int fileAccessID = FileFormat.WRITE;
            String filename = null;
            
            if (ViewProperties.isReadOnly())
                fileAccessID = FileFormat.READ;

            if (cmd.equals("Open file: from file bar"))
            {
                filename = (String) urlBar.getSelectedItem();
                if (filename == null || filename.length()<1) {
                    return;
                }

                // local file
                if (!(filename.startsWith("http://") || filename.startsWith("ftp://")))
                {
                    File tmpFile = new File(filename);
                    
                    if (!tmpFile.exists())
                      return;
                    
                    if (tmpFile.isDirectory())
                    {
                        currentDir = filename;
                        filename = openLocalFile();
                    }
                }
            }
            else if (cmd.equals("Open file read-only"))
            {
                fileAccessID = FileFormat.READ;
                filename = openLocalFile();
            }
            else if (cmd.startsWith("Open file://"))
            {
                filename = cmd.substring(12);
            } else {
                filename = openLocalFile();
            }

            if (filename == null) {
                return;
            }

            if (filename.startsWith("http://") || filename.startsWith("ftp://") )
            {
                filename = openRemoteFile(filename);
            }

            if ((filename == null) ||
                (filename.length() < 1) ||
                filename.equals(currentFile)) {
                return;
            }

            currentFile = filename;
            try {
                urlBar.removeItem(filename);
                urlBar.insertItemAt(filename, 0);
                urlBar.setSelectedIndex(0);
            } catch (Exception ex ) {}

            try {
                treeView.openFile(filename, fileAccessID);
            } catch (Throwable ex)
            {
                try {
                    treeView.openFile(filename, FileFormat.READ);
                } catch (Throwable ex2)
                {
                    String msg = "Failed to open file "+filename+"\n"+ex2;
                    toolkit.beep();
                    JOptionPane.showMessageDialog( this, msg, getTitle(), JOptionPane.ERROR_MESSAGE);
                }
            }
        }
        else if (cmd.equals ("Open from irods"))
        {
            try { openFromSRB(); }
            catch (Exception ex)
            {
                toolkit.beep();
                JOptionPane.showMessageDialog(
                    this,
                    ex,
                    getTitle(),
                    JOptionPane.ERROR_MESSAGE);
            }
        }
        else if (cmd.startsWith("New HDF"))
        {
            String ftype = FileFormat.FILE_TYPE_HDF5;
            if (cmd.equals("New HDF4 file")) {
                ftype = FileFormat.FILE_TYPE_HDF4;
            }

            NewFileDialog dialog = new NewFileDialog(this, currentDir, ftype, treeView.getCurrentFiles());
            //dialog.show();

            if (!dialog.isFileCreated()) {
                return;
            }
            String filename = dialog.getFile();
            if (filename == null) {
                return;
            }

            try {
                treeView.openFile(filename, FileFormat.WRITE);
                currentFile = filename;
                try {
                    urlBar.removeItem(filename);
                    urlBar.insertItemAt(filename, 0);
                    urlBar.setSelectedIndex(0);
                } catch (Exception ex2 ) {}
            } catch (Exception ex)
            {
                toolkit.beep();
                JOptionPane.showMessageDialog(
                    this,
                    ex.getMessage()+"\n"+filename,
                    getTitle(),
                    JOptionPane.ERROR_MESSAGE);
            }
        }
        else if (cmd.equals("Close file"))
        {
            FileFormat theFile = treeView.getSelectedFile();
            if (theFile == null)
            {
                toolkit.beep();
                JOptionPane.showMessageDialog( this, "Select a file to close", getTitle(), JOptionPane.ERROR_MESSAGE);
                return;
            }

            // close all the data windows of this file
            JInternalFrame[] frames = contentPane.getAllFrames();
            if (frames != null)
            {
                for (int i=0; i<frames.length; i++)
                {
                    HObject obj = (HObject)(((DataView)frames[i]).getDataObject());
                    if (obj == null) {
                        continue;
                    }

                    if ( obj.getFileFormat().equals(theFile))
                    {
                        frames[i].dispose();
                        frames[i] = null;
                    }
                }
            }

            String fname = (String) urlBar.getSelectedItem();
            if (theFile.getFilePath().equals(fname))
            {
                currentFile = null;
                urlBar.setSelectedIndex(-1);
            }

            try { treeView.closeFile(theFile); }
            catch (Exception ex) {;}
            theFile = null;
            attributeArea.setText("");
            System.gc();
        }
        else if (cmd.equals("Close all file"))
        {
            closeAllWindow();
            List files = treeView.getCurrentFiles();

            while(!files.isEmpty()) {
                try { treeView.closeFile((FileFormat)files.get(0)); }
                catch (Exception ex) {}
            }
            currentFile = null;
            
            attributeArea.setText("");
        }
        else if (cmd.equals("Save current file as"))
        {
            try {
                treeView.saveFile(treeView.getSelectedFile());
            }
            catch (Exception ex) {
                toolkit.beep();
                JOptionPane.showMessageDialog(
                    this, ex, getTitle(), JOptionPane.ERROR_MESSAGE);
            }
        }
        else if (cmd.equals("Save current file"))
        {
            /* save what have been changed in memory into file */
            try {
                FileFormat file = treeView.getSelectedFile();
                List views = getDataViews();
                Object theView = null;
                TableView tableView = null;
                TextView textView = null;
                FileFormat theFile = null;
                if (views != null)
                {
                    int n = views.size();
                    for (int i=0; i<n; i++)
                    {
                        theView = views.get(i);
                        if (theView instanceof TableView)
                        {
                            tableView = (TableView)theView;
                            theFile = tableView.getDataObject().getFileFormat();
                            if (file.equals(theFile))
                            {
                                tableView.updateValueInFile();
                            }
                        }
                        else if (theView instanceof TextView)
                        {
                            textView = (TextView)theView;
                            theFile = textView.getDataObject().getFileFormat();
                            if (file.equals(theFile))
                            {
                                textView.updateValueInFile();
                            }
                        }
                    } // for (int i=0; i<n; i++)
                } // if (views != null)
            }
            catch (Exception ex) {
                toolkit.beep();
                JOptionPane.showMessageDialog(
                    this, ex, getTitle(), JOptionPane.ERROR_MESSAGE);
            }
        }
        else if (cmd.equals("Cascade all windows"))
        {
            cascadeWindow();
        }
        else if (cmd.equals("Tile all windows"))
        {
            tileWindow();
        }
        else if (cmd.equals("Close a window"))
        {
            JInternalFrame frame = contentPane.getSelectedFrame();

            if (frame != null)
            {
                frame.dispose();
            }
        }
        else if (cmd.equals("Close all windows"))
        {
            closeAllWindow();
        }
        else if (cmd.startsWith("SHOW WINDOW"))
        {
            // a window is selected to be shown at the front
            showWindow(cmd);
        }
        else if (cmd.startsWith("Convert image file:"))
        {
            String typeFrom=null, typeTo =null;

            if (cmd.equals("Convert image file: Image to HDF5"))
            {
              typeFrom = Tools.FILE_TYPE_IMAGE;
                typeTo = FileFormat.FILE_TYPE_HDF5;
            }
            else if (cmd.equals("Convert image file: Image to HDF4"))
            {
              typeFrom = Tools.FILE_TYPE_IMAGE;
                typeTo = FileFormat.FILE_TYPE_HDF4;
            } else {
                return;
            }

            FileConversionDialog dialog = new FileConversionDialog(
                this,
                typeFrom,
                typeTo,
                currentDir,
                treeView.getCurrentFiles());
            dialog.setVisible(true);

            if (dialog.isFileConverted())
            {
                String filename = dialog.getConvertedFile();
                File theFile = new File(filename);

                if (!theFile.exists() || !theFile.exists()) {
                    return;
                }

                currentDir = theFile.getParentFile().getAbsolutePath();
                currentFile = theFile.getAbsolutePath();

                try {
                    treeView.openFile(filename, FileFormat.WRITE);
                    try {
                        urlBar.removeItem(filename);
                        urlBar.insertItemAt(filename, 0);
                        urlBar.setSelectedIndex(0);
                    } catch (Exception ex2 ) {}
                } catch (Exception ex)
                {
                    showStatus(ex.toString());
                }
            }
        }
        else if (cmd.equals("User options"))
        {
          if (userOptionDialog == null) {
                userOptionDialog = new UserOptionsDialog(this, rootDir);
            }

          userOptionDialog.setVisible(true);

            if (userOptionDialog.isWorkDirChanged())
            {
                currentDir = ViewProperties.getWorkDir();
            }
            
            if (userOptionDialog.isFontChanged()) {
              Font font = null;
              try { 
                font = new Font(ViewProperties.getFontType(), Font.PLAIN, ViewProperties.getFontSize());
              } catch (Exception ex) { font = null; }
              
              if (font != null) {
                    updateFontSize(font);
              }
            }
        }
        else if (cmd.equals("Register file format")) {
            String msg = "Register a new file format by \nKEY:FILE_FORMAT:FILE_EXTENSION\n"+
                "where, KEY: the unique identifier for the file format"+
                "\n           FILE_FORMAT: the full class name of the file format"+
                "\n           FILE_EXTENSION: the file extension for the file format"+
                "\n\nFor example, "+
                "\n\t to add NetCDF, \"NetCDF:ncsa.hdf.object.nc2.NC2File:nc\"" +
                "\n\t to add FITS, \"FITS:ncsa.hdf.object.fits.FitsFile:fits\"\n\n";
             String str = (String)JOptionPane.showInputDialog(this, msg, "Register a file format", 
                   JOptionPane.PLAIN_MESSAGE, ViewProperties.getLargeHdfIcon(), 
                   null, null);
            if ((str == null) || (str.length()<1)) {
                return;
            }

            int idx1 = str.indexOf(':');
            int idx2 = str.lastIndexOf(':');

            if ((idx1<0) || (idx2<=idx1)) {
                JOptionPane.showMessageDialog(
                        this, "Failed to register "+str +"\n\nMust in the form of KEY:FILE_FORMAT:FILE_EXTENSION",
                        "Register File Format",
                        JOptionPane.ERROR_MESSAGE);
                return;
            }

            String key = str.substring(0, idx1);
            String className = str.substring(idx1+1, idx2);
            String extension = str.substring(idx2+1);

            // check is the file format has been registered or the key is taken.
            String theKey = null;
            String theClassName = null;
            Enumeration local_enum = FileFormat.getFileFormatKeys();
            while (local_enum.hasMoreElements()) {
                theKey = (String)local_enum.nextElement();
                if (theKey.endsWith(key)) {
                    JOptionPane.showMessageDialog(
                        this,
                        "Invalid key: "+key+" is taken.",
                        "Register File Format",
                        JOptionPane.ERROR_MESSAGE);
                    return;
                }

                theClassName = FileFormat.getFileFormat(theKey).getClass().getName();
                if (theClassName.endsWith(className)) {
                    JOptionPane.showMessageDialog(
                        this,
                        "The file format has already been registered: "+className,
                        "Register File Format",
                        JOptionPane.ERROR_MESSAGE);
                    return;
                }
            }

            // enables use of JHDF5 in JNLP (Web Start) applications, the system class loader with reflection first.
            Class theClass = null;
            try { theClass = Class.forName(className); }
            catch (Exception ex)
            {
                try { theClass = ViewProperties.loadExtClass().loadClass(className); }
                catch (Exception ex2) {theClass = null;}
            }
            if (theClass == null) {
                return;
            }

            try {
                Object theObject = theClass.newInstance();
                if (theObject instanceof FileFormat) {
                    FileFormat.addFileFormat(key, (FileFormat)theObject);
                }
            } catch (Throwable ex) {
                JOptionPane.showMessageDialog(
                        this, "Failed to register "+str +"\n\n"+ex,
                        "Register File Format",
                        JOptionPane.ERROR_MESSAGE);
                return;
            }

            if ((extension != null) && (extension.length()>0))
            {
               extension = extension.trim();
               String ext = ViewProperties.getFileExtension();
               ext += ", "+extension;
               ViewProperties.setFileExtension(ext);
            }
        }
        else if (cmd.equals("Unregister file format")) {
            Enumeration keys = FileFormat.getFileFormatKeys();
            ArrayList keylist = new ArrayList();

            while(keys.hasMoreElements()) {
                keylist.add(keys.nextElement());
            }

            String theKey = (String)JOptionPane.showInputDialog(this,
                "Unregister a file format", "Unregister a file format",
                JOptionPane.WARNING_MESSAGE, ViewProperties.getLargeHdfIcon(), keylist.toArray(),
                null);

            if (theKey == null) {
                return;
            }

            FileFormat.removeFileFormat(theKey);
        }
        else if (cmd.equals("Users guide"))
        {
            String ugPath = ViewProperties.getUsersGuide();

            // URL is invalid, use default path.
            if (ugPath == null || !ugPath.startsWith("http://"))
            {
                String sep = File.separator;
                File tmpFile = new File(ugPath);
                if (!(tmpFile.exists())) {
                    ugPath = rootDir+sep+"UsersGuide"+sep+"index.html";
                    tmpFile = new File(ugPath);
                    if (!(tmpFile.exists())) {
                        // use the online copy
                        ugPath = "http://www.hdfgroup.org/hdf-java-html/hdfview/UsersGuide/index.html";
                    }
                    ViewProperties.setUsersGuide(ugPath);
                }
            }

            try {
                Tools.launchBrowser(ugPath);
            } catch (Exception ex) {
                JOptionPane.showMessageDialog(
                        this,
                        ex.getMessage(),
                        "HDFView",
                        JOptionPane.ERROR_MESSAGE,
                        ViewProperties.getLargeHdfIcon());
            }                
        }
        else if (cmd.equals("HDF4 library"))
        {
            FileFormat thefile = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF4);
            if (thefile == null) {
                return;
            }

            JOptionPane.showMessageDialog(
                this,
                thefile.getLibversion(),
                "HDFView",
                JOptionPane.PLAIN_MESSAGE,
                ViewProperties.getLargeHdfIcon());
        }
        else if (cmd.equals("HDF5 library"))
        {
            FileFormat thefile = FileFormat.getFileFormat(FileFormat.FILE_TYPE_HDF5);
            if (thefile == null) {
                return;
            }

            JOptionPane.showMessageDialog(
                this,
                thefile.getLibversion(),
                "HDFView",
                JOptionPane.PLAIN_MESSAGE,
                ViewProperties.getLargeHdfIcon());
        }
        else if (cmd.equals("Java version"))
        {
            String info = "Compiled at "+JAVA_COMPILER+
                "\nRunning at "+System.getProperty("java.vm.version");
            JOptionPane.showMessageDialog(
                this,
                info,
                "HDFView",
                JOptionPane.PLAIN_MESSAGE,
                ViewProperties.getLargeHdfIcon());
        }
        else if (cmd.equals("File format list"))
        {
            Enumeration formatKeys = FileFormat.getFileFormatKeys();

            String str = "\nSupported File Formats: \n";
            while (formatKeys.hasMoreElements()) {
                str += "    " + formatKeys.nextElement() +"\n";
            }
            str += "\n";

            JOptionPane.showMessageDialog(
                this,
                str,
                "HDFView",
                JOptionPane.PLAIN_MESSAGE,
                ViewProperties.getLargeHdfIcon());
        }
        else if (cmd.equals("About"))
        {
            JOptionPane.showMessageDialog(
                this,
                aboutHDFView,
                "HDFView",
                JOptionPane.PLAIN_MESSAGE,
                ViewProperties.getLargeHdfIcon());
        }
        else if (cmd.equals("Popup URL list"))
        {
            urlBar.setPopupVisible(true);
        } else
        {
            if ((helpViews == null) || (helpViews.size() <= 0)) {
                return;
            }

            // try if one of the user help information;
            int n = helpViews.size();
            for (int i=0; i<n; i++)
            {
                HelpView theView = (HelpView)helpViews.get(i);
                if (cmd.equals(theView.getActionCommand()))
                {
                    theView.show();
                    break;
                }
            } // for (int i=0; i<n; i++)
        }
    }

    public void stateChanged(ChangeEvent e)
    {
        Object src = e.getSource();

        if (src.equals(infoTabbedPane))
        {
            int idx = infoTabbedPane.getSelectedIndex();
            if (idx == 1)
            {
                // meta info pane is selected
                attributeArea.setText("");
                showMetaData(treeView.getCurrentObject());
            }
        }
    }
    
    public void dragEnter(DropTargetDragEvent evt)
    {
    }

    public void drop(DropTargetDropEvent evt)
    {
        try {
            final Transferable tr = evt.getTransferable();

            if (tr.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
                evt.acceptDrop(DnDConstants.ACTION_COPY);

                final List fileList = (List) tr.getTransferData(DataFlavor.javaFileListFlavor);
                int n = fileList.size();
                for (int i=0; i<n; i++) {
                  File file = (File)fileList.get(i);
                  if (file.isDirectory())
                    continue;

                  String filename = file.getAbsolutePath();
                  
                    currentFile = filename;
                    try {
                        treeView.openFile(filename, FileFormat.WRITE);
                    } catch (Throwable ex)
                    {
                        try {
                            treeView.openFile(filename, FileFormat.READ);
                        } catch (Throwable ex2)
                        {
                            String msg = "Failed to open file "+filename+"\n"+ex2;
                            toolkit.beep();
                            JOptionPane.showMessageDialog( this, msg, getTitle(), JOptionPane.ERROR_MESSAGE);
                            continue;
                        }
                    }
                    
                    try {
                        urlBar.removeItem(filename);
                        urlBar.insertItemAt(filename, 0);
                        urlBar.setSelectedIndex(0);
                    } catch (Exception ex ) {}
                    
                }
                evt.getDropTargetContext().dropComplete(true);
            }
            else {
                evt.rejectDrop();
            }
        }
        catch (final IOException io) {
            evt.rejectDrop();
        }
        catch (final UnsupportedFlavorException ufe) {
            evt.rejectDrop();
        }
    }

    public void dragExit(DropTargetEvent evt)
    {
    }

    public void dropActionChanged(DropTargetDragEvent evt)
    {
    }

    public void dragOver(DropTargetDragEvent dtde)
    {
    }

    public void dispose()
    {
        try { props.save() ; }
        catch (Exception ex) {}

        try { closeAllWindow(); }
        catch (Exception ex) {}

        // close all open files
        try {
        List filelist = treeView.getCurrentFiles();
        if ((filelist != null) && (filelist.size()>0)) {
            Object[] files = filelist.toArray();
            int n = files.length;
            for (int i=0; i<n; i++) {
                try { treeView.closeFile((FileFormat)files[i]); }
                catch (Throwable ex) {continue;}
            }
        }
        } catch (Exception ex) {}

        try { super.dispose(); }
        catch (Exception ex ) {}

        System.exit(0);
    }

    /** data content is displayed, and add the dataview to the main windows */
    public void addDataView(DataView dataView) {
        if (dataView == null) {
            return;
        }

        if (!(dataView instanceof JInternalFrame))
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                "Unsupported DataView: the dataview is not a JInternalFrame.",
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return;
        }

        // check if the data content is already displayed
        JInternalFrame[] frames = contentPane.getAllFrames();
        JInternalFrame theFrame = null;
        if (frames != null)
        {
            // test if the data is already displayed
            for (int i=0; i<frames.length; i++) {
                if ( dataView.equals(frames[i]) ) {
                    theFrame = frames[i];
                    break;
                }
            }
        }

        if (theFrame != null) {
            // Data is already displayed. Just bring the dataview to the front
            theFrame.toFront();
            try {
                theFrame.setSelected(true);
            } catch (java.beans.PropertyVetoException e) {}

            return;
        }

        JInternalFrame frame = (JInternalFrame)dataView;
        contentPane.add(frame);
        HObject dataObject = dataView.getDataObject();

        String fullPath = dataObject.getPath()+dataObject.getName();
        String cmd = "SHOW WINDOW"+dataObject.getFID() + fullPath; // make the window to be uniquie: fid+path

        frame.setName(cmd); // data windows are identified by full path the file id
        frame.setMaximizable(true);
        frame.setClosable(true);
        frame.setResizable(true);

        JMenuItem item = new JMenuItem( fullPath );
        item.setActionCommand(cmd);
        item.addActionListener(this);

        if (windowMenu.getMenuComponentCount() == 6)
        {
            Component[] menuItems = windowMenu.getMenuComponents();
            for (int i=0; i<6; i++)
            {
                menuItems[i].setEnabled(true);
            }
        }

        windowMenu.add(item);

        frame.setLocation(frameOffset, frameOffset);
        if (frameOffset < 60) {
            frameOffset += 15;
        } else {
            frameOffset = 0;
        }

        Dimension d = contentPane.getSize();
        frame.setSize(d.width-60, d.height-60);

        frame.show();
    }

    /** data content is closed, and remove the dataview from the main window */
    public void removeDataView(DataView dataView) {
        if (!(dataView instanceof JInternalFrame))
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                "The dataview is not a JInternalFrame.",
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return;
        }

        JInternalFrame frame = (JInternalFrame)dataView;
        String name = frame.getName();

        int n = windowMenu.getItemCount();
        JMenuItem theItem = null;
        for (int i=6; i<n; i++)
        {
            theItem = windowMenu.getItem(i);

            if (theItem == null) {
                continue;
            }

            if (theItem.getActionCommand().equals(name))
            {
                windowMenu.remove(i);
                theItem = null;
                break;
            }
        }

        if (windowMenu.getMenuComponentCount() == 6)
        {
            Component[] menuItems = windowMenu.getMenuComponents();
            for (int i=0; i<6; i++)
            {
                menuItems[i].setEnabled(false);
            }
        }
    }

    public TreeView getTreeView() { return treeView; }
    
    /** Tree mouse event fired */
    public void mouseEventFired(java.awt.event.MouseEvent e)
    {
        HObject obj = treeView.getCurrentObject();

        if (obj == null) {
            return;
        }
       
        Object src = e.getSource();
        if ((src instanceof JComponent))
        {
            String filename = obj.getFile();
            urlBar.setSelectedItem(filename);
           
            if (infoTabbedPane.getSelectedIndex()==1)
                showMetaData(obj);
        }
    }

    private void showMetaData(HObject obj)
    {
        if (obj == null ||  currentFile == null) {
            return;
        }

        metadata.setLength(0);
        metadata.append(obj.getName());
        
        String oidStr = null;
        long[] OID = obj.getOID();
        if (OID != null)
        {
            oidStr = String.valueOf(OID[0]);
            for (int i=1; i<OID.length; i++) {
                oidStr += ", "+ OID[i];
            }
        }
        metadata.append(" (");
        metadata.append(oidStr);
        metadata.append(")");

        if (obj instanceof Group)
        {
            Group g = (Group)obj;
            metadata.append("\n    Group size = ");
            metadata.append(g.getMemberList().size());
        }
        else if (obj instanceof Dataset)
        {
            Dataset d = (Dataset)obj;
            if (d.getRank() <= 0) {
                d.init();
            }

            metadata.append("\n    ");
            if (d instanceof ScalarDS) {
                Datatype dtype = d.getDatatype();
                if (dtype != null)
                    metadata.append(dtype.getDatatypeDescription());
            } else if (d instanceof CompoundDS) {
                metadata.append("Compound/Vdata");
            }
            metadata.append(",    ");

            long dims[] = d.getDims();

           if (dims != null)
            {
                 metadata.append(dims[0]);
                for (int i=1; i<dims.length; i++)
                {
                    metadata.append(" x ");
                    metadata.append(dims[i]);
                }
            }
        } // else if (obj instanceof Dataset)

        List attrList = null;
        try { attrList = obj.getMetadata(); } catch (Exception ex) {ex.printStackTrace();}

        if (attrList == null) {
            metadata.append("\n    Number of attributes = 0");
        } else
        {
            int n = attrList.size();
            metadata.append("\n    Number of attributes = ");
            metadata.append(n);

            for (int i=0; i<n; i++)
            {
                Object attrObj = attrList.get(i);
                if (!(attrObj instanceof Attribute)) {
                    continue;
                }
                Attribute attr = (Attribute)attrObj;
                metadata.append("\n        ");
                metadata.append(attr.getName());
                metadata.append(" = ");
                metadata.append(attr.toString(","));
            }
        }

        attributeArea.setText(metadata.toString());
        attributeArea.setCaretPosition(0);
    }

    /** Returns DataView contains the specified data object.
     * It is useful to avoid redundant display of data object that is opened already.
     * @param dataObject the whose presence in the main view is to be tested.
     * @return DataView contains the specified data object, null if the data object
     * is not displayed.
     */
    public DataView getDataView(HObject dataObject) {
        if (dataObject == null) {
            return null;
        }

        // check if the data content is already displayed
        JInternalFrame[] frames = contentPane.getAllFrames();
        JInternalFrame theFrame = null;

        if (frames == null) {
            return null;
        }

        HObject obj = null;
        for (int i=0; i<frames.length; i++) {
            if ( !(frames[i] instanceof DataView) ) {
                continue;
            }

            obj = (HObject)(((DataView)frames[i]).getDataObject());
            if (dataObject.equals(obj)) {
                theFrame = frames[i];
                break; // data is already displayed
            }
        }

        return (DataView)theFrame;
    }

    /** Returns a list of all open DataViews
     */
    public List getDataViews()
    {
        // check if the data content is already displayed
        JInternalFrame[] frames = contentPane.getAllFrames();
        JInternalFrame theFrame = null;

        if ((frames == null) || (frames.length<=0)) {
            return null;
        }

        Vector views = new Vector(frames.length);
        HObject obj = null;
        for (int i=0; i<frames.length; i++)
        {
            if ( !(frames[i] instanceof DataView) ) {
                continue;
            } else {
                views.add(frames[i]);
            }
        }

        return views;
    }

    /**
     * @return a list of treeview implementations.
     */
    public static final List getListOfTreeView() {
        return treeViews;
    }

    /**
     * @return a list of imageview implementations.
     */
    public static final List getListOfImageView() {
        return imageViews;
    }

    /**
     * @return a list of tableview implementations.
     */
    public static final List getListOfTableView() {
        return tableViews;
    }

    /**
     * @return a list of textview implementations.
     */
    public static final List getListOfTextView() {
        return textViews;
    }

    /**
     * @return a list of metaDataview implementations.
     */
    public static final List getListOfMetaDataView() {
        return metaDataViews;
    }

    /**
     * @return a list of paletteview implementations.
     */
    public static final List getListOfPaletteView() {
        return paletteViews;
    }

    /**
     * Display feedback message.
     * @param msg the message to display.
     */
    public void showStatus(String msg) {
        message.append(msg);
        message.append("\n");
        statusArea.setText(message.toString());
    }

     /** choose local file */
     private String openLocalFile()
     {
         JFileChooser fchooser = new JFileChooser(currentDir);
         fchooser.setFileFilter(DefaultFileFilter.getFileFilter());
 
         int returnVal = fchooser.showOpenDialog(this);
         
         if(returnVal != JFileChooser.APPROVE_OPTION) {
            return null;
        }

        File choosedFile = fchooser.getSelectedFile();
        if (choosedFile == null) {
            return null;
        }

        if (choosedFile.isDirectory()) {
            currentDir = choosedFile.getPath();
        } else {
            currentDir = choosedFile.getParent();
        }
        
        return choosedFile.getAbsolutePath();
    }


     /** load remote file and save it to local temporary directory*/
     private String openRemoteFile(String urlStr)
     {
         String localFile = null;

         if (urlStr == null) {
            return null;
        }

         if (urlStr.startsWith("http://")) {
            localFile = urlStr.substring(7);
        } else if (urlStr.startsWith("ftp://")) {
            localFile = urlStr.substring(6);
        } else {
            return null;
        }

         localFile = localFile.replace('/', '@');
         localFile = localFile.replace('\\', '@');

         // search the local file cache
        String tmpDir = System.getProperty("java.io.tmpdir");
        
        File tmpFile = new File (tmpDir);
        if (!tmpFile.canWrite())
          tmpDir = System.getProperty("user.home");
          
        localFile =   tmpDir + localFile;

        tmpFile = new File( localFile);
        if (tmpFile.exists()) {
            return localFile;
        }

        URL url = null;

        try { url = new URL(urlStr); }
        catch (Exception ex)
        {
            url = null;
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                ex,
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return null;
        }

        BufferedInputStream in = null;
        BufferedOutputStream out = null;
        try
        {
            in = new BufferedInputStream(url.openStream());
            out = new BufferedOutputStream(new FileOutputStream(tmpFile));
        }
        catch (Exception ex)
        {
            in = null;
            toolkit.beep();
            JOptionPane.showMessageDialog(
                this,
                ex,
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            try { in.close();} catch (Exception ex2) {}
            try { out.close();} catch (Exception ex2) {}
            return null;
        }

        setCursor(java.awt.Cursor.getPredefinedCursor(java.awt.Cursor.WAIT_CURSOR));
        byte[] buff = new byte[512]; // set the default buff size to 512
        try {
            int n = 0;
            while ((n = in.read(buff)) > 0) {
                out.write(buff, 0, n);
            }
        } catch (Exception ex) {}

        try { in.close();} catch (Exception ex2) {}
        try { out.close();} catch (Exception ex2) {}

        setCursor(java.awt.Cursor.getPredefinedCursor(java.awt.Cursor.DEFAULT_CURSOR));

         return localFile;
    }

    /** open file from SRB server */
    private void openFromSRB() throws Exception
    {
        if (ctrSrbFileDialog == null) {
            Class theClass = null;
            
            try { 
                theClass = Class.forName("ncsa.hdf.srb.SRBFileDialog"); 
            } catch (Exception ex) {
                theClass = null;
                showStatus(ex.toString());
                throw (new ClassNotFoundException("Cannot find SRBFileDialog"));
            }

            try {
                Class[] paramClass = {Class.forName("java.awt.Frame")};
                ctrSrbFileDialog = theClass.getConstructor(paramClass);
            } catch (Exception ex) {
                ctrSrbFileDialog = null;
                throw (new InstantiationException("Cannot construct SRBFileDialog"));
            }
        }

        if (srbFileDialog == null) {
            try {
                Object[] paramObj = {(java.awt.Frame)this};
                srbFileDialog = (JDialog)ctrSrbFileDialog.newInstance(paramObj);
            } catch (Exception ex) {
              throw ex;
            }
        } else {
          srbFileDialog.setVisible(true);
        }
        
        currentFile = srbFileDialog.getName();
    }
    
    // For test only. Delete it from production.
    // Add plugin modules.
    private static final void loadExtModules() throws Exception
    {
        ClassLoader extClassLoader = ViewProperties.loadExtClass();
        Vector moduleListTreeView  = ViewProperties.getTreeViewList();
        Vector moduleListImageView = ViewProperties.getImageViewList();
        
        if (extClassLoader == null ||
                moduleListTreeView == null ||
                moduleListImageView == null)
            return;
        
        String[] extTreeViews = {
                "ext.erdc.TreeViewERDC",
                "ext.npoess.TreeViewNPOESS"};
        
        String[] extImageViews = {
                "ext.erdc.ImageViewERDC"};
   
        for (int i=0; i<extTreeViews.length; i++) {
            if (!moduleListTreeView.contains(extTreeViews[i])) {
                try {
                    extClassLoader.loadClass(extTreeViews[i]); // make sure the class exists
                    moduleListTreeView.add(extTreeViews[i]);   
                } catch (Exception ex) { continue; }
            }            
        }

        for (int i=0; i<extImageViews.length; i++) {
            if (!moduleListImageView.contains(extImageViews[i])) {
                try {
                    extClassLoader.loadClass(extImageViews[i]); // make sure the class exists
                    moduleListImageView.add(extImageViews[i]);  
                } catch (Exception ex) { continue; }
            }        
        }
    }

    /**
     * The starting point of this application.
     * <pre>
     * Usage: java(w)
     *        -Dncsa.hdf.hdf5lib.H5.hdf5lib="your HDF5 library path"
     *        -Dncsa.hdf.hdflib.HDFLibrary.hdflib="your HDF4 library path"
     *        -root "the directory where the HDFView is installed"
     *        [filename] "the file to open"
     * </pre>
     */
    public static void main( String args[] )
    {
        try {
            UIManager.setLookAndFeel(
                    UIManager.getCrossPlatformLookAndFeelClassName());
        } catch (Exception e) { }
        
        String rootDir = System.getProperty("user.dir");
        File tmpFile = null;
        int i=0, j=0, W=0, H=0, X=0, Y=0;
        
        for ( i = 0; i < args.length; i++)
        {
            if ("-root".equalsIgnoreCase(args[i]))
            {
                try {
                    tmpFile = new File(args[++i]);
                    j = i;

                    if (tmpFile.isDirectory())
                    {
                        rootDir = tmpFile.getPath();
                    }
                    else if (tmpFile.isFile())
                    {
                        rootDir = tmpFile.getParent();
                    }
                } catch (Exception e) {}
            } else if ("-g".equalsIgnoreCase(args[i]) || "-geometry".equalsIgnoreCase(args[i]))
            {
                // -geometry WIDTHxHEIGHT+XOFF+YOFF 
                try {
                  String geom = args[++i];
                    j = i;
                    
                  int idx = 0;
                    int idx0 = geom.lastIndexOf('-');
                    int idx1 = geom.lastIndexOf('+');
                    
                    idx = Math.max(idx0, idx1);
                    if (idx>0) {
                        Y = Integer.parseInt(geom.substring(idx+1));
                        if (idx == idx0) {
                            Y =-Y;
                        }
                        geom = geom.substring(0, idx);
                        idx0 = geom.lastIndexOf('-');
                        idx1 = geom.lastIndexOf('+');
                        idx = Math.max(idx0, idx1);
                        if (idx>0) {
                            X = Integer.parseInt(geom.substring(idx+1));
                            if (idx == idx0) {
                                X =-X;
                            }
                            geom = geom.substring(0, idx);
                        }
                    }
                    
                    idx = geom.indexOf('x');
                  if (idx > 0) {
                    W = Integer.parseInt(geom.substring(0, idx));
                    H = Integer.parseInt(geom.substring(idx+1));
                  }
                } catch (Exception e) {e.printStackTrace();}
            } else if ("-java.vm.version".equalsIgnoreCase(args[i]))
            {
                String info = "Compiled at "+JAVA_COMPILER+
                    "\nRunning at "+System.getProperty("java.vm.version");
                JOptionPane.showMessageDialog(
                new JFrame(),
                info,
                "HDFView",
                JOptionPane.PLAIN_MESSAGE,
                ViewProperties.getLargeHdfIcon());
                System.exit(0);
            }
        }

        Vector flist = new Vector();
        tmpFile = null;
        if (j>=0) {
            for (i = j; i<args.length; i++) {
                tmpFile = new File(args[i]);
                if (tmpFile.exists() && tmpFile.isFile()) {
                    flist.add(new File (tmpFile.getAbsolutePath()));
                }
            }
        }

        HDFView frame = new HDFView(rootDir, flist, W, H, X, Y);
        frame.setVisible(true);
        //try { loadExtModules(); } catch (Exception ex) {}
    }
}
