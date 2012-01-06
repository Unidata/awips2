/****************************************************************************
 * NCSA HDF                                                                 *
 * National Comptational Science Alliance                                   *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf-java/COPYING file.                                                   *
 *                                                                          *
 ****************************************************************************/
package ncsa.hdf.tools.h5ingest;

import javax.swing.*;
import java.awt.event.*;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import javax.swing.border.*;

import ncsa.hdf.object.HObject;

import java.awt.Toolkit;
import java.awt.Color;
import java.awt.Event;
import java.net.URL;
import java.text.DateFormat;
import java.util.*;
import java.io.*;

import edu.harvard.hul.ois.mets.*;
import edu.harvard.hul.ois.mets.helper.*;


/**
 * <p>Title: HDFView</p>
 * <p>Description: HDFView is the main class of this HDF visual tool.
 * It is used to layout the graphical components of the hdfview. The major GUI
 * components of the HDFView include Menubar, Toolbar, TreeView, ContentView,
 * and MessageArea.
 *
 * The HDFView is designed in such a way that it does not have direct access to
 * the HDF library. All the HDF library access is done through HDF objects.
 * Therefore, the HDFView package depends on the object package but not the
 * library package. The source code of the view package (ncsa.hdf.view) should
 * be complied with the library package (ncsa.hdf.hdflib and ncsa.hdf.hdf5lib).
 * </p>
 *
 * <p>Company: NCSA, University of Illinois at Urbana-Champaign</p>
 * @author Peter X. Cao
 * @version 1.0, 06/20/2003
 */

public class H5Ingest extends JFrame implements ActionListener 
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    private static final String VERSION = "1.0";
    private final JTextArea statusArea;
    private static Icon openIcon, closeIcon, newIcon, saveIcon, hdfIcon, textIcon, validateIcon;

    private Toolkit toolkit = null;

    /* file name for the mets document */
    private String metsFileName = null;

    private JTextArea textViewer = null;

    /** the string buffer holding the status message */
    private final StringBuffer message;

    private Mets metsObj = null;

    private MetsReader metsReader = null;

    private DateFormat df = DateFormat.getDateTimeInstance(DateFormat.SHORT , DateFormat.SHORT);

    // fields for file headers
    private JTextField metsHdr_createdate;
    private JTextField metsHdr_lastmoddate;
    private JTextField metsHdr_recordstatus;
    private JTable metsHdr_agent;

    // mets content elements
    private MetsHdr mets_MetsHdr = null;
    private DmdSec mets_DmdSec = null;
    private AmdSec mets_AmdSec = null;
    private FileSec mets_FileSec = null;
    private StructMap mets_StructMap = null;

    /* MetsHdr content elements */
    Agent mets_MetsHdr_Agent_Creator = null;
    Agent mets_MetsHdr_Agent_Editor = null;

    /**
     * Constructs the HDFView with a given root directory, where the
     * HDFView is installed, and opens the given file in the viewer.
     * <p>
     * @param root the directory where the HDFView is installed.
     * @param filename the file to open.
     */
    public H5Ingest(String fname, String xmlFileName, String h5FileName)
    {
        super("H5Ingest");
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        toolkit = Toolkit.getDefaultToolkit();

        metsFileName = fname;
        if (metsFileName == null) {
            help();
            System.exit(0);
        }

        message = new StringBuffer();

        java.io.File metsFile = new java.io.File(metsFileName);
        if (!metsFile.exists()) {
            FileOutputStream out = null;
            try {
                out = new FileOutputStream (metsFileName);
                createTemplate(out, xmlFileName, h5FileName);
            }  catch (Exception ex) {ex.printStackTrace();}
            finally { try {out.close();} catch (Exception ex) {} }
        }

        FileInputStream in = null;
        try {
            in = new FileInputStream (metsFile);
            metsObj = Mets.reader (metsReader = new MetsReader (in));
            metsObj.validate(new MetsValidator ());
        } catch (Exception e) {
            showStatus(e.toString());
            e.printStackTrace();
        } finally {
            try {in.close();} catch (Exception ex) {}
        }

        getContentMets(metsObj);
        loadIcons();

        try {
            setIconImage(((ImageIcon)hdfIcon).getImage());
        } catch (Exception ex ) {}

        setJMenuBar(createMenuBar());

        statusArea = new JTextArea();
        statusArea.setEditable(false);
        statusArea.setBackground(new java.awt.Color(240, 240, 240));
        statusArea.setLineWrap(true);

        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.addTab("METS Header", createPanelHeader());
        tabbedPane.addTab("Descriptive Metadata", new JPanel());
        tabbedPane.addTab("Administrative Metadata", new JPanel());
        tabbedPane.addTab("File Inventory", new JPanel());
        tabbedPane.addTab("Structural Map", new JPanel());
        tabbedPane.addTab("Text Viewer", textIcon, createPanelText());

        JSplitPane splitPane = new JSplitPane(
            JSplitPane.VERTICAL_SPLIT,
            tabbedPane,
            new JScrollPane(statusArea));
        splitPane.setDividerLocation(500);

        JPanel contentPane = (JPanel)getContentPane();
        contentPane.setLayout(new BorderLayout(8,8));
        contentPane.setBorder(BorderFactory.createEmptyBorder(0,5,15,5));
        contentPane.setPreferredSize(new Dimension(800, 600));
        contentPane.add(createToolBar(), BorderLayout.NORTH);
        contentPane.add(splitPane, BorderLayout.CENTER);

        Dimension d = toolkit.getScreenSize();
        int x0 = Math.max(0, (d.width-800)/3);
        int y0 = Math.max(0, (d.height-600)/3);
        this.setLocation(x0, y0);
    }

    private void getContentMets(Mets parent)
    {
        if (parent == null) {
            return;
        }

        List content = parent.getContent();
        int n = content.size();
        Object elm = null;
        for (int i =0; i<n; i++)
        {
            elm = content.get(i);
            if (elm instanceof MetsHdr) {
                mets_MetsHdr = (MetsHdr)elm;
            } else if (elm instanceof DmdSec) {
                mets_DmdSec = (DmdSec)elm;
            } else if (elm instanceof AmdSec) {
                mets_AmdSec = (AmdSec)elm;
            } else if (elm instanceof FileSec) {
                mets_FileSec = (FileSec)elm;
            } else if (elm instanceof StructMap) {
                mets_StructMap = (StructMap)elm;
            }
        }
    }

    private void getContentMetsHdr(MetsHdr parent)
    {
        if (parent == null) {
            return;
        }

        List content = parent.getContent();
        int n = content.size();
        Object elm = null;
        Agent agent = null;
        for (int i =0; i<n; i++)
        {
            elm = content.get(i);
            if (elm instanceof Agent) {
                agent = (Agent)elm;
                if (agent.getROLE()==Role.CREATOR) {
                    mets_MetsHdr_Agent_Creator = agent;
                } else if (agent.getROLE()==Role.EDITOR) {
                    mets_MetsHdr_Agent_Editor = agent;
                }
            }
        }
    }

    private void showStatus(String msg) {
        message.append(msg);
        message.append("\n");
        statusArea.setText(message.toString());
    }

    private void loadIcons()
    {
        URL u = null;
        ClassLoader classLoader = ClassLoader.getSystemClassLoader();

        // load icon images
        if (hdfIcon == null) {
            u = classLoader.getResource("ncsa/hdf/tools/h5ingest/icons/hdf.gif");

            if (u != null) {
                hdfIcon = new ImageIcon (u);
            }
        }

        if (openIcon == null) {
            u = classLoader.getResource("ncsa/hdf/tools/h5ingest/icons/fileopen.gif");
            if (u != null) {
                openIcon = new ImageIcon (u);
            }
        }

        if (closeIcon == null) {
            u = classLoader.getResource("ncsa/hdf/tools/h5ingest/icons/fileclose.gif");
            if (u != null) {
                closeIcon = new ImageIcon (u);
            }
        }

        if (newIcon == null) {
            u = classLoader.getResource("ncsa/hdf/tools/h5ingest/icons/filenew.gif");
            if (u != null) {
                newIcon = new ImageIcon (u);
            }
        }

        if (saveIcon == null) {
            u = classLoader.getResource("ncsa/hdf/tools/h5ingest/icons/filesave.gif");
            if (u != null) {
                saveIcon = new ImageIcon (u);
            }
        }

        if (textIcon == null) {
            u = classLoader.getResource("ncsa/hdf/tools/h5ingest/icons/text.gif");
            if (u != null) {
                textIcon = new ImageIcon (u);
            }
        }

        if (validateIcon == null) {
            u = classLoader.getResource("ncsa/hdf/tools/h5ingest/icons/validate.gif");
            if (u != null) {
                validateIcon = new ImageIcon (u);
            }
        }
    }

    private JMenuBar createMenuBar()
    {
        JMenuBar mbar = new JMenuBar();
        JMenuItem item;
        JMenu fileMenu = new JMenu( "File" );;

        // add file menu
        fileMenu.setMnemonic('f');
        mbar.add(fileMenu);

        item = new JMenuItem( "New");
        item.setMnemonic(KeyEvent.VK_N);
        item.addActionListener(this);
        item.setActionCommand("New file");
        item.setAccelerator( KeyStroke.getKeyStroke(KeyEvent.VK_N, Event.CTRL_MASK, true));
        fileMenu.add(item);

        item = new JMenuItem( "Open");
        item.setMnemonic(KeyEvent.VK_O);
        item.addActionListener(this);
        item.setActionCommand("Open file");
        item.setAccelerator( KeyStroke.getKeyStroke(KeyEvent.VK_O, Event.CTRL_MASK, true));
        fileMenu.add(item);

        item = new JMenuItem( "Save");
        item.setMnemonic(KeyEvent.VK_S);
        item.addActionListener(this);
        item.setActionCommand("Save file");
        item.setAccelerator( KeyStroke.getKeyStroke(KeyEvent.VK_S, Event.CTRL_MASK, true));
        fileMenu.add(item);

        item = new JMenuItem( "Save As...");
        item.addActionListener(this);
        item.setActionCommand("Save file as");
        fileMenu.add(item);

        fileMenu.addSeparator();

        item = new JMenuItem( "Validate");
        item.setMnemonic(KeyEvent.VK_V);
        item.addActionListener(this);
        item.setActionCommand("Validate file");
        fileMenu.add(item);

        fileMenu.addSeparator();

        item = new JMenuItem( "Exit");
        item.setMnemonic(KeyEvent.VK_E);
        item.addActionListener(this);
        item.setActionCommand("Exit");
        item.setAccelerator( KeyStroke.getKeyStroke(KeyEvent.VK_Q, Event.CTRL_MASK, true));
        fileMenu.add(item);

        return mbar;
    }

    private JToolBar createToolBar()
    {
        JToolBar tbar = new JToolBar();
        tbar.setFloatable(false);

        JButton button = new JButton( newIcon );
        tbar.add( button );
        button.setToolTipText( "New" );
        button.addActionListener( this );
        button.setActionCommand( "New file" );

        button = new JButton( openIcon );
        tbar.add( button );
        button.setToolTipText( "Open" );
        button.addActionListener( this );
        button.setActionCommand( "Open file" );

        button = new JButton( closeIcon );
        tbar.add( button );
        button.setToolTipText( "Close" );
        button.addActionListener( this );
        button.setActionCommand( "Close file" );

        button = new JButton( saveIcon );
        tbar.add( button );
        button.setToolTipText( "Save" );
        button.addActionListener( this );
        button.setActionCommand( "Save file" );

        button = new JButton( validateIcon );
        tbar.add( button );
        button.setToolTipText( "Validate" );
        button.addActionListener( this );
        button.setActionCommand( "Validate file" );

        return tbar;
    }

    private JPanel createPanelHeader()
    {
        JPanel panel = new JPanel();
        panel.setLayout (new BorderLayout());
        TitledBorder tborder = new TitledBorder("File Headers");
        tborder.setTitleColor(Color.darkGray);
        panel.setBorder(tborder);

        JPanel topPanel = new JPanel();
        topPanel.setLayout(new GridLayout(1, 6, 5, 5));

        topPanel.add(new JLabel ("Creating Date:",SwingConstants.RIGHT));
        topPanel.add(metsHdr_createdate = new JTextField(df.format(mets_MetsHdr.getCREATEDATE())));
        topPanel.add(new JLabel ("Last Modified Date:",SwingConstants.RIGHT));
        topPanel.add(metsHdr_lastmoddate = new JTextField(df.format(mets_MetsHdr.getLASTMODDATE())));
        topPanel.add(new JLabel ("Recording Status:", SwingConstants.RIGHT));
        topPanel.add(metsHdr_recordstatus = new JTextField(mets_MetsHdr.getRECORDSTATUS()));
        panel.add(topPanel, BorderLayout.NORTH);

        return panel;
    }

    private JPanel createPanelText()
    {
        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout());
        TitledBorder tborder = new TitledBorder(metsFileName);
        tborder.setTitleColor(Color.darkGray);
        panel.setBorder(tborder);

        textViewer = new JTextArea();
        textViewer.setEditable(false);
        textViewer.setWrapStyleWord(true);

        BufferedReader in = null;
        String strLine = null;
        try {
            in = new BufferedReader(new FileReader(metsFileName));
            while ( (strLine=in.readLine()) != null) {
                textViewer.append("\n"+strLine);
            }
        } catch (Exception ex) {ex.printStackTrace();}
        finally { try {in.close();} catch (Exception ex) {;} }

        panel.add((new JScrollPane(textViewer)), BorderLayout.CENTER);

        return panel;
    }


    private static void help()
    {
        System.out.println("\nH5Ingest version "+VERSION);

        System.out.println("\nNAME");
        System.out.println("\t H5Ingest - HDF5 METS document editor");

        System.out.println("\nUSAGE");
        System.out.println("\t H5Ingest [options] [file]");

        System.out.println("\nDESCRIPTION");
        System.out.println("\t H5Ingest is a HDF5 METS document viewer and editor for HDF5 METS document built on the METS Java Toolkit.");
        System.out.println("\t For more information on METS, visit http://www.loc.gov/standards/mets/.");
        System.out.println("\t For more information on METS Java Toolkit, visit http://hul.harvard.edu/mets/.");

        System.out.println("\n\t Most often H5Ingest is started to edit a file with the command");
        System.out.println("\t\tH5Ingest file");

        System.out.println("\n\t More generally H5Ingest is started with:");
        System.out.println("\t\tH5Ingest [options] [file]");

        System.out.println("\n\t If the file is mission, the editor will start with an empty buffer.");
        System.out.println("\t Otherwise the following options may be used to file to be edited.");

        System.out.println("\n\t file           \t The name of the file to be edited.");
        System.out.println("\t -h             \t Show this information.");
        System.out.println("\t -t [-template] \t Create a new template with command-line mode.");
        System.out.println("\t -v [-validate] \t Validate the document with command-line mode.");
        System.out.println("\t -x [-xml] xml_file \t XML file that contains the HDF5 file structure.");
        System.out.println("\t -5 [-hdf5] hdf5_file \t HDF5 file archived.");
    }

    /* create a template mets document */
    private static void createTemplate(FileOutputStream out, String xmlFileName, String h5FileName) throws Exception
    {
        String strID = "_"+System.currentTimeMillis();
        PCData unknownData = new PCData("UNKNOWN");
        edu.harvard.hul.ois.mets.File xmlFile = null;

        if (xmlFileName == null) {
            xmlFileName = "UNKNOWN_XML_Header_File";
        }

        if (h5FileName == null) {
            h5FileName = "UNKNOWN_HDF5_File";
        }

        Mets mets = new Mets();
        mets.setOBJID("_H5METS"+strID);
        mets.setLABEL("METS document file for HDF5");
        mets.setTYPE("HDF5");

        // METS header
        {
             MetsHdr metsHdr = new MetsHdr ();
             metsHdr.setCREATEDATE (new Date ());
             metsHdr.setLASTMODDATE(new Date ());
             metsHdr.setRECORDSTATUS ("Template");
             {
                 Agent agent = new Agent ();
                 agent.setROLE (Role.CREATOR);
                 agent.setTYPE (Type.OTHER);
                 agent.setOTHERTYPE ("creatingAgent");
                 {
                     Name name = new Name ();
                     name.getContent ().add (unknownData);
                     agent.getContent ().add (name);
                     Note note = new Note ();
                     note.getContent ().add (new PCData ("Automatically generated template"));
                     agent.getContent ().add (note);
                 }
                 metsHdr.getContent ().add (agent);

                 agent = new Agent ();
                 agent.setROLE (Role.EDITOR);
                 agent.setTYPE (Type.OTHER);
                 agent.setOTHERTYPE ("editingAgent");
                 {
                     Name name = new Name ();
                     name.getContent ().add (unknownData);
                     agent.getContent ().add (name);
                     Note note = new Note ();
                     note.getContent ().add (new PCData ("Automatically generated template"));
                     agent.getContent ().add (note);
                 }
                 metsHdr.getContent ().add (agent);
             }
             mets.getContent ().add (metsHdr);
        } // METS header

        // Descriptive Metadata
        {
            DmdSec dmdSec = new DmdSec ();
            dmdSec.setID ("_H5METS_DMD"+strID);
            {
                // External descriptive metadata
                MdRef mdRef = new MdRef();
                mdRef.setLOCTYPE(Loctype.URL);
                mdRef.setMIMETYPE("text/xml");
                mdRef.setMDTYPE(Mdtype.OTHER);
                mdRef.setOTHERMDTYPE("URL link");
                mdRef.setLABEL("HDF5 File Format Specification");
                mdRef.setXlinkHref("http://hdf.ncsa.uiuc.edu/HDF5/doc/H5.format.html");
                dmdSec.getContent().add(mdRef);

                // Internal descriptive metadata
                MdWrap mdWrap = new MdWrap ();
                mdWrap.setMIMETYPE ("text/xml");
                mdWrap.setMDTYPE (Mdtype.MODS);
                mdWrap.setLABEL("MODS Metadata");
                {
                    // MODS metadata
                    XmlData xmlData = new XmlData ();
                    xmlData.setSchema ("mods", "http://www.loc.gov/mods/v3/");

                    Any titleInfo = new Any ("mods:titleInfo");
                    Any title = new Any ("mods:title");
                    title.getContent().add(new PCData("METS File for HDF5 AIP"));
                    titleInfo.getContent().add(title);
                    xmlData.getContent().add(titleInfo);

                    Any name = new Any("mods:name");
                    Any namePart = new Any("mods:namePart");
                    namePart.getContent().add(unknownData);
                    name.getContent().add(namePart);
                    xmlData.getContent().add(name);

                    Any originInfo = new Any ("mods:originInfo");
                    Any publisher = new Any ("mods:publisher");
                    publisher.getContent().add(unknownData);
                    originInfo.getContent().add(publisher);
                    Any dateIssued = new Any ("mods:dateIssued");
                    dateIssued.getContent().add(unknownData);
                    originInfo.getContent().add(dateIssued);
                    xmlData.getContent().add(originInfo);

                    mdWrap.getContent ().add (xmlData);
                }
                dmdSec.getContent ().add (mdWrap);
            }
            mets.getContent ().add (dmdSec);
        }// Descriptive Metadata

        // Administrative metadata
        {
            AmdSec amdSec = new AmdSec ();
            amdSec.setID ("_H5METS_AMD"+strID);

            // Technical Metadata
            {
                TechMD techMD = new TechMD();
                techMD.setID("_H5METS_TechMD"+strID);
                MdRef mdRef = new MdRef();
                mdRef.setLOCTYPE(Loctype.URL);
                mdRef.setMIMETYPE("text/xml");
                mdRef.setMDTYPE(Mdtype.OTHER);
                mdRef.setOTHERMDTYPE("URL link");
                mdRef.setLABEL("HDF5 File Format Specification");
                mdRef.setXlinkHref("http://hdf.ncsa.uiuc.edu/HDF5/doc/H5.format.html");
                techMD.getContent().add(mdRef);
                amdSec.getContent().add(techMD);
            }

            // Rights Metadata
            {
                RightsMD rightsMD = new RightsMD();
                rightsMD.setID("_H5METS_RightsMD"+strID);
                MdWrap mdWrap = new MdWrap ();
                mdWrap.setMDTYPE (Mdtype.OTHER);
                mdWrap.setOTHERMDTYPE ("METSRights");
                mdWrap.setLABEL("METSRights Metadata");
                {
                    // METS Rights
                    XmlData xmlData = new XmlData ();
                    xmlData.setSchema ("rts", "http://www.loc.gov/standards/rights/");

                    Any rightsDeclarationMD = new Any ("rts:RightsDeclarationMD");
                    Any rightsDeclaration = new Any ("rts:RightsDeclaration");
                    rightsDeclaration.getContent().add(new PCData("Copyright status of this object is unknown"));
                    rightsDeclarationMD.getContent().add(rightsDeclaration);

                    Any context = new Any("rts:Context");
                    Any constraints = new Any("rts:Constraints");
                    Any constraintDescription = new Any("rts:ConstraintDescription");
                    constraintDescription.getContent().add(new PCData("Copyright: " +unknownData));
                    constraints.getContent().add(constraintDescription);
                    context.getContent().add(constraints);
                    rightsDeclarationMD.getContent().add(context);

                    xmlData.getContent().add(rightsDeclarationMD);
                    mdWrap.getContent ().add (xmlData);
                }
                rightsMD.getContent ().add (mdWrap);
                amdSec.getContent().add(rightsMD);
            } // Rights Metadata
            mets.getContent ().add (amdSec);
        } // Administrative metadata

        // File inventory
        {
            FileSec fileSec = new FileSec ();
            {
                FileGrp fileGrp = new FileGrp ();
                fileGrp.setID("HDF5_FILE"+strID);

                // hdf file
                edu.harvard.hul.ois.mets.File file = new edu.harvard.hul.ois.mets.File ();
                file.setID(h5FileName);
                file.setMIMETYPE("application/hdf");
                FLocat fLocat = new FLocat ();
                fLocat.setLOCTYPE(Loctype.OTHER);
                fLocat.setOTHERLOCTYPE("Local file");
                fLocat.setXlinkHref("file://"+h5FileName);
                file.getContent().add(fLocat);
                fileGrp.getContent().add(file);

                // xml file (from h5dump)
                xmlFile = new edu.harvard.hul.ois.mets.File ();
                xmlFile.setID(xmlFileName);
                xmlFile.setMIMETYPE("application/xml");
                fLocat = new FLocat ();
                fLocat.setLOCTYPE(Loctype.OTHER);
                fLocat.setOTHERLOCTYPE("Local file");
                fLocat.setXlinkHref("file://"+xmlFileName);
                xmlFile.getContent().add(fLocat);
                fileGrp.getContent().add(xmlFile);

                fileSec.getContent().add(fileGrp);
            }
            mets.getContent ().add (fileSec);
        } // File inventory

        //Structural map
        {
            StructMap structMap = new StructMap ();

            Div div = new Div ();
            div.setTYPE ("file");
            div.setLABEL("File Hierarchical Structure");
            Fptr fptr = new Fptr ();
            fptr.setFILEID(xmlFileName, xmlFile);
            div.getContent().add(fptr);

            structMap.getContent().add(div);
            mets.getContent ().add (structMap);
        } //Structural map

        mets.validate (new MetsValidator ());
        mets.write (new MetsWriter (out));
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
        }
        else if (cmd.startsWith("New file"))
        {
        }
        else if (cmd.equals("Close file"))
        {
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
        boolean isTemplate=false, isValidate=false, isHelp=false;
        String h5File=null, xmlFile=null, metsFile=null;
        boolean backup = false;
        java.io.File tmpFile = null;
        int i=0;

        for ( i = 0; i < args.length; i++)
        {
            if ("-help".equalsIgnoreCase(args[i]) || "-h".equalsIgnoreCase(args[i])) {
                isHelp = true;
            } else if ("-template".equalsIgnoreCase(args[i]) || "-t".equalsIgnoreCase(args[i])) {
                isTemplate = true;
            } else if ("-validate".equalsIgnoreCase(args[i]) || "-v".equalsIgnoreCase(args[i])) {
                isValidate = true;
            } else if ("-xml".equalsIgnoreCase(args[i]) || "-x".equalsIgnoreCase(args[i])) {
                xmlFile = args[++i];
            } else if ("-hdf5".equalsIgnoreCase(args[i]) || "-5".equalsIgnoreCase(args[i])) {
                h5File = args[++i];
            } else {
                backup = true;
            }
        }

        if (backup) {
            i--;
        }

        if (i<0) {
            isHelp = true;
        } else {
            metsFile = args[i];
        }

        if (isHelp) {
            help();
            System.exit(0);
        } else if (isValidate) {
            FileInputStream in = null;
            try {
                in = new FileInputStream (metsFile);
                Mets mets = Mets.reader (new MetsReader (in));
                mets.validate(new MetsValidator ());
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                try {in.close();} catch (Exception ex) {}
                System.exit(0);
            }
        } else if (isTemplate) {
            FileOutputStream out = null;
            try {
                out = new FileOutputStream (metsFile);
                H5Ingest.createTemplate(out, xmlFile, h5File);
            }  catch (Exception ex) {
                ex.printStackTrace();
            } finally {
                try {out.close();} catch (Exception ex) {}
                System.exit(0);
            }
        } else {
            System.out.println("\nGUI editor for HDF5 METS XML was not implmented.\n"+
                "You can use any text editor or XML editor to modify the METS file.");
            System.exit(0);
        }

        H5Ingest frame = new H5Ingest(metsFile, xmlFile, h5File);
        frame.pack();
        frame.setVisible(true);
    }
}
