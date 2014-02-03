/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.palette.NsharpPaletteWindow
 * 
 * This java class performs the NSHARP GUI construction.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/16/2010	229			Chin Chen	Initial coding
 * 03/11/2013   972         Greg Hull   NatlCntrsEditor
 * 09/03/2013   1031        Greg Hull   try 5 times to initialize the inventory.
 * 01/08/2014               Chin Chen   Only initializing inventory when in NCP
 * 01/13/2014               Chin Chen   TTR829- when interpolation, edit graph is allowed 
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.view;

import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGridInventory;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.ui.display.NatlCntrsEditor;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextActivation;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.d2d.ui.perspectives.D2D5Pane;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

public class NsharpPaletteWindow extends ViewPart implements SelectionListener,
        DisposeListener, IPartListener {
    private MessageBox mb;

    protected Button loadBtn, unloadBtn, overlayBtn, interpBtn, dataEditBtn,
            compareStnBtn, compareSndBtn, compareTmBtn, graphEditBtn,
            graphModeBtnSkew, graphModeBtnIcing, graphModeBtnTurb,
            effBulkShearBtn, stpBtn, shipBtn, winterBtn, fireBtn, hailBtn,
            sarsBtn, cfgBtn;

    private Shell shell;

    private Label spcGplbl;

    private Composite parent;

    private Group spcGp;

    private boolean overlayIsOn = false, compareStnIsOn = false,
            compareSndIsOn = false, compareTmIsOn = false;

    protected boolean interpolateIsOn = false, editGraphOn = false;

    private static String INTP_OFF = "  Interp(off)    ";

    private static String INTP_ON = "  Interp(on)     ";

    private static String COMP_STN_OFF = "CompStn(off)";

    private static String COMP_STN_ON = "CompStn(on)  ";

    private static String COMP_TM_OFF = "CompTm(off)";

    private static String COMP_TM_ON = "CompTm(on)  ";

    private static String COMP_SND_OFF = "CompSrc(off)";

    private static String COMP_SND_ON = "CompSrc(on)  ";

    private static String OVLY_OFF = "Ovrlay2(off)  ";

    private static String OVLY_ON = "Ovrlay2(on)   ";

    protected static String EDIT_GRAPH_OFF = "EditGraph(off)";

    protected static String EDIT_GRAPH_ON = "EditGraph(on) ";

    private IWorkbenchPage page;

    private NsharpPrintHandle printHandle;

    private Font newFont;

    private boolean isEditorVisible = true;

    private static NsharpPaletteWindow instance = null;

    // one instance per perspective. To avoid cross reference.
    private static NsharpPaletteWindow ncpInstance = null;

    private static NsharpPaletteWindow d2dInstance = null;

    private int currentGraphMode = NsharpConstants.GRAPH_SKEWT;

    private String paneConfigurationName;

    private static NsharpConstants.SPCGraph leftGraph = NsharpConstants.SPCGraph.EBS;

    private static NsharpConstants.SPCGraph rightGraph = NsharpConstants.SPCGraph.STP;

    private boolean spcGpCreated = false;

    public static NsharpPaletteWindow getInstance() {
        if (VizPerspectiveListener.getCurrentPerspectiveManager() != null) {
            if (VizPerspectiveListener.getCurrentPerspectiveManager()
                    .getPerspectiveId().equals(D2D5Pane.ID_PERSPECTIVE))
                return d2dInstance;
            else if (VizPerspectiveListener.getCurrentPerspectiveManager()
                    .getPerspectiveId()
                    .equals(NmapCommon.NatlCntrsPerspectiveID))
                return ncpInstance;
        }
        return instance;
    }

    public int getCurrentGraphMode() {
        return currentGraphMode;
    }

    public void restorePaletteWindow(String paneConfigurationName,
            int currentGraphMode, boolean interpolateIsOn, boolean overlayIsOn,
            boolean compareStnIsOn, boolean compareTmIsOn, boolean editGraphOn,
            boolean compareSndIsOn) {
        // System.out.println("restorePaletteWindow "+ this.toString());
        updateSpcGraphBtn(paneConfigurationName);
        this.currentGraphMode = currentGraphMode;
        this.interpolateIsOn = interpolateIsOn;
        this.overlayIsOn = overlayIsOn;
        this.compareStnIsOn = compareStnIsOn;
        this.compareSndIsOn = compareSndIsOn;
        this.compareTmIsOn = compareTmIsOn;
        this.editGraphOn = editGraphOn;
        graphModeBtnSkew.setEnabled(true);
        graphModeBtnIcing.setEnabled(true);
        graphModeBtnTurb.setEnabled(true);
        if (currentGraphMode == NsharpConstants.GRAPH_SKEWT) {
            graphModeBtnSkew.setBackground(colorBlue);
            graphModeBtnTurb.setBackground(colorGrey);
            graphModeBtnIcing.setBackground(colorGrey);
            graphEditBtn.setEnabled(true);
            dataEditBtn.setEnabled(true);
            compareTmBtn.setEnabled(true);
            compareSndBtn.setEnabled(true);
            compareStnBtn.setEnabled(true);
            overlayBtn.setEnabled(true);
            interpBtn.setEnabled(true);
            overlayBtn.setText(OVLY_OFF);
            interpBtn.setText(INTP_OFF);
            graphEditBtn.setText(EDIT_GRAPH_OFF);
            compareTmBtn.setText(COMP_TM_OFF);
            compareSndBtn.setText(COMP_SND_OFF);
            compareStnBtn.setText(COMP_STN_OFF);
            if (interpolateIsOn) {
                //TTR829 graphEditBtn.setEnabled(false);
                //dataEditBtn.setEnabled(false);
                compareTmBtn.setEnabled(false);
                compareSndBtn.setEnabled(false);
                compareStnBtn.setEnabled(false);
                overlayBtn.setEnabled(false);
                interpBtn.setText(INTP_ON);
            } else if (overlayIsOn) {
                overlayBtn.setText(OVLY_ON);
                graphEditBtn.setEnabled(false);
                dataEditBtn.setEnabled(false);
                compareTmBtn.setEnabled(false);
                compareSndBtn.setEnabled(false);
                compareStnBtn.setEnabled(false);
                interpBtn.setEnabled(false);
                graphModeBtnIcing.setEnabled(false);
                graphModeBtnTurb.setEnabled(false);
            } else if (compareStnIsOn) {
                compareStnBtn.setText(COMP_STN_ON);
                graphEditBtn.setEnabled(false);
                dataEditBtn.setEnabled(false);
                compareTmBtn.setEnabled(false);
                compareSndBtn.setEnabled(false);
                overlayBtn.setEnabled(false);
                interpBtn.setEnabled(false);
                graphModeBtnIcing.setEnabled(false);
                graphModeBtnTurb.setEnabled(false);
            } else if (compareSndIsOn) {
                compareSndBtn.setText(COMP_SND_ON);
                graphEditBtn.setEnabled(false);
                dataEditBtn.setEnabled(false);
                compareTmBtn.setEnabled(false);
                compareStnBtn.setEnabled(false);
                overlayBtn.setEnabled(false);
                interpBtn.setEnabled(false);
                graphModeBtnIcing.setEnabled(false);
                graphModeBtnTurb.setEnabled(false);
            } else if (compareTmIsOn) {
                compareTmBtn.setText(COMP_TM_ON);
                compareSndBtn.setEnabled(false);
                graphEditBtn.setEnabled(false);
                dataEditBtn.setEnabled(false);
                compareStnBtn.setEnabled(false);
                overlayBtn.setEnabled(false);
                interpBtn.setEnabled(false);
                graphModeBtnIcing.setEnabled(false);
                graphModeBtnTurb.setEnabled(false);
            } else if (editGraphOn) {
                graphEditBtn.setText(EDIT_GRAPH_ON);
                dataEditBtn.setEnabled(false);
                compareStnBtn.setEnabled(false);
                compareTmBtn.setEnabled(false);
                compareSndBtn.setEnabled(false);
                overlayBtn.setEnabled(false);
                interpBtn.setEnabled(false);
                graphModeBtnIcing.setEnabled(false);
                graphModeBtnTurb.setEnabled(false);
            }

        } else if (currentGraphMode == NsharpConstants.GRAPH_TURB) {
            graphModeBtnTurb.setBackground(colorBlue);
            graphModeBtnSkew.setBackground(colorGrey);
            graphModeBtnIcing.setBackground(colorGrey);
            graphEditBtn.setEnabled(false);
            dataEditBtn.setEnabled(false);
            compareTmBtn.setEnabled(false);
            compareSndBtn.setEnabled(false);
            compareStnBtn.setEnabled(false);
            overlayBtn.setEnabled(false);
            if (interpolateIsOn)
                interpBtn.setText(INTP_ON);
            else
                interpBtn.setText(INTP_OFF);

        } else if (currentGraphMode == NsharpConstants.GRAPH_ICING) {
            graphModeBtnIcing.setBackground(colorBlue);
            graphModeBtnSkew.setBackground(colorGrey);
            graphModeBtnTurb.setBackground(colorGrey);
            graphEditBtn.setEnabled(false);
            dataEditBtn.setEnabled(false);
            compareTmBtn.setEnabled(false);
            compareSndBtn.setEnabled(false);
            compareStnBtn.setEnabled(false);
            overlayBtn.setEnabled(false);
            if (interpolateIsOn)
                interpBtn.setText(INTP_ON);
            else
                interpBtn.setText(INTP_OFF);
        }
    }

    public static NsharpConstants.SPCGraph getLeftGraph() {
        return leftGraph;
    }

    public static NsharpConstants.SPCGraph getRightGraph() {
        return rightGraph;
    }

    private Color colorGrey = new Color(Display.getDefault(), 211, 211, 211);

    private Color colorBlue = new Color(Display.getDefault(), 135, 206, 235);

    // private Color colorButtonOriginalBg; // will be initialized later
    private void updateSPCGraphs() {
        if (leftGraph == NsharpConstants.SPCGraph.EBS
                || rightGraph == NsharpConstants.SPCGraph.EBS) {
            effBulkShearBtn.setBackground(colorBlue);
        } else {
            effBulkShearBtn.setBackground(colorGrey);
        }
        if (leftGraph == NsharpConstants.SPCGraph.STP
                || rightGraph == NsharpConstants.SPCGraph.STP) {
            stpBtn.setBackground(colorBlue);
        } else {
            stpBtn.setBackground(colorGrey);
        }
        if (leftGraph == NsharpConstants.SPCGraph.SHIP
                || rightGraph == NsharpConstants.SPCGraph.SHIP) {
            shipBtn.setBackground(colorBlue);
        } else {
            shipBtn.setBackground(colorGrey);
        }
        if (leftGraph == NsharpConstants.SPCGraph.WINTER
                || rightGraph == NsharpConstants.SPCGraph.WINTER) {
            winterBtn.setBackground(colorBlue);
        } else {
            winterBtn.setBackground(colorGrey);
        }
        if (leftGraph == NsharpConstants.SPCGraph.FIRE
                || rightGraph == NsharpConstants.SPCGraph.FIRE) {
            fireBtn.setBackground(colorBlue);
        } else {
            fireBtn.setBackground(colorGrey);
        }
        if (leftGraph == NsharpConstants.SPCGraph.HAIL
                || rightGraph == NsharpConstants.SPCGraph.HAIL) {
            hailBtn.setBackground(colorBlue);
        } else {
            hailBtn.setBackground(colorGrey);
        }
        if (leftGraph == NsharpConstants.SPCGraph.SARS
                || rightGraph == NsharpConstants.SPCGraph.SARS) {
            sarsBtn.setBackground(colorBlue);
        } else {
            sarsBtn.setBackground(colorGrey);
        }
        NsharpResourceHandler rsc = getRscHandler();
        if (rsc != null && rsc.getSpcGraphsPaneRsc() != null) {
            rsc.getSpcGraphsPaneRsc().setGraphs(leftGraph, rightGraph);
        }

    }

    public void setAndOpenMb(String msg) {
        if (mb != null) {
            mb.setMessage(msg);
            try {
                mb.open();
            } catch (Exception e) {

                // e.printStackTrace();
            }
        }
    }

    public NsharpPaletteWindow() {
        super();
        instance = this;
        boolean imD2d=false; //fixMark:NcInventory
        if (VizPerspectiveListener.getCurrentPerspectiveManager() != null) {
            if (VizPerspectiveListener.getCurrentPerspectiveManager()
                    .getPerspectiveId().equals(D2D5Pane.ID_PERSPECTIVE)){
                d2dInstance = this;
                imD2d = true;//fixMark:NcInventory
            }
            else if (VizPerspectiveListener.getCurrentPerspectiveManager()
                    .getPerspectiveId()
                    .equals(NmapCommon.NatlCntrsPerspectiveID))
                ncpInstance = this;
        }
        // System.out.println("view NsharpPaletteWindow constructed!! "+
        // this.toString());
        printHandle = NsharpPrintHandle.getPrintHandle();
        shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

        mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);
        mb.setMessage("Data is not loaded yet!");
        NsharpConfigManager configMgr = NsharpConfigManager.getInstance();
        NsharpConfigStore configStore = configMgr
                .retrieveNsharpConfigStoreFromFs();
        NsharpGraphProperty graphConfigProperty = configStore
                .getGraphProperty();
        paneConfigurationName = graphConfigProperty.getPaneConfigurationName();
        if(!imD2d){//fixMark:NcInventory
        	for( int a=1 ; a<=5 ; a++ ) {
        		if( NsharpGridInventory.getInstance().isInitialized()  ) {
        			break;
        		}

        		try {
        			NsharpGridInventory.getInstance().initialize();			
        		} 
        		catch (VizException e) {		
        			// TODO : could call createInventory() here but for now this will be considered
        			// an error since the grid inventory should/must be on the server.
        			System.out.println("NsharpGridInventory initialize attempt #"+a+" failed");

        			try { Thread.sleep(a*500); } catch (InterruptedException e1) { }
        		}
        	}

        	if( !NsharpGridInventory.getInstance().isInitialized() ) {
        		// TODO : change to a confirm to create an inventory.
        		MessageDialog errDlg = new MessageDialog(
        				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), "Error", null,
        				"Unable to find an Inventory to support Grid Model times. Please wait while one"+
        						" is created.", MessageDialog.ERROR,
        						new String[] { "OK" }, 0);
        		errDlg.open();

        		try {
        			NsharpGridInventory.createInventory();
        		} 
        		catch (VizException e) {			
        			errDlg = new MessageDialog(
        					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), "Error", null,
        					"Error creating Inventory to support Grid Model times.", MessageDialog.ERROR,
        					new String[] { "OK" }, 0);
        			errDlg.open();
        		}
        	}
        }
    }

    /**
     * Invoked by the workbench to initialize this View.
     */
    public void init(IViewSite site) {
        // System.out.println("NsharpPaletteWindow inited!!"+ this.toString());
        try {

            super.init(site);

        } catch (PartInitException pie) {

            pie.printStackTrace();

        }

        page = site.getPage();
        page.addPartListener(this);

        NsharpMapResource.registerMouseHandler();
        // Chin : to fix Ticket#11034::::
        // get several control information back from SkewT resource, in the case
        // that
        // NsharpPaletteWindow view was disposed and re-constructed while SkewT
        // resource is still alive.
        // This case applied to D2D implementation.
        NsharpResourceHandler rsc = getRscHandler();
        if (rsc != null) {
            interpolateIsOn = rsc.isInterpolateIsOn();
            overlayIsOn = rsc.isOverlayIsOn();
            compareStnIsOn = rsc.isCompareStnIsOn();
            editGraphOn = rsc.isEditGraphOn();
            compareSndIsOn = rsc.isCompareSndIsOn();
            compareTmIsOn = rsc.isCompareTmIsOn();
        }

    }

    /**
     * Disposes resource. invoked by the workbench
     */
    public void dispose() {
        // System.out.println("NsharpPaletteWindow "+this.toString()+" dispose() called!! isEditorVisible="+
        // isEditorVisible);
        if (!isEditorVisible) {
            NsharpMapResource.unregisterMouseHandler();
            instance = null;
            d2dInstance = null;
            ncpInstance = null;
            return;
        } else {
            super.dispose();
            currentGraphMode = NsharpConstants.GRAPH_SKEWT;
            isEditorVisible = false;
            NatlCntrsEditor editor = NsharpMapResource.getMapEditor();
            if (editor != null) {
                for (IRenderableDisplay display : UiUtil
                        .getDisplaysFromContainer(editor)) {
                    // NsharpMapResource mrsc =
                    // (NsharpMapResource)NcDisplayMngr.findAllResources(
                    // NsharpMapResource.class, editor );
                    // if( mrsc != null ) {
                    // mrsc.unload();
                    // display.getDescriptor().getResourceList().removePreRemoveListener(mrsc);
                    // }
                    // System.out.println("display " + display.toString());
                    for (ResourcePair rp : display.getDescriptor()
                            .getResourceList()) {
                        if (rp.getResource() instanceof NsharpMapResource) {
                            NsharpMapResource rsc = (NsharpMapResource) rp
                                    .getResource();
                            rsc.unload();
                            display.getDescriptor().getResourceList()
                                    .removePreRemoveListener(rsc);

                        }
                    }
                }
            }
            if (newFont != null) {
                newFont.dispose();
                newFont = null;
            } /*
               * remove the workbench part listener
               */
            page.removePartListener(this);

            try {
                if (NsharpLoadDialog.getAccess() != null) {
                    NsharpLoadDialog.getAccess().close();
                }
            } catch (Exception e) {

            }
            instance = null;
            d2dInstance = null;
            ncpInstance = null;
        }
    }

    protected boolean checkLoadedData() {
        NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
        if (editor == null) {
            mb.open();
            return false;
        }
        NsharpResourceHandler rsc = editor.getRscHandler();
        if (rsc == null) {
            mb.open();
            return false;
        }
        return true;
    }

    protected NsharpResourceHandler getRscHandler() {
        NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
        if (editor == null) {

            return null;
        }
        NsharpResourceHandler rsc = editor.getRscHandler();
        if (rsc == null)
            return null;

        return rsc;
    }

    public void createDataControlGp(Composite parent) {
        this.parent = parent;
        Group textModeGp = new Group(parent, SWT.SHADOW_OUT);
        textModeGp.setLayout(new RowLayout(SWT.HORIZONTAL));
        textModeGp.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        Font font = textModeGp.getFont();
        FontData[] fontData = font.getFontData();
        for (int i = 0; i < fontData.length; i++) {
            fontData[i].setHeight(7);
            // fontData[i].setName("courier");
        }
        newFont = new Font(font.getDevice(), fontData);

        loadBtn = new Button(textModeGp, SWT.PUSH);
        loadBtn.setFont(newFont);
        loadBtn.setText("      Load        ");
        loadBtn.setEnabled(true);
        // loadBtn.setSize(btnWidth,pushbtnHeight);
        loadBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();

                NsharpLoadDialog loadDia = NsharpLoadDialog.getInstance(shell);

                if (loadDia != null) {
                    // System.out.println("Load Button is calling dialog open()");
                    loadDia.open();
                }
            }
        });

        unloadBtn = new Button(textModeGp, SWT.PUSH);
        unloadBtn.setFont(newFont);
        unloadBtn.setText("     UnLoad     ");
        unloadBtn.setEnabled(true);
        // loadBtn.setSize(btnWidth,pushbtnHeight);
        unloadBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (checkLoadedData()) {
                    Shell shell = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getShell();

                    NsharpUnloadDialog unloadDia = NsharpUnloadDialog
                            .getInstance(shell);

                    if (unloadDia != null) {
                        // System.out.println("Load Button is calling dialog open()");
                        unloadDia.open();

                    }
                }
            }
        });
        // Push buttons for SAVE
        Button saveBtn = new Button(textModeGp, SWT.PUSH);
        saveBtn.setFont(newFont);
        saveBtn.setText("      Save        ");
        saveBtn.setEnabled(true);
        // saveBtn.setSize(btnWidth,pushbtnHeight);
        saveBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {

                if (checkLoadedData()) {
                    // Action to save text report
                    NsharpSaveHandle.saveFile(shell);
                }
            }

        });

        // Push buttons for CONFIGURE
        cfgBtn = new Button(textModeGp, SWT.PUSH);
        cfgBtn.setFont(newFont);
        cfgBtn.setText("  Configure    ");
        cfgBtn.setEnabled(true);
        // cfgBtn.setSize(btnWidth,pushbtnHeight);
        cfgBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                // CHin, new develop if(checkLoadedData()) {
                // NsharpParametersSelectionConfigDialog dia =
                // NsharpParametersSelectionConfigDialog.getInstance(shell);
                NsharpConfigDialog dia = NsharpConfigDialog.getInstance(shell);
                if (dia != null) {
                    dia.open();
                }
                // }

            }
        });

        Button resetBtn = new Button(textModeGp, SWT.PUSH);
        resetBtn.setFont(newFont);
        resetBtn.setText("     Reset        ");
        resetBtn.setEnabled(true);
        resetBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                // RESET should turn off everything...
                shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getShell();
                cfgBtn.setEnabled(true);
                overlayIsOn = false;
                overlayBtn.setText(OVLY_OFF);
                overlayBtn.setEnabled(true);
                compareStnIsOn = false;
                compareStnBtn.setText(COMP_STN_OFF);
                compareStnBtn.setEnabled(true);
                compareTmIsOn = false;
                compareTmBtn.setText(COMP_TM_OFF);
                compareTmBtn.setEnabled(true);
                compareSndIsOn = false;
                compareSndBtn.setText(COMP_SND_OFF);
                compareSndBtn.setEnabled(true);
                interpolateIsOn = false;
                interpBtn.setText(INTP_OFF);
                interpBtn.setEnabled(true);
                editGraphOn = false;
                graphEditBtn.setEnabled(true);
                dataEditBtn.setEnabled(true);
                graphModeBtnIcing.setEnabled(true);
                graphModeBtnTurb.setEnabled(true);
                graphEditBtn.setText(EDIT_GRAPH_OFF);
                currentGraphMode = NsharpConstants.GRAPH_SKEWT;
                NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
                if (editor != null && editor.getRscHandler() != null) {
                    // note: resetRsc will reset currentPage, overlay, compare,
                    // interpolate flag in Resource
                    editor.getRscHandler().resetRsc();
                    // issue#18 - issue list
                    if (editor.getRscHandler().getDataPaneRsc() != null) {
                        editor.getRscHandler().getDataPaneRsc()
                                .resetCurrentParcel();
                    }
                    NsharpParcelDialog parcelDia = NsharpParcelDialog
                            .getInstance(shell);
                    if (parcelDia != null) {
                        parcelDia.reset();
                    }
                    // editor.getNsharpSkewTDescriptor().getSkewtResource().resetRsc();//
                    // need to called it twice to make refresh worked...dont
                    // know why
                    // know that current editor is NsharpSkewT editor, refresh
                    // it.
                    editor.refresh();
                    NsharpShowTextDialog textarea = NsharpShowTextDialog
                            .getAccess();
                    if (textarea != null) {
                        textarea.refreshTextData();
                    }
                }
                if (NsharpParcelDialog.getAccess() != null) {
                    NsharpParcelDialog.getAccess().resetUserDefParcel();
                }
            }
        });
        Button resetGfBtn = new Button(textModeGp, SWT.PUSH);
        resetGfBtn.setFont(newFont);
        resetGfBtn.setText("Reset Display");
        resetGfBtn.setEnabled(true);
        resetGfBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
                editor.resetGraph();
            }
        });

        Button parcelBtn = new Button(textModeGp, SWT.PUSH);
        parcelBtn.setFont(newFont);
        parcelBtn.setText("     Parcel       ");
        parcelBtn.setEnabled(true);
        // parcelBtn.setSize(btnWidth,pushbtnHeight);
        parcelBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                if (checkLoadedData()) {
                    NsharpParcelDialog parcelDia = NsharpParcelDialog
                            .getInstance(shell);

                    if (parcelDia != null) {
                        // System.out.println("calling parcel dialog open()");
                        parcelDia.open();

                    }
                }
            }
        });

        // Push buttons for NEXT PAGE info
        Button nextpageBtn = new Button(textModeGp, SWT.PUSH);
        nextpageBtn.setFont(newFont);
        nextpageBtn.setText("  Next Data    ");
        nextpageBtn.setEnabled(true);
        nextpageBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getShell();
                if (checkLoadedData()) {
                    NsharpResourceHandler rsc = getRscHandler();
                    if (rsc != null) {
                        rsc.setNextTextChapter();
                        rsc.refreshPane();
                    }
                }
            }
        });

        // Push buttons for NEXT INSET PAGE info
        Button nextInsetBtn = new Button(textModeGp, SWT.PUSH);
        nextInsetBtn.setFont(newFont);
        nextInsetBtn.setText("  Next Inset    ");
        nextInsetBtn.setEnabled(true);
        nextInsetBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getShell();
                if (checkLoadedData()) {
                    NsharpResourceHandler rsc = getRscHandler();
                    if (rsc != null) {
                        rsc.setNextInsetPage();
                        rsc.refreshPane();
                    }
                }
            }
        });

        // Push buttons for interpolate
        interpBtn = new Button(textModeGp, SWT.PUSH);
        interpBtn.setFont(newFont);
        interpBtn.setEnabled(true);
        if (interpolateIsOn) {
            interpBtn.setText(INTP_ON);
        } else {
            interpBtn.setText(INTP_OFF);
        }
        interpBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getShell();
                if (checkLoadedData()) {
                    if (interpolateIsOn == false) {
                        interpolateIsOn = true;
                        interpBtn.setText(INTP_ON);
                        //TTR829 graphEditBtn.setEnabled(false);
                        //dataEditBtn.setEnabled(false);
                        compareTmBtn.setEnabled(false);
                        compareSndBtn.setEnabled(false);
                        compareStnBtn.setEnabled(false);
                        overlayBtn.setEnabled(false);
                    } else {
                        interpolateIsOn = false;
                        interpBtn.setText(INTP_OFF);
                        if (currentGraphMode == NsharpConstants.GRAPH_SKEWT && editGraphOn == false) {
                            //TTR829 graphEditBtn.setEnabled(true);
                            dataEditBtn.setEnabled(true);
                            compareTmBtn.setEnabled(true);
                            compareSndBtn.setEnabled(true);
                            compareStnBtn.setEnabled(true);
                            overlayBtn.setEnabled(true);
                        }
                    }
                    // note:call resetInfoOnInterpolate() and pass interpolate
                    // flag to Resource
                    NsharpEditor editor = NsharpEditor.getActiveNsharpEditor();
                    if (editor != null) {
                        try {
                            editor.getRscHandler().resetInfoOnInterpolate(
                                    interpolateIsOn);
                        } catch (CloneNotSupportedException e) {
                            e.printStackTrace();
                        }
                        // know that current editor is NsharpSkewT editor,
                        // refresh it.
                        editor.refresh();

                        NsharpShowTextDialog textarea = NsharpShowTextDialog
                                .getAccess();
                        if (textarea != null) {
                            textarea.refreshTextData();
                        }
                    }
                }
            }
        });

        NsharpResourceHandler rscHandler = getRscHandler();

        // Push buttons for OVERLAY info
        overlayBtn = new Button(textModeGp, SWT.PUSH);
        overlayBtn.setFont(newFont);
        if (overlayIsOn) {
            overlayBtn.setText(OVLY_ON);
            overlayBtn.setEnabled(true);
        } else {
            overlayBtn.setText(OVLY_OFF);
            // comparison and overlay is mutual exclusive
            if ((rscHandler != null)
                    && (rscHandler.isCompareStnIsOn()
                            || rscHandler.isCompareTmIsOn() || rscHandler
                                .isCompareSndIsOn()))
                overlayBtn.setEnabled(false);
            else
                overlayBtn.setEnabled(true);
        }
        overlayBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getShell();
                if (overlayIsOn == false) {

                    overlayIsOn = true;
                    overlayBtn.setText(OVLY_ON);
                    compareStnBtn.setEnabled(false);
                    compareTmBtn.setEnabled(false);
                    compareSndBtn.setEnabled(false);
                    graphEditBtn.setEnabled(false);
                    dataEditBtn.setEnabled(false);
                    graphModeBtnTurb.setEnabled(false);
                    graphModeBtnIcing.setEnabled(false);
                    interpBtn.setEnabled(false);
                    cfgBtn.setEnabled(false);
                } else {
                    overlayIsOn = false;
                    overlayBtn.setText(OVLY_OFF);
                    compareStnBtn.setEnabled(true);
                    compareTmBtn.setEnabled(true);
                    compareSndBtn.setEnabled(true);
                    graphEditBtn.setEnabled(true);
                    dataEditBtn.setEnabled(true);
                    graphModeBtnTurb.setEnabled(true);
                    graphModeBtnIcing.setEnabled(true);
                    interpBtn.setEnabled(true);
                    cfgBtn.setEnabled(true);
                }
                NsharpResourceHandler rsc = getRscHandler();
                if (rsc != null) {
                    rsc.setOverlayIsOn(overlayIsOn);
                    rsc.refreshPane();
                }
            }
        });
        // Push buttons for CompByStn info
        compareStnBtn = new Button(textModeGp, SWT.PUSH);
        compareStnBtn.setFont(newFont);
        if (compareStnIsOn) {
            compareStnBtn.setText(COMP_STN_ON);
            compareStnBtn.setEnabled(true);
        } else {
            // comparison and overlay is mutual exclusive
            compareStnBtn.setText(COMP_STN_OFF);
            if ((rscHandler != null)
                    && (rscHandler.isOverlayIsOn()
                            || rscHandler.isCompareTmIsOn() || rscHandler
                                .isCompareSndIsOn()))
                compareStnBtn.setEnabled(false);
            else
                compareStnBtn.setEnabled(true);
        }
        compareStnBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getShell();
                if (compareStnIsOn == false) {

                    compareStnIsOn = true;
                    compareStnBtn.setText(COMP_STN_ON);
                    overlayBtn.setEnabled(false);
                    compareTmBtn.setEnabled(false);
                    compareSndBtn.setEnabled(false);
                    graphEditBtn.setEnabled(false);
                    dataEditBtn.setEnabled(false);
                    graphModeBtnTurb.setEnabled(false);
                    graphModeBtnIcing.setEnabled(false);
                    interpBtn.setEnabled(false);
                    cfgBtn.setEnabled(false);
                } else {
                    compareStnIsOn = false;
                    compareStnBtn.setText(COMP_STN_OFF);
                    overlayBtn.setEnabled(true);
                    compareTmBtn.setEnabled(true);
                    compareSndBtn.setEnabled(true);
                    graphEditBtn.setEnabled(true);
                    dataEditBtn.setEnabled(true);
                    graphModeBtnTurb.setEnabled(true);
                    graphModeBtnIcing.setEnabled(true);
                    interpBtn.setEnabled(true);
                    cfgBtn.setEnabled(true);
                }
                NsharpResourceHandler rsc = getRscHandler();
                if (rsc != null) {
                    rsc.setCompareStnIsOn(compareStnIsOn);
                    rsc.refreshPane();
                }

            }
        });
        // Push buttons for CompByTm info
        compareTmBtn = new Button(textModeGp, SWT.PUSH);
        compareTmBtn.setFont(newFont);
        if (compareTmIsOn) {
            compareTmBtn.setText(COMP_TM_ON);
            compareTmBtn.setEnabled(true);
        } else {
            // comparison and overlay is mutual exclusive
            compareTmBtn.setText(COMP_TM_OFF);
            if ((rscHandler != null)
                    && (rscHandler.isOverlayIsOn()
                            || rscHandler.isCompareStnIsOn() || rscHandler
                                .isCompareSndIsOn()))
                compareTmBtn.setEnabled(false);
            else
                compareTmBtn.setEnabled(true);
        }
        compareTmBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getShell();
                if (compareTmIsOn == false) {

                    compareTmIsOn = true;
                    compareTmBtn.setText(COMP_TM_ON);
                    compareSndBtn.setEnabled(false);
                    overlayBtn.setEnabled(false);
                    compareStnBtn.setEnabled(false);
                    graphEditBtn.setEnabled(false);
                    dataEditBtn.setEnabled(false);
                    graphModeBtnTurb.setEnabled(false);
                    graphModeBtnIcing.setEnabled(false);
                    interpBtn.setEnabled(false);
                    cfgBtn.setEnabled(false);
                } else {
                    compareTmIsOn = false;
                    compareTmBtn.setText(COMP_TM_OFF);
                    compareSndBtn.setEnabled(true);
                    overlayBtn.setEnabled(true);
                    compareStnBtn.setEnabled(true);
                    graphEditBtn.setEnabled(true);
                    dataEditBtn.setEnabled(true);
                    graphModeBtnTurb.setEnabled(true);
                    graphModeBtnIcing.setEnabled(true);
                    interpBtn.setEnabled(true);
                    cfgBtn.setEnabled(true);
                }
                NsharpResourceHandler rsc = getRscHandler();
                if (rsc != null) {
                    rsc.setCompareTmIsOn(compareTmIsOn);
                    rsc.refreshPane();
                }

            }
        });
        // Push buttons for CompBySrc info
        compareSndBtn = new Button(textModeGp, SWT.PUSH);
        compareSndBtn.setFont(newFont);
        if (compareSndIsOn) {
            compareSndBtn.setText(COMP_SND_ON);
            compareSndBtn.setEnabled(true);
        } else {
            // comparison and overlay is mutual exclusive
            compareSndBtn.setText(COMP_SND_OFF);
            if ((rscHandler != null)
                    && (rscHandler.isOverlayIsOn()
                            || rscHandler.isCompareStnIsOn() || rscHandler
                                .isCompareTmIsOn()))
                compareSndBtn.setEnabled(false);
            else
                compareSndBtn.setEnabled(true);
        }
        compareSndBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getShell();
                if (compareSndIsOn == false) {
                    compareSndIsOn = true;
                    compareSndBtn.setText(COMP_SND_ON);
                    overlayBtn.setEnabled(false);
                    compareStnBtn.setEnabled(false);
                    compareTmBtn.setEnabled(false);
                    graphEditBtn.setEnabled(false);
                    dataEditBtn.setEnabled(false);
                    graphModeBtnTurb.setEnabled(false);
                    graphModeBtnIcing.setEnabled(false);
                    interpBtn.setEnabled(false);
                    cfgBtn.setEnabled(false);
                } else {
                    compareSndIsOn = false;
                    compareSndBtn.setText(COMP_SND_OFF);
                    overlayBtn.setEnabled(true);
                    compareStnBtn.setEnabled(true);
                    compareTmBtn.setEnabled(true);
                    graphEditBtn.setEnabled(true);
                    dataEditBtn.setEnabled(true);
                    graphModeBtnTurb.setEnabled(true);
                    graphModeBtnIcing.setEnabled(true);
                    interpBtn.setEnabled(true);
                    cfgBtn.setEnabled(true);
                }
                NsharpResourceHandler rsc = getRscHandler();
                if (rsc != null) {
                    rsc.setCompareSndIsOn(compareSndIsOn);
                    rsc.refreshPane();
                }

            }
        });

        dataEditBtn = new Button(textModeGp, SWT.PUSH);
        dataEditBtn.setFont(newFont);
        dataEditBtn.setText("   Edit  Data    ");
        if (interpolateIsOn || editGraphOn)
            dataEditBtn.setEnabled(false);
        else
            dataEditBtn.setEnabled(true);
        dataEditBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (checkLoadedData()) {
                    Shell shell = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getShell();
                    NsharpEditDataDialog editDia = NsharpEditDataDialog
                            .getInstance(shell);
                    if (editDia != null) {
                        editDia.open();
                    }
                }
            }
        });

        graphEditBtn = new Button(textModeGp, SWT.PUSH);
        graphEditBtn.setFont(newFont);
        graphEditBtn.setEnabled(true);
        if (editGraphOn) {
            graphEditBtn.setText(EDIT_GRAPH_ON);
            dataEditBtn.setEnabled(false);
            interpBtn.setEnabled(false);
        } else {
            graphEditBtn.setText(EDIT_GRAPH_OFF);
            dataEditBtn.setEnabled(true);
            interpBtn.setEnabled(true);
        }
        graphEditBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (checkLoadedData()) {
                    if (editGraphOn) {
                        editGraphOn = false;
                        graphEditBtn.setText(EDIT_GRAPH_OFF);
                        if(interpolateIsOn == false){ //TTR829 
                        	graphModeBtnIcing.setEnabled(true);
                        	graphModeBtnTurb.setEnabled(true);
                        	dataEditBtn.setEnabled(true);
                        	//interpBtn.setEnabled(true);TTR829 
                        	compareTmBtn.setEnabled(true);
                        	compareSndBtn.setEnabled(true);
                        	compareStnBtn.setEnabled(true);
                        	overlayBtn.setEnabled(true);
                        }
                    } else {
                        editGraphOn = true;
                        graphEditBtn.setText(EDIT_GRAPH_ON);
                        graphModeBtnIcing.setEnabled(false);
                        graphModeBtnTurb.setEnabled(false);
                        dataEditBtn.setEnabled(false);
                        //interpBtn.setEnabled(false);
                        compareTmBtn.setEnabled(false);
                        compareSndBtn.setEnabled(false);
                        compareStnBtn.setEnabled(false);
                        overlayBtn.setEnabled(false);
                    }
                    NsharpResourceHandler rsc = getRscHandler();
                    if (rsc != null) {
                        rsc.setEditGraphOn(editGraphOn);
                        rsc.refreshPane();
                    }
                }
            }
        });
        /*
         * Button bndryMotionBtn = new Button(textModeGp, SWT.PUSH);
         * bndryMotionBtn.setText("BoundaryMotion"); bndryMotionBtn.setEnabled(
         * true ); bndryMotionBtn.addListener( SWT.MouseUp, new Listener() {
         * public void handleEvent(Event event) { shell =
         * PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
         * if(checkLoadedData()) { NsharpShowTextDialog osDia =
         * NsharpShowTextDialog.getInstance( shell ); if(osDia != null)
         * osDia.open(); } } } );
         */

        // Push buttons for show text info
        Button showtextBtn = new Button(textModeGp, SWT.PUSH);
        showtextBtn.setFont(newFont);
        showtextBtn.setText("  Show  Text   ");
        showtextBtn.setEnabled(true);
        showtextBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getShell();
                if (checkLoadedData()) {
                    NsharpShowTextDialog osDia = NsharpShowTextDialog
                            .getInstance(shell);
                    if (osDia != null)
                        osDia.open();
                }
            }
        });
        Group graphModeGp = new Group(textModeGp, SWT.SHADOW_ETCHED_IN);
        graphModeGp.setLayout(new RowLayout(SWT.HORIZONTAL));// new GridLayout(
                                                             // 2, false ) );

        // Push buttons for graphMode
        graphModeBtnSkew = new Button(graphModeGp, SWT.PUSH);
        graphModeBtnSkew.setFont(newFont);
        graphModeBtnSkew.setText("S");
        graphModeBtnSkew.setEnabled(true);
        // colorButtonOriginalBg= graphModeBtnSkew.getBackground();
        rscHandler = getRscHandler();
        if (rscHandler != null) {
            currentGraphMode = rscHandler.getCurrentGraphMode();
        }
        graphModeBtnSkew.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (currentGraphMode != NsharpConstants.GRAPH_SKEWT) {
                    currentGraphMode = NsharpConstants.GRAPH_SKEWT;
                    graphModeBtnSkew.setBackground(colorBlue);
                    graphModeBtnTurb.setBackground(colorGrey);
                    graphModeBtnIcing.setBackground(colorGrey);
                    if (!interpolateIsOn) {
                        graphEditBtn.setEnabled(true);
                        dataEditBtn.setEnabled(true);
                        compareTmBtn.setEnabled(true);
                        compareSndBtn.setEnabled(true);
                        compareStnBtn.setEnabled(true);
                        overlayBtn.setEnabled(true);
                    } else {
                        graphEditBtn.setEnabled(false);
                        dataEditBtn.setEnabled(false);
                        compareTmBtn.setEnabled(false);
                        compareSndBtn.setEnabled(false);
                        compareStnBtn.setEnabled(false);
                        overlayBtn.setEnabled(false);
                    }
                    NsharpResourceHandler rsc = getRscHandler();
                    if (rsc != null) {
                        rsc.setCurrentGraphMode(currentGraphMode);
                        // rsc.getSkewtPaneRsc().handleResize();
                    }

                }
            }
        });
        graphModeBtnTurb = new Button(graphModeGp, SWT.PUSH);
        graphModeBtnTurb.setFont(newFont);
        graphModeBtnTurb.setText("T");
        graphModeBtnTurb.setEnabled(true);
        graphModeBtnTurb.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (currentGraphMode != NsharpConstants.GRAPH_TURB) {
                    currentGraphMode = NsharpConstants.GRAPH_TURB;
                    graphModeBtnTurb.setBackground(colorBlue);
                    graphModeBtnSkew.setBackground(colorGrey);
                    graphModeBtnIcing.setBackground(colorGrey);
                    graphEditBtn.setEnabled(false);
                    dataEditBtn.setEnabled(false);
                    compareTmBtn.setEnabled(false);
                    compareSndBtn.setEnabled(false);
                    compareStnBtn.setEnabled(false);
                    overlayBtn.setEnabled(false);
                    NsharpResourceHandler rsc = getRscHandler();
                    if (rsc != null) {
                        rsc.setCurrentGraphMode(currentGraphMode);
                        rsc.getSkewtPaneRsc().handleResize();
                    }
                }
            }
        });
        graphModeBtnIcing = new Button(graphModeGp, SWT.PUSH);
        graphModeBtnIcing.setFont(newFont);
        graphModeBtnIcing.setText("I");
        graphModeBtnIcing.setEnabled(true);
        graphModeBtnIcing.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (currentGraphMode != NsharpConstants.GRAPH_ICING) {
                    currentGraphMode = NsharpConstants.GRAPH_ICING;
                    graphModeBtnIcing.setBackground(colorBlue);
                    graphModeBtnSkew.setBackground(colorGrey);
                    graphModeBtnTurb.setBackground(colorGrey);
                    graphEditBtn.setEnabled(false);
                    dataEditBtn.setEnabled(false);
                    compareTmBtn.setEnabled(false);
                    compareSndBtn.setEnabled(false);
                    compareStnBtn.setEnabled(false);
                    overlayBtn.setEnabled(false);
                    NsharpResourceHandler rsc = getRscHandler();
                    if (rsc != null) {
                        rsc.setCurrentGraphMode(currentGraphMode);
                        rsc.getSkewtPaneRsc().handleResize();
                    }
                }
            }
        });
        if (currentGraphMode == NsharpConstants.GRAPH_SKEWT) {
            graphModeBtnSkew.setBackground(colorBlue);
            graphModeBtnTurb.setBackground(colorGrey);
            graphModeBtnIcing.setBackground(colorGrey);
        } else if (currentGraphMode == NsharpConstants.GRAPH_TURB) {
            graphModeBtnTurb.setBackground(colorBlue);
            graphModeBtnSkew.setBackground(colorGrey);
            graphModeBtnIcing.setBackground(colorGrey);
        } else if (currentGraphMode == NsharpConstants.GRAPH_ICING) {
            graphModeBtnIcing.setBackground(colorBlue);
            graphModeBtnSkew.setBackground(colorGrey);
            graphModeBtnTurb.setBackground(colorGrey);
        }

        // Push buttons for Print
        Button printBtn = new Button(textModeGp, SWT.PUSH);
        printBtn.setFont(newFont);
        printBtn.setText("      Print         ");
        printBtn.setEnabled(true);
        printBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                        .getShell();
                // 12.7.1 testing if(checkLoadedData()) {
                if (true) {
                    printHandle.handlePrint("");
                }
            }
        });

        if (paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR))
            createSPCGp();

        if (rscHandler != null) {
            restorePaletteWindow(paneConfigurationName,
                    rscHandler.getCurrentGraphMode(),
                    rscHandler.isInterpolateIsOn(), rscHandler.isOverlayIsOn(),
                    rscHandler.isCompareStnIsOn(),
                    rscHandler.isCompareTmIsOn(), rscHandler.isEditGraphOn(),
                    rscHandler.isCompareSndIsOn());
        }
        parent.redraw();
    }

    private void createSPCGp() {
        // System.out.println("createSPCGp..........................................");
        spcGp = new Group(parent, SWT.SHADOW_OUT);
        spcGp.setLayout(new RowLayout(SWT.HORIZONTAL));
        spcGp.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        spcGplbl = new Label(spcGp, SWT.NO);
        spcGplbl.setText("SPC Graphs");
        if (paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)) {
            spcGplbl.setEnabled(true);
        } else {
            spcGplbl.setEnabled(false);
        }
        effBulkShearBtn = new Button(spcGp, SWT.PUSH);
        effBulkShearBtn.setFont(newFont);
        effBulkShearBtn.setText("EBS Stats ");
        if (paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)) {
            effBulkShearBtn.setEnabled(true);
        } else {
            effBulkShearBtn.setEnabled(false);
        }
        effBulkShearBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (leftGraph != NsharpConstants.SPCGraph.EBS
                        && rightGraph != NsharpConstants.SPCGraph.EBS) {
                    rightGraph = leftGraph;
                    leftGraph = NsharpConstants.SPCGraph.EBS;
                    updateSPCGraphs();
                }
            }
        });

        stpBtn = new Button(spcGp, SWT.PUSH);
        stpBtn.setFont(newFont);
        stpBtn.setText("STP Stats ");
        if (paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)) {
            stpBtn.setEnabled(true);
        } else {
            stpBtn.setEnabled(false);
        }
        stpBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (leftGraph != NsharpConstants.SPCGraph.STP
                        && rightGraph != NsharpConstants.SPCGraph.STP) {
                    rightGraph = leftGraph;
                    leftGraph = NsharpConstants.SPCGraph.STP;
                    updateSPCGraphs();
                }
            }
        });

        shipBtn = new Button(spcGp, SWT.PUSH);
        shipBtn.setFont(newFont);
        shipBtn.setText("SHIP Stats");
        if (paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)) {
            shipBtn.setEnabled(true);
        } else {
            shipBtn.setEnabled(false);
        }
        shipBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (leftGraph != NsharpConstants.SPCGraph.SHIP
                        && rightGraph != NsharpConstants.SPCGraph.SHIP) {
                    rightGraph = leftGraph;
                    leftGraph = NsharpConstants.SPCGraph.SHIP;
                    updateSPCGraphs();
                }
            }
        });
        winterBtn = new Button(spcGp, SWT.PUSH);
        winterBtn.setFont(newFont);
        winterBtn.setText(" WINTER  ");
        if (paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)) {
            winterBtn.setEnabled(true);
        } else {
            winterBtn.setEnabled(false);
        }
        winterBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (leftGraph != NsharpConstants.SPCGraph.WINTER
                        && rightGraph != NsharpConstants.SPCGraph.WINTER) {
                    rightGraph = leftGraph;
                    leftGraph = NsharpConstants.SPCGraph.WINTER;
                    updateSPCGraphs();
                }
            }
        });
        fireBtn = new Button(spcGp, SWT.PUSH);
        fireBtn.setFont(newFont);
        fireBtn.setText("    FIRE    ");
        if (paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)) {
            fireBtn.setEnabled(true);
        } else {
            fireBtn.setEnabled(false);
        }
        fireBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (leftGraph != NsharpConstants.SPCGraph.FIRE
                        && rightGraph != NsharpConstants.SPCGraph.FIRE) {
                    rightGraph = leftGraph;
                    leftGraph = NsharpConstants.SPCGraph.FIRE;
                    updateSPCGraphs();
                }
            }
        });
        hailBtn = new Button(spcGp, SWT.PUSH);
        hailBtn.setFont(newFont);
        hailBtn.setText("    HAIL    ");
        if (paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)) {
            hailBtn.setEnabled(true);
        } else {
            hailBtn.setEnabled(false);
        }
        hailBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (leftGraph != NsharpConstants.SPCGraph.HAIL
                        && rightGraph != NsharpConstants.SPCGraph.HAIL) {
                    rightGraph = leftGraph;
                    leftGraph = NsharpConstants.SPCGraph.HAIL;
                    updateSPCGraphs();
                }
            }
        });
        sarsBtn = new Button(spcGp, SWT.PUSH);
        sarsBtn.setFont(newFont);
        sarsBtn.setText("    SARS    ");
        if (paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)) {
            sarsBtn.setEnabled(true);
        } else {
            sarsBtn.setEnabled(false);
        }
        sarsBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                if (leftGraph != NsharpConstants.SPCGraph.SARS
                        && rightGraph != NsharpConstants.SPCGraph.SARS) {
                    rightGraph = leftGraph;
                    leftGraph = NsharpConstants.SPCGraph.SARS;
                    updateSPCGraphs();
                }
            }
        });
        updateSPCGraphs();
        spcGpCreated = true;
        parent.layout();
    }

    private void disposeSpcGp() {
        spcGplbl.dispose();
        effBulkShearBtn.dispose();
        stpBtn.dispose();
        shipBtn.dispose();
        winterBtn.dispose();
        fireBtn.dispose();
        hailBtn.dispose();
        sarsBtn.dispose();
        spcGp.dispose();

        spcGplbl = null;
        effBulkShearBtn = null;
        stpBtn = null;
        shipBtn = null;
        winterBtn = null;
        fireBtn = null;
        hailBtn = null;
        sarsBtn = null;
        spcGp = null;
    }

    public boolean isEditorVisible() {
        return isEditorVisible;
    }

    public void setEditorVisible(boolean isEditorVisible) {
        this.isEditorVisible = isEditorVisible;
    }

    public void updateSpcGraphBtn(String paneConfigurationName) {
        this.paneConfigurationName = paneConfigurationName;
        if (paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)) {
            if (spcGpCreated == false) {
                createSPCGp();
            }
        } else {
            if (spcGpCreated) {
                spcGpCreated = false;
                if (spcGp != null) {
                    disposeSpcGp();
                    parent.layout();
                }

            }
        }

    }

    /**
     * Invoked by the workbench, this method sets up the SWT controls for the
     * nsharp palette
     */

    @Override
    public void createPartControl(Composite parent) {
        // System.out.println("nlist @"+NsharpConstants.getNlistFile());
        parent.setLayout(new GridLayout(1, true));
        createDataControlGp(parent);
    }

    @Override
    public void widgetDefaultSelected(SelectionEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void widgetSelected(SelectionEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void widgetDisposed(DisposeEvent e) {

    }

    private IContextActivation context;

    @Override
    public void partActivated(IWorkbenchPart part) {
        if (context == null) {
            IContextService ctxSvc = (IContextService) PlatformUI
                    .getWorkbench().getService(IContextService.class);
            context = ctxSvc
                    .activateContext("gov.noaa.nws.ncep.ui.nsharp.nsharpContext");
           // System.out.println("Activated " + context.getContextId());
        }
        if (part instanceof NsharpPaletteWindow) {
            NsharpMapResource rsc = NsharpMapResource.getMapRsc();
            if (rsc != null) {
                rsc.setEditable(true);
            }
        }
    }

    @Override
    public void partBroughtToTop(IWorkbenchPart part) {
        // TODO Auto-generated method stub

    }

    @Override
    public void partClosed(IWorkbenchPart part) {
    	//System.out.println("view closed ");
        /*FixMark:SwapPaneShowText
        NsharpShowTextDialog textarea =  NsharpShowTextDialog.getAccess();
		if(textarea != null){
			textarea.close();
		}//end FixMark:SwapPaneShowText */

    }

    @Override
    public void partDeactivated(IWorkbenchPart part) {
        if (context != null) {

            IContextService ctxSvc = (IContextService) PlatformUI
                    .getWorkbench().getService(IContextService.class);
            ctxSvc.deactivateContext(context);
            //System.out.println("view Deactivated " + context.getContextId());
            context = null;
        }
    }

    @Override
    public void partOpened(IWorkbenchPart part) {
        // TODO Auto-generated method stub

    }

    @Override
    public void setFocus() {
        // TODO Auto-generated method stub

    }
}
