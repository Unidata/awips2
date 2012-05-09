/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor
 * 
 * This java class performs the NSHARP NsharpSkewTEditor functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 * 										Reused some software from com.raytheon.viz.skewt
 * 03/24/2011   R1G2-9      Chin Chen   migration
 * 06/14/2011   11-5        Chin Chen   migration
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.skewt;


import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTMouseHandler;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource;
import gov.noaa.nws.ncep.viz.common.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.common.EditorManager;
import gov.noaa.nws.ncep.viz.ui.display.NCLoopProperties;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
//import org.eclipse.ui.contexts.IContextActivation;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.EditorInput;
import com.raytheon.viz.ui.input.InputManager;
import com.raytheon.viz.ui.panes.PaneManager;
import com.raytheon.viz.ui.panes.VizDisplayPane;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpSkewTEditor extends AbstractEditor implements AddListener,
        RemoveListener, AbstractNcEditor {

    public static final String EDITOR_ID = "gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor";
    private  int editorNum=0;
    
 
    public int getEditorNum() {
		return editorNum;
	}



	/** The map input manager */
    protected InputManager inputManager;

    /** The activated context, else null if not activated. */
    //protected IContextActivation contextActivation;

    private NsharpSkewTMouseHandler mouseHandler = null;


    //private List<NsharpSkewTResource> rscList = new ArrayList<NsharpSkewTResource>();
    private static NsharpSkewTEditor instance=null;
 
    protected VizDisplayPane displayPane[]= new VizDisplayPane[1];

    public static NsharpSkewTEditor getActiveNsharpEditor() {
        // bsteffen instead of getting the static instance try to find an editor in the window
        IEditorPart ep = EditorUtil.getActiveEditor();
        if (ep instanceof NsharpSkewTEditor) {
        	//System.out.println("getActiveNsharpEditor return ep from EditorUtil.getActiveEditor()");
            return (NsharpSkewTEditor) ep;
        }
        
        // It might be desirable to stop here so that we only have an "active"
        // editor if it really is active.
        IWorkbenchPage activePage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        IEditorReference[] references = new IEditorReference[0];
        if (activePage != null) {
            references = activePage.getEditorReferences();
        }

        for (IEditorReference ref : references) {
            ep = ref.getEditor(false);
            if (ep instanceof NsharpSkewTEditor) {
            	//System.out.println("getActiveNsharpEditor return ep from IEditorReference.getEditor");
                return (NsharpSkewTEditor) ep;
            }
        }
        return null;
    }/* testing 
    public static NsharpSkewTEditor getActiveNsharpEditor() {
    	if (instance != null) {
        	System.out.println("getActiveNsharpEditor return existing editor ");
            //PlatformUI.getWorkbench().getActiveWorkbenchWindow()
            //        .getActivePage().bringToTop(instance);
            return instance;
        } else {
            try {
                EditorInput edInput = new EditorInput(new NCLoopProperties(),
                        new NsharpSkewTDisplay());
                System.out.println("getActiveNsharpEditor creating new one");
                instance = (NsharpSkewTEditor) PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage()
                        .openEditor(edInput, EDITOR_ID);
                return instance;
            } catch (PartInitException e) {
                UiPlugin.getDefault()
                        .getLog()
                        .log(new Status(Status.ERROR, UiPlugin.PLUGIN_ID,
                                "Error constituting NsharpSkewTeditor", e));
            }
        }
        return null;
    }*/
    
    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public void createPartControl(Composite comp) {
    	//System.out.println("nsharpSkewTEditor createPartControl called");
        Composite baseComposite = new Composite(comp, SWT.NONE);
        baseComposite
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        final GridLayout mainGL = new GridLayout(1, true);
        mainGL.horizontalSpacing = 0;
        mainGL.marginHeight = 0;
        baseComposite.setLayout(mainGL);

 
        // drawing composite
        Composite drawingComposite = new Composite(baseComposite, SWT.NONE);
        drawingComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));
        GridLayout drawGL = new GridLayout();
        drawGL.marginWidth = 0;
        drawGL.marginHeight = 0;
        drawGL.horizontalSpacing = 0;
        drawingComposite.setLayout(drawGL);
        GridLayout gl = new GridLayout(1, true);
        gl.horizontalSpacing = 3;
        gl.verticalSpacing = 3;
        gl.marginHeight = 0;
        gl.marginWidth = 0;

        try {
            if (displayPane[0] == null ) {
                displayPane[0] = new VizDisplayPane(this, drawingComposite,
                        displaysToLoad[0]);
                //displayPane[0].setAdjustExtent(false);
                inputManager = new InputManager(this);
                registerHandlers(displayPane[0]);
                displayPane[0].setRenderableDisplay(displaysToLoad[0]);
                registerListener(displayPane[0]);
                
            }
        }

        catch (Exception e) {
            final String errMsg = "Error setting up NsharpSkewTEditor";
            UFStatus.getHandler().handle(Priority.SIGNIFICANT,  errMsg, e);
        }
        
        contributePerspectiveActions();


    }

    protected void registerHandlers(IDisplayPane pane) {

        // Enable the mouse inspect adapter
    	mouseHandler = new NsharpSkewTMouseHandler(this);
        inputManager.registerMouseHandler(mouseHandler);

        pane.addListener(SWT.MouseUp, inputManager);
        pane.addListener(SWT.MouseDown, inputManager);
        pane.addListener(SWT.MouseMove, inputManager);
        pane.addListener(SWT.MouseWheel, inputManager);
        pane.addListener(SWT.MouseHover, inputManager);
        pane.addListener(SWT.MenuDetect, inputManager);
        pane.addListener(SWT.KeyUp, inputManager);
        pane.addListener(SWT.KeyDown, inputManager);
        
    }


    @Override
    public void init(IEditorSite site, IEditorInput input)
            throws PartInitException {
        super.init(site, input);
        EditorInput editorInput = (EditorInput) input;
        if (input instanceof EditorInput) {
        	displaysToLoad = editorInput.getRenderableDisplays();
        }
        //System.out.println("SkewtEditor  title " + this.getTitle() );
        if (editorNum == 0 ){
        	editorNum = EditorManager.getEditorNumber();
        	
        }
        else {
        	
        }
        // a new instance, do the registration
        EditorManager.registerEditorNumber(editorNum);
        this.setTabTitle(editorNum+"-NsharpSkewt");
    }


    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        getActiveDisplayPane().setFocus();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#dispose()
     */
    @Override
    public void dispose() {
    	//System.out.println("NsharpSkewTEditor disposed!! "  );
    	if (EditorManager.unregisterEditorNumber(editorNum) ==0 ){
    		super.dispose();
    		synchronized (this) {
    			if (mouseHandler != null && inputManager != null) {
    				//System.out.println("NsharpSkewTEditor disposed!! " + this + " mouseHandler "+ mouseHandler );
    				mouseHandler.setEditor(null);
    				inputManager.unregisterMouseHandler(mouseHandler);
    				mouseHandler = null;
    				inputManager = null;
    			}

    			editorNum=0;
    		}
    	}
    	try{
    		IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
    		IViewPart vpart = wpage.findView( "gov.noaa.nws.ncep.ui.nsharp" );
    		wpage.hideView(vpart);
    	}
    	catch(Exception e){
    		
    	}
    	
    	
    }

    /**
     * Perform a refresh asynchronously
     * 
     */
    @Override
    public void refresh() {
        for (IDisplayPane pane : displayPane) {
            pane.refresh();
        }
        //System.out.println("NsharpSkewTEditor refresh called");
    }
    @Override
    public Coordinate translateClick(double x, double y) {
        double[] grid = getActiveDisplayPane().screenToGrid(x, y, 0);

        return new Coordinate(grid[0], grid[1], grid[2]);
    }

     @Override
    public double[] translateInverseClick(Coordinate c) {

        if (Double.isNaN(c.z)) {
            c.z = 0.0;
        }
        return getActiveDisplayPane().gridToScreen(
                new double[] { c.x, c.y, c.z });
    }

    public void registerMouseHandler(IInputHandler handler, IInputHandler.InputPriority priority) {
        inputManager.registerMouseHandler(handler, priority);
    }
    
    /**
     * Register a mouse handler to a map
     * 
     * @param handler
     *            the handler to register
     */
    @Override
    public void registerMouseHandler(IInputHandler handler) {
        inputManager.registerMouseHandler(handler);
    }

    /**
     * Unregister a mouse handler to a map
     * 
     * @param handler
     *            the handler to unregister
     */
    @Override
    public void unregisterMouseHandler(IInputHandler handler) {
        // already unregistered
        if (inputManager != null) {
            inputManager.unregisterMouseHandler(handler);
        }
    }
    @Override
    public IDisplayPane[] getDisplayPanes() {
    	//System.out.println("SkewtEditor getDisplayPanes called");
        return this.displayPane;
    }

    

    /**
     * Returns the mouse manager
     * 
     * @return
     */
    public InputManager getMouseManager() {
        return inputManager;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.AbstractEditor#getActiveDisplayPane()
     */
    @Override
    public IDisplayPane getActiveDisplayPane() {
        return this.displayPane[0];
    }

   /**
     * Add the listenerList
     * 
     * @param main
     */
    private void registerListener(VizDisplayPane pane) {
        synchronized (this) {
            if (pane != null) {
                try {
                    // add the Editor as a resource listener
                    pane.getRenderableDisplay().getDescriptor()
                            .getResourceList().addPostAddListener(this);
                    pane.getRenderableDisplay().getDescriptor()
                            .getResourceList().addPostRemoveListener(this);
                } catch (Exception e) {
                    UFStatus.getHandler()
                            .handle(
                                    Priority.PROBLEM,                                   
                                    "Error recovering Resource, Can't add listenerList",
                                    e);
                }
            }
        }
    }

    @Override
    public void notifyAdd(ResourcePair rp) throws VizException {
        if (rp.getResource() instanceof NsharpSkewTResource) {
        	//NsharpSkewTResource rsc = (NsharpSkewTResource) rp.getResource();
            //rscList.add(rsc);
        }
    }
 
    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        //
    }

    /**
     * Add a resource listener on first pane
     * 
     * @param SkewTResource
     */
    public void addResource(NsharpSkewTResource str) {
        if (str != null) {
            displayPane[0].getRenderableDisplay().getDescriptor()
                    .getResourceList().add(str);
        }
    }

 
 
    @Override
    public NCLoopProperties getLoopProperties(){
    	// bsteffen added check for type and force it to NCLoopProperties
        LoopProperties loopProperties = this.editorInput.getLoopProperties();
        if(!(loopProperties instanceof NCLoopProperties)) {
            this.editorInput.setLoopProperties(new NCLoopProperties());
        }
        return (NCLoopProperties)this.editorInput.getLoopProperties();
    }

	@Override
	protected PaneManager getNewPaneManager() {
		
		return null;
	}
	public NsharpSkewTDescriptor getNsharpSkewTDescriptor() {
        IDescriptor desc = getActiveDisplayPane().getDescriptor();
        if (desc instanceof NsharpSkewTDescriptor) {
            return (NsharpSkewTDescriptor) desc;
        }
        return null;
    }
	
	public static NsharpSkewTEditor createOrOpenSkewTEditor(  ) {
		NsharpSkewTEditor editor = getActiveNsharpEditor();
        if (editor != null) {
        	//System.out.println("createOrOpenSkewTEditor return editor from getActiveNsharpEditor");
            //PlatformUI.getWorkbench().getActiveWorkbenchWindow()
            //        .getActivePage().bringToTop(editor);
            return editor;
        } else {
            try {
                EditorInput edInput = new EditorInput(new NCLoopProperties(),
                        new NsharpSkewTDisplay());
                //System.out.println("createOrOpenSkewTEditor creating new one");
                return (NsharpSkewTEditor) PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage()
                        .openEditor(edInput, EDITOR_ID);
            } catch (PartInitException e) {
                UiPlugin.getDefault()
                        .getLog()
                        .log(new Status(Status.ERROR, UiPlugin.PLUGIN_ID,
                                "Error constituting NsharpSkewTeditor", e));
            }
        }
        return null;
	}/* testing 
	public static NsharpSkewTEditor createOrOpenSkewTEditor(  ) {
		//NsharpSkewTEditor editor = getActiveNsharpEditor();
        if (instance != null) {
        	System.out.println("createOrOpenSkewTEditor return existed editor ");
            //PlatformUI.getWorkbench().getActiveWorkbenchWindow()
             //       .getActivePage().bringToTop(instance);
            return instance;
        } else {
            try {
                EditorInput edInput = new EditorInput(new NCLoopProperties(),
                        new NsharpSkewTDisplay());
                System.out.println("createOrOpenSkewTEditor creating new one");
                instance = (NsharpSkewTEditor) PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage()
                        .openEditor(edInput, EDITOR_ID);
                return instance;
            } catch (PartInitException e) {
                UiPlugin.getDefault()
                        .getLog()
                        .log(new Status(Status.ERROR, UiPlugin.PLUGIN_ID,
                                "Error constituting NsharpSkewTeditor", e));
            }
        }
        return null;
	}*/
	public static void bringSkewTEditorToTop(  ) {
		NsharpSkewTEditor editor = getActiveNsharpEditor();
        if (editor != null) {
            PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getActivePage().bringToTop(editor);
            
        } 
	}
	public void refreshGUIElements() {
        ICommandService service = (ICommandService) getSite().getService(
                ICommandService.class);
        String[] guiUpdateElementCommands = {
                // "gov.noaa.nws.ncep.viz.tools.pan",
                "gov.noaa.nws.ncep.viz.ui.options.SyncPanes",
                "gov.noaa.nws.ncep.viz.ui.actions.loopBackward",
                "gov.noaa.nws.ncep.viz.ui.actions.loopForward",
                "gov.noaa.nws.ncep.viz.ui.actions.rock",
                "gov.noaa.nws.ncep.viz.ui.actions.frameTool",
                "gov.noaa.nws.ncep.viz.ui.autoUpdate",
                "gov.noaa.nws.ncep.viz.ui.actions.hideFrames" };

        // Update the GUI elements on the menus and toolbars
        for (String toolbarID : guiUpdateElementCommands) {
            service.refreshElements(toolbarID, null);
        }

    }
    
}