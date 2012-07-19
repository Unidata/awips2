package gov.noaa.nws.ncep.ui.nsharp.display;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpEditor
 * 
 * This java class performs the NSHARP NsharpEditor functions.
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

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpAbstractPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpDataPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpHodoPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpInsetPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpSkewTPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpTimeStnPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpWitoPaneResource;
import gov.noaa.nws.ncep.ui.pgen.tools.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.viz.common.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.common.EditorManager;
import gov.noaa.nws.ncep.viz.ui.display.NCLoopProperties;

import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
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

public class NsharpEditor extends AbstractEditor implements AddListener,
        RemoveListener, AbstractNcEditor {

    public static final String EDITOR_ID = "gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpEditor";
    private  int editorNum=0;
    private static NsharpResourceHandler rscHandler;
    private int baseWidth;
    private int baseHeight;
    private static IRenderableDisplay[] displayArray;
    
	public NsharpResourceHandler getRscHandler() {
		return rscHandler;
	}
	public int getEditorNum() {
		return editorNum;
	}

    private Composite[] nsharpComp = new Composite[NsharpConstants.DISPLAY_TOTAL];
    private Composite baseComposite;
    private Group rightTopGp, leftTopGp;
	/** The map input manager */
    protected InputManager skewtInputManager;
    //protected InputManager witoInputManager;
    protected InputManager hodoInputManager;
    protected InputManager timeStnInputManager;
    protected InputManager dataInputManager;
    protected InputManager insetInputManager;

    /** The activated context, else null if not activated. */
    //protected IContextActivation contextActivation;

    private NsharpSkewTPaneMouseHandler skewtPaneMouseHandler = null;
    //private NsharpAbstractMouseHandler witoPaneMouseHandler = null;
    private NsharpHodoPaneMouseHandler hodoPaneMouseHandler = null;
    private NsharpTimeStnPaneMouseHandler timeStnPaneMouseHandler = null;
    private NsharpDataPaneMouseHandler dataPaneMouseHandler = null;
    private NsharpAbstractMouseHandler insetPaneMouseHandler = null;
 
    protected VizDisplayPane displayPane[]= new VizDisplayPane[NsharpConstants.DISPLAY_TOTAL];
    protected VizDisplayPane selectedPane = displayPane[0];
    public static NsharpEditor getActiveNsharpEditor() {
        IEditorPart ep = EditorUtil.getActiveEditor();
        if (ep instanceof NsharpEditor) {
        	//System.out.println("getActiveNsharpEditor return ep from EditorUtil.getActiveEditor()");
            return (NsharpEditor) ep;
        }
        
        // It might be desirable to stop here so that we only have an "active"
        // editor if it really is active.
        if(PlatformUI.getWorkbench()==null || PlatformUI.getWorkbench().getActiveWorkbenchWindow() ==null )
        	return null;
        IWorkbenchPage activePage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        IEditorReference[] references = new IEditorReference[0];
        if (activePage != null) {
            references = activePage.getEditorReferences();
        }

        for (IEditorReference ref : references) {
            ep = ref.getEditor(false);
            if (ep instanceof NsharpEditor) {
            	//System.out.println("getActiveNsharpEditor return ep from IEditorReference.getEditor");
                return (NsharpEditor) ep;
            }
        }
        return null;
    }    
    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public void createPartControl(Composite comp) {
    	//System.out.println("NsharpEditor createPartControl called");
        baseComposite = comp;// new Composite(comp, SWT.NONE);
        final GridLayout mainGL = new GridLayout(2, true);
        mainGL.horizontalSpacing = 0;
        mainGL.marginHeight = 0;
        baseComposite.setLayout(mainGL);
        //System.out.println("createPartControl...baseComposite w= " + baseComposite.getBounds().width + " h= "+ baseComposite.getBounds().height);
        baseHeight = baseComposite.getSize().y;
        baseWidth = baseComposite.getSize().x;
        baseComposite.addListener(SWT.Resize, new Listener() {
            @Override
            public void handleEvent(Event event) {
            	//System.out.println("Before resizing...baseComposite w= " + baseComposite.getBounds().width + " h= "+ baseComposite.getBounds().height);
            	//System.out.println("Before resizing...nsharpComp[0] w= " + nsharpComp[0].getBounds().width + " h= "+ nsharpComp[0].getBounds().height);
            	//System.out.println("Before resizing...nsharpComp[1] w= " + nsharpComp[1].getBounds().width + " h= "+ nsharpComp[1].getBounds().height);
            	//System.out.println("Before resizing...nsharpComp[2] w= " + nsharpComp[2].getBounds().width + " h= "+ nsharpComp[2].getBounds().height);
            	//System.out.println("Before resizing...nsharpComp[3] w= " + nsharpComp[3].getBounds().width + " h= "+ nsharpComp[3].getBounds().height);
            	baseHeight = baseComposite.getSize().y;
                baseWidth = baseComposite.getSize().x;
            	GridData skewtGd = new GridData(SWT.FILL, SWT.FILL, true,
                        true);
                skewtGd.heightHint = (int)(baseHeight*0.8);
                skewtGd.widthHint = (int)(baseWidth*0.5*0.6875);//width is 11:5 between skewTComp and witoComp
                nsharpComp[NsharpConstants.DISPLAY_SKEWT].setLayoutData(skewtGd);
                GridData witoGd = new GridData(SWT.END, SWT.FILL, false,
                        true);
                witoGd.heightHint = (int)(baseHeight*0.8);
                witoGd.widthHint = (int)(baseWidth*0.5*0.3125);//width is 11:5 between skewTComp and witoComp
                nsharpComp[NsharpConstants.DISPLAY_WITO].setLayoutData(witoGd);
                nsharpComp[NsharpConstants.DISPLAY_WITO].setSize((int)(baseWidth*0.5*0.3125),(int)(baseHeight*0.8));
                GridData insetGd = new GridData(SWT.FILL, SWT.END, true,
                        false);
                insetGd.heightHint =  (int)(baseHeight*0.2);
                insetGd.widthHint = (int)(baseWidth*0.5);
                nsharpComp[NsharpConstants.DISPLAY_INSET].setLayoutData(insetGd);
               
                
                GridData hodoGd = new GridData(SWT.FILL, SWT.FILL, true,
                        true);
                hodoGd.widthHint = (int)(baseWidth*0.5*0.65);//width is 65:35 between hodoComp and timeStnComp
                nsharpComp[NsharpConstants.DISPLAY_HODO].setLayoutData(hodoGd);
                
                GridData timeStnGd = new GridData(SWT.END, SWT.FILL, false,
                        true);
                timeStnGd.widthHint = (int)(baseWidth*0.5*0.35);//width is 65:35 between hodoComp and timeStnComp
                nsharpComp[NsharpConstants.DISPLAY_TIMESTN].setLayoutData(timeStnGd);
                
                GridData dataGd = new GridData(SWT.FILL, SWT.END, true,
                        false);
                dataGd.heightHint = (int)(baseHeight*0.3);
                dataGd.widthHint = (int)(baseWidth*0.5);
                nsharpComp[NsharpConstants.DISPLAY_DATA].setLayoutData(dataGd);
                
                for(int i=0; i< NsharpConstants.DISPLAY_TOTAL; i++){
                	ResourcePair rscPair =  displayArray[i].getDescriptor().getResourceList().get(0);
                	if (rscPair.getResource() instanceof NsharpAbstractPaneResource){
                		NsharpAbstractPaneResource paneRsc = (NsharpAbstractPaneResource)rscPair.getResource() ;
                		paneRsc.setResize(true);
                	}
                }
            	//System.out.println("After resizing...nsharpComp[0] w= " + nsharpComp[0].getBounds().width + " h= "+ nsharpComp[0].getBounds().height);
            	//System.out.println("After resizing...nsharpComp[1] w= " + nsharpComp[1].getBounds().width + " h= "+ nsharpComp[1].getBounds().height);
            	
            }
        });
        
        Group leftGp = new Group(baseComposite, SWT.NONE);
        GridData leftGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        leftGp.setLayoutData(leftGpGd);
        GridLayout leftGpLayout = new GridLayout(1, true);
        leftGpLayout.marginWidth = 0;
        leftGpLayout.marginHeight = 0;
        leftGpLayout.verticalSpacing = 0;
        leftGp.setLayout(leftGpLayout);
        
      //left-top group : upper part of left group
        leftTopGp = new Group(leftGp, SWT.NONE);
        GridData leftTopGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        leftTopGpGd.heightHint = 4;//height is 4:1 between leftTopGp and leftBotGp( only insetComp for now)
        leftTopGp.setLayoutData(leftTopGpGd);
        GridLayout leftTopGpLayout = new GridLayout(2, false);
        leftTopGpLayout.marginWidth = 0;
        leftTopGpLayout.marginHeight = 0;
        leftTopGpLayout.verticalSpacing=0;
        leftTopGp.setLayout(leftTopGpLayout);
        
        // skewt composite
        Composite skewtComp = new Composite(leftTopGp, SWT.NONE);
        GridData skewtGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        //skewtGd.heightHint = 4; //Height is 4:1 between skewtComp and insetComp
        skewtComp.setLayoutData(skewtGd);
        GridLayout skewtLayout = new GridLayout(1, true);
        skewtLayout.marginWidth = 0;
        skewtLayout.marginHeight = 0;
        skewtLayout.verticalSpacing = 0;
        skewtComp.setLayout(skewtLayout);
        nsharpComp[NsharpConstants.DISPLAY_SKEWT] = skewtComp;
        
        // wito composite
        Composite witoComp = new Composite(leftTopGp, SWT.NONE);
        GridData witoGd = new GridData(SWT.END, SWT.FILL, false,
                true);
        witoGd.widthHint = NsharpConstants.WITO_PANE_REC_WIDTH;
        witoComp.setLayoutData(witoGd);
        GridLayout witoLayout = new GridLayout(1, true);
        witoLayout.marginWidth = 0;
        witoLayout.marginHeight = 0;
        witoLayout.verticalSpacing = 0;
        witoComp.setLayout(witoLayout);
        nsharpComp[NsharpConstants.DISPLAY_WITO] = witoComp;
               
        //inset composite
        Composite insetComp = new Composite(leftGp, SWT.NONE);
        GridData insetGd = new GridData(SWT.FILL, SWT.FILL, true,
                false);
        insetGd.heightHint =  1;
        insetComp.setLayoutData(insetGd);
        GridLayout insetLayout = new GridLayout(1, true);
        insetLayout.marginHeight = 0;
        insetLayout.marginWidth = 0;
		insetComp.setLayout(insetLayout);
		nsharpComp[NsharpConstants.DISPLAY_INSET] = insetComp;
		
        //right group
        Group rightGp = new Group(baseComposite, SWT.NONE);
        GridData rightGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        rightGp.setLayoutData(rightGpGd);
        GridLayout rightGpLayout = new GridLayout(1, true);
        rightGpLayout.marginWidth = 0;
        rightGpLayout.marginHeight = 0;
        rightGpLayout.verticalSpacing=0;
        rightGp.setLayout(rightGpLayout);
        
        //right-top group : upper part of right group
        rightTopGp = new Group(rightGp, SWT.NONE);
        GridData rightTopGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        rightTopGpGd.heightHint = 7;//height is 7:3 between rightTopGp and rightBotGp( only dataComp for now)
        rightTopGp.setLayoutData(rightTopGpGd);
        GridLayout rightTopGpLayout = new GridLayout(2, false);
        rightTopGpLayout.marginWidth = 0;
        rightTopGpLayout.marginHeight = 0;
        rightTopGpLayout.verticalSpacing=0;
        rightTopGp.setLayout(rightTopGpLayout);
		//hodo composite
        Composite hodoComp = new Composite(rightTopGp, SWT.NONE);
        GridData hodoGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        //hodoGd.widthHint = NsharpConstants.HODO_PANE_REC_WIDTH;//width is 65:35 between hodoComp and timeStnComp
        hodoComp.setLayoutData(hodoGd);
        GridLayout hodoLayout = new GridLayout(1, true);
        hodoLayout.marginHeight =0;
        hodoLayout.marginWidth=0;
        hodoLayout.verticalSpacing=0;
        hodoComp.setLayout(hodoLayout);
		nsharpComp[NsharpConstants.DISPLAY_HODO] = hodoComp;
        		
		//time-stn composite
        Composite timeStnComp = new Composite(rightTopGp, SWT.NONE);
        GridData timeStnGd = new GridData(SWT.END, SWT.FILL, false,
                true);
        timeStnGd.widthHint = NsharpConstants.TIMESTN_PANE_REC_WIDTH;//width is 65:35 between hodoComp and timeStnComp
        timeStnComp.setLayoutData(timeStnGd);
        GridLayout timeStnLayout = new GridLayout(1, true);
        timeStnLayout.marginHeight =0;
        timeStnLayout.marginWidth=0;
        timeStnLayout.verticalSpacing=0;
        timeStnComp.setLayout(timeStnLayout);
		nsharpComp[NsharpConstants.DISPLAY_TIMESTN] = timeStnComp;
		
		//data composite
        Composite dataComp = new Composite(rightGp, SWT.NONE);
        GridData dataGd = new GridData(SWT.FILL, SWT.FILL, true,
                false);
        dataGd.heightHint = 3;
        dataComp.setLayoutData(dataGd);
        GridLayout dataLayout = new GridLayout(1, true);
        dataLayout.marginHeight = 2;
        dataLayout.marginWidth=0;
        dataComp.setLayout(dataLayout);
		nsharpComp[NsharpConstants.DISPLAY_DATA] = dataComp; 
		
		skewtInputManager = new InputManager(this);
		//witoInputManager = new InputManager(this);
		timeStnInputManager = new InputManager(this);
		hodoInputManager = new InputManager(this);
		dataInputManager = new InputManager(this);
		insetInputManager = new InputManager(this);
		try {
			for(int i=0; i < displayPane.length; i++){
				if (displayPane[i] == null ) {
					displayPane[i] = new VizDisplayPane(this, nsharpComp[i],
							displaysToLoad[i]);
					
					
					displayPane[i].setRenderableDisplay(displaysToLoad[i]);
					registerListener(displayPane[i]);
				}
			}
			registerHandlers();
			
			displayPane[NsharpConstants.DISPLAY_SKEWT].addListener( SWT.MouseEnter, new Listener() {
				@Override
				public void handleEvent(Event e) {						
					if ( e.button==0 ) {
						selectedPane = displayPane[NsharpConstants.DISPLAY_SKEWT];
						//System.out.println("selectedPane =skewt");
					}
				}
			});
			displayPane[NsharpConstants.DISPLAY_WITO].addListener(SWT.MouseEnter, new Listener() {
				@Override
				public void handleEvent(Event e) {						
					if (e.button==0 ) {
						selectedPane = displayPane[NsharpConstants.DISPLAY_WITO];
						//System.out.println("selectedPane =date");
					}
				}
			});
			displayPane[NsharpConstants.DISPLAY_HODO].addListener( SWT.MouseEnter, new Listener() {
				@Override
				public void handleEvent(Event e) {						
					if ( e.button==0 ) {
						selectedPane = displayPane[NsharpConstants.DISPLAY_HODO];
						//System.out.println("selectedPane =hodo");
					}
				}
			});
			displayPane[NsharpConstants.DISPLAY_TIMESTN].addListener( SWT.MouseEnter, new Listener() {
				@Override
				public void handleEvent(Event e) {						
					if ( e.button==0 ) {
						selectedPane = displayPane[NsharpConstants.DISPLAY_TIMESTN];
						//System.out.println("selectedPane =hodo");
					}
				}
			});
			displayPane[NsharpConstants.DISPLAY_DATA].addListener(SWT.MouseEnter, new Listener() {
				@Override
				public void handleEvent(Event e) {						
					if (e.button==0 ) {
						selectedPane = displayPane[NsharpConstants.DISPLAY_DATA];
						//System.out.println("selectedPane =date");
					}
				}
			});
			displayPane[NsharpConstants.DISPLAY_INSET].addListener( SWT.MouseEnter, new Listener() {
				@Override
				public void handleEvent(Event e) {						
					if ( e.button==0 ) {
						selectedPane = displayPane[NsharpConstants.DISPLAY_INSET];
						//System.out.println("selectedPane =inset");
					}
				}
			});
			
		}
        catch (Exception e) {
            final String errMsg = "Error setting up NsharpEditor";
            UFStatus.getHandler().handle(Priority.SIGNIFICANT,  errMsg, e);
        }
        contributePerspectiveActions();
    }

    protected void registerHandlers() {
    	for(int i=0; i < displayPane.length; i++){
    		IDisplayPane pane = displayPane[i];
    		InputHandlerDefaultImpl mouseHandler=null;
    		InputManager inputMgr;
    		// Enable the mouse inspect adapter
    		switch(i){
    		case NsharpConstants.DISPLAY_SKEWT:
    		default:
        		skewtPaneMouseHandler = new NsharpSkewTPaneMouseHandler(this, pane);
    			mouseHandler = skewtPaneMouseHandler;
    			inputMgr = skewtInputManager;
    			break;
    		case NsharpConstants.DISPLAY_WITO:
    			/*witoPaneMouseHandler = new NsharpAbstractMouseHandler(this,pane);
    			mouseHandler =witoPaneMouseHandler;
    			inputMgr =witoInputManager;*/
    			continue;
    		case NsharpConstants.DISPLAY_HODO:
    			hodoPaneMouseHandler = new NsharpHodoPaneMouseHandler(this,pane);
    			mouseHandler =hodoPaneMouseHandler;
    			inputMgr =hodoInputManager;
    			break;
    		case NsharpConstants.DISPLAY_TIMESTN:
    			timeStnPaneMouseHandler = new NsharpTimeStnPaneMouseHandler(this,pane);
    			mouseHandler =timeStnPaneMouseHandler;
    			inputMgr =timeStnInputManager;
    			break;
    		case NsharpConstants.DISPLAY_INSET:
    			insetPaneMouseHandler = new NsharpAbstractMouseHandler(this,pane);
    			mouseHandler =insetPaneMouseHandler;
    			inputMgr =insetInputManager;
    			break;
    		case NsharpConstants.DISPLAY_DATA:
    			dataPaneMouseHandler = new NsharpDataPaneMouseHandler(this,pane);
    			mouseHandler =dataPaneMouseHandler;
    			inputMgr =dataInputManager;
    			break;
    			
    		}

    		inputMgr.registerMouseHandler(mouseHandler);

    		pane.addListener(SWT.MouseUp, inputMgr);
    		pane.addListener(SWT.MouseDown, inputMgr);
    		pane.addListener(SWT.MouseMove, inputMgr);
    		pane.addListener(SWT.MouseWheel, inputMgr);
    		pane.addListener(SWT.MouseHover, inputMgr);
    		pane.addListener(SWT.MouseEnter, inputMgr);
    		pane.addListener(SWT.MouseExit, inputMgr);
    		pane.addListener(SWT.MenuDetect, inputMgr);
    		pane.addListener(SWT.KeyUp, inputMgr);
    		pane.addListener(SWT.KeyDown, inputMgr);       
    	}
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
       // a new instance, do the registration
        EditorManager.registerEditorNumber(editorNum);
        this.setTabTitle(editorNum+"-NsharpEditor");
    }


    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
    	if(selectedPane!= null)
    		selectedPane.setFocus();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#dispose()
     */
    @Override
    public void dispose() {
    	//System.out.println("NsharpEditor disposed!! "  );
    	if (EditorManager.unregisterEditorNumber(editorNum) ==0 ){
    		super.dispose();
    		synchronized (this) {
    			if (skewtPaneMouseHandler != null && skewtInputManager != null) {
    				skewtPaneMouseHandler.setEditor(null);
    				skewtInputManager.unregisterMouseHandler(skewtPaneMouseHandler);
    				skewtPaneMouseHandler = null;
    				skewtInputManager = null;
    			}
    			if (hodoPaneMouseHandler != null && hodoInputManager != null) {
    				hodoPaneMouseHandler.setEditor(null);
    				hodoInputManager.unregisterMouseHandler(hodoPaneMouseHandler);
    				hodoPaneMouseHandler = null;
    				hodoInputManager = null;
    			}
    			if (dataPaneMouseHandler != null && dataInputManager != null) {
    				dataPaneMouseHandler.setEditor(null);
    				dataInputManager.unregisterMouseHandler(dataPaneMouseHandler);
    				dataPaneMouseHandler = null;
    				dataInputManager = null;
    			}
    			if (insetPaneMouseHandler != null && insetInputManager != null) {
    				insetPaneMouseHandler.setEditor(null);
    				insetInputManager.unregisterMouseHandler(insetPaneMouseHandler);
    				insetPaneMouseHandler = null;
    				insetInputManager = null;
    			}
    			/*if (witoPaneMouseHandler != null && witoInputManager != null) {
    				witoPaneMouseHandler.setEditor(null);
    				witoInputManager.unregisterMouseHandler(witoPaneMouseHandler);
    				witoPaneMouseHandler = null;
    				witoInputManager = null;
    			}*/
    			if (timeStnPaneMouseHandler != null && timeStnInputManager != null) {
    				timeStnPaneMouseHandler.setEditor(null);
    				timeStnInputManager.unregisterMouseHandler(timeStnPaneMouseHandler);
    				timeStnPaneMouseHandler = null;
    				timeStnInputManager = null;
    			}

    			editorNum=0;
    			if(rscHandler!=null)
    				rscHandler.disposeInternal();
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
        //System.out.println("NsharpEditor refresh called");
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

    public void resetGraph(){
    	for(int i=0; i < NsharpConstants.DISPLAY_TOTAL; i++){
    		displayPane[i].getRenderableDisplay().getExtent().reset();
    		displayPane[i].getRenderableDisplay().zoom(1);
    		displayPane[i].getRenderableDisplay().refresh();
    	}
    	if(rscHandler!=null && rscHandler.getWitoPaneRsc()!=null)
    		rscHandler.getWitoPaneRsc().createAllWireFrameShapes();
    }
    public void registerMouseHandler(IInputHandler handler, IInputHandler.InputPriority priority) {
        skewtInputManager.registerMouseHandler(handler, priority);
    }
    
    /**
     * Register a mouse handler to a map
     * 
     * @param handler
     *            the handler to register
     */
    @Override
    public void registerMouseHandler(IInputHandler handler) {
        skewtInputManager.registerMouseHandler(handler);
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
        if (skewtInputManager != null) {
            skewtInputManager.unregisterMouseHandler(handler);
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
        return skewtInputManager;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.editor.AbstractEditor#getActiveDisplayPane()
     */
    @Override
    public IDisplayPane getActiveDisplayPane() {
    	return selectedPane;
        //return this.displayPane[0];
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
    }
 
    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        //
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
	
	
	public static NsharpEditor createOrOpenEditor(  ) {
		NsharpEditor editor = getActiveNsharpEditor();
		if (editor != null) {
			//System.out.println("createOrOpenSkewTEditor return editor from getActiveNsharpEditor");
			//PlatformUI.getWorkbench().getActiveWorkbenchWindow()
			//        .getActivePage().bringToTop(editor);
			return editor;
		} else {
			//System.out.println("createOrOpenSkewTEditor....... ");
			try {
				displayArray = new IRenderableDisplay[NsharpConstants.DISPLAY_TOTAL];
				displayArray[NsharpConstants.DISPLAY_SKEWT]= new NsharpSkewTPaneDisplay(new PixelExtent(NsharpConstants.SKEWT_DISPLAY_REC),NsharpConstants.DISPLAY_SKEWT);
				displayArray[NsharpConstants.DISPLAY_WITO]= new NsharpWitoPaneDisplay(new PixelExtent(NsharpConstants.WITO_DISPLAY_REC),NsharpConstants.DISPLAY_WITO);
				displayArray[NsharpConstants.DISPLAY_HODO]= new NsharpHodoPaneDisplay(new PixelExtent(NsharpConstants.HODO_DISPLAY_REC),NsharpConstants.DISPLAY_HODO);
				displayArray[NsharpConstants.DISPLAY_TIMESTN]= new NsharpTimeStnPaneDisplay(new PixelExtent(NsharpConstants.TIMESTN_DISPLAY_REC),NsharpConstants.DISPLAY_TIMESTN);
				displayArray[NsharpConstants.DISPLAY_DATA]= new NsharpDataPaneDisplay(new PixelExtent(NsharpConstants.DATA_DISPLAY_REC),NsharpConstants.DISPLAY_DATA);
				displayArray[NsharpConstants.DISPLAY_INSET]= new NsharpInsetPaneDisplay(new PixelExtent(NsharpConstants.INSET_DISPLAY_REC),NsharpConstants.DISPLAY_INSET);
								
				EditorInput edInput = new EditorInput(new NCLoopProperties(),
						displayArray);

				//System.out.println("createOrOpenEditor creating new one");
				editor = (NsharpEditor) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
							.openEditor(edInput, EDITOR_ID);
				//Note: NsharpResourceHandler should be created after editor is created, so all display pane properties and
				// pane resource are also constructed
				rscHandler = new NsharpResourceHandler(displayArray);
				ResourcePair skewtRscPair =  displayArray[NsharpConstants.DISPLAY_SKEWT].getDescriptor().getResourceList().get(0);
				if (skewtRscPair.getResource() instanceof NsharpSkewTPaneResource){
					NsharpSkewTPaneResource skewtPaneRsc = (NsharpSkewTPaneResource)skewtRscPair.getResource() ;
					skewtPaneRsc.setRscHandler(rscHandler);
				}
				ResourcePair witoRscPair =  displayArray[NsharpConstants.DISPLAY_WITO].getDescriptor().getResourceList().get(0);
				if (witoRscPair.getResource() instanceof NsharpWitoPaneResource){
					NsharpWitoPaneResource witoPaneRsc = (NsharpWitoPaneResource)witoRscPair.getResource() ;
					witoPaneRsc.setRscHandler(rscHandler);
				}
				ResourcePair hodoRscPair =  displayArray[NsharpConstants.DISPLAY_HODO].getDescriptor().getResourceList().get(0);
				if (hodoRscPair.getResource() instanceof NsharpHodoPaneResource){
					NsharpHodoPaneResource hodoPaneRsc = (NsharpHodoPaneResource)hodoRscPair.getResource() ;
					hodoPaneRsc.setRscHandler(rscHandler);
				}
				ResourcePair timeStnRscPair =  displayArray[NsharpConstants.DISPLAY_TIMESTN].getDescriptor().getResourceList().get(0);
				if (timeStnRscPair.getResource() instanceof NsharpTimeStnPaneResource){
					NsharpTimeStnPaneResource timeStnPaneRsc = (NsharpTimeStnPaneResource)timeStnRscPair.getResource() ;
					timeStnPaneRsc.setRscHandler(rscHandler);
				}
				ResourcePair dataRscPair =  displayArray[NsharpConstants.DISPLAY_DATA].getDescriptor().getResourceList().get(0);
				if (dataRscPair.getResource() instanceof NsharpDataPaneResource){
					NsharpDataPaneResource dataPaneRsc = (NsharpDataPaneResource)dataRscPair.getResource() ;
					dataPaneRsc.setRscHandler(rscHandler);
				}
				ResourcePair insetRscPair =  displayArray[NsharpConstants.DISPLAY_INSET].getDescriptor().getResourceList().get(0);
				if (insetRscPair.getResource() instanceof NsharpInsetPaneResource){
					NsharpInsetPaneResource insetPaneRsc = (NsharpInsetPaneResource)insetRscPair.getResource() ;
					insetPaneRsc.setRscHandler(rscHandler);
				}
				return editor;
			} catch (PartInitException e) {
				UiPlugin.getDefault()
				.getLog()
				.log(new Status(Status.ERROR, UiPlugin.PLUGIN_ID,
						"Error constituting NsharpEditor", e));
			}
		}
		return null;
	}
	public static void bringEditorToTop(  ) {
		NsharpEditor editor = getActiveNsharpEditor();
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

	public VizDisplayPane getSelectedPane() {
		return selectedPane;
	}
	public void setSelectedPane(VizDisplayPane selectedPane) {
		this.selectedPane = selectedPane;
	}
    
	public int getBaseWidth() {
		return baseWidth;
	}
	public int getBaseHeight() {
		return baseHeight;
	}
    
}