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

import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpAbstractPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpDataPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpHodoPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpInsetPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpPartListener;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpSkewTPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpSpcGraphsPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpTimeStnPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpWitoPaneResource;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpPaletteWindow;
import gov.noaa.nws.ncep.ui.pgen.tools.InputHandlerDefaultImpl;
import gov.noaa.nws.ncep.viz.common.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.common.EditorManager;
import gov.noaa.nws.ncep.viz.ui.display.NCLoopProperties;

import java.util.List;

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
import com.raytheon.uf.viz.core.IRenderableDisplayChangedListener;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.EditorInput;
import com.raytheon.viz.ui.input.InputManager;
import com.raytheon.viz.ui.panes.PaneManager;
import com.raytheon.viz.ui.panes.VizDisplayPane;
import com.vividsolutions.jts.geom.Coordinate;

public class NsharpEditor extends AbstractEditor implements AddListener,
        RemoveListener, AbstractNcEditor, IRenderableDisplayChangedListener {
	private boolean restarting= false;
	private final int DISPLAY_SKEWT =0; // always set it as first display, dont change it
	private  int DISPLAY_WITO;
	private  int DISPLAY_INSET;
	private  int DISPLAY_HODO;
	private  int DISPLAY_TIMESTN;
	private  int DISPLAY_DATA;
	private  int DISPLAY_SPC_GRAPHS;
	private  int DISPLAY_FUTURE;
	private  int DISPLAY_TOTAL;
    public static final String EDITOR_ID = "gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor";
    private  int editorNum=0;
    private  NsharpResourceHandler rscHandler;
    private int baseWidth;
    private int baseHeight;
    private double skewTHeightHintRatio;
    private double skewTWidthHintRatio;
    private double witoHeightHintRatio;
    private double witoWidthHintRatio;
    private double hodoHeightHintRatio;
    private double hodoWidthHintRatio;
    private double insetHeightHintRatio;
    private double insetWidthHintRatio;
    private double timeStnHeightHintRatio;
    private double timeStnWidthHintRatio;
    private double dataHeightHintRatio;
    private double dataWidthHintRatio;
    private double leftGroupWidthRatio;
    private double leftTopGroupHeightRatio;//, rightTopGroupHeightRatio;
    private double topGroupHeightRatio, botGroupHeightRatio;
    private int skewTHeightHint;
    private int skewTWidthHint;
    private int witoHeightHint;
    private int witoWidthHint;
    private int hodoHeightHint;
    private int hodoWidthHint;
    private int insetHeightHint;
    private int insetWidthHint;
    private int timeStnHeightHint;
    private int timeStnWidthHint;
    private int dataHeightHint;
    private int dataWidthHint;
    private int spcHeightHint;
    private int spcWidthHint;
    private int futureHeightHint;
    private int futureWidthHint;
    private  String paneConfigurationName;
    private  IRenderableDisplay[] displayArray;
    private ResizeListener resizeLsner;
	public NsharpResourceHandler getRscHandler() {
		return rscHandler;
	}
	public int getEditorNum() {
		return editorNum;
	}
	//Note: nsharpComp used to store composite for each pane.  
    private Composite[] nsharpComp;// = new Composite[DISPLAY_TOTAL];
    private GridData leftGpGd,topGpGd, botGpGd;
    private GridData rightGpGd;
    private GridData rightTopGpGd,leftTopGpGd, leftBotGpGd;
    private Composite parantComp,  baseComposite;
    private Group rightTopGp=null, leftTopGp=null, leftBotGp=null, leftGp, rightGp, topGp, botGp;
	/**  input managers */
    protected InputManager skewtInputManager;
    //protected InputManager witoInputManager;
    protected InputManager hodoInputManager;
    protected InputManager timeStnInputManager;
    protected InputManager dataInputManager;
    protected InputManager insetInputManager;
    protected InputManager spcGraphsInputManager;
    /** The activated context, else null if not activated. */
    //protected IContextActivation contextActivation;

    private NsharpSkewTPaneMouseHandler skewtPaneMouseHandler = null;
    //private NsharpAbstractMouseHandler witoPaneMouseHandler = null;
    private NsharpHodoPaneMouseHandler hodoPaneMouseHandler = null;
    private NsharpTimeStnPaneMouseHandler timeStnPaneMouseHandler = null;
    private NsharpDataPaneMouseHandler dataPaneMouseHandler = null;
    private NsharpAbstractMouseHandler insetPaneMouseHandler = null;
    private NsharpAbstractMouseHandler spcGraphsPaneMouseHandler = null;
 
    protected VizDisplayPane displayPane[];//= new VizDisplayPane[DISPLAY_TOTAL];
    protected VizDisplayPane selectedPane;
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
    	parantComp = comp; 
    	if(baseComposite!= null)
    		baseComposite.dispose();
    	baseComposite = new Composite(comp, SWT.NONE);
    	final GridLayout mainGL;
        baseHeight = baseComposite.getSize().y;
        baseWidth = baseComposite.getSize().x;
        //System.out.println("createPartControl...baseComposite w= " + baseWidth + " h= "+ baseHeight);
        
		//TBD for testing, hard code  now
		//paneCfgNum= NsharpConstants.PANE_CONFIGURATION_1;
        resizeLsner = new ResizeListener(paneConfigurationName);
		baseComposite.addListener(SWT.Resize, resizeLsner);
		if(paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_2_STR)){
			mainGL = new GridLayout(2, false);
	        baseComposite.setLayout(mainGL);
	        mainGL.horizontalSpacing = 0;
	        mainGL.marginHeight = 0;
			leftGroupWidthRatio =NsharpConstants.PANE_DEF_CFG_2_LEFT_GP_WIDTH_RATIO;
			leftTopGroupHeightRatio = NsharpConstants.PANE_DEF_CFG_2_LEFT_TOP_GP_HEIGHT_RATIO;
			//rightTopGroupHeightRatio = NsharpConstants.PANE_DEF_CFG_2_RIGHT_TOP_GP_HEIGHT_RATIO;
			skewTHeightHintRatio=NsharpConstants.PANE_DEF_CFG_2_SKEWT_HEIGHT_RATIO;
		    skewTWidthHintRatio=NsharpConstants.PANE_DEF_CFG_2_SKEWT_WIDTH_RATIO;
		    witoHeightHintRatio=NsharpConstants.PANE_DEF_CFG_2_WITO_HEIGHT_RATIO;
		    witoWidthHintRatio=NsharpConstants.PANE_DEF_CFG_2_WITO_WIDTH_RATIO;
		    hodoHeightHintRatio=NsharpConstants.PANE_DEF_CFG_2_HODO_HEIGHT_RATIO;
		    hodoWidthHintRatio=NsharpConstants.PANE_DEF_CFG_2_HODO_WIDTH_RATIO;
		    insetHeightHintRatio=NsharpConstants.PANE_DEF_CFG_2_INSET_HEIGHT_RATIO;
		    insetWidthHintRatio=NsharpConstants.PANE_DEF_CFG_2_INSET_WIDTH_RATIO;
		    timeStnHeightHintRatio=NsharpConstants.PANE_DEF_CFG_2_TIMESTN_HEIGHT_RATIO;
		    timeStnWidthHintRatio=NsharpConstants.PANE_DEF_CFG_2_TIMESTN_WIDTH_RATIO;
		    dataHeightHintRatio=NsharpConstants.PANE_DEF_CFG_2_DATA_HEIGHT_RATIO;
		    dataWidthHintRatio=NsharpConstants.PANE_DEF_CFG_2_DATA_WIDTH_RATIO;
			createDefConfig2( baseComposite) ;
		} else if(paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_1_STR)){
			mainGL = new GridLayout(2, false);
			mainGL.horizontalSpacing = 0;
	        mainGL.marginHeight = 0;
			baseComposite.setLayout(mainGL);
			leftGroupWidthRatio =NsharpConstants.PANE_DEF_CFG_1_LEFT_GP_WIDTH_RATIO;
			leftTopGroupHeightRatio = NsharpConstants.PANE_DEF_CFG_1_LEFT_TOP_GP_HEIGHT_RATIO;
			//rightTopGroupHeightRatio = NsharpConstants.PANE_DEF_CFG_1_RIGHT_TOP_GP_HEIGHT_RATIO;
			skewTHeightHintRatio=NsharpConstants.PANE_DEF_CFG_1_SKEWT_HEIGHT_RATIO;
		    skewTWidthHintRatio=NsharpConstants.PANE_DEF_CFG_1_SKEWT_WIDTH_RATIO;
		    witoHeightHintRatio=NsharpConstants.PANE_DEF_CFG_1_WITO_HEIGHT_RATIO;
		    witoWidthHintRatio=NsharpConstants.PANE_DEF_CFG_1_WITO_WIDTH_RATIO;
		    hodoHeightHintRatio=NsharpConstants.PANE_DEF_CFG_1_HODO_HEIGHT_RATIO;
		    hodoWidthHintRatio=NsharpConstants.PANE_DEF_CFG_1_HODO_WIDTH_RATIO;
		    insetHeightHintRatio=NsharpConstants.PANE_DEF_CFG_1_INSET_HEIGHT_RATIO;
		    insetWidthHintRatio=NsharpConstants.PANE_DEF_CFG_1_INSET_WIDTH_RATIO;
		    timeStnHeightHintRatio=NsharpConstants.PANE_DEF_CFG_1_TIMESTN_HEIGHT_RATIO;
		    timeStnWidthHintRatio=NsharpConstants.PANE_DEF_CFG_1_TIMESTN_WIDTH_RATIO;
		    dataHeightHintRatio=NsharpConstants.PANE_DEF_CFG_1_DATA_HEIGHT_RATIO;
		    dataWidthHintRatio=NsharpConstants.PANE_DEF_CFG_1_DATA_WIDTH_RATIO;
			createDefConfig1( baseComposite) ;
		} else if(paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)){
			mainGL = new GridLayout(1, true);
			mainGL.horizontalSpacing = 0;
	        mainGL.marginHeight = 0;
			baseComposite.setLayout(mainGL);
			topGroupHeightRatio =NsharpConstants.PANE_SPCWS_CFG_TOP_GP_HEIGHT_RATIO;
			botGroupHeightRatio = NsharpConstants.PANE_SPCWS_CFG_BOT_GP_HEIGHT_RATIO;
			skewTHeightHintRatio=NsharpConstants.PANE_SPCWS_CFG_SKEWT_HEIGHT_RATIO;
		    skewTWidthHintRatio=NsharpConstants.PANE_SPCWS_CFG_SKEWT_WIDTH_RATIO;
		    witoHeightHintRatio=NsharpConstants.PANE_SPCWS_CFG_WITO_HEIGHT_RATIO;
		    witoWidthHintRatio=NsharpConstants.PANE_SPCWS_CFG_WITO_WIDTH_RATIO;
		    hodoHeightHintRatio=NsharpConstants.PANE_SPCWS_CFG_HODO_HEIGHT_RATIO;
		    hodoWidthHintRatio=NsharpConstants.PANE_SPCWS_CFG_HODO_WIDTH_RATIO;
		    insetHeightHintRatio=NsharpConstants.PANE_SPCWS_CFG_INSET_HEIGHT_RATIO;
		    insetWidthHintRatio=NsharpConstants.PANE_SPCWS_CFG_INSET_WIDTH_RATIO;
		    dataHeightHintRatio=NsharpConstants.PANE_SPCWS_CFG_DATA_HEIGHT_RATIO;
		    dataWidthHintRatio=NsharpConstants.PANE_SPCWS_CFG_DATA_WIDTH_RATIO;
		    createSPCWsConfig( baseComposite) ;
		} else if(paneConfigurationName.equals(NsharpConstants.PANE_SIMPLE_D2D_CFG_STR)){
			mainGL = new GridLayout(1, true);
			mainGL.horizontalSpacing = 0;
	        mainGL.marginHeight = 0;
			baseComposite.setLayout(mainGL);
			topGroupHeightRatio  =NsharpConstants.PANE_SIMPLE_D2D_CFG_TOP_GP_HEIGHT_RATIO;
			botGroupHeightRatio = 1-topGroupHeightRatio;
			skewTHeightHintRatio=NsharpConstants.PANE_SIMPLE_D2D_CFG_SKEWT_HEIGHT_RATIO;
		    skewTWidthHintRatio=NsharpConstants.PANE_SIMPLE_D2D_CFG_SKEWT_WIDTH_RATIO;
		    timeStnHeightHintRatio=NsharpConstants.PANE_SIMPLE_D2D_CFG_TIMESTN_HEIGHT_RATIO;
		    timeStnWidthHintRatio=NsharpConstants.PANE_SIMPLE_D2D_CFG_TIMESTN_WIDTH_RATIO;
		    hodoHeightHintRatio=NsharpConstants.PANE_SIMPLE_D2D_CFG_HODO_HEIGHT_RATIO;
		    hodoWidthHintRatio=NsharpConstants.PANE_SIMPLE_D2D_CFG_HODO_WIDTH_RATIO;
		    dataHeightHintRatio=NsharpConstants.PANE_SIMPLE_D2D_CFG_DATA_HEIGHT_RATIO;
		    dataWidthHintRatio=NsharpConstants.PANE_SIMPLE_D2D_CFG_DATA_WIDTH_RATIO;
		    createSimpleD2DConfig(baseComposite);
		}
		skewtInputManager = new InputManager(this);
		//witoInputManager = new InputManager(this);
		timeStnInputManager = new InputManager(this);
		hodoInputManager = new InputManager(this);
		dataInputManager = new InputManager(this);
		insetInputManager = new InputManager(this);
		spcGraphsInputManager = new InputManager(this);
		try {
			for(int i=0; i < displayPane.length; i++){
				if (displayPane[i] == null && nsharpComp [i]!=null) {
					displayPane[i] = new VizDisplayPane(this, nsharpComp[i],
							displaysToLoad[i]);
					
					
					displayPane[i].setRenderableDisplay(displaysToLoad[i]);
					registerListener(displayPane[i]);
				}
			}
			registerHandlers();
			//set default selected pane to skewT pane
			selectedPane = displayPane[DISPLAY_SKEWT];
			for(int i=0; i < displayPane.length; i++){
				if(displayPane[i] != null)
					displayPane[i].addListener( SWT.MouseEnter, new PaneMouseListener(i));
			}
		}
        catch (Exception e) {
            final String errMsg = "Error setting up NsharpEditor";
            UFStatus.getHandler().handle(Priority.SIGNIFICANT,  errMsg, e);
        }
        if(!restarting)
        	contributePerspectiveActions();

    }
    private void createSPCWsConfig(Composite comp) {      
        topGp = new Group(baseComposite, SWT.NONE);
        topGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        topGp.setLayoutData(topGpGd);
        GridLayout topGpLayout = new GridLayout(3, false);
        topGpLayout.marginWidth = 0;
        topGpLayout.marginHeight = 0;
        topGpLayout.verticalSpacing = 0;
        topGp.setLayout(topGpLayout);
        
     // skewt composite
        Composite skewtComp = new Composite(topGp, SWT.NONE);
        GridData skewtGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        skewtComp.setLayoutData(skewtGd);
        GridLayout skewtLayout = new GridLayout(1, true);
        skewtLayout.marginWidth = 0;
        skewtLayout.marginHeight = 0;
        skewtLayout.verticalSpacing = 0;
        skewtComp.setLayout(skewtLayout);
        nsharpComp[DISPLAY_SKEWT] = skewtComp;
        // wito composite
        Composite witoComp = new Composite(topGp, SWT.NONE);
        GridData witoGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        witoComp.setLayoutData(witoGd);
        GridLayout witoLayout = new GridLayout(1, true);
        witoLayout.marginWidth = 0;
        witoLayout.marginHeight = 0;
        witoLayout.verticalSpacing = 0;
        witoComp.setLayout(witoLayout);
        nsharpComp[DISPLAY_WITO] = witoComp;
        
        
      // right-top group : right part of top group
        rightTopGp = new Group(topGp, SWT.NONE);
        rightTopGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        rightTopGp.setLayoutData(rightTopGpGd);
        GridLayout rightTopGpLayout = new GridLayout(1, true);
        rightTopGpLayout.marginWidth = 0;
        rightTopGpLayout.marginHeight = 0;
        rightTopGpLayout.verticalSpacing=0;
        rightTopGp.setLayout(rightTopGpLayout);
        
      //hodo composite
        Composite hodoComp = new Composite(rightTopGp, SWT.NONE);
        GridData hodoGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        hodoComp.setLayoutData(hodoGd);
        GridLayout hodoLayout = new GridLayout(1, true);
        hodoLayout.marginHeight =0;
        hodoLayout.marginWidth=0;
        hodoLayout.verticalSpacing=0;
        hodoComp.setLayout(hodoLayout);
		nsharpComp[DISPLAY_HODO] = hodoComp;
		
        //inset composite
        Composite insetComp = new Composite(rightTopGp, SWT.NONE);
        GridData insetGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        //insetGd.heightHint =  1;
        insetComp.setLayoutData(insetGd);
        GridLayout insetLayout = new GridLayout(1, true);
        insetLayout.marginHeight = 0;
        insetLayout.marginWidth = 0;
		insetComp.setLayout(insetLayout);
		nsharpComp[DISPLAY_INSET] = insetComp;
		
		botGp = new Group(baseComposite, SWT.NONE);
        botGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        botGp.setLayoutData(botGpGd);
        GridLayout botGpLayout = new GridLayout(2, true);
        botGpLayout.marginWidth = 0;
        botGpLayout.marginHeight = 0;
        botGpLayout.verticalSpacing = 0;
        botGp.setLayout(botGpLayout);
		//data composite
        Composite dataComp = new Composite(botGp, SWT.NONE);
        GridData dataGd = new GridData(SWT.FILL, SWT.FILL, true,
                false);
        dataComp.setLayoutData(dataGd);
        GridLayout dataLayout = new GridLayout(1, true);
        dataLayout.marginHeight = 0;
        dataLayout.marginWidth=0;
        dataComp.setLayout(dataLayout);
		nsharpComp[DISPLAY_DATA] = dataComp; 
		//spc composite
        Composite spcComp = new Composite(botGp, SWT.NONE);
        GridData spcGd = new GridData(SWT.FILL, SWT.FILL, true,
                false);
        spcComp.setLayoutData(spcGd);
        GridLayout spcLayout = new GridLayout(1, true);
        spcLayout.marginHeight = 0;
        spcLayout.marginWidth=0;
        spcComp.setLayout(spcLayout);
		nsharpComp[DISPLAY_SPC_GRAPHS] = spcComp; 
    }
    private void createSimpleD2DConfig(Composite comp) {      
    	topGp = new Group(baseComposite, SWT.NONE);
        topGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        topGp.setLayoutData(topGpGd);
        GridLayout topGpLayout = new GridLayout(2, false);
        topGpLayout.marginWidth = 0;
        topGpLayout.marginHeight = 0;
        topGpLayout.verticalSpacing = 0;
        topGp.setLayout(topGpLayout);
                
        // skewt composite
        Composite skewtComp = new Composite(topGp, SWT.NONE);
        GridData skewtGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        skewtComp.setLayoutData(skewtGd);
        GridLayout skewtLayout = new GridLayout(1, true);
        skewtLayout.marginWidth = 0;
        skewtLayout.marginHeight = 0;
        skewtLayout.verticalSpacing = 0;
        skewtComp.setLayout(skewtLayout);
        nsharpComp[DISPLAY_SKEWT] = skewtComp;
        
     // right-top group : right part of top group
        rightTopGp = new Group(topGp, SWT.NONE);
        rightTopGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        rightTopGp.setLayoutData(rightTopGpGd);
        GridLayout rightTopGpLayout = new GridLayout(1, true);
        rightTopGpLayout.marginWidth = 0;
        rightTopGpLayout.marginHeight = 0;
        rightTopGpLayout.verticalSpacing=0;
        rightTopGp.setLayout(rightTopGpLayout);
        
      //time-stn composite
        Composite timeStnComp = new Composite(rightTopGp, SWT.NONE);
        GridData timeStnGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        timeStnComp.setLayoutData(timeStnGd);
        GridLayout timeStnLayout = new GridLayout(1, true);
        timeStnLayout.marginHeight =0;
        timeStnLayout.marginWidth=0;
        timeStnLayout.verticalSpacing=0;
        timeStnComp.setLayout(timeStnLayout);
		nsharpComp[DISPLAY_TIMESTN] = timeStnComp;
		
		//future composite
        Composite futureComp = new Composite(rightTopGp, SWT.NONE);
        GridData fuGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        futureComp.setLayoutData(fuGd);
        GridLayout futureLayout = new GridLayout(1, true);
        futureLayout.marginHeight = 0;
        futureLayout.marginWidth=0;
        futureComp.setLayout(futureLayout);
		nsharpComp[DISPLAY_FUTURE] = futureComp; 
		
        botGp = new Group(baseComposite, SWT.NONE);
        botGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        botGp.setLayoutData(botGpGd);
        GridLayout botGpLayout = new GridLayout(2, false);
        botGpLayout.marginWidth = 0;
        botGpLayout.marginHeight = 0;
        botGpLayout.verticalSpacing = 0;
        botGp.setLayout(botGpLayout);
        
      //hodo composite
        Composite hodoComp = new Composite(botGp, SWT.NONE);
        GridData hodoGd = new GridData(SWT.FILL, SWT.FILL, true,
                false);
        hodoComp.setLayoutData(hodoGd);
        GridLayout hodoLayout = new GridLayout(1, true);
        hodoLayout.marginHeight =0;
        hodoLayout.marginWidth=0;
        hodoLayout.verticalSpacing=0;
        hodoComp.setLayout(hodoLayout);
		nsharpComp[DISPLAY_HODO] = hodoComp;
 		
		//data composite
        Composite dataComp = new Composite(botGp, SWT.NONE);
        GridData dataGd = new GridData(SWT.FILL, SWT.FILL, true,
                false);
        dataComp.setLayoutData(dataGd);
        GridLayout dataLayout = new GridLayout(1, true);
        dataLayout.marginHeight = 0;
        dataLayout.marginWidth=0;
        dataComp.setLayout(dataLayout);
		nsharpComp[DISPLAY_DATA] = dataComp; 
    }
    private void createDefConfig2(Composite comp) {      
        leftGp = new Group(baseComposite, SWT.NONE);
        leftGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        leftGp.setLayoutData(leftGpGd);
        GridLayout leftGpLayout = new GridLayout(1, true);
        leftGpLayout.marginWidth = 0;
        leftGpLayout.marginHeight = 0;
        leftGpLayout.verticalSpacing = 0;
        leftGp.setLayout(leftGpLayout);
        
      //left-top group : upper part of left group
        leftTopGp = new Group(leftGp, SWT.NONE);
        leftTopGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
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
        skewtComp.setLayoutData(skewtGd);
        GridLayout skewtLayout = new GridLayout(1, true);
        skewtLayout.marginWidth = 0;
        skewtLayout.marginHeight = 0;
        skewtLayout.verticalSpacing = 0;
        skewtComp.setLayout(skewtLayout);
        nsharpComp[DISPLAY_SKEWT] = skewtComp;
        // wito composite
        Composite witoComp = new Composite(leftTopGp, SWT.NONE);
        GridData witoGd = new GridData(SWT.END, SWT.FILL, false,
                true);
        witoComp.setLayoutData(witoGd);
        GridLayout witoLayout = new GridLayout(1, true);
        witoLayout.marginWidth = 0;
        witoLayout.marginHeight = 0;
        witoLayout.verticalSpacing = 0;
        witoComp.setLayout(witoLayout);
        nsharpComp[DISPLAY_WITO] = witoComp;
        //inset composite
        Composite insetComp = new Composite(leftGp, SWT.NONE);
        GridData insetGd = new GridData(SWT.FILL, SWT.FILL, true,
                false);
        //insetGd.heightHint =  1;
        insetComp.setLayoutData(insetGd);
        GridLayout insetLayout = new GridLayout(1, true);
        insetLayout.marginHeight = 0;
        insetLayout.marginWidth = 0;
		insetComp.setLayout(insetLayout);
		nsharpComp[DISPLAY_INSET] = insetComp;
		//right group
        rightGp = new Group(baseComposite, SWT.NONE);
        rightGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        rightGp.setLayoutData(rightGpGd);
        GridLayout rightGpLayout = new GridLayout(1, true);
        rightGpLayout.marginWidth = 0;
        rightGpLayout.marginHeight = 0;
        rightGpLayout.verticalSpacing=0;
        rightGp.setLayout(rightGpLayout);
        
        //right-top group : upper part of right group
        rightTopGp = new Group(rightGp, SWT.NONE);
        rightTopGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
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
        hodoComp.setLayoutData(hodoGd);
        GridLayout hodoLayout = new GridLayout(1, true);
        hodoLayout.marginHeight =0;
        hodoLayout.marginWidth=0;
        hodoLayout.verticalSpacing=0;
        hodoComp.setLayout(hodoLayout);
		nsharpComp[DISPLAY_HODO] = hodoComp;
		//time-stn composite
        Composite timeStnComp = new Composite(rightTopGp, SWT.NONE);
        GridData timeStnGd = new GridData(SWT.END, SWT.FILL, false,
                true);
        timeStnComp.setLayoutData(timeStnGd);
        GridLayout timeStnLayout = new GridLayout(1, true);
        timeStnLayout.marginHeight =0;
        timeStnLayout.marginWidth=0;
        timeStnLayout.verticalSpacing=0;
        timeStnComp.setLayout(timeStnLayout);
		nsharpComp[DISPLAY_TIMESTN] = timeStnComp;
		//data composite
        Composite dataComp = new Composite(rightGp, SWT.NONE);
        GridData dataGd = new GridData(SWT.FILL, SWT.FILL, true,
                false);
        dataComp.setLayoutData(dataGd);
        GridLayout dataLayout = new GridLayout(1, true);
        dataLayout.marginHeight = 0;
        dataLayout.marginWidth=0;
        dataComp.setLayout(dataLayout);
		nsharpComp[DISPLAY_DATA] = dataComp; 
    }
    private void createDefConfig1(Composite comp) {
        leftGp = new Group(baseComposite, SWT.NONE);
        leftGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        
        leftGp.setLayoutData(leftGpGd);
        GridLayout leftGpLayout = new GridLayout(1, true);
        leftGpLayout.marginWidth = 0;
        leftGpLayout.marginHeight = 0;
        leftGpLayout.verticalSpacing = 0;
        leftGp.setLayout(leftGpLayout);
        
      //left-top group : upper part of left group
        leftTopGp = new Group(leftGp, SWT.NONE);
        leftTopGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
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
        skewtComp.setLayoutData(skewtGd);
        GridLayout skewtLayout = new GridLayout(1, true);
        skewtLayout.marginWidth = 0;
        skewtLayout.marginHeight = 0;
        skewtLayout.verticalSpacing = 0;
        skewtComp.setLayout(skewtLayout);
        nsharpComp[DISPLAY_SKEWT] = skewtComp;
        // wito composite
        Composite witoComp = new Composite(leftTopGp, SWT.NONE);
        GridData witoGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        witoComp.setLayoutData(witoGd);
        GridLayout witoLayout = new GridLayout(1, true);
        witoLayout.marginWidth = 0;
        witoLayout.marginHeight = 0;
        witoLayout.verticalSpacing = 0;
        witoComp.setLayout(witoLayout);
        nsharpComp[DISPLAY_WITO] = witoComp;
       //left-bottom group 
        leftBotGp = new Group(leftGp, SWT.NONE);
        leftBotGpGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        leftBotGp.setLayoutData(leftBotGpGd);
        GridLayout leftBotGpLayout = new GridLayout(2, true);
        leftBotGpLayout.marginWidth = 0;
        leftBotGpLayout.marginHeight = 0;
        leftBotGpLayout.verticalSpacing=0;
        leftBotGp.setLayout(leftBotGpLayout);
      //time-stn composite
        Composite timeStnComp = new Composite(leftBotGp, SWT.NONE);
        GridData timeStnGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        timeStnComp.setLayoutData(timeStnGd);
        GridLayout timeStnLayout = new GridLayout(1, true);
        timeStnLayout.marginHeight =0;
        timeStnLayout.marginWidth=0;
        timeStnLayout.verticalSpacing=0;
        timeStnComp.setLayout(timeStnLayout);
		nsharpComp[DISPLAY_TIMESTN] = timeStnComp;
		//inset composite
        Composite insetComp = new Composite(leftBotGp, SWT.NONE);
        GridData insetGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        //insetGd.heightHint =  1;
        insetComp.setLayoutData(insetGd);
        GridLayout insetLayout = new GridLayout(1, true);
        insetLayout.marginHeight = 0;
        insetLayout.marginWidth = 0;
		insetComp.setLayout(insetLayout);
		nsharpComp[DISPLAY_INSET] = insetComp;
		//right group
        rightGp = new Group(baseComposite, SWT.NONE);
        rightGpGd = new GridData(SWT.END, SWT.FILL, true,
                true);
        
        rightGp.setLayoutData(rightGpGd);
        GridLayout rightGpLayout = new GridLayout(1, true);
        rightGpLayout.marginWidth = 0;
        rightGpLayout.marginHeight = 0;
        rightGpLayout.verticalSpacing=0;
        rightGp.setLayout(rightGpLayout);
        
		//hodo composite
        Composite hodoComp = new Composite(rightGp, SWT.NONE);
        GridData hodoGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        hodoComp.setLayoutData(hodoGd);
        GridLayout hodoLayout = new GridLayout(1, true);
        hodoLayout.marginHeight =0;
        hodoLayout.marginWidth=0;
        hodoLayout.verticalSpacing=0;
        hodoComp.setLayout(hodoLayout);
		nsharpComp[DISPLAY_HODO] = hodoComp;
		//data composite
        Composite dataComp = new Composite(rightGp, SWT.NONE);
        GridData dataGd = new GridData(SWT.FILL, SWT.FILL, true,
                true);
        dataComp.setLayoutData(dataGd);
        GridLayout dataLayout = new GridLayout(1, true);
        dataLayout.marginHeight = 1;
        dataLayout.marginWidth=0;
        dataComp.setLayout(dataLayout);
		nsharpComp[DISPLAY_DATA] = dataComp; 
    }

    protected void registerHandlers() {
    	for(int i=0; i < displayPane.length; i++){
    		if(displayPane[i]!=null){
    			IDisplayPane pane = displayPane[i];
    			InputHandlerDefaultImpl mouseHandler=null;
    			InputManager inputMgr;
    			// Enable the mouse inspect adapter
    			if(i == DISPLAY_SKEWT){
    				skewtPaneMouseHandler = new NsharpSkewTPaneMouseHandler(this, pane);
    				mouseHandler = skewtPaneMouseHandler;
    				inputMgr = skewtInputManager;
    			}
    			else if(i == DISPLAY_HODO) {

    				hodoPaneMouseHandler = new NsharpHodoPaneMouseHandler(this,pane);
    				mouseHandler =hodoPaneMouseHandler;
    				inputMgr =hodoInputManager;
    			}
    			else if(i== DISPLAY_TIMESTN) {
    				timeStnPaneMouseHandler = new NsharpTimeStnPaneMouseHandler(this,pane);
    				mouseHandler =timeStnPaneMouseHandler;
    				inputMgr =timeStnInputManager;
    			}
    			else if(i==  DISPLAY_INSET) {
    				insetPaneMouseHandler = new NsharpAbstractMouseHandler(this,pane);
    				mouseHandler =insetPaneMouseHandler;
    				inputMgr =insetInputManager;
    			}
    			else if(i==  DISPLAY_SPC_GRAPHS) {
    				spcGraphsPaneMouseHandler = new NsharpAbstractMouseHandler(this,pane);
    				mouseHandler =spcGraphsPaneMouseHandler;
    				inputMgr =spcGraphsInputManager;
    			}
    			else if(i==  DISPLAY_DATA){
    				dataPaneMouseHandler = new NsharpDataPaneMouseHandler(this,pane);
    				mouseHandler =dataPaneMouseHandler;
    				inputMgr =dataInputManager;
    			}
    			else
    				continue;

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
    }


    @Override
    public void init(IEditorSite site, IEditorInput editorInput)
            throws PartInitException {
    	//System.out.println("NsharpEditor init!! " + this.toString()  );
    	NsharpConfigManager configMgr = NsharpConfigManager.getInstance();
    	NsharpConfigStore configStore = configMgr.retrieveNsharpConfigStoreFromFs();
    	NsharpGraphProperty graphConfigProperty = configStore.getGraphProperty();
    	paneConfigurationName = graphConfigProperty.getPaneConfigurationName();
    	initDisplayPublicParms();
    	createRenderableDisplayArray();
    	/*
    	 * Chin note: we ignore "Renderable displays" sent with "editorInput" parameter here, as in D2D
    	 * application the "Renderable displays" only contains skewTPaneDisplay.  
    	 * We recreate displayArray and reset them to EditorInput here based on current "paneConfigurationName"
    	 * 
    	 */
    	
        EditorInput edInput  = (EditorInput)editorInput;
        boolean getRscHandlerDone = false;
        // bsteffen: reuse any displays that already exist in the current editorInput
        for(IRenderableDisplay display : edInput.getRenderableDisplays()){
            int index = -1;
            if(display instanceof NsharpSkewTPaneDisplay ){
                index = DISPLAY_SKEWT;
            }else if(display instanceof NsharpWitoPaneDisplay ){
                index = DISPLAY_WITO;
            }else if(display instanceof NsharpInsetPaneDisplay ){
                index = DISPLAY_INSET;
            }else if(display instanceof NsharpHodoPaneDisplay ){
                index = DISPLAY_HODO;
            }else if(display instanceof NsharpTimeStnPaneDisplay ){
                index = DISPLAY_TIMESTN;
            }else if(display instanceof NsharpDataPaneDisplay ){
                index = DISPLAY_DATA;
            }else if(display instanceof NsharpSpcGraphsPaneDisplay ){
                index = DISPLAY_SPC_GRAPHS;
            }else if(display instanceof NsharpAbstractPaneDisplay ){
                index = DISPLAY_FUTURE;
            }
            if(index >= 0){
            	//System.out.println("displayArray["+index+"] was "+ displayArray[index]+" set to "+display.toString());
            	displayArray[index].dispose();
                displayArray[index] = display;
                //Chin: only do this once
                if(!getRscHandlerDone){
                
                	// attempt to find and reuse rscHandler if possible
                	List<NsharpAbstractPaneResource> paneRscs = display.getDescriptor()
                	.getResourceList().getResourcesByTypeAsType(
                			NsharpAbstractPaneResource.class);
                	for (NsharpAbstractPaneResource paneRsc : paneRscs) {
                		NsharpResourceHandler handler = paneRsc.getRscHandler();
                		if (handler != null) {
                			rscHandler = handler;
                			getRscHandlerDone = true;
                        	
                		}
                	}
                	// bsteffen: not sure if this is necessary, getPaneNumber is never used
                	IDescriptor descriptor = display.getDescriptor();
                	if(descriptor instanceof NsharpAbstractPaneDescriptor){
                		((NsharpAbstractPaneDescriptor) descriptor).setPaneNumber(index);
                	}
                }

            }
        }
        
    	edInput.setRenderableDisplays(displayArray);

    	
    	super.init(site, edInput);

    	
        //System.out.println("SkewtEditor  title " + this.getTitle() );
        if (editorNum == 0 ){
        	editorNum = EditorManager.getEditorNumber();
        	
        }
       // a new instance, do the registration
        EditorManager.registerEditorNumber(editorNum);
        this.setTabTitle(editorNum+"-NsharpEditor");
    	
    	//Note: NsharpResourceHandler should be created after editor is created, so all display pane properties and
    	// pane resource are also constructed
		// bsteffen: only create rscHandler if it doesn't exist already
    	if(rscHandler == null){
    	    rscHandler = new NsharpResourceHandler(displayArray,this);
    	}else{
    	    rscHandler.updateDisplay(displayArray, paneConfigurationName);
    	    
    	}
    	NsharpPaletteWindow paletteWin = NsharpPaletteWindow.getInstance();
    	if(paletteWin!=null){
    		paletteWin.restorePaletteWindow(paneConfigurationName, rscHandler.getCurrentGraphMode(),
    				rscHandler.isInterpolateIsOn(), rscHandler.isOverlayIsOn(),
    				rscHandler.isCompareStnIsOn(),rscHandler.isCompareTmIsOn(),rscHandler.isEditGraphOn()); 
    	}
    	createPaneResource();
    	
    	rscHandler.resetData();
    	//  listen for changes to renderable displays
    	addRenderableDisplayChangedListener(this);
    	//add a new part listener if not added yet
		NsharpPartListener.addPartListener();
		
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
    				skewtPaneMouseHandler.disposeCursor();
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
    			if (spcGraphsPaneMouseHandler != null && spcGraphsInputManager != null) {
    				spcGraphsPaneMouseHandler.setEditor(null);
    				spcGraphsInputManager.unregisterMouseHandler(spcGraphsPaneMouseHandler);
    				spcGraphsPaneMouseHandler = null;
    				spcGraphsInputManager = null;
    			}
    			if (timeStnPaneMouseHandler != null && timeStnInputManager != null) {
    				timeStnPaneMouseHandler.setEditor(null);
    				timeStnInputManager.unregisterMouseHandler(timeStnPaneMouseHandler);
    				timeStnPaneMouseHandler = null;
    				timeStnInputManager = null;
    			}

    			editorNum=0;
    			// bsteffen only dispose the rscHandler if not swapping.
    			if(!displayArray[DISPLAY_SKEWT].isSwapping()){
        			if(rscHandler!=null){
    				rscHandler.disposeInternal();
    		}
    	}
                rscHandler = null;
    		}
    	}
    	try{
    		IWorkbenchPage wpage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
    		IViewPart vpart = wpage.findView( "gov.noaa.nws.ncep.ui.nsharp.defaultview1" );
    		wpage.hideView(vpart);
    	}
    	catch(Exception e){
    		
    	}
    	
    	
    }

    public void restartEditor(String paneConfigurationName) {
    	//System.out.println("reStartEditor! "  );
    	this.paneConfigurationName = paneConfigurationName ;
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
    	if (spcGraphsPaneMouseHandler != null && spcGraphsInputManager != null) {
    				spcGraphsPaneMouseHandler.setEditor(null);
    				spcGraphsInputManager.unregisterMouseHandler(spcGraphsPaneMouseHandler);
    				spcGraphsPaneMouseHandler = null;
    				spcGraphsInputManager = null;
    	}
    	if (timeStnPaneMouseHandler != null && timeStnInputManager != null) {
    		timeStnPaneMouseHandler.setEditor(null);
    		timeStnInputManager.unregisterMouseHandler(timeStnPaneMouseHandler);
    		timeStnPaneMouseHandler = null;
    		timeStnInputManager = null;
    	}
    	baseComposite.removeListener(SWT.Resize, resizeLsner);
    	displayArray = null;
    	nsharpComp=null;
    	if(displayPane!=null){
    	for (VizDisplayPane pane: displayPane){
    			if(pane!=null)
    		pane.dispose();
    	}
    	}
    	displayPane = null;
    	rightTopGp= leftTopGp= leftBotGp= leftGp= rightGp= topGp= botGp=null;
    	//createOrUpdateEditor(true);
    	updateEditor();
    	restarting = true;
    	createPartControl(parantComp);
    	editorInput.setRenderableDisplays(getRenderableDisplays());
    	//Chin: note: after reset all resource in editor, editor displays an empty screen. Refresh() does not work.
    	//Therefore, play the following trick. I.e. bring map editor to top and then this editor to top. After this trick,
    	// editor displays normally.
    	parantComp.layout(); // from ben
    	//NsharpMapResource.bringMapEditorToTop();
    	//bringEditorToTop();
    	NsharpPaletteWindow paletteWin = NsharpPaletteWindow.getInstance();
    	if(paletteWin!=null){
    		paletteWin.updateSpcGraphBtn(paneConfigurationName);
    	}
    }
    /**
     * Perform a refresh asynchronously
     * 
     */
    @Override
    public void refresh() {
    	if(displayPane!= null)
        for (IDisplayPane pane : displayPane) {
    			if(pane!=null)
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
    	for(int i=0; i < DISPLAY_TOTAL; i++){
    		if(displayPane[i]!=null){
    			displayPane[i].getRenderableDisplay().getExtent().reset();
    			displayPane[i].getRenderableDisplay().zoom(1.0);
    			displayPane[i].getRenderableDisplay().refresh();
    		}
    	}
    	if(rscHandler!=null) {
    		if(rscHandler.getWitoPaneRsc()!=null){
    		rscHandler.getWitoPaneRsc().createAllWireFrameShapes();
    }
    		//rscHandler.getSkewtPaneRsc().handleZooming();
    	}
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
        //return getDisplayPaneArray();//this.displayPane;
    	// changed for D2D
    	IDisplayPane[] pan = { displayPane[DISPLAY_SKEWT]};
    	return pan;
    }
    public IRenderableDisplay[] getRenderableDisplays() {
        IRenderableDisplay[] displays = new IRenderableDisplay[displayPane.length];
        int i = 0;
        for (IDisplayPane pane : displayPane) {
            displays[i++] = pane.getRenderableDisplay();
        }
        return displays;
    }
    
    @Override
    public IEditorInput getEditorInput() {
       // editorInput.setRenderableDisplays(getRenderableDisplays());
        return editorInput;
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
	private  void initDisplayPublicParms(){
		if(paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)){
			//DISPLAY_SKEWT = 0; 
			DISPLAY_WITO= DISPLAY_SKEWT+1;
			DISPLAY_INSET= DISPLAY_WITO+1;
		    DISPLAY_HODO= DISPLAY_INSET+1;
		    DISPLAY_DATA= DISPLAY_HODO+1;
		    DISPLAY_SPC_GRAPHS= DISPLAY_DATA+1;
		    DISPLAY_TOTAL= DISPLAY_SPC_GRAPHS+1;	
		    DISPLAY_FUTURE = -1;
		    DISPLAY_TIMESTN = -1;
		} else if(paneConfigurationName.equals(NsharpConstants.PANE_SIMPLE_D2D_CFG_STR)){
			//DISPLAY_SKEWT = 0; 
			DISPLAY_TIMESTN= DISPLAY_SKEWT+1;
			DISPLAY_HODO= DISPLAY_TIMESTN+1;
		    DISPLAY_DATA=DISPLAY_HODO+1;
		    DISPLAY_FUTURE= DISPLAY_DATA+1;		
		    DISPLAY_TOTAL= DISPLAY_FUTURE+1;		
		    DISPLAY_WITO= -1;
			DISPLAY_INSET= -1;
			DISPLAY_SPC_GRAPHS = -1;
		} 
		else { // case of default1 and default 2 pane configurations
			//DISPLAY_SKEWT = 0; 
			DISPLAY_WITO= DISPLAY_SKEWT+1;
			DISPLAY_INSET= DISPLAY_WITO+1;
		    DISPLAY_HODO= DISPLAY_INSET+1;
		    DISPLAY_TIMESTN= DISPLAY_HODO+1;
		    DISPLAY_DATA=DISPLAY_TIMESTN+1;
		    DISPLAY_TOTAL= DISPLAY_DATA+1;
		    DISPLAY_FUTURE = -1;
		    DISPLAY_SPC_GRAPHS = -1;
		}
		    nsharpComp = new Composite[DISPLAY_TOTAL];
			displayPane = new VizDisplayPane[DISPLAY_TOTAL];
			displayArray = new IRenderableDisplay[DISPLAY_TOTAL];
	}
	/*
	 * Note: initDisplayPublicParms() should be called before calling this function
	 */
	private  IRenderableDisplay[] createRenderableDisplayArray(){
		if(paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)){
		    
			displayArray[DISPLAY_SKEWT]= new NsharpSkewTPaneDisplay(new PixelExtent(NsharpConstants.SKEWT_DISPLAY_REC),DISPLAY_SKEWT);
			displayArray[DISPLAY_WITO]= new NsharpWitoPaneDisplay(new PixelExtent(NsharpConstants.WITO_DISPLAY_REC),DISPLAY_WITO);
			displayArray[DISPLAY_HODO]= new NsharpHodoPaneDisplay(new PixelExtent(NsharpConstants.HODO_DISPLAY_REC),DISPLAY_HODO);
			displayArray[DISPLAY_DATA]= new NsharpDataPaneDisplay(new PixelExtent(NsharpConstants.DATA_DISPLAY_REC),DISPLAY_DATA);
			displayArray[DISPLAY_INSET]= new NsharpInsetPaneDisplay(new PixelExtent(NsharpConstants.INSET_DISPLAY_REC),DISPLAY_INSET);
			displayArray[DISPLAY_SPC_GRAPHS]= new NsharpSpcGraphsPaneDisplay(new PixelExtent(NsharpConstants.SPC_GRAPH_DISPLAY_REC),DISPLAY_SPC_GRAPHS);	
		} else if(paneConfigurationName.equals(NsharpConstants.PANE_SIMPLE_D2D_CFG_STR)){
			displayArray[DISPLAY_SKEWT]= new NsharpSkewTPaneDisplay(new PixelExtent(NsharpConstants.SKEWT_DISPLAY_REC),DISPLAY_SKEWT);
			displayArray[DISPLAY_HODO]= new NsharpHodoPaneDisplay(new PixelExtent(NsharpConstants.HODO_DISPLAY_REC),DISPLAY_HODO);
			displayArray[DISPLAY_DATA]= new NsharpDataPaneDisplay(new PixelExtent(NsharpConstants.DATA_DISPLAY_REC),DISPLAY_DATA);
			displayArray[DISPLAY_TIMESTN]= new NsharpTimeStnPaneDisplay(new PixelExtent(NsharpConstants.TIMESTN_DISPLAY_REC),DISPLAY_TIMESTN);
			displayArray[DISPLAY_FUTURE]= new NsharpAbstractPaneDisplay(new PixelExtent(NsharpConstants.FUTURE_DISPLAY_REC),DISPLAY_FUTURE);
		}
		else { // case of default1 and default 2 pane configurations
		    displayArray[DISPLAY_SKEWT]= new NsharpSkewTPaneDisplay(new PixelExtent(NsharpConstants.SKEWT_DISPLAY_REC),DISPLAY_SKEWT);
			displayArray[DISPLAY_WITO]= new NsharpWitoPaneDisplay(new PixelExtent(NsharpConstants.WITO_DISPLAY_REC),DISPLAY_WITO);
			displayArray[DISPLAY_HODO]= new NsharpHodoPaneDisplay(new PixelExtent(NsharpConstants.HODO_DISPLAY_REC),DISPLAY_HODO);
			displayArray[DISPLAY_DATA]= new NsharpDataPaneDisplay(new PixelExtent(NsharpConstants.DATA_DISPLAY_REC),DISPLAY_DATA);
			displayArray[DISPLAY_INSET]= new NsharpInsetPaneDisplay(new PixelExtent(NsharpConstants.INSET_DISPLAY_REC),DISPLAY_INSET);
			displayArray[DISPLAY_TIMESTN]= new NsharpTimeStnPaneDisplay(new PixelExtent(NsharpConstants.TIMESTN_DISPLAY_REC),DISPLAY_TIMESTN);
			}
		return displayArray;
			}
	/*
	 * Note: initDisplayPublicParms() and createRenderableDisplayArray() should be called before calling this function
	 */
	private  void createPaneResource(){
			
			ResourcePair skewtRscPair =  displayArray[DISPLAY_SKEWT].getDescriptor().getResourceList().get(0);
		NsharpSkewTPaneResource skewtPaneRsc=null;
			if (skewtRscPair.getResource() instanceof NsharpSkewTPaneResource){
			skewtPaneRsc = (NsharpSkewTPaneResource)skewtRscPair.getResource() ;
				skewtPaneRsc.setRscHandler(rscHandler);
			}
			ResourcePair dataRscPair =  displayArray[DISPLAY_DATA].getDescriptor().getResourceList().get(0);
			if (dataRscPair.getResource() instanceof NsharpDataPaneResource){
				NsharpDataPaneResource dataPaneRsc = (NsharpDataPaneResource)dataRscPair.getResource() ;
				dataPaneRsc.setRscHandler(rscHandler);
			}
			
			ResourcePair hodoRscPair =  displayArray[DISPLAY_HODO].getDescriptor().getResourceList().get(0);
			if (hodoRscPair.getResource() instanceof NsharpHodoPaneResource){
				NsharpHodoPaneResource hodoPaneRsc = (NsharpHodoPaneResource)hodoRscPair.getResource() ;
				hodoPaneRsc.setRscHandler(rscHandler);
			}

			if(paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)|| 
					paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_1_STR)||
					paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_2_STR)){
				ResourcePair witoRscPair =  displayArray[DISPLAY_WITO].getDescriptor().getResourceList().get(0);
				if (witoRscPair.getResource() instanceof NsharpWitoPaneResource){
					NsharpWitoPaneResource witoPaneRsc = (NsharpWitoPaneResource)witoRscPair.getResource() ;
					witoPaneRsc.setRscHandler(rscHandler);
				}


				ResourcePair insetRscPair =  displayArray[DISPLAY_INSET].getDescriptor().getResourceList().get(0);
				if (insetRscPair.getResource() instanceof NsharpInsetPaneResource){
					NsharpInsetPaneResource insetPaneRsc = (NsharpInsetPaneResource)insetRscPair.getResource() ;
					insetPaneRsc.setRscHandler(rscHandler);
				}
			}
			if(paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)){
				ResourcePair spcGraphRscPair =  displayArray[DISPLAY_SPC_GRAPHS].getDescriptor().getResourceList().get(0);
				if (spcGraphRscPair.getResource() instanceof NsharpSpcGraphsPaneResource){
					NsharpSpcGraphsPaneResource spcPaneRsc = (NsharpSpcGraphsPaneResource)spcGraphRscPair.getResource() ;
					spcPaneRsc.setRscHandler(rscHandler);
				}
			}
			if(paneConfigurationName.equals(NsharpConstants.PANE_SIMPLE_D2D_CFG_STR)){
				ResourcePair futureRscPair =  displayArray[DISPLAY_FUTURE].getDescriptor().getResourceList().get(0);
			if (futureRscPair.getResource() instanceof NsharpAbstractPaneResource){
				NsharpAbstractPaneResource futurePaneRsc = (NsharpAbstractPaneResource)futureRscPair.getResource() ;
					futurePaneRsc.setRscHandler(rscHandler);
				}
			}
			if(paneConfigurationName.equals(NsharpConstants.PANE_SIMPLE_D2D_CFG_STR)|| 
					paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_1_STR)||
					paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_2_STR)){
				ResourcePair timeStnRscPair =  displayArray[DISPLAY_TIMESTN].getDescriptor().getResourceList().get(0);
				if (timeStnRscPair.getResource() instanceof NsharpTimeStnPaneResource){
					NsharpTimeStnPaneResource timeStnPaneRsc = (NsharpTimeStnPaneResource)timeStnRscPair.getResource() ;
					timeStnPaneRsc.setRscHandler(rscHandler);
				}
			}
		if(skewtPaneRsc!=null){
			skewtPaneRsc.setCurrentGraphMode(rscHandler.getCurrentGraphMode());
			//skewtPaneRsc.handleResize();
		}
		}
	
	private void updateEditor() {	
		initDisplayPublicParms();
		createRenderableDisplayArray();
		nsharpComp = new Composite[DISPLAY_TOTAL];
		displayPane = new VizDisplayPane[DISPLAY_TOTAL];
		
		EditorInput edInput = new EditorInput(new NCLoopProperties(),
				displayArray);

		this.setInput(edInput);
		this.displaysToLoad = displayArray;
		for (IRenderableDisplay display : displayArray) {
			if (display != null) {
				this.initDisplay(display);
			}
		}
		rscHandler.updateDisplay(displayArray,paneConfigurationName);
		rscHandler.resetRscSoundingData();

		createPaneResource();
		
	}
	public static NsharpEditor createOrOpenEditor(  ) {	
		NsharpEditor editor = getActiveNsharpEditor();
		if (editor != null) {
			//System.out.println("createOrOpenSkewTEditor return existing editor "+ editor.toString());
			return editor;
		} else {
			try {
				/*
				 * Chin note: all initialization logics now are done in init() method to support standard  eclipse
				 * Editor open procedure.
				 * At init(), display array and EditorInput will be created based on user configured  pane configuration. 
				 * Therefore, we just create a dummy EditorInput here.
				 * The following IWorkbenchPage.openEditor() will eventually invoke init() method.
				 */
				IRenderableDisplay[] tempDisp  =  new IRenderableDisplay[1];
				tempDisp[0] = new NsharpSkewTPaneDisplay(new PixelExtent(NsharpConstants.SKEWT_DISPLAY_REC),0);
				EditorInput edInput = new EditorInput(new NCLoopProperties(),
						tempDisp);
				editor = (NsharpEditor) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
				.openEditor(edInput, EDITOR_ID);
				
			} catch (PartInitException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			//System.out.println("createOrOpenSkewTEditor return new editor "+ editor.toString());
			return editor;
		}
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
    
	class PaneMouseListener implements Listener {
		private int paneIndex;
		
		public PaneMouseListener(int index) {
			super();
			this.paneIndex = index;
		}

		@Override
		public void handleEvent(Event e) {						
			if ( e.button==0 ) {
				selectedPane = displayPane[paneIndex];
			}
		}
	}

	class ResizeListener implements Listener {
		private String paneConfigurationName="";
		
		
		public ResizeListener(String name) {
			super();
			this.paneConfigurationName = name;
		}


		@Override
        public void handleEvent(Event event) {
			baseHeight = baseComposite.getSize().y;
	        baseWidth = baseComposite.getSize().x;
	        //System.out.println("ResizeListener resizing...nsharp base w= " + baseWidth + " h= "+ baseHeight);
	        if(paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_2_STR)){
	        	skewTHeightHint = (int) (baseHeight * skewTHeightHintRatio);
	        	skewTWidthHint = (int) (baseWidth*leftGroupWidthRatio *skewTWidthHintRatio);
	        	witoHeightHint = (int) (baseHeight * witoHeightHintRatio);
	        	witoWidthHint = (int) (baseWidth*leftGroupWidthRatio *witoWidthHintRatio);
	        	hodoHeightHint = (int) (baseHeight * hodoHeightHintRatio);
	        	hodoWidthHint = (int) (baseWidth*(1-leftGroupWidthRatio) *hodoWidthHintRatio);
	        	insetHeightHint = (int) (baseHeight * insetHeightHintRatio);
	        	insetWidthHint = (int) (baseWidth*(leftGroupWidthRatio) *insetWidthHintRatio);
	        	timeStnHeightHint = (int) (baseHeight * timeStnHeightHintRatio);
	        	timeStnWidthHint = (int) (baseWidth*(1-leftGroupWidthRatio) *timeStnWidthHintRatio);
	        	dataHeightHint = (int) (baseHeight * dataHeightHintRatio);
	        	dataWidthHint = (int) (baseWidth*(1-leftGroupWidthRatio) *dataWidthHintRatio);
	        } else if(paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_1_STR)){			
				skewTHeightHint = (int) (baseHeight * skewTHeightHintRatio);
	        	skewTWidthHint = (int) (baseWidth*leftGroupWidthRatio *skewTWidthHintRatio);
	        	witoHeightHint = (int) (baseHeight * witoHeightHintRatio);
	        	witoWidthHint = (int) (baseWidth*leftGroupWidthRatio *witoWidthHintRatio);
	        	hodoHeightHint = (int) (baseHeight * hodoHeightHintRatio);
	        	hodoWidthHint = (int) (baseWidth*(1-leftGroupWidthRatio) *hodoWidthHintRatio);
	        	insetHeightHint = (int) (baseHeight * insetHeightHintRatio);
	        	insetWidthHint = (int) (baseWidth*(leftGroupWidthRatio) *insetWidthHintRatio);
	        	timeStnHeightHint = (int) (baseHeight * timeStnHeightHintRatio);
	        	timeStnWidthHint = (int) (baseWidth*(leftGroupWidthRatio) *timeStnWidthHintRatio);
	        	dataHeightHint = (int) (baseHeight * dataHeightHintRatio);
	        	dataWidthHint = (int) (baseWidth*(1-leftGroupWidthRatio) *dataWidthHintRatio);
			}
	        else if(paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)){			
	        	skewTHeightHint = (int) (baseHeight * topGroupHeightRatio* skewTHeightHintRatio);
	        	skewTWidthHint = (int) (baseWidth*skewTWidthHintRatio);
	        	witoHeightHint = (int) (baseHeight * topGroupHeightRatio * witoHeightHintRatio);
	        	witoWidthHint = (int) (baseWidth*witoWidthHintRatio);
	        	hodoHeightHint = (int) (baseHeight *topGroupHeightRatio* hodoHeightHintRatio);
	        	hodoWidthHint = (int) (baseWidth*hodoWidthHintRatio);
	        	insetHeightHint = (int) (baseHeight * topGroupHeightRatio* insetHeightHintRatio);
	        	insetWidthHint = (int) (baseWidth*insetWidthHintRatio);
	        	dataHeightHint = (int) (baseHeight * botGroupHeightRatio*dataHeightHintRatio);
	        	dataWidthHint = (int) (baseWidth*dataWidthHintRatio);
	        	spcHeightHint = (int) (baseHeight * botGroupHeightRatio*dataHeightHintRatio);
	        	spcWidthHint = (int) (baseWidth*dataWidthHintRatio);
			} 
	        else if(paneConfigurationName.equals(NsharpConstants.PANE_SIMPLE_D2D_CFG_STR)){
	        	skewTHeightHint = (int) (baseHeight * topGroupHeightRatio* skewTHeightHintRatio);
	        	skewTWidthHint = (int) (baseWidth*skewTWidthHintRatio);
	        	timeStnHeightHint = (int) (baseHeight * topGroupHeightRatio* timeStnHeightHintRatio);
	        	timeStnWidthHint = (int) (baseWidth* timeStnWidthHintRatio);   	
	        	futureHeightHint = (int) (baseHeight * topGroupHeightRatio* (1-timeStnHeightHintRatio));
	        	futureWidthHint = timeStnWidthHint;
	        	dataHeightHint = (int) (baseHeight * botGroupHeightRatio*dataHeightHintRatio);
	        	dataWidthHint = (int) (baseWidth* dataWidthHintRatio);
	        	hodoHeightHint = (int) (baseHeight *botGroupHeightRatio* hodoHeightHintRatio);
	        	hodoWidthHint = (int) (baseWidth*hodoWidthHintRatio);
	        }
	       
	        if(paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_2_STR) || paneConfigurationName.equals(NsharpConstants.PANE_DEF_CFG_1_STR)){
	        	leftGpGd = new GridData(SWT.FILL, SWT.FILL, true,
	        			true);
	        	leftGpGd.widthHint = (int) (baseWidth*leftGroupWidthRatio);
	        	leftGp.setLayoutData(leftGpGd);

	        	rightGpGd = new GridData(SWT.FILL, SWT.FILL, true,
	        			true);
	        	rightGpGd.widthHint = (int) (baseWidth*(1-leftGroupWidthRatio));
	        	rightGp.setLayoutData(rightGpGd);

	        	leftTopGpGd = new GridData(SWT.FILL, SWT.FILL, true,
	        			true);
	        	leftTopGpGd.heightHint = (int) (baseHeight *leftTopGroupHeightRatio);
	        	leftTopGp.setLayoutData(leftTopGpGd);
	        	if(leftBotGp!=null){
	        		leftBotGpGd = new GridData(SWT.FILL, SWT.FILL, true,
	        				true);
	        		leftBotGpGd.heightHint = (int) (baseHeight *(1-leftTopGroupHeightRatio));
	        		leftBotGp.setLayoutData(leftBotGpGd);
	        	}
	        	GridData skewtGd = new GridData(SWT.FILL, SWT.FILL, true,
	        			true);//gridDataArray[DISPLAY_SKEWT];
	        	skewtGd.heightHint = skewTHeightHint;
	        	skewtGd.widthHint = skewTWidthHint;
	        	nsharpComp[DISPLAY_SKEWT].setLayoutData(skewtGd);
	        	GridData witoGd = new GridData(SWT.FILL, SWT.FILL, true,
	        			true);//gridDataArray[DISPLAY_WITO];
	        	witoGd.heightHint = witoHeightHint;
	        	witoGd.widthHint = witoWidthHint;
	        	nsharpComp[DISPLAY_WITO].setLayoutData(witoGd);

	        	GridData timeStnGd = new GridData(SWT.FILL, SWT.FILL, true,
	        			true);//gridDataArray[DISPLAY_TIMESTN];
	        	timeStnGd.heightHint = timeStnHeightHint;
	        	timeStnGd.widthHint = timeStnWidthHint;
	        	nsharpComp[DISPLAY_TIMESTN].setLayoutData(timeStnGd);
	        	GridData insetGd = new GridData(SWT.FILL, SWT.FILL, true,
	        			true);//gridDataArray[DISPLAY_INSET];
	        	insetGd.heightHint =  insetHeightHint;
	        	insetGd.widthHint = insetWidthHint;
	        	nsharpComp[DISPLAY_INSET].setLayoutData(insetGd);

	        	GridData hodoGd = new GridData(SWT.FILL, SWT.FILL, true,
	        			true);//gridDataArray[DISPLAY_HODO];
	        	hodoGd.heightHint = hodoHeightHint;
	        	hodoGd.widthHint = hodoWidthHint;
	        	nsharpComp[DISPLAY_HODO].setLayoutData(hodoGd);

	        	GridData dataGd = new GridData(SWT.FILL, SWT.FILL, true,
	        			true);//gridDataArray[DISPLAY_DATA];
	        	dataGd.heightHint = dataHeightHint;
	        	dataGd.widthHint = dataWidthHint;
	        	nsharpComp[DISPLAY_DATA].setLayoutData(dataGd);

	        }else if(paneConfigurationName.equals(NsharpConstants.PANE_SPCWS_CFG_STR)){	
	            topGpGd = new GridData(SWT.FILL, SWT.FILL, true,
	                    true);
	            topGpGd.heightHint = (int) (baseHeight * topGroupHeightRatio);
	            topGp.setLayoutData(topGpGd);
	            	            
	         // skewt composite
	            GridData skewtGd = new GridData(SWT.FILL, SWT.FILL, true,
	                    true);
	            skewtGd.heightHint = skewTHeightHint;
	        	skewtGd.widthHint = skewTWidthHint;
	        	nsharpComp[DISPLAY_SKEWT].setLayoutData(skewtGd);
	            
	            
	            // wito composite
	            GridData witoGd = new GridData(SWT.FILL, SWT.FILL, true,
	                    true);
	            witoGd.heightHint = witoHeightHint;
	        	witoGd.widthHint = witoWidthHint;
	        	nsharpComp[DISPLAY_WITO].setLayoutData(witoGd);	            
	            
	          // right-top group : right part of top group
	            rightTopGpGd = new GridData(SWT.FILL, SWT.FILL, true,
	                    true);
	            rightTopGpGd.widthHint = (int) (baseWidth*hodoWidthHintRatio);
	            rightTopGp.setLayoutData(rightTopGpGd);
	            	            
	          //hodo composite
	           GridData hodoGd = new GridData(SWT.FILL, SWT.FILL, true,
	                    true);
	            hodoGd.heightHint = hodoHeightHint;
	        	hodoGd.widthHint = hodoWidthHint;
	        	nsharpComp[DISPLAY_HODO].setLayoutData(hodoGd);
	
	    		
	            //inset composite
	            GridData insetGd = new GridData(SWT.FILL, SWT.FILL, true,
	                    true);
	            insetGd.heightHint =  insetHeightHint;
	        	insetGd.widthHint = insetWidthHint;
	        	nsharpComp[DISPLAY_INSET].setLayoutData(insetGd);
	    		/*
	    		//time-stn composite
	            Composite timeStnComp = new Composite(rightTopGp, SWT.NONE);
	            GridData timeStnGd = new GridData(SWT.END, SWT.FILL, false,
	                    true);
	            timeStnComp.setLayoutData(timeStnGd);
	    		*/
	    		
	    		botGpGd = new GridData(SWT.FILL, SWT.END, true,
	                    true);
	    		botGpGd.heightHint = (int) (baseHeight * botGroupHeightRatio);
	            botGp.setLayoutData(botGpGd);
	    		//data composite
	            GridData dataGd = new GridData(SWT.FILL, SWT.FILL, true,
	                    false);
	            dataGd.heightHint = dataHeightHint;
	        	dataGd.widthHint = dataWidthHint;
	        	nsharpComp[DISPLAY_DATA].setLayoutData(dataGd);
	    		//spc composite
	            GridData spcGd = new GridData(SWT.FILL, SWT.FILL, true,
	                    false);
	            spcGd.heightHint = spcHeightHint;
	            spcGd.widthHint = spcWidthHint;
	        	nsharpComp[DISPLAY_SPC_GRAPHS].setLayoutData(spcGd);

	        } else if(paneConfigurationName.equals(NsharpConstants.PANE_SIMPLE_D2D_CFG_STR)){
	        	topGpGd = new GridData(SWT.FILL, SWT.FILL, true,
	                    true);
	            topGpGd.heightHint = skewTHeightHint;
	            topGp.setLayoutData(topGpGd);
	            	            
	         // skewt composite
	            GridData skewtGd = new GridData(SWT.FILL, SWT.FILL, true,
	                    true);
	            skewtGd.heightHint = skewTHeightHint;
	        	skewtGd.widthHint = skewTWidthHint;
	        	nsharpComp[DISPLAY_SKEWT].setLayoutData(skewtGd);
	        	
	        	// right-top group : right part of top group
	            rightTopGpGd = new GridData(SWT.FILL, SWT.FILL, true,
	                    true);
	            rightTopGpGd.widthHint = timeStnWidthHint;
	            rightTopGp.setLayoutData(rightTopGpGd);
	            
	        	GridData timeStnGd = new GridData(SWT.FILL, SWT.FILL, true,
	        			true);//gridDataArray[DISPLAY_TIMESTN];
	        	timeStnGd.heightHint = timeStnHeightHint;
	        	timeStnGd.widthHint = timeStnWidthHint;
	        	nsharpComp[DISPLAY_TIMESTN].setLayoutData(timeStnGd);
	        	
	        	//future composite
	            GridData futureGd = new GridData(SWT.FILL, SWT.FILL, true,
	                    true);
	            futureGd.heightHint = futureHeightHint;
	            futureGd.widthHint = futureWidthHint;
	        	nsharpComp[DISPLAY_FUTURE].setLayoutData(futureGd);
	        	
	        	botGpGd = new GridData(SWT.FILL, SWT.END, true,
	                    true);
	    		botGpGd.heightHint = (int) (baseHeight * botGroupHeightRatio);
	            botGp.setLayoutData(botGpGd);
	            //hodo composite
	            GridData hodoGd = new GridData(SWT.FILL, SWT.FILL, true,
	            		false);
	            hodoGd.heightHint = hodoHeightHint;
	            hodoGd.widthHint = hodoWidthHint;
	            nsharpComp[DISPLAY_HODO].setLayoutData(hodoGd);
	            GridData dataGd = new GridData(SWT.FILL, SWT.FILL, true,
	            		false);//gridDataArray[DISPLAY_DATA];
	            dataGd.heightHint = dataHeightHint;
	            dataGd.widthHint = dataWidthHint;
	            nsharpComp[DISPLAY_DATA].setLayoutData(dataGd);
	        }
	        //System.out.println("After resizing...nsharpComp[0] w= " + nsharpComp[0].getBounds().width + " h= "+ nsharpComp[0].getBounds().height);
	        //System.out.println("After resizing...nsharpComp[1] w= " + nsharpComp[1].getBounds().width + " h= "+ nsharpComp[1].getBounds().height);

        	for(int i=0; i< DISPLAY_TOTAL; i++){
        		if(displayArray[i]!=null){
        			ResourcePair rscPair =  displayArray[i].getDescriptor().getResourceList().get(0);
        			if (rscPair.getResource() instanceof NsharpAbstractPaneResource){
        				NsharpAbstractPaneResource paneRsc = (NsharpAbstractPaneResource)rscPair.getResource() ;
        				paneRsc.setResize(true);
        			}
        		}
        	}

		}
		
	}
	@Override
    public boolean isDirty() {
        if (!isCloseable()) {
            return true;
        } else {
            return false;
        }
    }
	@Override
    protected void setInput(IEditorInput input) {
        super.setInput(input);
        this.editorInput = (EditorInput)input;
    }
   @Override
    public void renderableDisplayChanged(IDisplayPane pane,
            IRenderableDisplay newRenderableDisplay, DisplayChangeType type) {
    	
        if(type == DisplayChangeType.ADD){
        	//System.out.println("Editor="+this.toString()+" renderableDisplayChanged ADD called,  pane = " + pane.toString()+" newRenderableDisplay="+newRenderableDisplay.toString());
        	boolean swapping = false;
            for(int i = 0 ; i < displayPane.length ; i+= 1){
                if(displayPane[i] == pane){
                    //displayArray[i] = newRenderableDisplay;
                    //System.out.println( "swapping pane, current handler "+rscHandler.toString()+"#############################");
                    /* 
                     * Chin Note: For the scenarios of 2 instance of Nsharps. One in Main and one in side pane.
                     * In order to handle pane swapping in D2D. When this renderableDisplayChanged event happened with
                     * change type of ADD.
                     * It indicates that current nsharp in main pane is about to be swapped with Nsharp in side pane.
                     * Current D2D architecture will not dispose this NsharpEditor (in main pane) but will reuse it for the
                     * existing Nsharp coming from side pane.
                     * In other words, we only swap "NsharpResourceHandler" and keep editor. 
                     * Therefore, rscHandler (at this moment) is about to be moved out. Before it is moved out,
                     *  we raise "justMoveToSidePane" flag to be used by skewTPaneRsc at side pane. 
                     */
                	if(rscHandler.getSkewtPaneRsc()!=null)
                		rscHandler.getSkewtPaneRsc().setJustMoveToSidePane(true);
                	if(rscHandler.getWitoPaneRsc() !=null)
                		rscHandler.getWitoPaneRsc().setInSidePane(true);
                	swapping= true;
                }
                
            }
            /*
             * Chin Note: We only have to handle the following procedures once, either during swapping or NshaepEditor is created.
             * Since, in any case, we will have NsharpSkewTPane, therefore we are checking  NsharpSkewTPaneDescriptor
             * to avoid multiple access of the following code. 
             */
            if(newRenderableDisplay.getDescriptor() instanceof NsharpSkewTPaneDescriptor){
                NsharpAbstractPaneDescriptor desc = (NsharpAbstractPaneDescriptor) newRenderableDisplay.getDescriptor();
                NsharpResourceHandler handler = desc.getRscHandler();
                if (handler != null) {
                	//System.out.println("renderableDisplayChanged current handler=" + rscHandler.toString() + " set new Handler=" + handler.toString() + " editor "+ this.toString());
                	rscHandler = handler;
                	if(swapping){
                		/*
                		 * Chin Note: If swapping happened, rscHandler is the newly swapping nsharp's. 
                		 * We will have to restart editor to construct all graphs and displays for this nsharp instance.
                		 * We also have to re-store nsharp to its previous status (when it was in main pane). 
                		 */
                		restartEditor( paneConfigurationName);
                    	/*rscHandler.resetRsc();
                    	NsharpPaletteWindow paletteWin = NsharpPaletteWindow.getInstance();
                    	if(paletteWin!=null){
                    		paletteWin.restorePaletteWindow(paneConfigurationName, rscHandler.getCurrentGraphMode(),
                    				rscHandler.isInterpolateIsOn(), rscHandler.isOverlayIsOn(),
                    				rscHandler.isCompareStnIsOn(),rscHandler.isCompareTmIsOn(),rscHandler.isEditGraphOn());
                    	}*/
                    	if(rscHandler.getSkewtPaneRsc()!=null)
                    		rscHandler.getSkewtPaneRsc().setJustBackToMainPane(true);
                    	if(rscHandler.getWitoPaneRsc() !=null)
                    		rscHandler.getWitoPaneRsc().setInSidePane(false);
                    	
                    }
                    else{
                    	// Chin Note: This is when NsharpEditor is created case.
                    	// Note that, in D2D when only one Nsharp instance is in side pane, then when it
                    	// is swapped back to main pane, NsharpEditor also is created.
                    	//System.out.println("Editor="+this.toString()+" renderableDisplayChanged ADD but not swapping"+" newRenderableDisplay="+newRenderableDisplay.toString());
                    }
                    return;
	}
	}
	}
       // else
       // 	System.out.println("Editor="+this.toString()+" renderableDisplayChanged REMOVE called,  pane = " + pane.toString()+" newRenderableDisplay="+newRenderableDisplay.toString());
	}
}