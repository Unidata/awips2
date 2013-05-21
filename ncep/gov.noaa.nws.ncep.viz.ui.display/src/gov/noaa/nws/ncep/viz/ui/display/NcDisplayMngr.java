package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.display.INatlCntrsRenderableDisplay;
import gov.noaa.nws.ncep.viz.common.display.INcPaneID;
import gov.noaa.nws.ncep.viz.common.display.INcPaneLayout;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayName;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
//import gov.noaa.nws.ncep.viz.ui.display.NcDisplayName;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.TreeMap;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.EditorInput;
import com.raytheon.viz.ui.panes.PaneManager;

/**
 * NcDisplayMngr - contains UI utility methods for the NC perspective (See
 * com.raytheon.viz.ui.UiUtils for more utilities...)
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/22/09       #99       Greg Hull   Initial Creation.
 * 09/09          #151      Jun Wu      Added method to set title for CAVE.
 * 09/27/09       #169      Greg Hull   Use NCMapEditor and NCDisplayPane 
 * 10/26/09       #180      Greg Hull   added getAllNCDisplays()
 * 02/26/10       #226      Greg Hull   create Editors with PaneLayout.
 * 04/01/10      #238,#239  Archana     Updated the method createNatlCntrsEditor(String,NcPaneID)
 *                                      to use NCMapDescriptor.  
 *  02/10/2011              Chin Chen   handle multiple editor copies dispose issue    
 *  03/08/2011   migration  Greg Hull   move Display Name methods to DisplayIdManager
 * 05/17/2012     #791       Quan Zhou   Added findEmptyEditor() to check if default editor is empty
 * 08/09/2012     837       Archana      Updated getNcDisplayID() to fix a NumberFormatException     
 * 02/10/2012     #971      Greg Hull    Renamed from NmapUiUtils and add support for NcDisplayType and NcDisplayName
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcDisplayMngr {
	
    public static final String CaveTitle = "CAVE";

    // We could maintain an active list or map of all the open Editors and
    // reference but instead just have some methods to get all the open editors from
    // the workbench whenever we need them.
    //
    private static Object lock = new Object();
    
    /**
     * Get a reference to the current editor, if it is a NCMapEditor.
     */
    public static final AbstractEditor getActiveNatlCntrsEditor() {
    	IEditorPart epart = EditorUtil.getActiveEditor();
    	
    	if( epart instanceof AbstractEditor &&    			
            isNatlCntrsEditor( (AbstractEditor) epart ) ) {
    		return (AbstractEditor) epart;
        } 
        
        return null;
    }

    public static final Boolean isNatlCntrsEditor( AbstractEditor ed ) {
    	if( ed instanceof AbstractNcEditor) {
    		return true;
    	}
    	else {
    		EditorInput edIn = (EditorInput)ed.getEditorInput();    		
        	PaneManager pm = edIn.getPaneManager();
        	
        	return ( pm instanceof AbstractNcPaneManager );
        }
    }

    public static final Shell getCaveShell() {
    	IEditorPart epart = EditorUtil.getActiveEditor();
        if (epart != null) {
            return epart.getSite().getShell();
        } else {
            return PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getShell();
        }
    }

    // TODO: Do we need to look in all the panes or just the active (or the
    // selected panes)
    //
    public static final AbstractVizResource findResource(
            Class<? extends AbstractVizResource> rscClass, AbstractEditor aEdit) {
    	AbstractEditor editor = (aEdit != null ? aEdit : NcDisplayMngr
                .getActiveNatlCntrsEditor());
        if (editor == null)
            return null;

        IRenderableDisplay disp = editor.getActiveDisplayPane()
                .getRenderableDisplay();

        if (disp == null)
            return null;

        ResourceList rscList = disp.getDescriptor().getResourceList();

        for (ResourcePair rp : rscList) {
            AbstractVizResource rsc = rp.getResource();

            if (rsc.getClass() == rscClass) {
                return rsc;
            }
        }

        return null;
    }

    public static boolean bringToTop(AbstractEditor editor) {
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().bringToTop(editor);
		VizWorkbenchManager.getInstance().partBroughtToTop( editor );

        return false;
    }

    public static int getNextAvailableDisplayId( ) {
    	List<AbstractEditor> displays = getAllNcDisplays();
    	int dispCount = (displays == null ? 0 : displays.size() );
    	
    	Integer[] orderedIds = new Integer[ dispCount ];
    	    	
    	for( int i=0 ; i<dispCount ; i++ ) {
    		orderedIds[i] = NcEditorUtil.getDisplayName( displays.get(i) ).getId();    	
    	}
    	
    	Arrays.sort( orderedIds );
    	
    	for( int i=0 ; i<dispCount ; i++ ) {    	
    		if( orderedIds[i] > i+1 ) {
    			return i+1;
    		}
    	}
    	return dispCount+1;
    }
    
    // This can be called 
    public static AbstractEditor createNatlCntrsEditor( 
    		NcDisplayType dispType, String dispName )  throws VizException  {
    	return createNatlCntrsEditor( dispType, dispName, new NcPaneLayout(1,1) );
    }
    
    // This should only be called for DisplayTypes that support RBDs and 
    public static AbstractEditor createNatlCntrsEditor( 
    		 NcDisplayType dispType, String dispName, INcPaneLayout paneLayout )  throws VizException  {
    	
    	AbstractNcPaneManager pm = createNcPaneManagerForDisplayType( dispType, paneLayout );
    	if( pm != null ) {
    		return createNatlCntrsEditor( pm, dispName );			
    	} 
    	else {
		return null;
    		}
    }

    public static AbstractNcPaneManager createNcPaneManagerForDisplayType( 
   		 NcDisplayType dispType,  INcPaneLayout paneLayout )  throws VizException  {
   	
    	String ncPaneMngrClassName = dispType.getPaneManager();
		Exception e=null;
		try {
			Class<?> ncPaneMngrClass = Class.forName( ncPaneMngrClassName );
			Constructor<?> pmConstructor = ncPaneMngrClass.getConstructor(INcPaneLayout.class, NcDisplayType.class );
			
			Object pmObj = pmConstructor.newInstance( paneLayout, dispType );
			
			if( pmObj instanceof AbstractNcPaneManager ) {

				return (AbstractNcPaneManager) pmObj;				
			}

		} catch (SecurityException se) { e=se;
		} catch (NoSuchMethodException nsme) { e=nsme;
		} catch (IllegalArgumentException iae) { e=iae;
		} catch (InstantiationException ie) { e=ie;
		} catch (IllegalAccessException iae) { e=iae;
		} catch (InvocationTargetException ite) { e=ite;
		} catch (ClassNotFoundException cnfe) { e=cnfe;
		}
		
		System.out.println("Error instantiating PaneManager, "+ ncPaneMngrClassName +
				", for NcDisplayType, "+dispType.toString()+": "+e.getMessage() );
		throw new VizException( e );
   }

    public static AbstractEditor createNatlCntrsEditor( 
   		 AbstractNcPaneManager ncPaneMngr, String dispName )  throws VizException  {

    	INcPaneLayout paneLayout = ncPaneMngr.getPaneLayout();
    	
    	String editorId = ncPaneMngr.getEditorId();

    	try {
            int numPanes = paneLayout.getNumberOfPanes();
            INatlCntrsRenderableDisplay[] mapDispArray = new INatlCntrsRenderableDisplay[numPanes];

            // the name of the RenderableDisplay for multi-pane displays includes the pane number.
            //
            for( int paneIndx=0 ; paneIndx<paneLayout.getNumberOfPanes() ; paneIndx++ ) {            	
            	INcPaneID paneId = paneLayout.createPaneId( paneIndx );

            	mapDispArray[paneIndx] = 
            		ncPaneMngr.createNcRenderableDisplay( paneId ); 
            }
                        
            EditorInput edInput = new EditorInput(
            		new NCLoopProperties(), mapDispArray );

            edInput.setPaneManager( ncPaneMngr );

            IWorkbenchPage actPage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
            
            if( actPage == null ) {
            	return null;
            }

            synchronized ( lock ) {
            	NcDisplayName ncDispName = 
            		new NcDisplayName( getNextAvailableDisplayId(), dispName );
            	
            	ncPaneMngr.setDisplayName( ncDispName );
            	
            	edInput.setName( ncDispName.toString() );
            	
            	IEditorPart ep = actPage.openEditor( edInput, editorId );
         
            	if( ep instanceof AbstractNcEditor ) {
            		AbstractNcEditor new_editor = (AbstractNcEditor)ep;

            		return new_editor;
            	}
            	else {
            		System.out.println("openEditor returned :"+ ep.getClass().getName() ) ;
            		System.out.println( ep.toString() );
            	}
            }
        } catch (PartInitException e) {
            UiPlugin.getDefault().getLog().log(
                    new Status(Status.ERROR, UiPlugin.PLUGIN_ID,
                            "Error constituting editor", e));
//        } catch (VizException ve) {
//            UiPlugin.getDefault().getLog().log(
//                    new Status( Status.ERROR, UiPlugin.PLUGIN_ID,
//                            "Error constituting editor: ", ve));
        }
        return null;
    }
    
    public static AbstractRenderableDisplay[] createDisplaysForNcDisplayType( 
    		NcDisplayType dType, INcPaneLayout pLayout ) throws VizException {
    	AbstractRenderableDisplay[] rendDisps = new AbstractRenderableDisplay[ pLayout.getNumberOfPanes() ];

    	AbstractNcPaneManager pm = createNcPaneManagerForDisplayType( dType, pLayout );
    	
    	if( pm != null ) {
    		for( int d=0 ; d<pLayout.getNumberOfPanes() ; d++ ) {
    			INatlCntrsRenderableDisplay iRendDisp = pm.createNcRenderableDisplay( pLayout.createPaneId( d ) ); 
    			if( iRendDisp instanceof AbstractRenderableDisplay ) {
    				rendDisps[d] = (AbstractRenderableDisplay) iRendDisp; 
    			}
    		}
    	}
    	return rendDisps;
    }
    
//    public static INatlCntrsRenderableDisplay createNcRenderableDisplay( 
//    		NcDisplayType dispType, INcPaneID pid ) {
//    	
//    	try {
//			Class<?> rendDispClass = Class.forName( dispType.getRederableDisplay() );
//			
//			Object rendDispObj = rendDispClass.newInstance();
//			
//			if( rendDispObj instanceof INatlCntrsRenderableDisplay ) {
//
//				INatlCntrsRenderableDisplay iRendDisp = (INatlCntrsRenderableDisplay)rendDispObj;
//				iRendDisp.setPaneId( pid );
//				
//				Class<?> descrClass = Class.forName( dispType.getDescriptorType() );
//				Object descrObj = descrClass.newInstance();
//				
//				if( descrObj instanceof INatlCntrsDescriptor ) {
//					INatlCntrsDescriptor iDescr = (INatlCntrsDescriptor)descrObj;
//
//					iRendDisp.setDescriptor( iDescr );
//					
//					return iRendDisp;
//				}
//				else {
//					return null;
//				}
//			}
//    	} catch (InstantiationException e) {
//    	} catch (IllegalAccessException e) {
//    	} catch (ClassNotFoundException e) {
//    	}
//    	
//    	return null;
//    }
    
	public static List<AbstractEditor>  getAllDisplaysOfType( NcDisplayType dispType ) {
		ArrayList<NcDisplayType> dispTypes = new ArrayList<NcDisplayType>();
		if( dispType != null ) {
			dispTypes.add( dispType );
		}
		
		return getAllDisplaysByTypes( dispTypes );
	}
	
	public static List<AbstractEditor>  getAllDisplaysByTypes( List<NcDisplayType> dispTypes ) {
		List<AbstractEditor> allDisplays = getAllNcDisplays();
		List<AbstractEditor> displays = new ArrayList<AbstractEditor>();
		
		for( AbstractEditor disp : allDisplays ) {
			if( dispTypes.contains( NcEditorUtil.getNcDisplayType( disp ) ) ) {
				displays.add( disp );
			}
		}
		
		return displays;
	}

	// all the Nc Displays ordered by ID
    public static List<AbstractEditor> getAllNcDisplays( ) {
		ArrayList<AbstractEditor> displays = new ArrayList<AbstractEditor>();

		IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();

		for( IWorkbenchWindow window : windows ) {
			IWorkbenchPage pages[] = window.getPages();
			for (IWorkbenchPage page : pages) {
				IEditorReference[] refs = page.getEditorReferences();
				for (IEditorReference r : refs) {
					IWorkbenchPart wp = r.getPart(true);
					if( wp != null && 
						wp instanceof AbstractEditor &&
						isNatlCntrsEditor( (AbstractEditor)wp ) ) {

						displays.add( (AbstractEditor)wp ); 
					}

				}
			}
		}
		return displays;
    }

    public static AbstractEditor findDisplayByID( NcDisplayName dispName ) {
    	return findDisplayByID( null, dispName );
    }

    public static AbstractEditor findDisplayByID(NcDisplayType dispType, NcDisplayName dispName ) {

    	for( AbstractEditor d : getAllNcDisplays() ) {
    		if( (dispType == null || dispType.equals( NcEditorUtil.getNcDisplayType( d ) )) &&
    			 dispName.getId() == NcEditorUtil.getDisplayName( d ).getId() ) {                        

    			return d;
    		}
    	}
    	return null;
    }

    // lookup the editor by name (without the prefixed ID)
    //
    public static AbstractEditor findDisplayByName( NcDisplayType dispType, String dispName ) {
    	for( AbstractEditor d : getAllDisplaysOfType( null ) ) {
        	if( dispName == NcEditorUtil.getDisplayName(d).getName() ) {                        
        		return d;
        	}
    	}
    	return null;
    }
    
    // Assume 
    // First look for an empty display that 
    public static AbstractEditor findUnmodifiedDisplayToLoad( NcDisplayType dispType ) {
    	List<AbstractEditor> displays = getAllDisplaysOfType( dispType );
    	
    	for( AbstractEditor d : displays ) {
    		if( NcEditorUtil.isDisplayAvailableToLoad( d ) ) {
    			return d;
    		}
    	}
    	
    	return null;
    }
    
    /**
     * Modify the title of the CAVE to "CAVE:title".
     */
    public static final void setCaveTitle(String title) {
    	if( PlatformUI.getWorkbench().getActiveWorkbenchWindow() != null &&
    	    PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell() != null) {
    		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell().setText(CaveTitle + ":" + title);
    	}
    }
    
    /**
     * set current ModalTool to Panning mode
     */
    public static final void setPanningMode() {
    	AbstractEditor currEditor = NcDisplayMngr.getActiveNatlCntrsEditor();

        if (currEditor != null) {
            ICommandService service = (ICommandService) currEditor.getSite()
                    .getService(ICommandService.class);
            String dfltTool = currEditor.getDefaultTool(); // this is the
                                                           // PanTool
            Command cmd = service.getCommand(dfltTool);
            if (cmd != null) {
                try {
                    HashMap<String, Object> params = new HashMap<String, Object>();
                    // is this param no longer used?
                    params.put("editor", currEditor);
                    // params.put("mode", )
                    ExecutionEvent exec = new ExecutionEvent(cmd, params, null,
                            null);
                    cmd.executeWithChecks(exec);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }
    
}
