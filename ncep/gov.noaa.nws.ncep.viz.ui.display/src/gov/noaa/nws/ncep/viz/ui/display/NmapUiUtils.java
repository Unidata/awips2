package gov.noaa.nws.ncep.viz.ui.display;

import gov.noaa.nws.ncep.viz.common.EditorManager;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager.PaneLayout;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

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

/**
 * NmapUiUtils - contains UI utility methods for the NC perspective (See
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
 * 04/01/10      #238,#239  Archana     Updated the method createNatlCntrsEditor(String,PaneID)
 *                                      to use NCMapDescriptor.  
 *  02/10/2011              Chin Chen   handle multiple editor copies dispose issue    
 *  03/08/2011   migration  Greg Hull   move Display Name methods to DisplayNameManager
 * 05/17/2012     #791       Quan Zhou   Added findEmptyEditor() to check if default editor is empty
 * 08/09/2012     837       Archana      Updated getNcDisplayID() to fix a NumberFormatException     
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NmapUiUtils {

    public static final String NatlCntrsEditorType = NCMapEditor.class
            .getName();

    public static final String CaveTitle = "CAVE";

    /**
     * Get a reference to the current editor, if it is a NCMapEditor.
     */
    public static final NCMapEditor getActiveNatlCntrsEditor() {
        // bsteffen change to EditorUtils
        // if (VizApp.getCurrentEditor() instanceof NCMapEditor) {
        // return (NCMapEditor) VizApp.getCurrentEditor();
        if (EditorUtil.getActiveEditor() instanceof NCMapEditor) {
            return (NCMapEditor) EditorUtil.getActiveEditor();
        } else {
            return null;
        }
    }

    public static final Shell getCaveShell() {
        NCMapEditor disp = getActiveNatlCntrsEditor();
        if (disp != null) {
            return disp.getSite().getShell();
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
        AbstractEditor editor = (aEdit != null ? aEdit : NmapUiUtils
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

    /**
     * set current ModalTool to Panning mode
     */
    public static final void setPanningMode() {
        System.out.println("setPanningMode called ");
        NCMapEditor currEditor = NmapUiUtils.getActiveNatlCntrsEditor();

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

    public static boolean bringToTop(AbstractEditor editor) {
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().bringToTop(editor);
		VizWorkbenchManager.getInstance().partBroughtToTop( editor );

        return false;
    }

    public static NCMapEditor createNatlCntrsEditor(String editor_name) {
        return createNatlCntrsEditor(editor_name, new PaneLayout(1, 1), "NA");
    }

    public static NCMapEditor createNatlCntrsEditor(String editor_name,
            String applicationName) {
        return createNatlCntrsEditor(editor_name, new PaneLayout(1, 1),
                applicationName);
    }

    public static NCMapEditor createNatlCntrsEditor(String editor_name,
            PaneLayout paneLayout) {
        return createNatlCntrsEditor(editor_name, paneLayout, "NA");
    }

    public static NCMapEditor createNatlCntrsEditor(String editor_name,
            PaneLayout paneLayout, String applicationName) {
        try {
            // NCMapRenderableDisplay[] mapDisps = new
            // NCMapRenderableDisplay[numPanes];
            // Chin Not used.. HashMap<PaneID,NCMapRenderableDisplay> mapDispMap
            // =
            // new HashMap<PaneID,NCMapRenderableDisplay>();
            int numPanes = paneLayout.getNumberOfPanes();
            NCMapRenderableDisplay[] mapDispArray = new NCMapRenderableDisplay[numPanes];

            // the name of the RenderableDisplay for multi-pane displays includes the pane number.
            //
            for( int r=0 ; r<paneLayout.getRows() ; r++ ) {
                for( int c=0 ; c<paneLayout.getColumns() ; c++ ) {                	
                	PaneID paneId = new PaneID( r,c );
                	mapDispArray[paneLayout.getPaneIndex(paneId)] = 
                		new NCMapRenderableDisplay( paneId, new NCMapDescriptor() );
                }            	
            }
            /*
             * Chin,, not used.. for( int r=0 ; r<paneLayout.getRow() ; r++ ) {
             * for( int c=0 ; c<paneLayout.getColumn() ; c++ ) { mapDispMap.put(
             * new PaneID(r,c), new NCMapRenderableDisplay( new
             * NCMapDescriptor() ) ); } }
             */
            // NCMapEditorInput edInput = new NCMapEditorInput( paneLayout,
            // mapDispMap );
            NCPaneManager paneMngr = new NCPaneManager(paneLayout);
            // paneMngr.

            EditorInput edInput = new EditorInput(new NCLoopProperties(),
                    mapDispArray);

            edInput.setPaneManager(paneMngr);

            if (editor_name != null && !editor_name.isEmpty()) {

                int uniqNum = EditorManager.getEditorNumber();

//                for (int m = 0; m < numPanes; m++) {
//                    mapDispArray[m].setEditorNum(uniqNum);
//                }
                String edtDispName = new String(Integer.toString(uniqNum) + "-"
                        + editor_name);
                edInput.setName(edtDispName);
            }

            IWorkbenchPage actPage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
            
            if( actPage == null ) {
            	return null;
            }

            NCMapEditor new_editor = 
            	(NCMapEditor) actPage.openEditor(edInput, NatlCntrsEditorType);

            new_editor.setApplicationName(applicationName);

            return new_editor;

        } catch (PartInitException e) {
            UiPlugin.getDefault()
                    .getLog()
                    .log(new Status(Status.ERROR, UiPlugin.PLUGIN_ID,
                            "Error constituting editor", e));
        } catch (VizException ve) {
            UiPlugin.getDefault()
                    .getLog()
                    .log(new Status(
                            Status.ERROR,
                            UiPlugin.PLUGIN_ID,
                            "Error constituting editor. Can't create MapDescriptor",
                            ve));
        }
        return null;
    }

    // return a list of the names of all of the current editors
    // This will only include NCMapEditor displays which will exclude nwx and
    // other editors
    //
    public static String[] getNatlCntrsDisplayNames(boolean includeId) {
        ArrayList<String> editor_names = new ArrayList<String>();
        IWorkbenchWindow[] windows = PlatformUI.getWorkbench()
                .getWorkbenchWindows();

        for (IWorkbenchWindow window : windows) {
            IWorkbenchPage pages[] = window.getPages();
            for (IWorkbenchPage page : pages) {
                IEditorReference[] refs = page.getEditorReferences();

                for (IEditorReference r : refs) {
                    IEditorPart editorPart = r.getEditor(false);
                    if (editorPart instanceof NCMapEditor) {
                        NCMapEditor editor = (NCMapEditor) editorPart;

                        if (includeId) {
                            editor_names.add(r.getTitle());
                        } else {
                            editor_names.add(getNcDisplayNameWithoutID(r
                                    .getTitle()));
                        }
                        // This code was used to return a separate name for each
                        // pane in multi-pane displays
                        // if( editor.getNumberofPanes() == 1 ) {
                        // editor_names.add( r.getTitle() );
                        // }
                        // else {
                        // int p=1;
                        // for( IDisplayPane pane : editor.getDisplayPanes() ) {
                        // editor_names.add( r.getTitle() + "(" + p++ + ")" );
                        // }
                        // }
                    }
                }
            }
        }

        return editor_names.toArray(new String[0]);
    }

    public static ArrayList<NCMapEditor> getAllNCDisplays() {
        ArrayList<NCMapEditor> editors = new ArrayList<NCMapEditor>();
        IWorkbenchWindow[] windows = PlatformUI.getWorkbench()
                .getWorkbenchWindows();

        for (IWorkbenchWindow window : windows) {
            IWorkbenchPage pages[] = window.getPages();
            for (IWorkbenchPage page : pages) {
                IEditorReference[] refs = page.getEditorReferences();

                for (IEditorReference r : refs) {
                    IEditorPart editorPart = r.getEditor(false);
                    if (editorPart instanceof NCMapEditor) { // change to NC Map
                                                             // Editor
                        if (isNcDisplayName(r.getTitle())) {
                            editors.add((NCMapEditor) editorPart);
                        }
                    }
                }
            }
        }
        return editors;
    }

    // if the editor name doesn't have a '-' in the first 4 chars then assume
    // that it was not created
    // by Nmap and ignore it.
    public static boolean isNcDisplayName(String displayTitle) {
        return (getNcDisplayID(displayTitle) != -1);
    }

    public static int getNcDisplayID(String displayTitle) {
        int indx = displayTitle.indexOf("-");
        if (indx == -1 || indx > 3) {
            return -1;
        } else {
        	      try{
        	             int displayID = Integer.parseInt(displayTitle.substring(0, indx));
        	             return displayID;
        	
        	          }catch(NumberFormatException e ){
        		         return -1;
        	         }
        }
    }

    // without the prefix
    public static String getNcDisplayNameWithoutID(String dispName) {
        if (isNcDisplayName(dispName)) {
            return dispName.substring(dispName.indexOf("-") + 1);
        } else
            return dispName;
    }

    // generate a name by prefixing a unique id number to the name of the RBD.
    //
    public static String createNatlCntrsDisplayName(String rbdName) {
        /*
         * Chin TBD String[] editorNames = NmapUiUtils.getNatlCntrsDisplayNames(
         * true ); int[] editorNumbers = new int[editorNames.length]; for( int
         * i=0 ; i<editorNames.length ; i++ ) { editorNumbers[i] =
         * Integer.parseInt( editorNames[i].substring( 0,
         * editorNames[i].indexOf("-") ) ); }
         * 
         * int uniqNum = editorNames.length+1; Arrays.sort( editorNumbers );
         * 
         * // find the lowest available editor number for( int i=0 ;
         * i<editorNames.length ; i++ ) { if( editorNumbers[i] != i+1 ) {
         * uniqNum = i+1; break; } }
         */
        int uniqNum = EditorManager.getEditorNumber();
        return new String(Integer.toString(uniqNum) + "-" + rbdName);
    }

    // lookup the editor by name (without the prefixed ID)
    //
    // bsteffen changed to AbstractEditor
    // public static NCMapEditor findDisplayByName( String dispName ) {
    public static AbstractEditor findDisplayByName(String dispName) {
        IWorkbenchWindow[] windows = PlatformUI.getWorkbench()
                .getWorkbenchWindows();
        System.out.println("findDisplayByName called");
        for (IWorkbenchWindow window : windows) {
            IWorkbenchPage pages[] = window.getPages();
            for (IWorkbenchPage page : pages) {
                IEditorReference[] refs = page.getEditorReferences();

                for (IEditorReference r : refs) {
                    String nameWithoutID = getNcDisplayNameWithoutID(r
                            .getTitle());

                    if (NatlCntrsEditorType.equals(r.getId())
                            && dispName.equals(nameWithoutID)) {
                        // PlatformUI.getWorkbench().getActiveWorkbenchWindow().
                        // getActivePage().bringToTop(r.getPart(true));
                        return (NCMapEditor) r.getPart(true);
                    }
                }
            }
        }
        return null;
    }

    public static NCMapEditor findDisplayByID(String dispName) {
        IWorkbenchWindow[] windows = PlatformUI.getWorkbench()
                .getWorkbenchWindows();
        int dispId = getNcDisplayID(dispName);
        if (dispId == -1) {
            return null;
        }
        for (IWorkbenchWindow window : windows) {
            IWorkbenchPage pages[] = window.getPages();
            for (IWorkbenchPage page : pages) {
                IEditorReference[] refs = page.getEditorReferences();
                for (IEditorReference r : refs) {
                    // System.out.println("title= "+ r.getTitle() + " id="+
                    // r.getId() );

                    if (NatlCntrsEditorType.equals(r.getId())
                            && dispId == getNcDisplayID(r.getTitle())) {
                        // System.out.println("title= "+ r.getTitle() + " id="+
                        // r.getId() );
                        return (NCMapEditor) r.getPart(true);
                    }
                }
            }
        }
        return null;
    }

    /**
     * Modify the title of the CAVE to "CAVE:title".
     */
    public static final void setCaveTitle(String title) {
    	if (PlatformUI.getWorkbench().getActiveWorkbenchWindow() != null
    			&& PlatformUI.getWorkbench().getActiveWorkbenchWindow()
    			.getShell() != null) {
    		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()
    		.setText(CaveTitle + ":" + title);
    	}

    }

    /**
     * Reset the title of the CAVE to "CAVE".
     */
    public static final void resetCaveTitle() {
        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()
                .setText(CaveTitle);
    }

    public static boolean findEmptyEditor(List<String> defaultRscList) {
//    	NCMapEditor defaultEditor = (NCMapEditor) NmapUiUtils.findDisplayByName( "Welcome" );
    	
    	IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
		for (IWorkbenchWindow window : windows) {
			IWorkbenchPage pages[] = window.getPages();
			for (IWorkbenchPage page : pages) {
				IEditorReference[] refs = page.getEditorReferences();
				for (IEditorReference r : refs) {
					String name = r.getName();
					String name1 = name.substring(0, name.indexOf("-")+1);
					String name2 = name.substring(name.indexOf("-")+1, name.length());
					NCMapEditor defaultEditor = (NCMapEditor) (r.getEditor(false));
					
					if (defaultEditor != null && name != null) {
						if (defaultEditor.getDescriptor() != null) 	{
							ResourceList rl = defaultEditor.getDescriptor().getResourceList();
							
							if (rl != null ) { //&& name2.equalsIgnoreCase(defaultRscList.get(0)) && name1.equalsIgnoreCase("1-")) {
								boolean flag = true;
								for( ResourcePair rp : rl ) {
					    			if( rp != null && rp.getResourceData() != null) {
//					    				try {
//					    					AbstractNatlCntrsResourceData resourceData = (AbstractNatlCntrsResourceData) (rp.getResourceData());
//					    					String s = resourceData.getResourceName().toString();
					    					String s = rp.getResourceData().getClass().getName();
//					    				
					    					if (defaultRscList.contains(s)) {
												continue;	
					    					}
					    					else {
					    						flag = false;
					    						break;
					    					}
//					    				}
//					    				catch (Exception e){
//					    					try{
//					    					AbstractResourceData resourceData = (AbstractResourceData) (rp.getResourceData());
//					    					String s = resourceData.getClass().getName().toString(); //pirep,..  NcLeagend and NcSelectedPane are null
//					    					if (defaultRscList.contains(s)) 
//												continue;	
//					    					else {
//					    						flag = false;
//					    						break;
//					    					}
//					    					}catch (Exception ex) {}
//					    				}
									}																				
					    		}
								
								if (flag == true) {
									return true;
								}
							}
						}
					}
				}
			}
		}	
		
		return false;
    }
}
