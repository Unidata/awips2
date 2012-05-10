package gov.noaa.nws.ncep.standalone;


import org.eclipse.jface.action.IMenuManager;
import org.eclipse.ui.IWorkbenchWindow;
//import org.eclipse.ui.actions.ActionFactory;
//import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;

/**
 * Converter Dialog
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/25/2009   187         Q. Zhou     Initial
 * 
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
public class ApplicationActionBarAdvisor extends ActionBarAdvisor {
	//Declaring all the Workbench Actions
	
//	private IWorkbenchAction newAction;
//	private IWorkbenchAction saveAction;
//	private IWorkbenchAction openPerspective;
	
    public ApplicationActionBarAdvisor(IActionBarConfigurer configurer) {
        super(configurer);
    }

    protected void makeActions(IWorkbenchWindow window) {

//    	openPerspective= ActionFactory.OPEN_PERSPECTIVE_DIALOG.create(window);
//    	register(openPerspective);
    	
    }

    protected void fillMenuBar(IMenuManager menuBar) {
  
    	
    }
    
}
