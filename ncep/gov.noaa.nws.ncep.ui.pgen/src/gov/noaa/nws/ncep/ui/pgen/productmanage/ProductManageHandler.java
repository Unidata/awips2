/*
 * gov.noaa.nws.ncep.ui.pgen.productManage.ProductManageHandler
 * 
 * August 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.productmanage;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.viz.ui.actions.AbstractDropDownMenuHandler;


/**
 * Opens PGEN activity dropdown menu when clicking on "Start" on PGEN toolbar.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09  		#151        J. Wu    	Initial creation. 
 * 04/13  		TTR         J. Wu    	Use AbstractDropDownMenuHandler so
 *                                      menu could pop up when clicking on
 *                                      "Start". 
 * 
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 * 
 */
public class ProductManageHandler extends AbstractDropDownMenuHandler {
	
	/*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.actions.AbstractDropDownMenuHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
    	
//     	PgenSession.getInstance().getPgenResource().activateProductManage();

    	/* 
    	 * Issue: clicking the "Start" button on PGEN toolbar or the little "triangle"
    	 * at its right end needs to present the user with the same menu. However,
    	 * the dynamic menu needs to extend ContributionItem, and this handler 
    	 * needs to extend AbstractHandler. So somehow the menu created in 
    	 * "ProductLauncher" (extends ContributionIteme) needs to be opened here.  
    	 * AbstractDropDownMenuHandler.execute did this by adapting the code
    	 * from CommandContributionItem.openDropDownMenu(), which fits this need.
         */    	
    	super.execute( arg0 );
		
        return null;
    
    }
    
}


