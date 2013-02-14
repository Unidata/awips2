/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.viz.gfe.actions;

import com.raytheon.viz.gfe.Activator;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.resource.ImageDescriptor;

import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.ShowISCGridsMsg;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.ToolItem;

/**
 * Action to show the ISC grids
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 07/20/09      1995       bphillip    Initial release
 * 12/06/12      DR 15574   jzeng       Change the image of 
 * 										the icon when it is activated 
 * 01/11/13      DR 15574   jzeng       delete all fields to local variables
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class ShowISCGridsAction extends AbstractHandler {
	
	@Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        boolean current = Message.inquireLastMessage(ShowISCGridsMsg.class).show();
     
        if (arg0.getTrigger() instanceof Event) {
            Event e = (Event) arg0.getTrigger();
            if ( e.widget instanceof ToolItem) {
                ToolItem ti = (ToolItem) e.widget;
                if (ti != null ){
                    ImageDescriptor id;

                    if (!current){
                	    id = Activator.imageDescriptorFromPlugin(
                	        Activator.PLUGIN_ID, "icons/isc1.gif" );
                	} else {
                	    id = Activator.imageDescriptorFromPlugin(
                		    Activator.PLUGIN_ID, "icons/isc0.gif" );
                	}
                    
                	if (id != null){
                	    Image  img = id.createImage();
                	    ti.setImage(img);
                        img.dispose();
                    }
                }
            }
        }
       
        new ShowISCGridsMsg(!current).send();
        return null;
    }
}
