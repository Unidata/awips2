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
package com.raytheon.uf.viz.d2d.ui.actions;

import java.io.File;
import java.util.ArrayList;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.OpenProcedureListDlg;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureTree;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.SaveProcedureListDlg;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureDlg;
import com.raytheon.uf.viz.d2d.ui.dialogs.procedures.ProcedureComm.BundlePair;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.actions.LoadSerializedXml;
import com.raytheon.viz.ui.actions.SaveProcedure;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * OpenAWIPSProcedure
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Apr 19, 2015             mjames	   Copied from OpenAWIPSProcedure
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class SaveAWIPSProcedure extends AbstractHandler {
	
	private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProcedureDlg.class);
	
	private java.util.List<BundlePair> bundles;
	
	private String fileName;
	
	private Boolean frozen = false;

    private SaveProcedureListDlg dialog;
    
    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
    	 Procedure procedure = SaveProcedure.getCurrentProcedure();
    	 
    	 this.bundles = new ArrayList<BundlePair>();
         frozen = false;
         if (procedure != null && procedure.getBundles() != null) {
             Bundle[] bund = procedure.getBundles();
             for (Bundle b : bund) {
                 // remove any bad resource pairs from each display
                 for (AbstractRenderableDisplay display : b.getDisplays()) {
                     ResourceList rList = display.getDescriptor()
                             .getResourceList();
                     // modify the resource list so that we remove any null
                     // resource datas
                     for (ResourcePair rp : rList) {
                         if (rp.getResourceData() == null) {
                             rList.remove(rp);
                         }
                     }
                 }
                 // Check to see if frozen
                 for (AbstractRenderableDisplay display : b.getDisplays()) {
                     ResourceList rList = display.getDescriptor()
                             .getResourceList();
                     for (ResourcePair rp : rList) {
                         if (rp.getResourceData().isFrozen()) {
                             frozen = true;
                             break;
                         }
                     }
                     if (frozen) {
                         break;
                     }
                 }
                 BundlePair bp = new BundlePair();
                 try {
                     bp.xml = b.toXML();
                 } catch (VizException e) {
                     e.printStackTrace();
                 }
                 bp.name = (b.getName() != null ? b.getName() : " ");
                 this.bundles.add(bp);
             }
         }
         
         
    	if (dialog == null || dialog.getShell() == null || dialog.isDisposed()) {
            dialog = new SaveProcedureListDlg(HandlerUtil.getActiveShell(event));
            dialog.setCloseCallback(new ICloseCallback() {
            	
            	@Override
                public void dialogClosed(Object returnValue) {
                    String fn = dialog.getSelectedFileName();
                    if (fn != null) {
                    	frozen = dialog.isFrozen();
                        fileName = fn;
                        saveProcedure(frozen,fileName);
                    }
                }
            });
            dialog.open();
        } else {
            dialog.bringToTop();
        }

        return null;
    }
    
    private void saveProcedure(boolean frozen, String fileName) {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();

            LocalizationContext context = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.USER);

            LocalizationFile localizationFile = pm.getLocalizationFile(context,
            		ProcedureDlg.PROCEDURES_DIR + File.separator + fileName);

            Procedure procedure = SaveProcedure.getCurrentProcedure();
            BundlePair[] bp = bundles.toArray(new BundlePair[bundles.size()]);
            Bundle[] bundlesToSave = new Bundle[bp.length];
            int i = 0;
            for (BundlePair b : bp) {
                Bundle bundle = Bundle.unmarshalBundle(b.xml, null);

				if (!frozen) {
                    for (AbstractRenderableDisplay display : bundle
                            .getDisplays()) {
                        for (ResourcePair rp : display.getDescriptor()
                                .getResourceList()) {
                            if (rp.getResourceData() != null
                                    && rp.getResourceData() instanceof AbstractRequestableResourceData) {
                                ((AbstractRequestableResourceData) rp
                                        .getResourceData()).setFrozenTime(null);
                            }
                        }
                    }
                }
                bundlesToSave[i] = bundle;
                bundlesToSave[i].setName(b.name);
                i++;
            }
            procedure.setBundles(bundlesToSave);

            String procedureXML = procedure.toXML();
            FileUtil.bytes2File(procedureXML.getBytes(),
                    localizationFile.getFile());
            localizationFile.save();

            //shell.setText("Procedure - " + fileName);
            //saved = true;
            //saveBtn.setEnabled(false);

            dialog.close();
        } catch (Exception e) {
            final String errMsg = "Error occurred during procedure save.";
			statusHandler.handle(Priority.PROBLEM, errMsg, e);
        }
    }


}
