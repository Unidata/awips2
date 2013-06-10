package com.raytheon.viz.mpe.ui.actions;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;

public class TransmitRFCBias extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		IEditorPart editor = EditorUtil.getActiveEditor();
		IDisplayPane pane = null;
		if (editor instanceof IMultiPaneEditor) {
			IMultiPaneEditor multiPane = (IMultiPaneEditor) editor;
			if (multiPane.getNumberofPanes() > 1
					&& multiPane.displayedPaneCount() > 1) {
				pane = multiPane.getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
			} else {
				pane = ((IDisplayPaneContainer) editor).getDisplayPanes()[0];
			}
		}

		MPEDisplayManager dm = MPEDisplayManager.getInstance(pane);
		Date currentDate = dm.getCurrentEditDate();
		SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMddHH");
		String transmitDate = formatter.format(currentDate);

		AppsDefaults appsDefaults = AppsDefaults.getInstance();
		String scriptDir = appsDefaults.getToken("pproc_bin");

		String scriptName = "transmit_rfc_bias";
		ProcessBuilder pb = new ProcessBuilder(scriptDir + "/" + scriptName,
				transmitDate);
		try {
			pb.start();
		} catch (IOException e) {
			e.printStackTrace();
		}

		return null;
	}

}
