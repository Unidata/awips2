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
package com.raytheon.viz.ui.actions;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.viz.ui.BundleLoader;
import com.raytheon.viz.ui.BundleLoader.BundleInfoType;
import com.raytheon.viz.ui.BundleProductLoader;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Handler for eclipse command that loads a bundle file to the display. This
 * handler can be used from plugin.xml by using command parameters to specify
 * what to load. It can also be used directly by configuring it using a
 * constructor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 30, 2013  2310     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class LoadBundleHandler extends AbstractHandler {

    private final String bundleFile;

    private final Map<String, String> variableSubstitutions;

    private final String editorType;

    private final Boolean fullBundleLoad;

    public LoadBundleHandler() {
        this(null);
    }

    public LoadBundleHandler(String bundleFile) {
        this(bundleFile, null, null, null);
    }

    public LoadBundleHandler(String bundleFile,
            Map<String, String> variableSubstitutions, String editorType,
            Boolean fullBundleLoad) {
        this.bundleFile = bundleFile;
        this.variableSubstitutions = variableSubstitutions;
        this.editorType = editorType;
        this.fullBundleLoad = fullBundleLoad;
    }

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        try {
            Bundle bundle = BundleLoader.getBundle(getBundleFile(event),
                    getVariableSubstitutions(event),
                    BundleInfoType.FILE_LOCATION);

            AbstractEditor editor = UiUtil.createOrOpenEditor(
                    getEditorType(event, bundle), bundle.getDisplays());
            BundleLoader loader;
            if (isFullBundleLoad(event)) {
                loader = new BundleLoader(editor, bundle);
            } else {
                loader = new BundleProductLoader(editor, bundle);
            }
            loader.schedule();
            return event;
        } catch (VizException e) {
            throw new ExecutionException("Unable to load bundle", e);
        }
    }

    protected String getBundleFile(ExecutionEvent event) {
        if (this.bundleFile != null) {
            return bundleFile;
        } else if (event != null){
            return event.getParameter("bundleFile");
        }else{
            throw new IllegalStateException(
                    "LoadBundleHandler requires a bundle file.");
        }
    }

    protected Map<String, String> getVariableSubstitutions(ExecutionEvent event) {
        if (this.variableSubstitutions != null) {
            return variableSubstitutions;
        } else if (event != null){
            Map<String,String> variableSubstitutions = new HashMap<String, String>();
            Map<?,?> parameters = event.getParameters();
            for(Entry<?, ?> parameter : parameters.entrySet()){
                if (parameter.getKey() instanceof String
                        && parameter.getValue() instanceof String) {
                    variableSubstitutions.put((String) parameter.getKey(),
                            (String) parameter.getValue());
                }
            }
            return variableSubstitutions;
        }else{
            return null;
        }
    }

    protected String getEditorType(ExecutionEvent event, Bundle bundle) {
        if (this.editorType != null) {
            return editorType;
        }else if(event != null){
            String editorType = event.getParameter("editorType");
            if(editorType != null){
                return editorType;
            }
        }
        String editorType = bundle.getEditor();
        if (editorType == null) {
            for (IRenderableDisplay display : bundle.getDisplays()) {
                String descEditorType = DescriptorMap.getEditorId(display
                        .getDescriptor().getClass().getName());
                if (descEditorType != null) {
                    if (editorType == null) {
                        editorType = descEditorType;
                    } else if (!editorType.equals(descEditorType)) {
                        // If this happens there are no reasonable guesses, just
                        // let UIUtil figure it out.
                        return null;
                    }
                }
            }
        }
        return editorType;
    }

    protected boolean isFullBundleLoad(ExecutionEvent event) {
        if (this.fullBundleLoad != null) {
            return fullBundleLoad;
        } else if (event != null) {
            return Boolean.valueOf(event.getParameter("fullBundleLoad"));
        }else{
            return false;
        }
    }
}
