package gov.noaa.nws.ncep.viz.rsc.solarimage;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.SolarImagePreferences;
import gov.noaa.nws.ncep.viz.ui.display.NCLoopProperties;

import java.io.File;
import java.util.Map;

import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.uf.viz.core.DescriptorMap;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.uf.viz.d2d.core.time.LoadMode;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.actions.LoadSerializedXml;
import com.raytheon.viz.ui.editor.AbstractEditor;

public class SolarImageBundleLoader {
    /**
     * Load a bundle from a file into a container
     * 
     * @param editor
     *            the container to load to
     * @param f
     *            the file containing the bundle
     * @param descriptor
     *            Optional: A descriptor that should be used for time matching
     * @throws VizException
     */
    public static void loadTo(File f, Map<String, String> variables)
            throws VizException {
        Bundle b = Bundle.unmarshalBundle(f, variables);
        
        IRenderableDisplay renderableDisplay = b.getDisplays()[0];
        IDescriptor bundleDescriptor = renderableDisplay.getDescriptor();
        bundleDescriptor.setNumberOfFrames(getMaxFrameNum());
       
        ((D2DTimeMatcher) bundleDescriptor.getTimeMatcher()).setLoadMode(LoadMode.VALID_TIME_SEQ);

        String bundleEditorId = DescriptorMap.getEditorId(bundleDescriptor
                .getClass().getName());
        // AbstractEditor editor = UiUtil.createOrOpenEditor(bundleEditorId,
        AbstractEditor editor = UiUtil.createEditor(bundleEditorId,
                b.getDisplays());
        editor.setLoopProperties(new NCLoopProperties());
        try {
        LoadSerializedXml.loadTo(editor, b);
    }
        catch (Exception e) {
        	System.out.println(" Error loading bundle" );
        }
    }
    
    public static int getMaxFrameNum() {
    	int maxFrameNum = 0;
    	IPreferenceStore prefs = NmapCommon.getNcepPreferenceStore();
    	maxFrameNum = prefs.getInt(SolarImagePreferences.NUM_FRAMES);
    	
    	if (maxFrameNum == 0)
    		maxFrameNum = SolarImagePreferences.DEFAULT_NUM_FRAMES;
    	
    	return maxFrameNum;
    }
}
