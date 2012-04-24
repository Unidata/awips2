package gov.noaa.nws.ncep.viz.tools.colorMapEditor;

import java.util.ArrayList;

import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.viz.ui.dialogs.colordialog.ColorBar;
import com.raytheon.viz.ui.dialogs.colordialog.ColorData;
import com.raytheon.viz.ui.dialogs.colordialog.IColorBarAction;

public class ColorBarViewer extends ColorBar {

	public ColorBarViewer(Composite parent, IColorBarAction callback,
			String[] sliderText, ColorMapParameters cmapParam) {
		super(parent, callback, cmapParam, false);
	}
	
	// TODO:   Not Migrated...
	public void setNewColorMap( String[] sliderText, ArrayList<ColorData> newColors ) {
		
		//setSliderText( sliderText );
		setStartingColors( newColors );		
		revertColorBar();
	}
	
	protected void setStartingColors(ArrayList<ColorData> newColors) {
        startingColors = new ArrayList<ColorData>(newColors);
    }
}
