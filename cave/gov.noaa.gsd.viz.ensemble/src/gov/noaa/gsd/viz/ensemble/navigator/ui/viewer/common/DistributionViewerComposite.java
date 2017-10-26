package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common;

import gov.noaa.gsd.viz.ensemble.display.distribution.DistributionViewUI;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

/**
 * 
 * The base composite to build the distribution view GUI.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * Jan 12, 2016    12301       poster   Initial skeletal creation
 * Jan 16, 2016    12301       jing     Add functioning body
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class DistributionViewerComposite extends Composite {

    private DistributionViewUI ghGUI;

    public DistributionViewerComposite(Composite parent, int style) {
        super(parent, style);
        setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));

        GridLayout this_gl = new GridLayout(1, false);
        this_gl.marginHeight = 0;
        this_gl.marginWidth = 0;
        this_gl.horizontalSpacing = 0;
        this_gl.verticalSpacing = 0;
        setLayout(this_gl);

        ghGUI = new DistributionViewUI(this, style);

    }

    public DistributionViewUI getGhGUI() {
        return ghGUI;
    }

    synchronized public void setViewEditable(boolean enabled) {
        setEnabled(enabled);
        ghGUI.setViewEditable(enabled);
    }

}
