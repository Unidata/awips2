package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common;

import gov.noaa.gsd.viz.ensemble.util.GlobalColor;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.viz.core.VizApp;

/***
 * 
 * This composite contains the meta-data associated with resources
 * loaded into the time series (plume) editor. 
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2015  12565      polster     Initial creation
 *
 * </pre>
 *
 * @author polster
 * @version 1.0
 */
public class TimeSeriesInfoComposite extends Composite {

    private Label timeSeriesPointLabelLbl = null;

    private Label timeSeriesPointValueLbl = null;

    public TimeSeriesInfoComposite(Composite parent, int style) {
        super(parent, style);
        createRootArea();
    }

    private void createRootArea() {

        setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));

        GridLayout timeSeriesInfoComposite_gl = new GridLayout(3, false);
        timeSeriesInfoComposite_gl.horizontalSpacing = 2;
        timeSeriesInfoComposite_gl.verticalSpacing = 3;
        setLayout(timeSeriesInfoComposite_gl);

        timeSeriesPointLabelLbl = new Label(this, SWT.BORDER);
        timeSeriesPointLabelLbl.setFont(SWTResourceManager.getFont("Dialog",
                10, SWT.NORMAL));
        timeSeriesPointLabelLbl.setLayoutData(new GridData(SWT.FILL,
                SWT.CENTER, false, false, 1, 1));
        timeSeriesPointLabelLbl.setText("  Location: ");

        timeSeriesPointValueLbl = new Label(this, SWT.BORDER);
        timeSeriesPointValueLbl.setFont(SWTResourceManager.getFont("Dialog",
                10, SWT.NORMAL));
        timeSeriesPointValueLbl.setAlignment(SWT.CENTER);
        timeSeriesPointValueLbl.setForeground(SWTResourceManager.getColor(0, 0,
                0));
        timeSeriesPointValueLbl.setBackground(GlobalColor
                .get(GlobalColor.PALE_EXTRA_LIGHT_AZURE));
        GridData timeSeriesPointValueLbl_gd = new GridData(SWT.FILL,
                SWT.CENTER, true, false, 2, 1);
        timeSeriesPointValueLbl.setLayoutData(timeSeriesPointValueLbl_gd);

        // need to fill some space
        Composite fillerComposite = new Composite(this, SWT.NONE);
        fillerComposite.setSize(20, 150);
        GridData fillerComposite_gd = new GridData(SWT.FILL, SWT.CENTER, true,
                false, 3, 1);
        fillerComposite.setLayoutData(fillerComposite_gd);

    }

    protected void updateTimeSeriesInfo(String pointValue) {
        if (!timeSeriesPointValueLbl.isDisposed()) {
            timeSeriesPointValueLbl.setText(pointValue);
        }
    }

    public void setViewEditable(final boolean enabled) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {

                if (enabled) {
                    timeSeriesPointValueLbl.setBackground(GlobalColor
                            .get(GlobalColor.PALE_EXTRA_LIGHT_AZURE));
                } else {
                    timeSeriesPointValueLbl.setBackground(GlobalColor
                            .get(GlobalColor.LIGHT_GRAY));
                }
                timeSeriesPointLabelLbl.setEnabled(enabled);
                timeSeriesPointValueLbl.setEnabled(enabled);
            }
        });

    }

}
