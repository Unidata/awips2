package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.common;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.grid.rsc.GridResourceData;

import gov.noaa.gsd.viz.ensemble.display.common.GridResourceHolder;
import gov.noaa.gsd.viz.ensemble.util.GlobalColor;
import gov.noaa.gsd.viz.ensemble.util.SWTResourceManager;

/***
 * 
 * This composite contains the meta-data widgets associated with resources
 * loaded into the plan view map.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2015   12565     polster     Initial creation
 * Dec 14, 2016   19443     polster     added isWidgetReady method
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class PlanViewInfoComposite extends Composite {

    private Label frameTimeUsingBasisLbl = null;

    private Label timeMatchResourceLbl = null;

    private Label primaryRscTimeLbl = null;

    private Label primaryRscLbl = null;

    private ScrolledComposite metaDataScrolledComposite = null;

    private Composite metaDataContents = null;

    public PlanViewInfoComposite(Composite parent, int style) {
        super(parent, style);
        createRootArea();
    }

    public void createRootArea() {

        setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
        GridLayout this_gl = new GridLayout(4, false);
        this_gl.horizontalSpacing = 2;
        this_gl.verticalSpacing = 3;
        this.setLayout(this_gl);

        primaryRscTimeLbl = new Label(this, SWT.BORDER);
        primaryRscTimeLbl
                .setFont(SWTResourceManager.getFont("Dialog", 10, SWT.NONE));
        primaryRscTimeLbl.setLayoutData(
                new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
        primaryRscTimeLbl.setText(" Time: ");

        frameTimeUsingBasisLbl = new Label(this, SWT.BORDER);
        frameTimeUsingBasisLbl
                .setFont(SWTResourceManager.getFont("Dialog", 9, SWT.NONE));
        frameTimeUsingBasisLbl.setAlignment(SWT.CENTER);
        GridData frameTimeUsingBasisLbl_gd = new GridData(SWT.FILL, SWT.CENTER,
                true, false, 3, 1);
        frameTimeUsingBasisLbl.setLayoutData(frameTimeUsingBasisLbl_gd);

        primaryRscLbl = new Label(this, SWT.BORDER);
        primaryRscLbl
                .setFont(SWTResourceManager.getFont("Dialog", 10, SWT.NORMAL));
        primaryRscLbl.setLayoutData(
                new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1));
        primaryRscLbl.setText(" Basis: ");

        timeMatchResourceLbl = new Label(this, SWT.BORDER);
        timeMatchResourceLbl
                .setFont(SWTResourceManager.getFont("Dialog", 9, SWT.NONE));
        timeMatchResourceLbl.setAlignment(SWT.CENTER);
        GridData timeMatchResourceLbl_gd = new GridData(SWT.FILL, SWT.CENTER,
                true, false, 3, 1);
        timeMatchResourceLbl.setLayoutData(timeMatchResourceLbl_gd);

        metaDataScrolledComposite = new ScrolledComposite(this,
                SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);

        GridData metaDataScrolledComposite_gd = new GridData(SWT.FILL, SWT.FILL,
                true, true, 4, 1);
        metaDataScrolledComposite.setLayoutData(metaDataScrolledComposite_gd);

        GridLayout metaDataScrolledComposite_gl = new GridLayout(1, true);
        metaDataScrolledComposite.setLayout(metaDataScrolledComposite_gl);
        metaDataScrolledComposite.setExpandHorizontal(true);
        metaDataScrolledComposite.setExpandVertical(true);
        metaDataContents = new Composite(metaDataScrolledComposite, SWT.NONE);
        GridData metaDataContents_gd = new GridData(SWT.FILL, SWT.FILL, true,
                true, 1, 1);
        metaDataContents.setLayoutData(metaDataContents_gd);
        metaDataScrolledComposite.layout();
        metaDataScrolledComposite.setContent(metaDataContents);
    }

    public void clearPlanViewInfo() {
        if (!timeMatchResourceLbl.isDisposed()) {
            timeMatchResourceLbl.setText("");
            frameTimeUsingBasisLbl.setText("");
        }
    }

    public void updatePlanViewInfo(String timeBasisRscName, String datatime) {
        if (!timeMatchResourceLbl.isDisposed()) {
            timeMatchResourceLbl.setBackground(
                    GlobalColor.get(GlobalColor.PALE_EXTRA_LIGHT_AZURE));
            timeMatchResourceLbl.setText(timeBasisRscName);
            frameTimeUsingBasisLbl.setBackground(
                    GlobalColor.get(GlobalColor.PALE_EXTRA_LIGHT_AZURE));
            frameTimeUsingBasisLbl.setText(datatime);
        }
    }

    public void updateMetaDataInfo(GridResourceHolder grh) {

        if (grh != null) {
            AbstractVizResource<?, ?> rsc = grh.getRsc();
            if (rsc != null) {
                GridResourceData grd = (GridResourceData) rsc.getResourceData();
                Map<String, RequestConstraint> map = grd.getMetadataMap();
                if (metaDataScrolledComposite.getChildren().length > 0) {
                    for (Control c : metaDataScrolledComposite.getChildren()) {
                        c.dispose();
                    }
                }
                metaDataContents = new Composite(metaDataScrolledComposite,
                        SWT.NONE);
                GridData metaDataContents_gd = new GridData(SWT.FILL, SWT.FILL,
                        true, true, 1, 1);
                metaDataContents.setLayoutData(metaDataContents_gd);
                metaDataContents.setLayout(new GridLayout(4, false));
                metaDataContents.setBackground(
                        GlobalColor.get(GlobalColor.PALE_EXTRA_LIGHT_AZURE));

                Set<String> keys = map.keySet();
                Iterator<String> iter = keys.iterator();
                String key = null;
                String value = null;
                RequestConstraint valueRC = null;
                Label metaDataDescription = null;
                Label metaDataValue = null;
                while (iter.hasNext()) {
                    key = iter.next();
                    if (key != null) {
                        valueRC = map.get(key);
                        value = valueRC.getConstraintValue();
                        if (value == null || value.indexOf("999999") > 0) {
                            continue;
                        } else {
                            addNameValuePairLabels(metaDataContents,
                                    metaDataDescription, metaDataValue, key,
                                    value);
                        }
                    }
                }
                key = "units";
                value = grh.getUnits();
                addNameValuePairLabels(metaDataContents, metaDataDescription,
                        metaDataValue, key, value);
            }
            metaDataScrolledComposite.setContent(metaDataContents);
        }
    }

    private void addNameValuePairLabels(Composite metaDataContents,
            Label metaDataDescription, Label metaDataValue, String name,
            String value) {
        metaDataDescription = new Label(metaDataContents, SWT.NONE);
        metaDataDescription.setLayoutData(
                new GridData(SWT.LEFT, SWT.CENTER, true, false, 2, 1));
        metaDataDescription.setText(name);
        metaDataDescription.setBackground(
                GlobalColor.get(GlobalColor.PALE_EXTRA_LIGHT_AZURE));
        metaDataValue = new Label(metaDataContents, SWT.NONE);
        metaDataValue.setLayoutData(
                new GridData(SWT.LEFT, SWT.CENTER, true, false, 2, 1));
        metaDataValue.setText(value);
        metaDataValue.setBackground(
                GlobalColor.get(GlobalColor.PALE_EXTRA_LIGHT_AZURE));
    }

    public void setEditable(final boolean enabled) {

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                if (isWidgetReady()) {
                    if (enabled) {
                        frameTimeUsingBasisLbl.setBackground(GlobalColor
                                .get(GlobalColor.PALE_EXTRA_LIGHT_AZURE));
                        timeMatchResourceLbl.setBackground(GlobalColor
                                .get(GlobalColor.PALE_EXTRA_LIGHT_AZURE));
                    } else {
                        frameTimeUsingBasisLbl.setBackground(
                                GlobalColor.get(GlobalColor.LIGHT_GRAY));
                        timeMatchResourceLbl.setBackground(
                                GlobalColor.get(GlobalColor.LIGHT_GRAY));
                    }

                    primaryRscLbl.setEnabled(enabled);
                    primaryRscTimeLbl.setEnabled(enabled);
                    timeMatchResourceLbl.setEnabled(enabled);
                    frameTimeUsingBasisLbl.setEnabled(enabled);
                }
            }
        });

    }

    private boolean isWidgetReady() {
        boolean isReady = false;

        if (frameTimeUsingBasisLbl != null
                && !frameTimeUsingBasisLbl.isDisposed()
                && timeMatchResourceLbl != null
                && !timeMatchResourceLbl.isDisposed() && primaryRscLbl != null
                && !primaryRscLbl.isDisposed() && primaryRscTimeLbl != null
                && !primaryRscTimeLbl.isDisposed()
                && metaDataScrolledComposite != null
                && !metaDataScrolledComposite.isDisposed()
                && metaDataContents != null && !metaDataContents.isDisposed()) {

            isReady = true;
        }
        return isReady;
    }
}
