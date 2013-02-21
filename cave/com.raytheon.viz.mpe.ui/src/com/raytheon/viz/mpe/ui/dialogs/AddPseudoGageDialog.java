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
package com.raytheon.viz.mpe.ui.dialogs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.shef.tables.Pseudogageval;
import com.raytheon.uf.common.dataplugin.shef.tables.Rwresult;
import com.raytheon.viz.hydrocommon.whfslib.IHFSDbGenerated;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.rsc.MPEGageResource;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2009             snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class AddPseudoGageDialog extends CaveSWTDialog {

    private Font font;

    private Button okBtn;

    private Coordinate latlon;

    private Label valueLabel;

    private Scale valueScale;

    private Button cancelButton;

    protected int sel = 0;

    private Date datetime;

    private SimpleDateFormat sdf;

    private String pgageId;

    private MPEGageResource resource;

    private static final String psstr = "PSEUDO";

    /**
     * Constructor
     * 
     * @param parent
     *            Parent shell
     */
    public AddPseudoGageDialog(Shell parent, Coordinate latLon,
            MPEGageResource resource) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Add Pseudo Gage");
        this.latlon = latLon;
        this.resource = resource;
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        font = new Font(shell.getDisplay(), "Courier", 10, SWT.NORMAL);
        datetime = MPEDisplayManager.getCurrent().getCurrentEditDate();
        sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(datetime);
        // datetime = String.format("%s", datetime.getTime());

        // Initialize all of the controls and layouts
        this.initializeComponents();
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        this.createGageValComp();
        this.createButtonComp();
    }

    /**
     * Create the data options group and controls.
     */
    private void createGageValComp() {

        // Create a container to hold the label and value scale.
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite comp1 = new Composite(shell, SWT.NONE);
        GridLayout gageValCompLayout = new GridLayout(2, false);
        comp1.setLayout(gageValCompLayout);
        comp1.setLayoutData(gd);

        /**
         * Pseudo Gage Value Scale
         */
        Label l = new Label(comp1, SWT.LEFT);
        l.setText("Pseudo Gage Accumulation (inches): ");

        valueLabel = new Label(comp1, SWT.LEFT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 180;
        valueLabel.setLayoutData(gd);
        valueLabel.setAlignment(SWT.LEFT);
        valueLabel.setText("0.00");

        valueScale = new Scale(comp1, SWT.HORIZONTAL);
        GridData data = new GridData(600, SWT.DEFAULT);
        data.horizontalSpan = 2;
        valueScale.setLayoutData(data);
        valueScale.setMinimum(0);
        valueScale.setMaximum(500);
        valueScale.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                sel = valueScale.getSelection();
                valueLabel.setText(String.format("%4.2f", sel / 100.0f));
            }

        });

    }

    private void createButtonComp() {
        // Create a container to hold the button.
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite comp2 = new Composite(shell, SWT.NONE);
        GridLayout ButtonCompLayout = new GridLayout(2, false);
        comp2.setLayout(ButtonCompLayout);
        comp2.setLayoutData(gd);

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = 90;
        okBtn = new Button(comp2, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                writePGage();
                close();
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 90;
        cancelButton = new Button(comp2, SWT.PUSH);
        cancelButton.setText("Cancel");
        cancelButton.setLayoutData(gd);
        cancelButton.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }

        });
    }

    protected void writePGage() {
        int maxpgage = 0;
        String where = " WHERE obstime='" + sdf.format(datetime) + "' ";
        ArrayList<Pseudogageval> pgages = MPEDataManager.getInstance()
                .getPseudoGageVal(where);
        if (!pgages.isEmpty()) {
            for (Pseudogageval gage : pgages) {
                int n = (Integer.parseInt(gage.getId().getPseudoGageId()
                        .substring(psstr.length())));
                if (n > maxpgage) {
                    maxpgage = n;
                }
            }
        }
        maxpgage = maxpgage + 1;
        StringBuilder sb = new StringBuilder(psstr);
        if (maxpgage < 10) {
            sb.append(0);
        }
        pgageId = sb.append(maxpgage).toString();
        UnitConverter conv = NonSI.INCH.getConverterTo(SI.MILLIMETER);
        float gval = (float) conv.convert(sel / 100.f);
        where = "(pseudo_gage_id, obstime, lat, lon, gage_value, man_edited, prev_gage_value)"
                + " values ('"
                + pgageId
                + "', '"
                + sdf.format(datetime)
                + "', "
                + latlon.y
                + ", "
                + (latlon.x * -1)
                + ", "
                + gval
                + ", '" + "F', " + -99.0 + ")";
        MPEDataManager.getInstance().insertPseudoGageVal(where);
        if (resource != null) {
            resource.reloadGages();
        }
        this.updateRWR();
    }

    private void updateRWR() {
        String where = "WHERE rfc='" + MPEDataManager.getInstance().getRFC()
                + "' AND obstime='" + sdf.format(datetime) + "'";
        List<Rwresult> rwr = IHFSDbGenerated.GetRWResult(where);
        Rwresult rsr = rwr.get(0);
        int np = rsr.getNumPseudoGages() + 1;
        rsr.setNumPseudoGages(np);
        IHFSDbGenerated.UpdateRWResult(rsr);
    }
}
