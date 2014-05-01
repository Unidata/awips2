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
package com.raytheon.viz.gfe.dialogs;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKeyDef;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherSubKey;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2010            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MaskDialog extends CaveJFACEDialog {

    private Parm parm;

    private String siteId;

    private String mask;

    private ToggleSelectList typeList;

    private Group covGroup;

    private Group intenGroup;

    private Group visGroup;

    private Group attrGroup;

    /**
     * @param parentShell
     */
    protected MaskDialog(Shell parentShell, Parm parm) {
        super(parentShell);
        this.parm = parm;
        this.siteId = parm.getParmID().getDbId().getSiteId();
    }

    @Override
    protected void configureShell(Shell newShell) {
        String title;

        if (parm.getGridInfo().getGridType().equals(GridType.DISCRETE)) {
            title = "Discrete Values";
        } else {
            title = "Weather Values";
        }

        newShell.setText(title);
        super.configureShell(newShell);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);

        if (parm.getGridInfo().getGridType().equals(GridType.DISCRETE)) {
            createDiscreteItems(comp);
        } else {
            createWxItems(comp);
        }
        return comp;
    }

    private void createDiscreteItems(Composite comp) {
        DiscreteDefinition def = DiscreteKey.discreteDefinition(siteId);

        Group group = new Group(comp, SWT.NONE | SWT.V_SCROLL);
        group.setText("Types");
        group.setLayout(new GridLayout(1, false));
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        group.setLayoutData(gridData);

        typeList = new ToggleSelectList(group, SWT.MULTI | SWT.V_SCROLL);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.heightHint = typeList.getItemHeight() * 30;
        typeList.setLayoutData(gridData);

        for (DiscreteKeyDef keyDef : def.keys(parm.getParmID())) {
            typeList.add(keyDef.getSymbol());
        }
    }

    private void createWxItems(Composite comp) {

        ((GridLayout) comp.getLayout()).numColumns = 5;

        covGroup = new Group(comp, SWT.NONE | SWT.V_SCROLL);
        covGroup.setText("Coverages");
        covGroup.setLayout(new GridLayout(1, false));
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        covGroup.setLayoutData(gridData);

        Group typeGroup = new Group(comp, SWT.NONE | SWT.V_SCROLL);
        typeGroup.setText("Types");
        typeGroup.setLayout(new GridLayout(1, false));
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        typeGroup.setLayoutData(gridData);

        intenGroup = new Group(comp, SWT.NONE | SWT.V_SCROLL);
        intenGroup.setText("Intensities");
        intenGroup.setLayout(new GridLayout(1, false));
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        intenGroup.setLayoutData(gridData);

        visGroup = new Group(comp, SWT.NONE | SWT.V_SCROLL);
        visGroup.setText("Visibilities");
        visGroup.setLayout(new GridLayout(1, false));
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        visGroup.setLayoutData(gridData);

        attrGroup = new Group(comp, SWT.NONE | SWT.V_SCROLL);
        attrGroup.setText("Attributes");
        attrGroup.setLayout(new GridLayout(1, false));
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        attrGroup.setLayoutData(gridData);

        typeList = new ToggleSelectList(typeGroup, SWT.MULTI);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);

        final List<String> WxTypes = WeatherSubKey.availableWxTypes(siteId);
        Collections.sort(WxTypes);
        List<String> visibilities = WeatherSubKey.availableVisibilities(siteId);

        Set<String> coverages = new HashSet<String>();
        Set<String> intensities = new HashSet<String>();
        Set<String> attributes = new HashSet<String>();
        for (String wxType : WxTypes) {
            typeList.add(wxType);
            coverages.addAll(WeatherSubKey.availableCoverages(siteId, wxType));
            intensities.addAll(WeatherSubKey.availableIntensities(siteId,
                    wxType));
            attributes
                    .addAll(WeatherSubKey.availableAttributes(siteId, wxType));
        }
        if (WxTypes.size() > 30) {
            gridData.heightHint = typeList.getItemHeight() * WxTypes.size();
        }
        typeList.setLayoutData(gridData);

        String[] covs = coverages.toArray(new String[coverages.size()]);
        Arrays.sort(covs);
        for (String cov : covs) {
            Button button = new Button(covGroup, SWT.CHECK);
            button.setText(cov);
            button.setLayoutData(new GridData(SWT.DEFAULT, SWT.FILL, false,
                    true));
        }

        String[] intens = intensities.toArray(new String[intensities.size()]);
        Arrays.sort(intens);
        for (String inten : intens) {
            Button button = new Button(intenGroup, SWT.CHECK);
            button.setText(inten);
            button.setLayoutData(new GridData(SWT.DEFAULT, SWT.FILL, false,
                    true));
        }

        for (String vis : visibilities) {
            Button button = new Button(visGroup, SWT.CHECK);
            button.setText(vis);
            button.setLayoutData(new GridData(SWT.DEFAULT, SWT.FILL, false,
                    true));
        }

        String[] atts = attributes.toArray(new String[attributes.size()]);
        Arrays.sort(atts);
        for (String att : atts) {
            Button button = new Button(attrGroup, SWT.CHECK);
            button.setText(att);
            button.setLayoutData(new GridData(SWT.DEFAULT, SWT.FILL, false,
                    true));
        }
    }

    @Override
    protected void okPressed() {
        StringBuilder sb = new StringBuilder();
        if (parm.getGridInfo().getGridType().equals(GridType.DISCRETE)) {
            sb.append("contains(");
            sb.append(parm.getParmID().getCompositeName()).append(", ");
            sb.append(selectedTypes());
            sb.append(")");
        } else {
            sb.append("wxcontains(Wx,");
            sb.append(selectedButtons(covGroup)).append(",");
            sb.append(selectedTypes()).append(",");
            sb.append(selectedButtons(intenGroup)).append(",");
            sb.append(selectedButtons(visGroup)).append(",");
            sb.append(selectedButtons(attrGroup));
            sb.append(")");
        }
        this.mask = sb.toString();
        super.okPressed();
    }

    /**
     * @return
     */
    private String selectedTypes() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");

        for (String s : typeList.getSelection()) {
            if (sb.length() > 1) {
                sb.append(", ");
            }
            sb.append("'").append(s).append("'");
        }

        sb.append("]");
        return sb.toString();
    }

    public String selectedButtons(Group group) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");

        for (Control control : group.getChildren()) {
            Button button = (Button) control;
            if (button.getSelection()) {
                if (sb.length() > 1) {
                    sb.append(", ");
                }
                sb.append("'").append(button.getText()).append("'");
            }
        }

        sb.append("]");
        return sb.toString();
    }

    public String mask() {
        return mask;
    }
}
