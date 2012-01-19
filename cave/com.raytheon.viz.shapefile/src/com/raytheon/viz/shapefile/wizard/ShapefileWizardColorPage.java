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

package com.raytheon.viz.shapefile.wizard;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.shapefile.rsc.ShapefileResource;
import com.raytheon.viz.shapefile.rsc.ShapefileUtil;

/**
 * Shapefile Wizard: select coloring of the data
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date            Ticket#     Engineer    Description
 *  ------------    ----------  ----------- --------------------------
 *  7/1/06                      chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ShapefileWizardColorPage extends WizardPage {

    private ListViewer theList;

    private TableViewer theValueList;

    private Button randomColorButton;

    private Button rgbColorButton;

    private Button regularCheckBox;

    public ShapefileWizardColorPage() {
        super("Shapefile Wizard: Select Coloring");
        this.setTitle("Shapefile Wizard: Select Coloring");
        this.setDescription("Select the method of coloring the data");

        setPageComplete(true);
    }

    public void createControl(Composite aParent) {

        Composite composite = new Composite(aParent, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        composite.setLayout(gl);

        Group buttonGroup = new Group(composite, SWT.SHADOW_OUT);
        buttonGroup.setLayout(new GridLayout(3, true));
        Button button1 = new Button(buttonGroup, SWT.RADIO);
        button1.setText("Wireframe Only");
        button1.setLayoutData(new GridData(GridData.FILL, GridData.CENTER,
                true, false, 1, 1));
        button1.setSelection(true);
        randomColorButton = new Button(buttonGroup, SWT.RADIO);
        randomColorButton.setText("Unique Value Color Map");
        randomColorButton.setLayoutData(new GridData(GridData.FILL,
                GridData.CENTER, true, false, 1, 1));
        rgbColorButton = new Button(buttonGroup, SWT.RADIO);
        rgbColorButton.setText("RGB Dynamic Color Map");
        rgbColorButton.setLayoutData(new GridData(GridData.FILL,
                GridData.CENTER, true, false, 1, 1));

        button1.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent anE) {
                ((ShapefileWizard) getWizard()).setShaded(false);
                theList.getList().setEnabled(false);
                setPageComplete(true);
            }

        });

        randomColorButton.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent anE) {
                ((ShapefileWizard) getWizard()).setShaded(true);
                theList.getList().setEnabled(true);
                setPageComplete(theList.getList().getSelectionCount() > 0);
            }

        });

        rgbColorButton.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent anE) {
                ((ShapefileWizard) getWizard()).setShaded(true);
                theList.getList().setEnabled(true);
                setPageComplete(theList.getList().getSelectionCount() > 0);
            }

        });

        theList = new ListViewer(composite, SWT.V_SCROLL | SWT.BORDER
                | SWT.SINGLE);
        theList.getList().setLayoutData(new GridData(GridData.FILL_BOTH));
        theList.getList().setEnabled(false);

        theValueList = new TableViewer(composite, SWT.V_SCROLL | SWT.BORDER
                | SWT.SINGLE);
        theValueList.getControl().setLayoutData(
                new GridData(GridData.FILL_BOTH));

        theValueList.setLabelProvider(new ColorProvider());
        composite.addPaintListener(new PaintListener() {

            public void paintControl(PaintEvent anE) {
                if (theList.getList().getItemCount() == 0) {
                    try {
                        String shpName = ((ShapefileWizard) getWizard())
                                .getShpFile();
                        File f = new File(shpName);
                        String[] attribs = ShapefileUtil
                                .getAttributes(f, false);
                        theList.getList().removeAll();
                        theList.add(attribs);
                    } catch (VizException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                }
            }

        });

        theList.addSelectionChangedListener(new ISelectionChangedListener() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged
             * (org.eclipse.jface.viewers.SelectionChangedEvent)
             */
            public void selectionChanged(SelectionChangedEvent event) {
                String attribute = (String) ((IStructuredSelection) theList
                        .getSelection()).getFirstElement();
                ((ShapefileWizard) getWizard()).setShadedAttribute(attribute);

                // Call the generation routine
                String shpName = ((ShapefileWizard) getWizard()).getShpFile();
                File f = new File(shpName);
                try {
                    theValueList.getTable().removeAll();
                    HashMap<Object, RGB> map = null;
                    if (randomColorButton.getSelection()) {
                        map = ShapefileResource.generateRandomAttributeMap(
                                attribute, f);
                    } else if (rgbColorButton.getSelection()) {
                        map = ShapefileResource.generateRGBColorMap(attribute,
                                f);
                    }

                    ((ShapefileWizard) getWizard()).setColorMap(map);

                    refreshLabels();
                    setPageComplete(true);

                } catch (VizException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

        });

        theValueList.addDoubleClickListener(new IDoubleClickListener() {

            public void doubleClick(DoubleClickEvent event) {
                Object o = ((IStructuredSelection) event.getSelection())
                        .getFirstElement();

                ColorDialog cd = new ColorDialog(theList.getControl()
                        .getShell());
                cd.setRGB(((ShapefileWizard) getWizard()).getColorMap().get(o));
                RGB newColor = cd.open();
                ((ShapefileWizard) getWizard()).getColorMap().put(o, newColor);

                theValueList.update(o, null);

            }

        });

        regularCheckBox = new Button(composite, SWT.CHECK);
        regularCheckBox.setText("Regular Polygon (check only for grids)");

        regularCheckBox.addSelectionListener(new SelectionListener() {

            public void widgetDefaultSelected(SelectionEvent e) {

            }

            public void widgetSelected(SelectionEvent e) {
                ((ShapefileWizard) getWizard())
                        .setRegularPolygon(regularCheckBox.getSelection());
            }

        });
        setControl(composite);

    }

    private void refreshLabels() {
        HashMap<Object, RGB> map = ((ShapefileWizard) getWizard())
                .getColorMap();
        Set<Object> objSet = map.keySet();

        objSet = new TreeSet<Object>(objSet);
        Iterator<Object> objIterator = objSet.iterator();

        while (objIterator.hasNext()) {
            Object o = objIterator.next();
            theValueList.add(o);
        }
    }

    public class ColorProvider implements IColorProvider, ILabelProvider {

        ArrayList<Color> allocatedColors = new ArrayList<Color>();

        public Color getBackground(Object element) {
            return null;
        }

        public Color getForeground(Object element) {
            RGB rgb = ((ShapefileWizard) getWizard()).getColorMap()
                    .get(element);

            if (rgb == null) {
                return null;
            }

            Color c = new Color(getControl().getDisplay(), rgb);
            allocatedColors.add(c);
            return c;
        }

        public void addListener(ILabelProviderListener listener) {

        }

        public void dispose() {
            for (Color c : allocatedColors) {
                c.dispose();
            }
        }

        public boolean isLabelProperty(Object element, String property) {
            return true;
        }

        public void removeListener(ILabelProviderListener listener) {
        }

        public Image getImage(Object element) {
            return null;
        }

        public String getText(Object element) {
            return element.toString();
        }

    }

}
