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

/**
 * 
 */
package com.raytheon.viz.shapefile.wizard;

import java.io.File;
import java.util.HashMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ShadeableCapability;
import com.raytheon.viz.shapefile.rsc.ShapefileResource;
import com.raytheon.viz.shapefile.rsc.ShapefileResourceData;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Shapefile Wizard
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ShapefileWizard extends Wizard implements INewWizard {

    private String theShpFile;

    private String theLabelAttribute;

    private ShapefileWizardFilePage theFilePage;

    private ShapefileWizardLayerPage theLayerPage;

    private ShapefileWizardColorPage theColorPage;

    private boolean isShaded;

    private boolean isRegularPolygon;

    private String theShadedAttribute;

    private HashMap<Object, RGB> colorMap;

    // private boolean isFinished;

    public ShapefileWizard() {
        super();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#addPages()
     */
    @Override
    public void addPages() {
        theFilePage = new ShapefileWizardFilePage();
        addPage(theFilePage);
        theLayerPage = new ShapefileWizardLayerPage();
        addPage(theLayerPage);
        theColorPage = new ShapefileWizardColorPage();
        addPage(theColorPage);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.Wizard#performFinish()
     */
    @Override
    public boolean performFinish() {
        Job j = new Job(theLabelAttribute) {

            @Override
            protected IStatus run(IProgressMonitor anMonitor) {
                IDisplayPaneContainer editor = EditorUtil
                        .getActiveVizContainer();
                if (editor != null) {
                    IDescriptor desc = editor.getActiveDisplayPane()
                            .getRenderableDisplay().getDescriptor();
                    ShapefileResource vector = null;
                    File f = new File(theShpFile);
                    boolean spatiallySplit = false;
                    long sz = f.length();
                    if (sz > (10 * 1024 * 1024)) {
                        // split the file
                        spatiallySplit = true;
                    }
                    try {
                        ShapefileResourceData data = new ShapefileResourceData(
                                theShpFile.toString(),
                                new String[] { theLabelAttribute });
                        vector = data.construct(new LoadProperties(), desc);
                        vector.getCapability(ColorableCapability.class)
                                .setColor(new RGB(255, 255, 255));
                        vector.setSpatiallySplit(spatiallySplit);
                        if (isShaded) {
                            vector.getCapability(ShadeableCapability.class)
                                    .setShadingField(theShadedAttribute);
                            vector.setColorMapAttribute(theShadedAttribute);
                            vector.setColorMap(colorMap);
                            vector.setRegularPolygon(isRegularPolygon);

                        }

                        desc.getResourceList().add(vector);
                    } catch (VizException e) {
                        return new Status(Status.ERROR, "com.raytheon.viz",
                                Status.OK, "Error loading shapefile", e);
                    }
                    editor.refresh();
                }

                return Status.OK_STATUS;
            }

        };
        j.schedule();

        return true;
    }

    public void init(IWorkbench anWorkbench, IStructuredSelection anSelection) {

    }

    /**
     * @return the shpFile
     */
    public String getShpFile() {
        return theShpFile;
    }

    /**
     * @param anShpFile
     *            the shpFile to set
     */
    public void setShpFile(String anShpFile) {
        theShpFile = anShpFile;
    }

    // /*
    // * (non-Javadoc)
    // *
    // * @see org.eclipse.jface.wizard.Wizard#canFinish()
    // */
    // @Override
    // public boolean canFinish() {
    // return isFinished;
    // }
    //
    // public void setFinished(boolean isFinished) {
    // this.isFinished = isFinished;
    // }
    //
    /**
     * @return the labelAttribute
     */
    public String getLabelAttribute() {
        return theLabelAttribute;
    }

    /**
     * @param anLabelAttribute
     *            the labelAttribute to set
     */
    public void setLabelAttribute(String anLabelAttribute) {
        theLabelAttribute = anLabelAttribute;
    }

    /**
     * @return the isShaded
     */
    public boolean isShaded() {
        return isShaded;
    }

    /**
     * @param isShaded
     *            the isShaded to set
     */
    public void setShaded(boolean isShaded) {
        this.isShaded = isShaded;
    }

    /**
     * @return the shadedAttribute
     */
    public String getShadedAttribute() {
        return theShadedAttribute;
    }

    /**
     * @param anShadedAttribute
     *            the shadedAttribute to set
     */
    public void setShadedAttribute(String anShadedAttribute) {
        theShadedAttribute = anShadedAttribute;
    }

    /**
     * @return the colorMap
     */
    public HashMap<Object, RGB> getColorMap() {
        return colorMap;
    }

    /**
     * @param colorMap
     *            the colorMap to set
     */
    public void setColorMap(HashMap<Object, RGB> colorMap) {
        this.colorMap = colorMap;
    }

    /**
     * @return the isRegularPolygon
     */
    public boolean isRegularPolygon() {
        return isRegularPolygon;
    }

    /**
     * @param isRegularPolygon
     *            the isRegularPolygon to set
     */
    public void setRegularPolygon(boolean isRegularPolygon) {
        this.isRegularPolygon = isRegularPolygon;
    }

}
