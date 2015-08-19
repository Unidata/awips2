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
package com.raytheon.uf.viz.localization.perspective.ui.compare;

import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.compare.CompareConfiguration;
import org.eclipse.compare.CompareEditorInput;
import org.eclipse.compare.ResourceNode;
import org.eclipse.compare.structuremergeviewer.Differencer;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.ISaveablesSource;
import org.eclipse.ui.Saveable;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.localization.perspective.editor.LocalizationEditorInput;

/**
 * Comparing editor input for localization files
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 24, 2011            mschenke    Initial creation
 * Jan 22, 2015  #4108     randerso    Allow editing in the compare editor
 * Aug 18, 2015  3806      njensen     Use SaveableOutputStream to save
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationCompareEditorInput extends CompareEditorInput
        implements ISaveablesSource {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalizationCompareEditorInput.class);

    private LocalizationEditorInput left;

    private LocalizationEditorInput right;

    private ResourceNode leftNode;

    private ResourceNode rightNode;

    private Saveable[] saveables;

    private static class LocalizationSaveable extends Saveable {

        private LocalizationCompareEditorInput parent;

        private boolean left;

        private LocalizationEditorInput input;

        private ResourceNode node;

        public LocalizationSaveable(LocalizationCompareEditorInput parent,
                boolean left) {
            this.parent = parent;
            this.left = left;

            if (left) {
                this.input = parent.left;
                this.node = parent.leftNode;
            } else {
                this.input = parent.right;
                this.node = parent.rightNode;
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.ui.Saveable#getName()
         */
        @Override
        public String getName() {
            return input.getName();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.ui.Saveable#getToolTipText()
         */
        @Override
        public String getToolTipText() {
            return input.getToolTipText();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.ui.Saveable#getImageDescriptor()
         */
        @Override
        public ImageDescriptor getImageDescriptor() {
            return input.getImageDescriptor();
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.ui.Saveable#doSave(org.eclipse.core.runtime.IProgressMonitor
         * )
         */
        @Override
        public void doSave(IProgressMonitor monitor) throws CoreException {
            // flush changes from the viewer into the node
            if (left) {
                parent.flushLeftViewers(monitor);
            } else {
                parent.flushRightViewers(monitor);
            }

            // write node contents to the localization file
            LocalizationFile lf = input.getLocalizationFile();
            try (SaveableOutputStream os = lf.openOutputStream();
                    InputStream is = node.getContents()) {

                byte[] buf = new byte[2048];
                int len = is.read(buf);
                while (len > 0) {
                    os.write(buf, 0, len);
                    len = is.read(buf);
                }

                os.save();

                // Force other editors on this file to update
                input.getFile().refreshLocal(IResource.DEPTH_ZERO, monitor);
            } catch (CoreException e) {
                statusHandler.error("Error refreshing local resources for "
                        + input.getFile().getName(), e);
            } catch (Exception e) {
                statusHandler.error("Error saving " + lf, e);
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.ui.Saveable#isDirty()
         */
        @Override
        public boolean isDirty() {
            return (left ? parent.isLeftSaveNeeded() : parent
                    .isRightSaveNeeded());
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = (prime * result)
                    + ((input == null) ? 0 : input.hashCode());
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            LocalizationSaveable other = (LocalizationSaveable) obj;
            if (input == null) {
                if (other.input != null) {
                    return false;
                }
            } else if (!input.equals(other.input)) {
                return false;
            }
            return true;
        }

    }

    public LocalizationCompareEditorInput(LocalizationEditorInput left,
            LocalizationEditorInput right) {
        super(new CompareConfiguration());

        CompareConfiguration config = getCompareConfiguration();
        config.setLeftEditable(!left.getFile().isReadOnly());
        config.setRightEditable(!right.getFile().isReadOnly());
        config.setLeftLabel(left.getName());
        config.setRightLabel(right.getName());

        this.left = left;
        this.right = right;

        this.leftNode = new ResourceNode(left.getFile());
        this.rightNode = new ResourceNode(right.getFile());

        this.saveables = new Saveable[] { new LocalizationSaveable(this, true),
                new LocalizationSaveable(this, false) };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.compare.CompareEditorInput#prepareInput(org.eclipse.core.
     * runtime.IProgressMonitor)
     */
    @Override
    protected Object prepareInput(IProgressMonitor pm)
            throws InvocationTargetException, InterruptedException {

        return new Differencer().findDifferences(false, pm, null, null,
                leftNode, rightNode);

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISaveablesSource#getSaveables()
     */
    @Override
    public Saveable[] getSaveables() {
        return this.saveables;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ISaveablesSource#getActiveSaveables()
     */
    @Override
    public Saveable[] getActiveSaveables() {
        return getSaveables();
    }
}
