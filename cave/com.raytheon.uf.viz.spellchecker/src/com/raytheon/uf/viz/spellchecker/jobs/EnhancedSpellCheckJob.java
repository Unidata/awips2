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
package com.raytheon.uf.viz.spellchecker.jobs;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.content.IContentTypeManager;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.spelling.ISpellingProblemCollector;
import org.eclipse.ui.texteditor.spelling.SpellingContext;
import org.eclipse.ui.texteditor.spelling.SpellingService;

/**
 * Spelling job that will run through the entire text and add all the problems
 * instead of one at a time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2014 3453       rblum     Initial creation
 * 
 * </pre>
 * 
 * @author rblum
 * @version 1.0
 */

public class EnhancedSpellCheckJob extends Job {

    private IDocument document;
    
    private IRegion[] region = new IRegion[1];

    private ISpellingProblemCollector collector;

    /**
     * @param name
     */
    public EnhancedSpellCheckJob(String name) {
        super(name);
    }

    public void setDocument(IDocument document) {
        this.document = document;
    }
    
    public void setRegion(IRegion region) {
        this.region[0] = region;
    }

    public void setCollector(ISpellingProblemCollector collector) {
        this.collector = collector;
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        SpellingService service = (SpellingService) PlatformUI.getWorkbench()
                .getService(SpellingService.class);
        if (service == null) {
            EditorsUI.getPreferenceStore().setValue(SpellingService.PREFERENCE_SPELLING_ENGINE,
                    "spellingEngine");
            service = new SpellingService(EditorsUI.getPreferenceStore());
        }

        SpellingContext context = new SpellingContext();
        context.setContentType(Platform.getContentTypeManager().getContentType(
                IContentTypeManager.CT_TEXT));
        synchronized (this) {
            service.check(document, region, context, collector, monitor);
        }

        return Status.OK_STATUS;
    }
}
