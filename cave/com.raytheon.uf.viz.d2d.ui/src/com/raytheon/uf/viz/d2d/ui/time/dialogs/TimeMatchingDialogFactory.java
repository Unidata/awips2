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
package com.raytheon.uf.viz.d2d.ui.time.dialogs;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Factory class for constructing time matching dialogs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TimeMatchingDialogFactory {

    private static class DialogStruct {
        private AbstractTimeMatchingDialog dialog;

        private Exception exception;
    }

    public static AbstractTimeMatchingDialog constructDialog(
            final Class<? extends AbstractTimeMatchingDialog> clazz,
            final LoadProperties loadProps, final D2DTimeMatcher timeMatcher,
            final DataTime[] availableTimes, final IDescriptor descriptor)
            throws VizException {
        final DialogStruct struct = new DialogStruct();
        struct.dialog = null;
        struct.exception = null;

        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                // construct the dialog with window's shell
                Shell shell = VizWorkbenchManager.getInstance()
                        .getCurrentWindow().getShell();

                Class<?>[] constructorClasses = new Class<?>[] { Shell.class };
                Object[] args = new Object[] { shell };

                try {
                    Constructor<? extends AbstractTimeMatchingDialog> constructor = clazz
                            .getConstructor(constructorClasses);
                    struct.dialog = constructor.newInstance(args);
                    struct.dialog.setDescriptor(descriptor);
                    struct.dialog.setLoadProperties(loadProps);
                    struct.dialog.setResourceData(availableTimes);
                    struct.dialog.setTimeMatcher(timeMatcher);
                    struct.dialog.init();
                } catch (SecurityException e) {
                    struct.exception = e;
                } catch (NoSuchMethodException e) {
                    struct.exception = e;
                } catch (IllegalArgumentException e) {
                    struct.exception = e;
                } catch (InstantiationException e) {
                    struct.exception = e;
                } catch (IllegalAccessException e) {
                    struct.exception = e;
                } catch (InvocationTargetException e) {
                    struct.exception = e;
                } catch (VizException e) {
                    struct.exception = e;
                }
            }
        });

        if (struct.exception != null) {
            throw new VizException(struct.exception);
        }

        return struct.dialog;
    }
}
