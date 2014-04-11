package com.raytheon.viz.ui.actions;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.internal.WorkbenchWindow;
import org.eclipse.ui.services.IEvaluationService;

import com.raytheon.viz.ui.VizWorkbenchManager;

public class ToggleToolbarContributionItem extends CompoundContributionItem {

    @Override
    protected IContributionItem[] getContributionItems() {
        ActionContributionItem toggleContributionItem = new ActionContributionItem(
                new Action("Toolbar", IAction.AS_CHECK_BOX) {
                    @Override
                    public boolean isChecked() {
                        IWorkbenchWindow activeWorkbenchWindow = VizWorkbenchManager
                                .getInstance().getCurrentWindow();
                        IEvaluationService service = (IEvaluationService) activeWorkbenchWindow
                                .getService(IEvaluationService.class);
                        IEvaluationContext appState = service.getCurrentState();
                        Boolean visible = (Boolean) appState
                                .getVariable(ISources.ACTIVE_WORKBENCH_WINDOW_IS_COOLBAR_VISIBLE_NAME);
                        return (visible != null ? visible : false);
                    }

                    @Override
                    public void run() {
                        IWorkbenchWindow activeWorkbenchWindow = VizWorkbenchManager
                                .getInstance().getCurrentWindow();
                        // ActionFactory.IWorkbenchAction toggleToolbar =
                        // ActionFactory.TOGGLE_COOLBAR
                        // .create(activeWorkbenchWindow);
                        // toggleToolbar.run();

                        if (activeWorkbenchWindow instanceof WorkbenchWindow) {
                            WorkbenchWindow window = (WorkbenchWindow) activeWorkbenchWindow;
                            window.toggleToolbarVisibility();
                        }
                    }

                });
        return new IContributionItem[] { toggleContributionItem };
    }

}
